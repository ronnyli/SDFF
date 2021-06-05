(ns sdff.ch2
  (:refer-clojure :exclude [iterate identity])
  (:require
   [clojure.string :as str]
   [clojure.java.shell :as shell])
  (:import
   (java.io File)))

(declare compose)

(defn identity [x] x)

(defn iterate [n]
  (fn [f]
    (if (zero? n)
      identity
      (compose f ((iterate (dec n)) f)))))

(defn get-arity [f]
  (letfn [(fn-arity [f] (-> f .getParameterTypes alength))
          (vararg? [f]
            (let [c (class f)]
              (and
               (pos? (alength (.getDeclaredMethods c)))
               (#(not (Double/isNaN %))
                (try (.getRequiredArity f)
                     (catch java.lang.IllegalArgumentException e ##NaN))))))]
    (if-let [arity (:arity (meta f))]
      arity
      (if (vararg? f)
        ##NaN
        (->> f class .getDeclaredMethods (map fn-arity) sort first)))))

(defn restrict-arity [f arity]
  (with-meta (fn [& args]
               (let [nargs (count args)]
                 (if (or (Double/isNaN arity) (= arity nargs))
                   (apply f args)
                   (throw (clojure.lang.ArityException.
                           (count args)
                           (str "restricted arity (" arity ") fn of " f))))))
    {:arity arity}))

(deftype Values [values]
  clojure.lang.ISeq
  (seq [this] (seq values))
  java.lang.Object
  (toString [this] (str (seq this))))

(defn values? [x] (instance? Values x))
(defn values [& args]
  (->Values args))

(defn append-values [a b]
  (cond (and (values? a) (values? b)) (apply values (concat a b))
        (values? a) (apply values (conj (vec a) b))
        (values? b) (apply values (conj (seq b) a))
        :else (values a b)))

(defn compose
  ([f g]
   (let [n (get-arity g)
         m (get-arity f)]
     (letfn [(composable-value? [result]
               (or (Double/isNaN m) (= 1 m)
                   (and (values? result) (= m (count result)))))
             (composed-fn [& args]
               (let [result (apply g args)]
                 (when-not (composable-value? result)
                   (throw (clojure.lang.ArityException. m (str f))))
                 (if (values? result)
                   (apply f result)
                   (f result))))]
       (restrict-arity composed-fn n))))
  ([f g & hs]
   (if (> (count hs) 1)
     (recur (compose f g) (first hs) (rest hs))
     (compose (compose f g) (first hs)))))

(defn parallel-apply [f g]
  (let [n (get-arity f)
        m (get-arity g)
        resulting-arity (if (Double/isNaN n) m n)]
    (assert (or (= n m) (Double/isNaN n) (Double/isNaN m)))
    (restrict-arity (fn [& args]
                      (append-values (apply f args) (apply g args)))
                    resulting-arity)))

(defn parallel-combine [h f g]
  (compose h (parallel-apply f g)))

(defn spread-apply [f g]
  (let [n (get-arity f)
        m (get-arity g)
        t (+ n m)
        nargs-for-f
        (cond (and (Double/isNaN n) (Double/isNaN m)) #(quot % 2)
              (Double/isNaN n) #(- % m)
              :else (constantly n))]
    (letfn [(the-combination [& args]
              (let [nargs (count args)
                    n (nargs-for-f nargs)]
                (assert (or (Double/isNaN t) (= nargs t)))
                (append-values (apply f (take n args))
                               (apply g (drop n args)))))]
      (restrict-arity the-combination t))))

(defn spread-combine [h f g]
  (compose h (spread-apply f g)))

(defn remove-indices [coll indices]
  (let [indices (set indices)]
    (transduce (comp (map-indexed (fn [& args] args))
                     (remove #(contains? indices (first %)))
                     (map second))
               conj
               coll)))

(defn discard-argument-indices [& indices]
  (assert (every? nat-int? indices))
  (fn [& args]
    (apply values (remove-indices args indices))))

(defn discard-argument [& indices]
  (let [discard-args (apply discard-argument-indices indices)]
    (fn [f]
      (let [m (inc (get-arity f))]
        (assert (or (Double/isNaN m) (every? #(< % m) indices)))
        (compose f discard-args)))))

(defn insert-at [coll idx value]
  (concat (take idx coll) [value] (drop idx coll)))

(defn curry-argument-indices [& indices]
  (fn [& args]
    (fn [& xs]
      (let [args (reduce (fn [accum [idx arg]]
                           (insert-at accum idx arg))
                         args
                         (map vector indices xs))]
        (apply values args)))))

(defn curry-argument [& indices]
  (fn [& args]
    (let [curry-args (apply (apply curry-argument-indices indices) args)]
      (fn [f]
        (assert (= (count args) (- (get-arity f) (count indices))))
        (compose f curry-args)))))

(defn make-permutation [permspec]
  (fn [coll]
    (let [coll (vec coll)]
      (map (fn [idx] (nth coll idx))
           permspec))))

(defn permute-argument-indices [& permspec]
  (let [permute (make-permutation permspec)]
    (fn [& args]
      (apply values (permute args)))))

(defn permute-arguments [& permspec]
  (let [permute-args (apply permute-argument-indices permspec)]
    (fn [f]
      (assert (= (get-arity f) (count permspec)))
      (compose f permute-args))))

(def r:dot ".")
(def r:bol "^")
(def r:eol "$")

(defn r:seq [& exprs]
  (str "\\(" (apply str exprs) "\\)"))

(def chars-needing-quoting #{\. \[ \\ \^ \$ \*})

(defn r:quote [string]
  (r:seq
   (apply str
    (mapcat (fn [char]
              (if (contains? chars-needing-quoting char)
                (list \\ char)
                (list char)))
            (seq string)))))

(defn r:alt [& exprs]
  (if (seq exprs)
    (apply r:seq
           (conj (mapcat (fn [expr] ["\\|" expr])
                         (rest exprs))
                 (first exprs)))
    (r:seq)))

(defn r:repeat [min max expr]
  (let [open "\\{"
        close "\\}"]
    (apply r:seq
           (concat expr
                   (case max
                     false (list open min "," close)
                     min ()
                     (list open min "," max close))))))

(defn bracket [string procedure]
  (apply str
         (concat '(\[)
                 (procedure (seq string))
                 '(\]))))

(def chars-needing-quoting-in-brackets #{\] \^ \-})

(defn quote-bracketed-contents [members]
  (let [members (into #{} members)]
    (letfn [(optional [char]
              (if (contains? members char)
                (list char)
                ()))]
      (concat (optional \])
              (remove chars-needing-quoting-in-brackets members)
              (optional \^)
              (optional \-)))))

(defn r:char-not-from [string]
  (bracket string
           (fn [members]
             (conj (quote-bracketed-contents members) \^))))

(defn r:char-from [string]
  (case (count string)
    0 (r:seq)
    1 (r:quote string)
    (bracket string
             (fn [members]
               (if (= #{\- \^} members)
                 #{\- \^}
                 (quote-bracketed-contents members))))))

(defn bourne-shell-quote-string [string]
  (apply str
         (mapcat (fn [char]
                   (if (= char \')
                     (list \' \\ char \')
                     (list char)))
                 string)))

(defn bourne-shell-grep-command-string [expr filename]
  (str "grep --basic-regexp"
       " --regexp=" (bourne-shell-quote-string expr)
       " "
       filename))

(defn write-bourne-shell-grep-command [expr filename]
  (println (bourne-shell-grep-command-string expr filename)))

(defn grep [expr filename]
  (let [filename (.getAbsolutePath (File. filename))]
    (-> (apply shell/sh
               (str/split (bourne-shell-grep-command-string expr filename)
                          #"\s+"))
        :out
        (str/split #"\n"))))

(def r:* (((curry-argument 2) 0 false) r:repeat))

(def r:+ (((curry-argument 2) 1 false) r:repeat))

