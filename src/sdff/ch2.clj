(ns sdff.ch2
  (:refer-clojure :exclude [iterate identity]))

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

(defn spread-combine [h f g]
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
                (h (apply f (take n args))
                   (apply g (drop n args)))))]
      (restrict-arity the-combination t))))

(defn compose [f g]
  (let [n (get-arity g)
        m (get-arity f)]
    (when-not (or (= 1 m) (Double/isNaN m))
      (throw (clojure.lang.ArityException. m (str f))))
    (restrict-arity (fn [& args] (f (apply g args)))
                    n)))

(defn parallel-combine [h f g]
  (let [n (get-arity f)
        m (get-arity g)
        o (get-arity h)]
    (assert (or (= n m) (Double/isNaN n) (Double/isNaN m)))
    (assert (or (= 2 o) (Double/isNaN o)))
    (restrict-arity (fn [& args]
                      (h (apply f args) (apply g args)))
                    (if (Double/isNaN n)
                      m
                      n))))
