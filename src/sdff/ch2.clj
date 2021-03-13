(ns sdff.ch2
  (:refer-clojure :exclude [iterate identity]))

(defn compose [f g]
  (fn [& args]
    (f (apply g args))))

(defn identity [x] x)

(defn iterate [n]
  (fn [f]
    (if (zero? n)
      identity
      (compose f ((iterate (dec n)) f)))))

(defn parallel-combine [h f g]
  (letfn [(the-combination [& args]
            (h (apply f args) (apply g args)))]
    the-combination))

(defn get-arity [f]
  (letfn [(fn-arity [f] (-> f .getParameterTypes alength))]
    (or (-> f meta :arity)
        (->> f class .getDeclaredMethods (map fn-arity) sort first))))

(defn restrict-arity [f arity]
  (with-meta (fn [& args]
               (when-not (= arity (count args))
                 (throw (clojure.lang.ArityException.
                         (count args)
                         (str "restricted arity (" arity ") fn of " f))))
               (apply f args))
    {:arity arity}))

(defn spread-combine [h f g]
  (let [n (get-arity f)
        m (get-arity g)
        t (+ n m)]
    (letfn [(the-combination [& args]
              (assert (= (count args) t))
              (h (apply f (take n args))
                 (apply g (drop n args))))]
      (restrict-arity the-combination t))))

(defn compose [f g]
  (let [n (get-arity g)
        m (get-arity f)]
    (when-not (= 1 m)
      (throw (clojure.lang.ArityException. m (str f))))
    (restrict-arity (fn [& args] (f (apply g args)))
                    n)))

(defn parallel-combine [h f g]
  (let [n (get-arity f)
        m (get-arity g)]
    (assert (= n m))
    (assert (= 2 (get-arity h)))
    (restrict-arity (fn [& args]
                      (h (apply f args) (apply g args)))
                    n)))
