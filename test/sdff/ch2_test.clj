(ns sdff.ch2-test
  (:require
   [clojure.test :refer [deftest testing is]]
   [sdff.ch2 :as ch2]))

(deftest compose-test
  (is (= '(foo (bar z))
         ((ch2/compose (fn [x] (list 'foo x))
                       (fn [x] (list 'bar x)))
          'z))))

(defn square [x] (* x x))

(deftest iterate-test
  (is (= 390625 (((ch2/iterate 3) square) 5))))

(deftest parallel-combine-test
  (is (= '((foo a b c) (bar a b c))
         ((ch2/parallel-combine (fn [x y] (list x y))
                                (fn [x y z] (list 'foo x y z))
                                (fn [u v w] (list 'bar u v w)))
          'a 'b 'c))))

(deftest get-arity-test
  (is (= 0 (ch2/get-arity (fn [] nil))))
  (is (= 1 (ch2/get-arity (fn [a] nil))))
  (is (= 2 (ch2/get-arity (fn [a b] nil))))
  (is (= 3 (ch2/get-arity (fn [a b c] nil))))
  (is (= 0 (ch2/get-arity (fn [& args] nil)))))

(deftest restrict-arity-test
  (is (= 0 (ch2/get-arity (ch2/restrict-arity (fn [a b c] nil) 0))))
  (is (= 0 (ch2/get-arity (ch2/restrict-arity (fn [& args] nil) 0))))
  (is (= 1 (ch2/get-arity (ch2/restrict-arity (fn [& args] nil) 1))))
  (is (= 2 (ch2/get-arity (ch2/restrict-arity (fn [& args] nil) 2))))
  (is (= 3 (ch2/get-arity (ch2/restrict-arity (fn [& args] nil) 3))))
  (is (= 0 ((ch2/restrict-arity + 0))))
  (is (= 4 ((ch2/restrict-arity + 1) 4)))
  (is (= 5 ((ch2/restrict-arity + 2) 2 3)))
  (is (thrown? clojure.lang.ArityException ((ch2/restrict-arity + 0) 1)))
  (is (thrown? clojure.lang.ArityException ((ch2/restrict-arity + 1))))
  (is (thrown? clojure.lang.ArityException ((ch2/restrict-arity + 1) 1 2)))
  (is (thrown? clojure.lang.ArityException ((ch2/restrict-arity + 2))))
  (is (thrown? clojure.lang.ArityException ((ch2/restrict-arity + 2) 1)))
  (is (thrown? clojure.lang.ArityException ((ch2/restrict-arity + 2) 1 2 3))))

(deftest spread-combine-test
  (is (= '((foo a b) (bar c d e))
         ((ch2/spread-combine list
                              (fn [x y] (list 'foo x y))
                              (fn [u v w] (list 'bar u v w)))
          'a 'b 'c 'd 'e))))

(deftest ex2.1-test
  (testing 'sdff.ch2/compose
    (testing "Checks arity of component fns"
      (is (thrown? clojure.lang.ArityException
                   (ch2/compose (fn [a b] (+ a b)) (fn [x] x))))
      (is (thrown? clojure.lang.ArityException
                   (ch2/compose (fn [a b c] a) (fn [x] x))))
      (is (thrown? clojure.lang.ArityException
                   ((ch2/compose - (fn [y] (* 5 y))) 1)))
      (is (= -5 ((ch2/compose (fn [x] (- x)) (fn [y] (* 5 y))) 1))))

    (testing "Combined fn checks number of args"
      (is (thrown? clojure.lang.ArityException
                   ((ch2/compose (fn [x] (- x)) (fn [u v] (* u v))) 2 3 4)))
      (is (= -6 ((ch2/compose (fn [x] (- x)) (fn [u v] (* u v))) 2 3))))

    (testing "Combined fn advertises correct arity"
      (is (= 0 (ch2/get-arity
                (ch2/compose (fn [x] (- x))
                             (fn [] (rand))))))
      (is (= 1 (ch2/get-arity
                (ch2/compose (fn [x] (- x))
                             (fn [u] (* u u))))))
      (is (= 2 (ch2/get-arity
                (ch2/compose (fn [x] (- x))
                             (fn [u v] (* u v))))))
      (is (= 3 (ch2/get-arity
                (ch2/compose (fn [x] (- x))
                             (fn [u v w] (* u v w))))))))

  (testing 'sdff.ch2/parallel-combine
    (testing "Checks arity of component functions"
      (is (thrown? java.lang.AssertionError
                   (ch2/parallel-combine (fn [x y] (* x y))
                                         (fn [a] (- a))
                                         (fn [b c] (+ b c)))))
      (is (thrown? java.lang.AssertionError
                   (ch2/parallel-combine (fn [x y] (* x y))
                                         (fn [b c] (+ b c))
                                         (fn [a] (- a)))))
      (is (thrown? java.lang.AssertionError
                   (ch2/parallel-combine (fn [x y z] (* x y z))
                                         (fn [a] (- a))
                                         (fn [b] (+ b)))))
      (is (= -16 ((ch2/parallel-combine (fn [x y] (* x y))
                                        (fn [a] (- a))
                                        (fn [b] (+ b))) 4))))

    (testing "Combined fn checks number of args"
      (is (thrown? clojure.lang.ArityException
                   ((ch2/parallel-combine (fn [x y] (* x y))
                                          (fn [a] (- a))
                                          (fn [b] (+ b)))
                    4 5 6)))
      (is (= -36 ((ch2/parallel-combine (fn [x y] (* x y))
                                        (fn [a b c] (- 0 a b c))
                                        (fn [x y z] (+ x y z)))
                  1 2 3))))

    (testing "Combined fn advertises correct arity"
      (is (= 1 (ch2/get-arity
                (ch2/parallel-combine (fn [x y] (* x y))
                                      (fn [a] (- a))
                                      (fn [b] (+ b))))))
      (is (= 2 (ch2/get-arity
                (ch2/parallel-combine (fn [x y] (* x y))
                                      (fn [a b] (- 0 a b))
                                      (fn [x y] (+ x y))))))
      (is (= 3 (ch2/get-arity
                (ch2/parallel-combine (fn [x y] (* x y))
                                      (fn [a b c] (- 0 a b c))
                                      (fn [x y z] (+ x y z)))))))))
