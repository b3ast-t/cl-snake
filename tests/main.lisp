(defpackage cl-snake/tests/main
  (:use :cl
        :cl-snake
        :rove))
(in-package :cl-snake/tests/main)

;; NOTE: To run this test file, execute `(asdf:test-system :cl-snake)' in your Lisp.

(deftest test-target-1
  (testing "should (= 1 1) to be true"
    (ok (= 1 1))))
