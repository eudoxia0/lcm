(in-package :cl-user)
(defpackage lcm-test
  (:use :cl :fiveam)
  (:export :run-tests))
(in-package :lcm-test)

(def-suite tests
  :description "lcm tests.")
(in-suite tests)

(test simple-test
  (is
   (equal 1 1))
  (is-true
   (and t t)))

(defun run-tests ()
  (run! 'tests))
