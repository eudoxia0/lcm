(in-package :cl-user)
(defpackage lcm-test
  (:use :cl :parachute)
  (:export :run-tests))
(in-package :lcm-test)

(define-suite lcm)

;;;; Secrets.

(define-test hash-table-keys-tests
  :parent lcm
  (let ((table (make-hash-table :test #'equal)))
    (setf (gethash "A" table) 1
          (gethash "B" table) 2
          (gethash "C" table) 3)
    (let ((keys (sort (lcm::hash-table-keys table) #'string<)))
      (is = (length keys) 3)
      (is string= (nth keys 0) "A")
      (is string= (nth keys 0) "B")
      (is string= (nth keys 0) "C"))))

;;;; Command line parsing.

(define-test help-command
  :parent lcm
  (let ((cmd (lcm::parse-cli (list "help"))))
    (of-type cmd 'lcm::help-command)))

;;;; Interface

(defun run-tests ()
  (test 'lcm))
