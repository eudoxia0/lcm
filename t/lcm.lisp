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

(define-test version-command
  :parent lcm
  (let ((cmd (lcm::parse-cli (list "version"))))
    (of-type cmd 'lcm::version-command)))

(define-test get-command
  :parent lcm
  (let ((cmd (lcm::parse-cli (list "get"))))
    (of-type cmd 'lcm::get-command)))

(define-test apply-command
  :parent lcm
  (let ((cmd (lcm::parse-cli (list "apply bar foo.lisp"))))
    (of-type cmd 'lcm::get-command)
    (is string= (command-name cmd) 'bar)
    (is equal (command-files cmd) (list "foo.lisp"))
    (is equal (command-secrets cmd) nil))
  (let ((cmd (lcm::parse-cli (list "apply bar foo.lisp --secrets=derp.sexp"))))
    (of-type cmd 'lcm::get-command)
    (is string= (command-name cmd) 'bar)
    (is equal (command-files cmd) (list "foo.lisp"))
    (is equal (command-secrets cmd) "derp.sexp")))

(define-test unapply-command
  :parent lcm
  (let ((cmd (lcm::parse-cli (list "unapply foo.lisp"))))
    (of-type cmd 'lcm::get-command)
    (is equal (command-files cmd) (list "foo.lisp"))
    (is equal (command-secrets cmd) nil))
  (let ((cmd (lcm::parse-cli (list "unapply foo.lisp --secrets=derp.sexp"))))
    (of-type cmd 'lcm::get-command)
    (is equal (command-files cmd) (list "foo.lisp"))
    (is equal (command-secrets cmd) "derp.sexp")))

;;;; Interface

(defun run-tests ()
  (test 'lcm))
