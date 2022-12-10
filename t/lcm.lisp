(in-package :cl-user)
(defpackage lcm-test
  (:use :cl :parachute)
  (:export :run-tests :run-tests-and-die))
(in-package :lcm-test)

(define-test suite)

;;;; Secrets.

(define-test hash-table-keys-tests
  :parent suite
  (let ((table (make-hash-table :test 'equal)))
    (setf (gethash "A" table) 1
          (gethash "B" table) 2
          (gethash "C" table) 3)
    (let ((keys (sort (lcm::hash-table-keys table) #'string<)))
      (is = (length keys) 3)
      (is string= (nth 0 keys) "A")
      (is string= (nth 1 keys) "B")
      (is string= (nth 2 keys) "C"))))

;;;; Command line parsing.

(define-test help-command
  :parent suite
  (let ((cmd (lcm::parse-cli (list "help"))))
    (of-type 'lcm::help-command cmd)))

(define-test version-command
  :parent suite
  (let ((cmd (lcm::parse-cli (list "version"))))
    (of-type 'lcm::version-command cmd)))

(define-test get-command
  :parent suite
  (let ((cmd (lcm::parse-cli (list "get"))))
    (of-type 'lcm::get-command cmd)))

(define-test apply-command
  :parent suite
  (let ((cmd (lcm::parse-cli (list "apply" "bar" "foo.lisp"))))
    (of-type 'lcm::apply-command cmd)
    (is eq (lcm::command-name cmd) "bar")
    (is equal (lcm::command-files cmd) (list "foo.lisp"))
    (is equal (lcm::command-secrets cmd) nil))
  (let ((cmd (lcm::parse-cli (list "apply" "bar" "foo.lisp" "--secrets=derp.sexp"))))
    (of-type 'lcm::apply-command cmd)
    (is eq (lcm::command-name cmd) "bar")
    (is equal (lcm::command-files cmd) (list "foo.lisp"))
    (is equal (lcm::command-secrets cmd) "derp.sexp")))

(define-test unapply-command
  :parent suite
  (let ((cmd (lcm::parse-cli (list "unapply" "foo.lisp"))))
    (of-type 'lcm::unapply-command cmd)
    (is equal (lcm::command-files cmd) (list "foo.lisp"))
    (is equal (lcm::command-secrets cmd) nil))
  (let ((cmd (lcm::parse-cli (list "unapply" "foo.lisp" "--secrets=derp.sexp"))))
    (of-type 'lcm::unapply-command cmd)
    (is equal (lcm::command-files cmd) (list "foo.lisp"))
    (is equal (lcm::command-secrets cmd) "derp.sexp")))

;;;; Interface

(defun run-tests ()
  (test 'suite))

(defun run-tests-and-die ()
  (let ((result (run-tests)))
    (if (eq (status result) :failed)
        (uiop:quit -1)
        (uiop:quit 0))))
