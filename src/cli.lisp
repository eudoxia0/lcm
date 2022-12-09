(in-package :lcm)

(defparameter +version+ "0.0.1")

;;;; Classes to represent CLI commands.

(defclass command ()
  ()
  (:documentation "The base class of CLI commands."))

(defclass help-command (command)
  ()
  (:documentation "Command to print usage information."))

(defclass version-command (command)
  ()
  (:documentation "Command to print version information."))

(defclass get-command (command)
  ()
  (:documentation "Command to get the name of the current configuration."))

(defclass apply-command (command)
  ((name :reader command-name
         :initarg :name
         :type symbol
         :documentation "The name of the configuration to apply.")
   (files :reader command-files
          :initarg :files
          :type list
          :documentation "The list of files to load in order.")
   (secrets :reader command-secrets
            :initarg :secrets
            :type (or null string)
            :documentation "The path to the secrets file, if any."))
  (:documentation "Command to apply a configuration."))

(defclass unapply-command (command)
  ((files :reader command-files
          :initarg :files
          :type list
          :documentation "The list of files to load in order.")
   (secrets :reader command-secrets
            :initarg :secrets
            :type (or null string)
            :documentation "The path to the secrets file, if any."))
  (:documentation "Command to unapply the current configuration."))

;;;; Command parsing.

(defun starts-with-p (string prefix)
  "Test whether STRING starts with the substring PREFIX. If PREFIX is longer than STRING, returns NIL."
  (if (> (length prefix) (length string))
      nil
      (string= (subseq string 0 (length prefix)) prefix)))

(defun secretsp (string)
  "Test whether STRING starts with the substring '--secrets='."
  (starts-with-p string "--secrets="))

(defun secrets-value (string)
  (starts-with-p string "--secrets="))

(defun find-last-secrets (strings)
  "Find the value of the `--secrets=` flag in the argument list. NIL otherwise. If there are multiple flags, return the last one."
  (let ((result nil))
    (dolist (string strings result)
      (when (secretsp string)
        (setq result (secrets-value string))))))

(defun file-args (strings)
  "Return a list of all strings in the list of strings STRINGS that don't satisfy the 'secretsp' predicate."
  (loop for string in strings
        unless (secretsp string)
          collect string))

(defun parse-cli (args)
  "Parse a list of command line arguments."
  (if (null args)
      ;; Empty list: print help
      (make-instance 'help-command)
      ;; At least one argument
      (let ((first (first args)))
        (cond ((string= first "get")
               (make-instance 'get-command))
              ((string= first "apply")
               (handler-case
                   (parse-apply-args args)
                 (error ()
                   (format t "Bad `apply` command line arguments.~%")
                   (uiop:quit -1))))
              ((string= first "unapply")
               (handler-case
                   (parse-unapply-args args)
                 (error ()
                   (format t "Bad `unapply` command line arguments.~%")
                   (uiop:quit -1))))
              ((string= first "help")
               (make-instance 'help-command))
              ((string= first "version")
               (make-instance 'version-command))
              (t
               (make-instance 'help-command))))))

(defun parse-apply-args (args)
  (destructuring-bind (name &rest args)
      (rest args)
    (let ((files (file-args args))
          (secrets (find-last-secrets args)))
      (make-instance 'apply-command
                     :name (let ((name (read-from-string name)))
                             (if (symbolp name)
                                 name
                                 (error "Name is not a symbol.")))
                     :files files
                     :secrets secrets))))

(defun parse-unapply-args (args)
  (let ((files (file-args args))
        (secrets (find-last-secrets args)))
    (make-instance 'unapply-command
                   :files files
                   :secrets secrets)))
