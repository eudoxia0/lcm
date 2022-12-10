(in-package :lcm)

(defgeneric execute (command)
  (:documentation "Execute a command."))

(defmethod execute ((command get-command))
  (format t "~A~%" (load-state)))

(defun load-secrets-if-needed (command config)
  (let ((template (configuration-secrets-template config)))
    (if (> (length (secrets-template-keys template)) 0)
        (if (command-secrets command)
            ;; Secrets required, and provided.
            (load-secrets template (command-secrets command))
            ;; Secrets required, not provided.
            (error "This configuration requires secrets, but the --secrets flag was not provided."))
        (if (command-secrets command)
            ;; No secrets required, but a path was provided.
            (error "This configuration doesn't require secrets, but a value was provided for --secrets.")
            ;; No secrets required, none provided.
            (make-instance 'vault :table (make-hash-table :test 'string=))))))

(defun apply-config (config vault)
  (loop for component in (configuration-components config vault) do
    (apply-component-if-needed config)))

(defmethod execute ((command apply-command))
  ;; Is a configuration already applied?
  (let ((state (load-state)))
    (when state
      (format t "A configuration is already applied: ~A~%" state)
      (uiop:quit -1)))
  ;; Load the Lisp files in order.
  (loop for pathname in (command-files command) do
    (format t "Loading ~A...~%" pathname)
    (load pathname))
  ;; Read the name as a symbol. This has to be done after loading the files, in
  ;; case the symbol is in a package defined in those files.
  (let ((name (read-from-string (command-name command))))
    (check-type name symbol)
    ;; Find the configuration with the given name.
    (let ((config (get-configuration name)))
      ;; Using the secrets template from the configuration, load the secrets file, if any.
      (let ((vault (load-secrets-if-needed command config)))
        ;; Apply the configuration.
        (apply-config config vault)
        ;; Write the state.
        (write-state name)))))

(defun unapply-config (config vault)
  (loop for component in (configuration-components config vault) do
    (unapply-component-if-needed config)))

(defmethod execute ((command unapply-command))
  ;; Load the Lisp files in order.
  (loop for pathname in (command-files command) do
    (format t "Loading ~A...~%" pathname)
    (load pathname))
  ;; Load the configuration name from the state.
  (let ((name (load-state)))
    ;; Find the configuration with this name.
    (let ((config (get-configuration name)))
      ;; Using the secrets template from the configuration, load the secrets file, if any.
      (let ((vault (load-secrets-if-needed command config)))
        ;; Unapply the configuration.
        (unapply-config config vault)
        ;; Delete the state.
        (delete-state)))))

(defmethod execute ((command help-command))
  (declare (ignore command))
  (format t "lcm~%~%")
  (format t "Usage:~%")
  (format t "  lcm <command>~%~%")
  (format t "Commands:~%")
  (format t "  get~%")
  (format t "    Get the name of the current configuration.~%~%")
  (format t "  apply <name> <file...> [--secrets=<file>]~%")
  (format t "    Apply a configuration. Load the given Lisp files in order,~%    optionally load a secrets file.~%~%")
  (format t "  unapply <file...> [--secrets=<file>]~%")
  (format t "    Unapply the current configuration. Load the given Lisp files~%    in order, optionally load a secrets file.~%~%")
  (format t "  help~%")
  (format t "    Print this text.~%~%")
  (format t "  version~%")
  (format t "    Show the current version.~%"))

(defmethod execute ((command version-command))
  (declare (ignore command))
  (format t "~A~%" +version+))
