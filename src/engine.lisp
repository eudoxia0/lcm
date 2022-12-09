(in-package :lcm)

(defgeneric execute (command)
  (:documentation "Execute a command."))

(defmethod execute ((command get-command))
  (format t "~A~%" (load-state)))

(defun load-secrets-if-needed (command config)
  (let ((template (configuration-secrets-template config)))
    (if (> (secrets-template-keys template) 0)
        (if (command-secrets command)
            ;; Secrets required, and provided.
            (load-secrets template (command-secrets command))
            ;; Secrets required, not provided.
            (error "This configuration requires secrets, but the --secrets flag was not provided."))
        (if (command-secrets command)
            ;; No secrets required, but a path was provided.
            (error "This configuration doesn't require secrets, but a value was provided for --secrets.")
            ;; No secrets required, none provided.
            (make-instance 'vault :table (make-hash-table :test #'string=))))))

(defun apply-loaded-config (config vault)
  (loop for component in (configuration-components config) do
    (apply-component-if-needed config vault)))

(defmethod execute ((command apply-command))
  ;; Load the Lisp files in order.
  (loop for pathname in (command-files command) do
    (load pathname))
  ;; Find the configuration with the given name.
  (let ((config (get-configuration (command-name command))))
    ;; Using the secrets template from the configuration, load the secrets file, if any.
    (let ((vault (load-secrets-if-needed command config)))
      ;; Apply the configuration.
      (apply-loaded-config config vault)
      ;; Write the state.
      (write-state (command-name command)))))

(defun unapply-config (config vault)
  (loop for component in (configuration-components config) do
    (unapply-component-if-needed config vault)))

(defmethod execute ((command unapply-command))
  ;; Load the Lisp files in order.
  (loop for pathname in (command-files command) do
    (load pathname))
  ;; Load the configuration name from the state.
  (let ((name (load-state)))
    ;; Find the configuration with this name.
    (let ((get-configuration name))
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
