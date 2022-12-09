(in-package :lcm)

(defgeneric execute (command)
  (:documentation "Execute a command."))

(defmethod execute ((command get-command))
  (format t "~A~%" (load-state)))

(defmethod execute ((command apply-command))
  'wip)

(defmethod execute ((command unapply-command))
  'wip)

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
