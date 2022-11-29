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
  ((name :reader :command-name
         :initarg :name
         :type symbol
         :documentation "The name of the configuration to apply."))
  (:documentation "Command to apply a configuration."))

(defclass unapply-command (command)
  ()
  (:documentation "Command to unapply the current configuration."))

;;;; Command execution.

(defgeneric execute (command)
  (:documentation "Execute a command."))

(defmethod execute ((command help-command))
  (declare (ignore command))
  (format t "lcm~%~%")
  (format t "Usage:~%")
  (format t "  lcm <command>~%~%")
  (format t "Commands:~%")
  (format t "  get                       Get the name of the current configuration.")
  (format t "  apply <name> <file...>    Apply a configuration~%")
  (format t "  unapply                   Unapply the current configuration~%")
  (format t "  help                      Print this text~%")
  (format t "  version                   Show the current version~%"))

(defmethod execute ((command version-command))
  (declare (ignore command))
  (format t "~A~%" +version+))

;;;; Command parsing.

(defun parse-cli (args)
  "Parse a list of command line arguments.")
