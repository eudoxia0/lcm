(in-package :lcm)

(defparameter *config-registry* (make-hash-table :test #'eq))

(defun register-configuration (config)
  (setf (gethash (configuration-name config) *config-registry*) config))
