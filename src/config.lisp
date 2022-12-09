(in-package :lcm)

(defclass configuration ()
  ((name :reader configuration-name
         :initarg :name
         :type symbol
         :documentation "The name of this configuration.")
   (secrets-template :reader configuration-secrets-template
                     :initarg :secrets-template
                     :type secrets-template
                     :documentation "The secrets this configuration requires."))
  (:documentation "A system configuration."))

(defgeneric configuration-components (configuration vault)
  (:documentation "Given a configuration and its secrets vault, return its component list."))
