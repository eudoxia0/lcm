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

(defclass loaded-configuration (configuration)
  ((vault :reader configuration-vault
          :initarg :vault
          :type vault
          :documentation "The loaded secrets vault."))
  (:documentation "A configuration whose secrets have been loaded and are ready to load."))

(defgeneric configuration-components (configuration)
  (:documentation "Given a LOADED-CONFIGURATION, return its component list."))
