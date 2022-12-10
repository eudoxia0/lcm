(in-package :lcm)

(defmacro defcomponent (name superclass-list slots &key documentation appliedp apply unapply)
  "Shorthand for defining a component."
  `(progn
     (defclass ,name ,superclass-list
       ,slots
       (:documentation ,documentation))

     (defmethod component-applied-p ,@appliedp)

     (defmethod component-apply ,@apply)

     (defmethod component-unapply ,@unapply)))

(defmacro defconfig (name &key secrets components)
  "Shorthand for defining a system configuration."
  `(progn


     (defclass ,name (configuration)
       ())

     (defmethod configuration-components ,@components)

     (register-configuration
      (make-instance ',name
                     :name name
                     :secrets-template (make-secrets ,@secrets)))))
