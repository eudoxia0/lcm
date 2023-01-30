(in-package :lcm.builtin)

(defclass command-component (component)
  ((forward :reader component-forward
            :initarg :forward
            :type string
            :documentation "The command to run when the component is applied.")
   (backward :reader component-backward
             :initarg :backward
             :type string
             :documentation "The command to run when the component is unapplied."))
  (:documentation "A component to run a shell command."))

(defmethod component-applied-p ((component command-component))
  (declare (ignore component))
  nil)

(defmethod component-apply ((component command-component))
  (with-slots (forward) component
    (uiop:run-program forward :ignore-error-status t)))

(defmethod component-unapply ((component command-component))
  (with-slots (backward) component
    (uiop:run-program backward :ignore-error-status t))))
