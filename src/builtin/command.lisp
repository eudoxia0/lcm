(in-package :lcm.builtin)

(defcomponent command-component (component)
  ((forward :reader component-forward
            :initarg :forward
            :type string
            :documentation "The command to run when the component is applied.")
   (backward :reader component-backward
             :initarg :backward
             :type string
             :documentation "The command to run when the component is unapplied."))
  :documentation "A component to run a shell command."

  :appliedp (((component file-component))
             (declare (ignore component))
             nil)

  :apply (((component file-component))
          (with-slots (forward) component
            (uiop:run-program forward :ignore-error-status t)))

  :unapply (((component file-component))
            (with-slots (backward) component
              (uiop:run-program backward :ignore-error-status t))))
