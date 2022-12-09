(in-package :lcm)

(defclass component ()
  ((title :reader component-title
          :initarg :title
          :type string
          :documentation "The component's title. This is used to tell the user what component is being applied."))
  (:documentation "The base class of components. Components are the atoms that
  make up a configuration. A component can install a package, ensure a directory
  exists, ensure a file exists and has the correct contents and permissions,
  etc. Components can be applied and unapplied: for example, applying a package
  component installs the package, unapplying it removes the package. To improve
  performance, components can check whether they're already applied."))

(defgeneric component-applied-p (component vault)
  (:documentation "Is this component applied? This is used to ensure we don't do
  work unnecessarily.")

  (:method ((component component) vault)
    "Default implementation: assume the component is not applied."
    nil))

(defgeneric component-apply (component vault)
  (:documentation "Apply a component. Return T on success.")

  (:method ((component component) vault)
    "Default implementation: do nothing."
    t))

(defgeneric component-unapply (component vault)
  (:documentation "Unapply a component. Return T on success.")

  (:method ((component component) vault)
    "Default implementation: do nothing."
    t))

(defun apply-component-if-needed (component vault)
  "Apply a component if it's not already applied."
  (if (component-applied-p component vault)
      (format t "  [ SKIP] ~A~%" (component-title component))
      (progn
        (format t "  [APPLY] ~A~%" (component-title component))
        (component-apply component vault))))

(defun unapply-component-if-needed (component vault)
  "Unapply a component if it's already applied."
  (if (component-applied-p component vault)
      (progn
        (format t "  [UNAPPLY] ~A~%" (component-title component))
        (component-unapply component vault))
      (format t "  [   SKIP] ~A~%" (component-title component))))
