(in-package :cl-user)
(defpackage lcm.builtin
  (:use :cl :lcm)
  (:export :file-component
           :component-path
           :component-contents
           :component-executable
           :make-file-component
           :command-component
           :component-forward
           :component-backward)
  (:documentation "Built-in LCM components."))
