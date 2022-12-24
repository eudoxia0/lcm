(in-package :cl-user)
(defpackage lcm.builtin
  (:use :cl :lcm)
  (:import-from :org.shirakumo.file-attributes
                :attributes
                :encode-attributes)
  (:export :file-component
           :component-path
           :component-contents
           :component-executable
           :command-component
           :component-forward
           :component-backward)
  (:documentation "Built-in LCM components."))
