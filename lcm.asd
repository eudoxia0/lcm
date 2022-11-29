(defsystem lcm
  :author "Fernando Borretti <fernando@borretti.me>"
  :maintainer "Fernando Borretti <fernando@borretti.me>"
  :license "GPLv3"
  :version "0.1"
  :homepage "https://github.com/eudoxia0/lcm"
  :bug-tracker "https://github.com/eudoxia0/lcm/issues"
  :source-control (:git "")
  :depends-on ()
  :components ((:module "src"
                :serial t
                :components
                ((:file "package"))))
  :description "System configuration manager for infrastructure-as-code in Common Lisp."
  :long-description
  #.(uiop:read-file-string
     (uiop:subpathname *load-pathname* "README.md"))
  :in-order-to ((test-op (test-op lcm-test))))
