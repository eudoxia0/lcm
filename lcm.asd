(defsystem lcm
  :author "Fernando Borretti <fernando@borretti.me>"
  :maintainer "Fernando Borretti <fernando@borretti.me>"
  :license "GPLv3"
  :version "0.1"
  :homepage "https://github.com/eudoxia0/lcm"
  :bug-tracker "https://github.com/eudoxia0/lcm/issues"
  :source-control (:git "git@github.com:eudoxia0/lcm.git")
  :depends-on (:file-attributes)
  :components ((:module "src"
                :serial t
                :components
                ((:file "package")
                 (:file "components")
                 (:file "secrets")
                 (:file "config")
                 (:file "state")
                 (:file "registry")
                 (:file "cli")
                 (:file "engine")
                 (:module "builtin"
                  :serial t
                  :components
                  ((:file "package")
                   (:file "file")
                   (:file "command")))
                 (:file "main"))))
  :description "System configuration manager for infrastructure-as-code in Common Lisp."
  :long-description
  #.(uiop:read-file-string
     (uiop:subpathname *load-pathname* "README.md"))
  :in-order-to ((test-op (test-op lcm-test))))
