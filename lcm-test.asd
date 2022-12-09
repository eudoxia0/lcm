(defsystem lcm-test
  :author "Fernando Borretti <fernando@borretti.me>"
  :license "GPLv3"
  :depends-on (:lcm
               :parachute)
  :components ((:module "t"
                :serial t
                :components
                ((:file "lcm")))))
