(in-package :lcm)

(defun entrypoint ()
  (execute (parse-cli (uiop:command-line-arguments))))
