(in-package :lcm)

(defun entrypoint ()
  (format t "Hello, world!~%")
  (format t "~A" (uiop:command-line-arguments))
  nil)
