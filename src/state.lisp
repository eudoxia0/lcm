(in-package :lcm)

(defun data-directory ()
  (uiop:merge-pathnames* #p"lcm" (uiop:xdg-data-home)))

(defun path-to-state-file ()
  (uiop:merge-pathnames* #p"state.sexp" (data-directory)))

(defun write-state (name)
  (ensure-directories-exist (data-directory))
  (with-open-file (stream (path-to-state-file)
                          :direction :output
                          :if-does-not-exist :create
                          :if-exists :overwrite)
    (write name :stream stream)))

(defun load-state ()
  (let ((path (path-to-state-file)))
    (if (probe-file path)
        (with-open-file (stream path
                                :direction :input)
          (read-line stream nil))
        nil)))

(defun delete-state ()
  (delete-file (path-to-state-file)))
