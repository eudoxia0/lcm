(in-package :lcm.builtin)

(defun chmod+x (pathname)
  (let ((attrs (list :other-execute nil
                     :other-write nil
                     :other-read t
                     :group-execute nil
                     :group-write t
                     :group-read t
                     :owner-execute t ;; here
                     :owner-write t
                     :owner-read t
                     :sticky nil
                     :set-group nil
                     :set-user nil
                     :fifo nil
                     :device nil
                     :directory nil
                     :normal t
                     :link nil
                     :socket nil)))
    (setf (attributes pathname) (encode-attributes attrs))))

(defclass file-component (component)
  ((path :reader component-path
         :initarg :path
         :type pathname
         :documentation "The path to the file.")
   (contents :reader component-contents
             :initarg :contents
             :type string
             :documentation "The file contents.")
   (executable :reader component-executable
               :initarg :executable
               :initform nil
               :type boolean
               :documentation "Whether the resulting file should be executable."))
  (:documentation "A component to create a file."))

(defmethod component-applied-p ((component file-component))
  "Applied when the file exists and its contents are identical to the string."
  (with-slots (path contents) component
    (and (probe-file path)
         (string= (uiop:read-file-string path) contents))))

(defmethod component-apply ((component file-component))
  (with-slots (path contents executable) component
    ;; Ensure the parent directories exist.
    (ensure-directories-exist (uiop:pathname-directory-pathname path))
    ;; Write the file.
    (with-open-file (stream path
                            :direction :output
                            :if-exists :supersede
                            :if-does-not-exist :create)
      (write-string contents stream))
    ;; chmod+x if needd
    (when executable
      (chmod+x path))))

(defmethod component-unapply ((component file-component))
  (delete-file (component-path component)))
