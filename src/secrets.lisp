(in-package :lcm)

(defclass secrets-template ()
  ((keys :reader secrets-template-keys
         :initarg :keys
         :type list
         :documentation "A list of strings, the list of keys the secrets plist must provide."))
  (:documentation "A secrets template tells us the set of keys that a secrets plist must provide."))

(defclass vault ()
  ((table :reader vault-table
          :initarg :table
          :type hash-table
          :documentation "The hash table that stores the secrets."))
  (:documentation "A secrets vault maps secret keys to values."))

(defun get-secret (vault key)
  "Get a secret from a vault by key."
  (gethash key (vault-table vault)))

(defun hash-table-keys (table)
  (loop for key being the hash-key of table using (hash-value value)
    collect key))

(defun alist-keys (alist)
  "Iterate over an alist, returning the list of keys."
  (loop for (key . value) in alist collecting key))

(defun secrets-alist-to-vault (template alist)
  "Convert an alist of secrets to a vault instance, doing all necessary checks."
  ;; Get the lists of keys.
  (let ((template-keys (secrets-template-keys template))
        (secrets-keys (alist-keys alist)))
    ;; Check: are there keys that are in the template and not in the secrets?
    (let ((missing-keys (set-difference template-keys secrets-keys :test #'string=)))
      (when missing-keys
        (error "The following keys are present in the secrets template, but were not provided in the secrets file: ~A" missing-keys)))
    ;; Check: are there keys in the secrets file, that are not in the template?
    (let ((extra-keys (set-difference secrets-keys template-keys :test #'string=)))
      (when extra-keys
        (error "The following keys are in the secrets file, but are not part of the secrets template: ~A" extra-keys)))
    ;; Construct a vault instance.
    (let ((table (make-hash-table :test #'equal)))
      (loop for (key . value) in alist do
        (setf (gethash key table) value))
      (make-instance 'vault :table table))))

(defun validate-secrets-alist (form)
  "Given a Lisp form, ensure it is an alist of strings to strings."
  (unless (listp form)
    (error "Secrets form must be a list."))
  (loop for elem in form do
    (unless (consp elem)
      (error "Every element in the secrets alist must be a cons whose car and cdr are strings."))
    (let ((key (car elem))
          (value (cdr elem)))
      (unless (stringp key)
        (error "The key of every key-value pair in the secrets must be a string."))
      (unless (stringp value)
        (error "The value of every key-value pair in the secrets must be a string.")))))

(defun load-secrets (template pathname)
  ;; Load the alist from the file.
  (let ((form (with-open-file (stream pathname :direction :input)
                (read stream))))
    ;; Ensure the structure of the alist.
    (validate-secrets-alist form)
    ;; Convert the alist into a vault instance.
    (secrets-alist-to-vault template form)))
