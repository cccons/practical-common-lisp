(in-package #:cl-user)

(defpackage #:ch03
  (:use #:cl))

(in-package #:ch03)

(defvar *db* nil)

(defun make-cd (title artist rating ripped)
  (list :title title :artist artist :rating rating :ripped ripped))

(defun add-record (cd)
  (push cd *db*))

(defun dump-db ()
  (format t "~{~{~a:~10t~a~%~}~%~}" *db*))

(defun prompt-read (prompt)
  (format *query-io* "~a: " prompt)
  (force-output *query-io*)
  (read-line *query-io*))

(defun prompt-for-cd ()
  (make-cd
   (prompt-read "Title")
   (prompt-read "Artist")
   (or
    (parse-integer
     (prompt-read "Rating")
     :junk-allowed t)
    0)
   (y-or-n-p "Ripped")))

(defun add-cds ()
  (loop (add-record (prompt-for-cd))
        (unless (y-or-n-p "Another?")
          (return))))

(defun save-db (filename)
  (with-open-file (out filename
                       :direction :output
                       :if-exists :supersede)
    (with-standard-io-syntax
      (print *db* out))))

(defun load-db (filename)
  (with-open-file (in filename)
    (with-standard-io-syntax
      (setf *db* (read in)))))

(defun select (selector-fn)
  (remove-if-not selector-fn *db*))

(defun update (selector-fn &key title artist rating (ripped nil ripped-p))
  (setf *db*
        (mapcar #'(lambda (row)
                    (when (funcall selector-fn row)
                      (when title    (setf (getf row :title)  title))
                      (when artist   (setf (getf row :artist) artist))
                      (when rating   (setf (getf row :rating) rating))
                      (when ripped-p (setf (getf row :ripped) ripped)))
                    row)
                *db*)))

(defun delete-rows (selector-fn)
  (setf *db* (remove-if-not selector-fn *db*)))

(defun make-comparison-expr (field value)
  `(equal (getf cd ,field) ,value))

(defun make-comparisons-list (fields)
  (loop while fields
        collecting (make-comparison-expr (pop fields) (pop fields))))

(defmacro where (&rest clauses)
  `#'(lambda (cd) (and ,@(make-comparisons-list clauses))))