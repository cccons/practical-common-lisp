(defpackage #:ch09
  (:use #:cl
        #:ch08)
  (:export
   #:deftest
   #:check
   #:combine-results
   #:report-result))

(defvar *test-name* nil)

(defmacro deftest (name parameters &body body)
  `(defun ,name ,parameters
     (let ((*test-name* (append *test-name* (list ',name))))
       ,@body)))

(defmacro check (&body forms)
  `(combine-results
    ,@(loop for f in forms collect `(report-result ,f ',f))))

(defmacro combine-results (&body forms)
  (ch08:with-gensyms (result)
    `(let ((,result t))
       ,@(loop for f in forms collect `(unless ,f (setf ,result nil)))
       ,result)))

(defun report-result (result form)
  (format t "~:[fail~;pass~] ... ~a: ~a~%" result *test-name* form)
  result)
