(in-package #:ec2-price-finder)

(defvar *acceptor* nil)

(defun start-server (port)
  (setf *acceptor* (hunchentoot:start (make-instance 'easy-routes:easy-routes-acceptor
                                                     :port port))))

(defun stop-server ()
  (hunchentoot:stop *acceptor*))

(defun main ()
  (let* ((arguments (uiop:command-line-arguments))
         (port (if (first arguments)
                   (parse-integer (first arguments))
                   8080)))
    (start-server port)
    (format t "Starting server on port: ~D~%" port)
    (loop (sleep 60))))

(defun load-pricing-file (path)
  (flet ((progress-fn (&key count done)
           (if done
               (format t "Read all ~D rows~%" count)
               (when (zerop (rem count 100000))
                 (format t "Read ~D rows~%" count)))))
    (setf *document* (read-pricing-file path :progress-fn #'progress-fn))))

#+sbcl
(defun build-image ()
  (setf *debugger-hook*
        (lambda (condition old-hook)
          (declare (ignore old-hook))
          (princ condition *error-output*)
          (terpri *error-output*)
          (finish-output *error-output*)
          (sb-ext:quit :unix-status 1)))
  (sb-ext:save-lisp-and-die "ec2-price-finder.core"
                            :toplevel 'main))
