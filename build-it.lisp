;; usage at shell prompt:
;;
;; cd ~/myproject
;; sbcl --load "build.lisp" --name myproject

#-quicklisp (load #P"~/quicklisp/setup.lisp")
(push (merge-pathnames "lib/" *default-pathname-defaults*) asdf:*central-registry*)
(require 'sb-posix)
(setf sb-impl::*default-external-format* :utf-8)

(defun argument (name)
  (let* ((args sb-ext:*posix-argv*)
	 (index (position name args :test 'equal))
	 (value (when (and (numberp index)
			   (< index (length args)))
		  (nth (1+ index) args))))
    value))

(defparameter *name* (argument "--name"))
(defparameter *binary*
  (or (argument "--binary")
      #+win32 (concatenate 'string *name* ".exe")
      #+linux (concatenate 'string *name* ".bin")))
(defparameter *system*
  (or (argument "--system")
      (intern *name* :keyword)))
(ql:quickload *system*)
(defparameter *main*
  (or (argument "--main")
      (concatenate 'string (string-upcase *name*) "::" (string-upcase *name*))))

(sb-ext:save-lisp-and-die *binary*
			  :toplevel (lambda ()
				      (sb-posix:putenv
				       (format nil "SBCL_HOME=~A"
				      	       #.(sb-ext:posix-getenv "SBCL_HOME")))
				      (funcall (read-from-string *main*))
				      0)
			  :executable t)
