;;; -*- Mode: LISP; Package: :cl-user; BASE: 10; Syntax: ANSI-Common-Lisp; -*-
;;;
;;;   Time-stamp: <>
;;;   Touched: Fri Aug 29 11:15:46 AM IST 2025 <enometh@net.meer>
;;;   Bugs-To: enometh@net.meer
;;;   Status: Experimental.  Do not redistribute
;;;   Copyright (C) 2025 Madhu.  All Rights Reserved.
;;;
(in-package "WAYFLAN-EXT")

(defun call-perror-fn (dispatch-fn fmt fmt-args)
  (apply dispatch-fn "~@?: ~A" fmt (append fmt-args (list (cffi:foreign-funcall "strerror" :int (cffi:mem-ref (cffi:foreign-funcall  "__errno_location" :pointer) :int) :string)))))

(defun sys-error (fmt &rest fmt-args)
  (call-perror-fn #'error fmt fmt-args))

(defun sys-cerror (fmt &rest fmt-args)
  (with-simple-restart (cont "Cont")
    (apply #'sys-error fmt fmt-args)))

(defun mkstemp (template)
  (cffi:with-foreign-string (str template)
    (let ((res (cffi:foreign-funcall "mkstemp" :pointer str :int)))
      (when (minusp res)
	(sys-error "mkstemp: opening ~A" template))
      (values res (cffi:foreign-string-to-lisp str)))))

(defun tmpfile (size)
  "Create an anonymous temporary file of the given size. Returns a file descriptor."
  (let (done fd pathname)
    (unwind-protect
	 (progn
	   (setf (values fd pathname) (mkstemp "/dev/shm/tmp.XXXXXXXX"))
	   (when (minusp (cffi:foreign-funcall "unlink" :string pathname :int))
	     (sys-cerror "error unlinking ~A" pathname))
	   (when (minusp (cffi:foreign-funcall "ftruncate" :int fd :int size :int))
	     (sys-cerror "error truncating ~A ~A" pathname fd))
	   (setf done t))
      (when (and fd (not done))
	(when (minusp (cffi:foreign-funcall "close" :int fd :int))
	  (sys-cerror "erro closing fd ~A ~A" pathname fd))))
    (values fd pathname)))
