;;; -*- Mode: LISP; Package: :cl-user; BASE: 10; Syntax: ANSI-Common-Lisp; -*-
;;;
;;;   Time-stamp: <>
;;;   Touched: Sun Aug 31 11:22:41 2025 +0530 <enometh@net.meer>
;;;   Bugs-To: enometh@net.meer
;;;   Status: Experimental.  Do not redistribute
;;;   Copyright (C) 2025 Madhu.  All Rights Reserved.
;;;
;;;  Ref: https://github.com/WayfireWM/pywayfire/raw/master/wayfire/ipc.py
;;;
(defpackage "WAYFIRE-IPC"
  (:use "CL")
  (:export "$PENDING-EVENTS" "$RESPONSE-BUFFER" "$SOCKET-TIMEOUT"
   "BYTES-TO-INT" "CLOSE-WAYFIRE-SOCKET" "DEF-SIMPLE-F"
   "GEOMETRY-TO-JSON" "GET-MSG-TEMPLATE"
   "CALL-IPC"
   "GET-OUTPUT" "GET-WAYFIRE-SOCKET-PATH" "HT->X" "INT-TO-BYTES"
   "LIST-METHODS" "MAKE-HEADER" "MAKE-MESSAGE" "OPEN-WAYFIRE-SOCKET"
   "READ-EXACT" "READ-MESSAGE" "READ-NEXT-EVENT" "SEND-JSON" "X->HT"))
(in-package "WAYFIRE-IPC")

(defun get-wayfire-socket-path
    (&key (wayland-display "wayland-0")
     (xdg-runtime-dir (uiop:getenv "XDG_RUNTIME_DIR")))
  (alexandria:when-let
      (path (or (uiop:getenv "WAYFIRE_SOCKET")
		(concatenate 'string xdg-runtime-dir "/" "wayfire-"
			     wayland-display "-.socket")))
    (pathname path)))

#+nil
(get-wayfire-socket-path)

(defun open-wayfire-socket (&key (socket-path (get-wayfire-socket-path)))
  (usocket:socket-connect socket-path nil :protocol :stream
			  :element-type '(unsigned-byte 8)))

(defun close-wayfire-socket (c)
  (usocket:socket-close c))

(defvar $response-buffer (make-array 8192 :element-type '(unsigned-byte 8)))

#+usocket-iolib
(defun read-exact (c n)
  (let ((response-buffer (make-array n :element-type '(unsigned-byte 8)
				     #+lispworks :allocation
				     #+lispworks :pinnable)))
    (multiple-value-bind (return-buffer length remote-host remote-port)
	(usocket:socket-receive c response-buffer (length response-buffer))
      (declare (ignorable return-buffer length remote-host remote-port))
      #+nil
      (format t "read-exact: socket-recv: same-buffer-p=~A, length=~A remote=~S~%"
	      (eql response-buffer return-buffer) length
	      (list remote-host remote-port))
      response-buffer)))

#-usocket-iolib
(defun read-exact (c n)
  (let ((response nil)
	(bytes-remaining n))
    (loop while (> bytes-remaining 0) do
	  (multiple-value-bind (return-buffer length remote-host remote-port)
	      (usocket:socket-receive c $response-buffer (min bytes-remaining (length $response-buffer)))
	    (declare (ignorable remote-host remote-port))
	    (assert (eql return-buffer $response-buffer))
	    (when (and (<= (decf bytes-remaining length) 0)
		       (not response))
	      (assert (zerop bytes-remaining))
	      (return-from read-exact (subseq $response-buffer 0 n)))
	    (unless response
	      (setq response (make-array 0 :element-type '(unsigned-byte 8)
					 :fill-pointer t :adjustable t)))
	    (dotimes (i length)
	      (vector-push-extend (elt $response-buffer i) response
				  length))))
    response))

;;#+nil(ensure-ctf:ensure-ctf :usocket-iolib)
;;
;; trigger an error if the file was compiled with :usocket-iolib in
;; the features and is being loaded in an environment where the
;; feature is not present (and vice-versa). this expands the
;; ensure-ctf macro from the eponymous package name instead of
;; depending on that package.
(progn
  (defvar *compile-time-value-definer* nil)
  (defvar g347 nil)
  (setq *compile-time-value-definer* nil)
  (eval-when (:compile-toplevel)
    (setq *compile-time-value-definer*
	  `(setq g347 ',(eval
			 '(read-from-string
			   (format nil "(~{#+~(~a~) t ~:*#-~(~a~) nil~^ ~})"
			    '(:usocket-iolib)))))))
  (eval-when (:compile-toplevel :load-toplevel :execute)
    (macrolet ((define-compile-time-value () *compile-time-value-definer*))
      (eval-when (:load-toplevel :execute)
        (define-compile-time-value)))))
(eval-when (:load-toplevel :execute)
   (let ((mismatches (loop :for ctf :in g347
			   :for feat :in '(:usocket-iolib)
			   unless (eq ctf
				      (and (find feat *features*) t))
			   append (list feat :ctv))))
     (assert (endp mismatches) nil
	 "~{~(~s~) was ~:[absent~;present~] at compile time but is ~:*~:[present~;absent~] at runtime.~^~%~}"
       mismatches)))

(defun bytes-to-int (byte-array &key (start 0) end (endian :little-endian))
  (let ((unsigned-value 0))
    (loop for i from start below (or end (length byte-array))
	  for byte = (elt byte-array i)
	  do (incf unsigned-value
		   (ecase endian
		     (:big-endian (* unsigned-value #x100) byte)
		     (:little-endian
		      (ash byte (* 8 i))))))
    unsigned-value))

(defun int-to-bytes (int &key (sizeof 4)  (endian :little-endian))
  (let ((out (make-array 0 :element-type '(unsigned-byte 8)
			 :adjustable t :fill-pointer t)))
    (ecase endian
      (:big-endian
       (loop for i = (* 8 (1- sizeof)) then (- i 8)
	     while (>= i 0)
	     do (vector-push-extend
		 (ldb (byte 8 i) int) out sizeof)))
      (:little-endian
       (loop for i below sizeof
	     do (vector-push-extend
		 (ldb (byte 8 (* 8 i)) int) out  sizeof))))
    out))

;; ht->x, x->ht - visualise hash-tables recursively. (hash-table is a
;; poor choice because the ordering of keys--which the user may find
;; useful--is lost.)

(defun ht->x (obj &optional (x :alist))
  (etypecase obj
    (hash-table
     (ecase x
       (:plist (let ((keys (sort (alexandria:hash-table-keys obj) #'string<)))
		 (loop for k in keys for v = (gethash k obj)
		       append (list k (ht->x v x)))))
       (:alist (sort (loop for (k . v) in (alexandria:hash-table-alist obj)
			   collect (cons k (ht->x v x)))
		     #'string< :key #'car))))
    (string obj)
    (vector (map 'vector (lambda (a) (ht->x a x)) obj))
    (atom obj)))

(defun x->ht (obj &optional (x :alist))
  (etypecase obj
    (cons (ecase x
	    (:plist (alexandria:plist-hash-table
		     (loop for (k v) on obj by #'cddr
			   append (list k (x->ht v x)))))
	    (:alist (alexandria:alist-hash-table
		     (loop for (k . v) in obj
			   collect (cons k (x->ht v x)))))))
    (string obj)
    (vector (map 'vector (lambda (a) (x->ht a x)) obj))
    (atom obj)))

(defun get-msg-template (method &rest data-key-val-plist)
  "Returns a object (hash-table) of the form
{ \"method\": method-name,
   \"data\": { \"key1\": \"val1\",
               \"key2\": \"val2\", .. }}"
  (let ((data (apply #'alexandria:plist-hash-table
		     data-key-val-plist
		     (list :test #'equal))))
    (alexandria:plist-hash-table
     (list "method" method "data" data)
     :test #'equal)))

#||
(com.inuoe.jzon:stringify
 (get-msg-template "window-rules/output-info" "id" 1)
 :pretty t)

"{
  \"method\": \"window-rules/output-info\",
  \"data\": {
    \"id\": 1
  }
}"
||#

(defun geometry-to-json (x y w h)
  (alexandria:plist-hash-table
   (list "x" x "y" y "width" w "height" h)
   :test #'equal))

(defun read-message (c)
  (let* ((rlen (bytes-to-int (read-exact c 4)))
	 (response-message (read-exact c rlen)))
    (com.inuoe.jzon:parse
     (babel:octets-to-string response-message :encoding :utf-8))))

(defvar $pending-events nil)
(defvar $socket-timeout 3)

(defun make-header (str)
  (int-to-bytes (length str) :sizeof 4))

(defun make-message (str)
  (babel:string-to-octets str :encoding :utf-8))

(defun send-json (c msg)
  (unless (gethash "method" msg)
    (error "Malformed json request: missing method"))
  (let* ((str (com.inuoe.jzon:stringify msg :pretty t))
	 (data (make-message str))
	 (header (make-header data)))
    (usocket:socket-send c header (length header))
    (usocket:socket-send c data (length data)))
  (finish-output (usocket:socket-stream c))
  (let (response)
    (loop (cond ((and (usocket:wait-for-input c :timeout $socket-timeout)
		      (member (usocket:socket-state c) '(:read :read-write)))
		 (setq response (read-message c))
		 (cond ((and (hash-table-p response) (gethash "event" response))
			(push response $pending-events))
		       (t (return response))))
		(t (error "Response timeout"))))))

(defun read-next-event (c)
  (if $pending-events
      (pop $pending-events)
      (cond ((and (usocket:wait-for-input c :timeout $socket-timeout)
		  (member (usocket:socket-state c) '(:read :read-write)))
	     (let ((response (read-message c)))
	       (cond ((and (hash-table-p response) (gethash "event" response))
		      response)
		     (t
		      (with-simple-restart (cont "Cont")
			(error "read-next-event: read a non-event ~S"
				 response))))))
	    (t (values nil :timeout)))))

(defun call-ipc (c method-name &rest msg-template-args)
  "Call method METHOD-NAME on connection C. MSG-TEMPLATE-ARGS
are an alternating list of string paramter name and lisp value, which
the method takes.  The specification of the types of the parameters
are in the C++ code plugins/ipc-rules/ipc-rules.cpp and other files
there."
  (let* ((m (apply #'get-msg-template msg-name msg-template-args))
	 (ret (send-json c m)))
    ret))

(defun list-methods (c)
  (gethash "methods" (call-ipc c "list-methods")))

;;; PORCELAIN - def-simple-f
;;;
;;; ;madhu 251006 - the ipc methods should really be defined by an xml
;;; like the rest of wayfire.
;;;
;;; don't intern the kargs of the functions we define in the lisp
;;; KEYWORD package and pollute it. maybe we should not use keywords
;;; at all but just positional arguments?
;;;
;;; ;madhu 260907 extend def-simple-f to accept an &optional marker in
;;; ARGS. args before the &:optional marker are required and those
;;; after it are treated as optional.

(defun parse-simple-f-args (args)
  "ARGS is a list of strings but may contain an optional
:&OPTIONAL keyword marker that separate required args from optional
args. returns 2 values the the required args and the optional args."
  (let* ((optional-cons (member :&optional args)))
    (values (ldiff args optional-cons) (cdr optional-cons))))

#+nil
(equal
 (multiple-value-list
  (parse-simple-f-args '("id" "geometry" :&optional "tiled")))
 '(("id" "geometry") ("tiled")))

(defun string->karg (x)
  (intern (string-upcase x) "WAYFIRE-IPC"))

(defmacro def-simple-f (method-name &rest args)
  "USAGE: (def-simple-f \"method-name\" \"arg1\" ...)
method-name and args are strings corresponding to the exposed wayfire
ipc api. place optional args after a :&optional marker."
  (let ((fname (string->karg method-name)) rargs oargs rkargs okargs osupps)
    (multiple-value-setq (rargs oargs) (parse-simple-f-args args))
    (setq rkargs (mapcar 'string->karg rargs))
    (setq okargs (mapcar 'string->karg oargs))
    (setq osupps (mapcar 'string->karg
			 (loop for arg in oargs
			       collect (concatenate 'string arg
						    "-supplied-p"))))
    `(progn
       (export '(,fname ,@rkargs ,@okargs ,@osupps ) "WAYFIRE-IPC")
       (defun ,fname (c &key
		      ,@(loop for a in rkargs collect `((,a ,a)))
		      ,@(loop for a in okargs for b in osupps
			      collect `((,a ,a) nil ,b)))
	   (let* ((m (apply #'get-msg-template
			    ,method-name
			    (append
			     ,@(loop for a in rargs for k in rkargs
				    collect `(list ,a ,k))
			     ,@(loop for a in oargs
				     for k in okargs
				     for s in osupps
				     collect `(when ,s
						(list ,a ,k))))))
		  (ret (send-json c m)))
	     (values (ht->x ret) ret))))))

#||
(def-simple-f  "window-rules/list-outputs")
(window-rules/list-outputs $c)
(macroexpand-1 '(def-simple-f "wf/filters/unset-fs-shader" "output-name"))
(wf/filters/unset-fs-shader $c 'output-name "eDP-1")
(macroexpand-1 '(def-simple-f "foo/bar" "arg1" :&optional "arg2" "arg3"))
||#


(defun get-output (c output-id)
  (call-ipc c  "window-rules/output-info" "id" output-id))

#+nil
(defvar $c (open-wayfire-socket))

#||
(close-wayfire-socket $c)
(setq $c (open-wayfire-socket))
(setq $ret (send-json $c (get-msg-template "wayfire/configuration")))
(ht->x $ret :alist)
(list-methods $c)
(ht->x (get-output $c 1) :plist)
(ht->x (send-json $c (get-msg-template  "wayfire/get-keyboard-state")))
||#
