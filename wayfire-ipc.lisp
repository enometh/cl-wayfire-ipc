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
   "BYTES-TO-INT" "CLOSE-WAYFIRE-SOCKET" "GEOMETRY-TO-JSON" "GET-MSG-TEMPLATE"
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
    (loop (cond ((usocket:wait-for-input c :timeout $socket-timeout)
		 (setq response (read-message c))
		 (cond ((gethash "event" response)
			(push response $pending-events))
		       (t (return response))))
		(t (error "Response timeout"))))))

(defun read-next-event (c)
  (if $pending-events
      (pop $pending-events)
      (read-message c)))

(defun list-methods (c)
  (gethash "methods" (send-json c (get-msg-template "list-methods"))))

(defun get-output (c output-id)
  (send-json c (get-msg-template "window-rules/output-info"
			       "id" output-id)))

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
