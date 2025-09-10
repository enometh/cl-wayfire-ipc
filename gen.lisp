;;; -*- Mode: LISP; Package: :cl-user; BASE: 10; Syntax: ANSI-Common-Lisp; -*-
;;;
;;;   Time-stamp: <>
;;;   Touched: Sun Sep 06 15:55:55 2025 +0530 <enometh@net.meer>
;;;   Bugs-To: enometh@net.meer
;;;   Status: Experimental.  Do not redistribute
;;;   Copyright (C) 2025 Madhu.  All Rights Reserved.
;;;
(in-package "WAYFLAN-USER")

(defun ensure-pkg (pkg)
  (or (find-package pkg) (make-package pkg :use nil)))

(defstruct wfl-ctx package exclude-defclasses syms-to-export)

(defun call-with-wfl-ctx (ctx thunk)
  (let ((*package* (let ((p (find-package (wfl-ctx-package ctx))))
		     (assert p) p))
	(wayflan-client.scanner::*exclude-defclasses*
	 (wfl-ctx-exclude-defclasses ctx))
	(wayflan-client.scanner::*syms-to-export*
	 (wfl-ctx-syms-to-export ctx)))
    (unwind-protect (funcall thunk)
      (setf (wfl-ctx-syms-to-export ctx)
	    wayflan-client.scanner::*syms-to-export*))))

(defmacro with-wfl-ctx (ctx &body body)
  `(call-with-wfl-ctx ,ctx (lambda () ,@body)))

(defun wfl-ctx-gen-forms (ctx proto)
  (with-wfl-ctx ctx
    (let* ((pkg-name (package-name *package*))
	   (impl-pkg-name (concatenate 'string pkg-name "-IMPL"))
	   (forms (wayflan-client.scanner::%transform-protocol proto)))
      `((uiop:define-package ,impl-pkg-name (:use "CL" "WAYFLAN-CLIENT"))
	(cl:in-package ,impl-pkg-name)
	(cl:defpackage ,pkg-name
		       (:use "WAYFLAN-CLIENT")
		       (:export ,@wayflan-client.scanner::*syms-to-export*))
	,@forms))))

(defstruct (wfl-ctx-full (:include wfl-ctx) (:conc-name "WFL-CTX-"))
  xml-file proto forms out-file)

(defun dump-forms-to-file (forms out-file)
  (user::string->file
   (with-output-to-string (s)
     (with-standard-io-syntax
       (let ((*print-pretty* t))
	 (dolist (form forms)
	   (write form :stream s)
	   (terpri s)))))
   out-file))

(defun wfl-ctx-gen (ctx &key xml-file out-file)
  (let ((xml-file (cl:pathname (or xml-file (wfl-ctx-xml-file ctx))))
	(out-file (or out-file (wfl-ctx-out-file ctx))))
    (assert xml-file nil "Specify XML file")
    (setf (wfl-ctx-proto ctx)
	  (wayflan-client.scanner::wl-parse xml-file))
    (setf (wfl-ctx-xml-file ctx) xml-file)
    (setf (wfl-ctx-forms ctx)
	  (wfl-ctx-gen-forms ctx (wfl-ctx-proto ctx)))
    (when out-file
      (with-wfl-ctx ctx
	(dump-forms-to-file (wfl-ctx-forms ctx)
			    out-file)))))

#||
 ;;(in-package #:xyz.shunter.wayflan.client)
(xyz.shunter.wayflan.client.scanner:wl-include
  '(#:wayflan/common #+mk-defsystem "src" #:protocols "wayland.xml")
  :exclude-defclasses (wl-display)
  :export t)
||#


#+nil
(defvar $wayland-ctx
  (make-wfl-ctx-full
   :package (ensure-pkg "WAYLAND-CLIENT")
   :xml-file #p"/usr/share/wayland/wayland.xml"
   :out-file "/dev/shm/wayland.lisp"
   :exclude-defclasses '(wayland-client::wl-display)))

#+nil
(wfl-ctx-gen $wayland-ctx)

#+nil
(defvar $gammactl-ctx
  (make-wfl-ctx-full
   :package (ensure-pkg 'wayflan-client.wlr-gamma-control)
   :out-file "/tmp/wlr-gamma-v1.lisp"
   :xml-file  #p"/7/gtk/EXT-WAYLAND/wlsunset/wlr-gamma-control-unstable-v1.xml"))

#+nil
(wfl-ctx-gen $gammactl-ctx)

#+nil
(load (compile-file (wfl-ctx-out-file $gammactl-ctx)))
