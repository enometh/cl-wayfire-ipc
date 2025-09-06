;;; -*- Mode: LISP; Package: :cl-user; BASE: 10; Syntax: ANSI-Common-Lisp; -*-
;;;
;;;   Time-stamp: <>
;;;   Touched: Fri Aug 29 06:07:57 PM IST 2025 <enometh@net.meer>
;;;   Bugs-To: enometh@net.meer
;;;   Status: Experimental.  Do not redistribute
;;;   Copyright (C) 2025 Madhu.  All Rights Reserved.
;;;
(in-package "WAYFLAN-EXT")

;; cache globals and other objects pertaining to a connection
(defstruct property-table-mixin
  (props (make-hash-table :test #'equal)))

(defun getprop (key ptm &optional default)
  (with-slots (props) ptm
    (gethash key props default)))

(defsetf getprop (key ptm &optional default) (new-value)
  `(with-slots (props) ,ptm
     (setf (gethash ,key props ,default) ,new-value)))

(defun call-with-internprop (key ptm value-thunk)
  "Second return value is T if key is interned with the value from
calling VALUE-THUNK for the first time."
  (multiple-value-bind (val foundp)
      (getprop key ptm)
    (cond (foundp (values val nil))
	  (t (values (setf (getprop key ptm) (funcall value-thunk))
		     nil)))))

(defmacro internprop (key ptm &body value-yielding-forms)
  "KEY PTM are evaluated eagerly. VALUE-YIELDING-FORMS is evaluated
only if the KEY is not found in the PTM"
  `(call-with-internprop ,key ,ptm (lambda () ,@value-yielding-forms)))

#||
(getprop 'foo $c 'xyz)
(setf (getprop 'foo $c) 'abc)
(call-with-internprop 'foo $c nil)
(with-slots (props) $c
  (remhash 'foo props))
(internprop 'foo $c 'def)
||#

(defstruct (client-state (:include property-table-mixin))
  display
  registry
  globals)

(defun disconnect (c)
  (with-slots (display registry globals props) c
    (when display
      (loop for k being each hash-key of props
	    using (hash-value v)
	    when (typep v 'wayflan-client:wl-proxy)
	    do (wayflan-client:destroy-proxy v))
      (clrhash props)
      (wayflan-client:wl-display-disconnect display)
      (setq display nil)
      (setq registry nil)
      (setq globals nil)))
  c)

(defun registry-find-interface (c interface &optional version)
  (with-slots (globals) c
    (dolist (global globals)
      (destructuring-bind (gname ginterface gversion) global
	(when (and (equal ginterface interface)
		   (or (null version) (>= gversion version)))
	  (return (values gname gversion)))))))

(defun make-registry-global-events-handler-fn (c)
  ;; returns a function which listens to global and global-remove
  ;; events and collects the names onto the globals slot of
  ;; client-state c.  interns the returned function for the
  ;; client-state on the properties-table, and returns it if already
  ;; created.
  "internal"
  (with-slots (globals) c
    (flet ((fn (event-name &rest args)
	     (warn "handle-registry-global ~S ~S" event-name args)
	     (when (eql event-name :global)
	       (destructuring-bind (name interface version) args
		 (pushnew (list name interface version) globals :test #'equal)))
	     (when (eql event-name :global-remove)
	       (destructuring-bind (name) args
		 (setq globals (delete name globals :key #'first :test #'equal))))))
      (internprop 'registry-global-events-handler-function c #'fn))))

#+nil
(eq (make-registry-global-events-handler-fn $c)  (make-registry-global-events-handler-fn $c))

(defun registry-collect-globals (c)
  (let ((fn (make-registry-global-events-handler-fn c)))
    (with-slots (display registry) c
      (when (find fn (wayflan-client:wl-proxy-hooks registry))
	(warn "global-events-handler-fn lready registered"))
      (pushnew fn (wayflan-client:wl-proxy-hooks registry))
      (wayflan-client:wl-display-roundtrip display))))

;; entrypoint
(defun connect (c)
  (with-slots (display registry globals) c
    (assert (not display))
    (setq display (wayflan-client:wl-display-connect))
    (assert (not registry))
    (setq registry (wayflan-client:wl-display.get-registry display))
    (assert (not globals)))
  (registry-collect-globals c))

(defun %unlispify (string)
  ;; reverses wayflan-client.scanner::%lispify
  (string-downcase
   (map 'string
	(lambda (char)
	  (if (char= char #\-)
	      #\_ char))
	(symbol-name string))))

#+nil
(equal (%unlispify 'wl-compositor) "wl_compositor")

(defun %ensure-wl-string (str)
  (etypecase str
    (symbol (%unlispify str))
    (string str)))

(defmacro ensure-wl-string (&rest vars)
  `(progn
     ,@(loop for var in vars
	     do (check-type var symbol)
	     collect `(setq ,var (%ensure-wl-string ,var)))))

;; Internal.  Call wl-registry.bind on the registry-global with name
;; STRING.
(defun %bind (c string &optional version)
  (check-type string string)
  (with-slots (registry) c
    (multiple-value-bind (name actual-version)
	(registry-find-interface c string version)
      (wayflan-client:wl-registry.bind
       registry
       name
       (class-name (wayflan-client:find-interface-named string))
       actual-version))))

(defun bind (c string &optional version)
  "Bind, cache, and return a global named STRING in the registry."
  (ensure-wl-string string)
  (internprop string c (%bind c string version)))

#||
(defvar $c (make-client-state))
(disconnect $c)
(connect $c)
(eql (bind $c 'wl-compositor) (bind $c "wl_compositor"))
(client-state-globals $c)
(bind $c 'wl-output)
(symbol-package (class-name (class-of (bind $c "hyprland_ctm_control_manager_v1"))))
||#

(defun find-meth (c interface-name method-name)
  ;; interface-name should be a registry global
  (ensure-wl-string interface-name method-name)
  (let ((obj (bind c interface-name)))
    (when obj
      (let* ((class-sym (class-name (class-of obj)))
	     (acc-name (concatenate 'string
				    (string class-sym)
				    "."
				    (wayflan-client.scanner::%lispify method-name)))
	     (acc-sym (find-symbol acc-name (symbol-package class-sym))))
	(assert (fboundp acc-sym))
	acc-sym))))

#+nil
(eql (find-meth $c 'wl-compositor 'create-surface)
     'wayflan-client:wl-compositor.create-surface)
