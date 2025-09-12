;;; -*- Mode: LISP; Package: :cl-user; BASE: 10; Syntax: ANSI-Common-Lisp; -*-
;;;
;;;   Time-stamp: <>
;;;   Touched: Sun Sep 07 08:35:09 2025 +0530 <enometh@net.meer>
;;;   Bugs-To: enometh@net.meer
;;;   Status: Experimental.  Do not redistribute
;;;   Copyright (C) 2025 Madhu.  All Rights Reserved.
;;;
;;; set gamma ramp with wlr-gamma-control-unstable-v1.xml (aka
;;; https://git.sr.ht/~kennylevinsen/wlsunset/)
;;;
(in-package "WAYFLAN-EXT")

(defun get-gamma-control (c)
  (let* ((intf-name "zwlr_gamma_control_manager_v1")
	 (acc-name "get_gamma_control")
	 (acc-sym (find-meth c intf-name acc-name)))
    (internprop acc-sym c
      (funcall acc-sym
	       (bind c intf-name)
	       (bind c "wl_output")))))

#+nil
(eq (get-gamma-control $c) (get-gamma-control $c))

(defstruct (client-state-wlr-gamma (:include client-state)
				   (:conc-name "CLIENT-STATE-"))
  gamma-size)

(defun make-gamma-control-listener-fn (c)
  (with-slots (gamma-size) c
    (flet ((handle-gamma-control (&rest args)
	     (wayflan-client:event-case args
	       (:gamma-size (size)
		(setq gamma-size size))
	       (:failed ()
		(warn "gamma gontrol no longer valid")))))
      (internprop 'gamma-control-listener-function c
	#'handle-gamma-control))))

(defun setup-gamma-control (c)
  (let* ((fn (make-gamma-control-listener-fn c))
	 (gamma-control (get-gamma-control c)))
    (pushnew fn (wayflan-client:wl-proxy-hooks gamma-control))
    (wayflan-client:wl-display-roundtrip (client-state-display c))))

#||
(disconnect $c)
(defvar $c (make-client-state-wlr-gamma))
(connect $c)
(setup-gamma-control $c)
(client-state-gamma-size $c)
||#

(defstruct ramp map-len fd path data)

(defun dispose-ramp (ramp)
  (with-slots (map-len fd path data) ramp
    (when fd
      (posix-shm/ffi:ffi-close fd)
      (setq fd nil))
    (when path
      (setq path nil))
    (when data
      (posix-shm/ffi:munmap data map-len)
      (setq data nil))
    (setq map-len nil)))

(defun init-ramp (ramp c)
  (with-slots (map-len fd path data) ramp
    (assert (not map-len))
    (setq map-len (* (client-state-gamma-size c)
		     3
		     (cffi:foreign-type-size :uint16)))
    (assert (not fd))
    (assert (not path))
    (multiple-value-setq (fd path) (tmpfile map-len))
    (assert (not data))
    (setq data (posix-shm/ffi:mmap (cffi:null-pointer)
				   (* (client-state-gamma-size c)
				      3
				      (cffi:foreign-type-size :uint16))
				   '(:write :read)
				   '(:shared)
				   fd
				   0))))

(defmacro coerce-to-uint16 (expr)
  `(let* ((val ,expr)
	  (val2 (* 65535 val)))
     (assert (<= val2 65535))
     (truncate val2)))

(defun make-rgb-array (gamma-size r g b gamma)
  (let ((array (make-array `(3 ,gamma-size)
			   #+nil :element-type #+nil
			   `(integer 0 ,(1- (expt 2 16))))))
    (loop for i below gamma-size
	  for val = (float (/  i (- gamma-size 1)))
	  do (setf (aref array 0 i) (expt (* val r) (/ 1.0 gamma))
		   (aref array 1 i) (expt (* val g) (/ 1.0 gamma))
		   (aref array 2 i) (expt (* val b) (/ 1.0 gamma))))
    array))

(defun make-gamma-table-via-rgb-array (gamma-size r g b gamma)
  (let ((array (make-rgb-array gamma-size r g b gamma))
	(ramp (make-array (* 3 gamma-size)
			  ;;#+nil
			  :element-type ;; #+nil
			  `(integer 0 ,(1- (expt 2 16))))))
    (loop for i below gamma-size
	  for j = (+ i gamma-size)
	  for k = (+ j gamma-size)
	  do (setf (aref ramp i) (coerce-to-uint16 (aref array 0 i))
		   (aref ramp j) (coerce-to-uint16 (aref array 1 i))
		   (aref ramp k) (coerce-to-uint16 (aref array 2 i))))
    ramp))

(defun fill-gamma-ramp-table (data gamma-size r g b gamma)
  (declare (optimize (debug 3) (speed 0) (safety 3)))
  (cffi:with-pointer-to-vector-data
      (ptr (make-gamma-table-via-rgb-array gamma-size r g b gamma))
    (cffi:foreign-funcall "memcpy" :pointer data :pointer ptr
			  :int (* 3 gamma-size 2))))
  #+nil
  (loop for i below gamma-size
	for j = (+ i gamma-size)
	for k = (+ j gamma-size)
	for val = (float (/  i (- gamma-size 1)))
	do
	(setf (cffi:mem-aref data :uint16 i) (coerce-to-uint16 (expt (* val r) (/ 1.0 gamma))))
	(setf (cffi:mem-aref data :uint16 j) (coerce-to-uint16 (expt (* val g) (/ 1.0 gamma))))
	(setf (cffi:mem-aref data :uint16 k) (coerce-to-uint16 (expt (* val b) (/ 1.0 gamma))))))

#+nil
(defun data->array (data gamma-size)
  (let ((ret (make-array (* 3 gamma-size) :element-type '(integer 0 65535))))
    (loop for i below gamma-size
	  for j = (+ i gamma-size)
	  for k = (+ j gamma-size)
	  do
	  (setf (aref ret i) (cffi:mem-aref data ':uint16 i))
	  (setf (aref ret j) (cffi:mem-aref data ':uint16 j))
	  (setf (aref ret k) (cffi:mem-aref data ':uint16 k)))
    ret))

#||
(defvar $r (make-ramp))
(dispose-ramp $r)
(init-ramp $r $c)
(let ((r 0.8) (b 0.9) (g 0.9) (gamma 1.0))
  (fill-gamma-ramp-table (ramp-data $r) (client-state-gamma-size $c) r g b gamma)
  (equalp (make-gamma-table (client-state-gamma-size $c) r g b gamma)
	  (data->array (ramp-data $r) (client-state-gamma-size $c))))
||#

(defmacro with-ramp ((ramp) &body body)
  `(let ((,ramp (make-ramp)))
     (unwind-protect (progn ,@body)
       (dispose-ramp ramp))))

(defun set-gammactl (c r g b gamma)
  (with-slots (gamma-size) c
    (with-ramp (ramp)
      (init-ramp ramp c)
      (with-slots (data fd) ramp
	(fill-gamma-ramp-table data gamma-size r g b gamma)
	(wayflan-client.wlr-gamma-control:zwlr-gamma-control-v1.set-gamma
	 (get-gamma-control c)
	 fd)))))

#+nil
(set-gammactl $c  1.0 1.0 1.0 1.0)

#||
(defun unlink (path)
  (unless (zerop (cffi:foreign-funcall "unlink" :string (namestring path) :int))
    (sys-cerror "Error unlinking ~A" path)))
(mapcar (lambda (x) (unlink (namestring x))) (directory "/dev/shm/tmp.*"))
(prin1 (cl-user::shell-command-to-string "ls -l /proc/$(pidof ccl)/fd"))
(setq $f /dev/shm/tmp.XXHl3TlN)
(setq $fd 7)
(defvar $statbuf (cffi:foreign-alloc '(:struct posix-shm/ffi:stat)))
(posix-shm/ffi:fstat $fd $statbuf)
(defun slots (obj type)
  (mapcar (lambda (name)
	  (cons name (cffi:foreign-slot-value obj type name)))
	(mapcar (lambda (slot)  (slot-value slot 'cffi::name))
		(cffi::slots-in-order (cffi::parse-type type)))))
(slots $statbuf '(:struct posix-shm/ffi:stat))
(package-promote "POSIX-SHM/FFI")
(slots $statbuf '(:struct posix-shm/ffi:stat))
(wayflan-client:wl-display.sync (client-state-display $c))
(closer-mop:class-direct-superclasses (class-of $gamma-control))

        Set the gamma table. The file descriptor can be memory-mapped to provide
        the raw gamma table, which contains successive gamma ramps for the red,
        green and blue channels. Each gamma ramp is an array of 16-byte unsigned
        integers which has the same length as the gamma size.

        The file descriptor data must have the same length as three times the
        gamma size.

;; (cffi:mem-aref $data :uint16 (1- (* 3 $gamma-size)))
;; 0-1023
;; 1023-2047
;; 2048-3071

#+nil
(with-slots (gamma-size) $c
  (with-slots (data fd) $r
    (loop with r = 0.9 with b = 0.9 and g = 0.9
	  for gamma from 0.5 to 1.0 by 0.05
	  do
	  (fill-gamma-ramp-table data gamma-size r g b gamma)
	  (wayflan-client.wlr-gamma-control:zwlr-gamma-control-v1.set-gamma
	   (get-gamma-control $c)
	   fd)
	  (sleep 0.4))))
||#