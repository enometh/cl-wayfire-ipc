;;; -*- Mode: LISP; Package: :cl-user; BASE: 10; Syntax: ANSI-Common-Lisp; -*-
;;;
;;;   Time-stamp: <>
;;;   Touched: Sun Sep 07 11:01:31 2025 +0530 <enometh@net.meer>
;;;   Bugs-To: enometh@net.meer
;;;   Status: Experimental.  Do not redistribute
;;;   Copyright (C) 2025 Madhu.  All Rights Reserved.
;;;
;;; manipulate "vibrant" color saturation with hyprland-ctm
;;;
(in-package "WAYFLAN-EXT")

(defun ctm-commit (c)
  (wayflan-client.hyprctm:hyprland-ctm-control-manager-V1.COMMIT
   (bind c "hyprland_ctm_control_manager_v1")))

(defun ctm-from-rgb (rgb)
  (destructuring-bind (r g b) rgb
    (list r 0 0
	  0 g 0
	  0 0 b)))

(defun ctm-set-matrix (c vector)
  (apply #'wayflan-client.hyprctm:hyprland-ctm-control-manager-v1.set-ctm-for-output
   (bind c "hyprland_ctm_control_manager_v1")
   (bind c "wl_output")
   #1=(map 'list 'identity vector))
  (ctm-commit c)
  #1#)

(defun ctm-set-identity (c)
  (ctm-set-matrix c (ctm-from-rgb '(1.0 1.0 1.0))))

;; see https://github.com/libvibrant/libvibrant src/util.c
(defun vibrant-saturation-to-coeffs (sat)
  "sat is between 0.0 and 4.0"
  (let ((coeff (/  (- 1.0 sat) 3.0))
	(coeffs (make-array 9 :element-type 'float :initial-element 0.0)))
    (loop for i below 9
	  do (setf (elt coeffs i)
		   (+ coeff
		      (if (zerop (mod i 4))
			  sat
			  0))))
    coeffs))

(defun vibrant-coeffs-to-saturation (coeffs)
  (- (elt coeffs 0) (elt coeffs 1)))

(defun ctm-set-saturation (c sat)
  (ctm-set-matrix c (vibrant-saturation-to-coeffs sat)))

#||
(ctm-set-saturation $c 0.5)
||#
