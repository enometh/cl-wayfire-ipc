;;; -*- Mode: LISP; Package: :cl-user; BASE: 10; Syntax: ANSI-Common-Lisp; -*-
;;;
;;;   Time-stamp: <>
;;;   Touched: Fri Sep 12 09:43:58 2025 +0530 <enometh@net.meer>
;;;   Bugs-To: enometh@net.meer
;;;   Status: Experimental.  Do not redistribute
;;;   Copyright (C) 2025 Madhu.  All Rights Reserved.
;;;
(in-package "WAYFLAN-EXT")

#+nil
(require 'numcl)
;;;
;;; numcl extensions
;;;
(defun repeat (array n &key axis)
  (if (arrayp array)
      (if axis
          (numcl:concatenate (make-list n :initial-element array) :axis axis)
          (numcl:flatten
           (numcl:concatenate (make-list n :initial-element (numcl:reshape array `(,@(shape array) -1))) :axis -1)))
      (progn
        (assert (null axis))
        (numcl:full n array))))

(defun tile (a reps)
  (if (atom reps) (setq reps (list reps))) ;tuple
  (unless (numcl:numcl-array-p a)
    (setq a (numcl:asarray (if (atom a) (list a) a))))
  (let* ((d (length reps))
	 (a-shape (numcl:shape a)))
    (when (< (length a-shape) d)
      (warn "resizing dims a")
      (setq a (reshape a `(,@(make-list (- d (length a-shape))
					:initial-element 1)
			     ,@a-shape ))))
    (when (< d (length (shape a)))
      (warn "resizing reps")
      (setq reps `(,@(make-list (- (length (shape a)) d)
				:initial-element 1)
		     ,@reps)))
    (assert (= (length reps) (ndim a)))
    (let* ((c (numcl:copy a))
	   (n (numcl:size c))
	   (shape-out (loop for s in (numcl:shape c)
			    for t1 in reps
			    collect (* s t1))))
      (when (> n 0)
	(loop for dim-in in (numcl:shape c)
	      for nrep in reps
	      if (/= nrep 1)
	      do
	      (setq c (numcl:reshape c `(-1 ,n)))
	      (setq c (repeat c nrep :axis 0))
	      end
	      do (setq n (truncate n dim-in))))
      (numcl:reshape c shape-out))))

;;; linear gamma ramp heuristics from psychopy:
;;; psychopy/visual/backends/gamma.py

(defun linramp (ramp-type ramp-size)
  (let ((a256 (/  ramp-size 4.0))
	(a512 (/ ramp-size 2))
	(a1024 (*  ramp-size 1.0)))
    (case ramp-type
      ;; 8-bit clut ranging 0:1
      (0 (numcl:linspace 0.0 1.0 :num ramp-size))
      ;; 8-bit clut ranging 1/256.0:1
      (1 (numcl:linspace (/ a256) 1.0 :num ramp-size))
      ;; 10-bit clut ranging 0:1023/1024
      (2 (numcl:linspace 0.0 (/ (- a1024 1) a1024) :num ramp-size))
      ;; 10-bit clut with upper half removed
      (3 (let ((ret (numcl:linspace 0.0 (/ (- a1024 1) a1024) :num ramp-size)))
	   (loop for i from a512 below ramp-size
		 do (setf (elt ret i) (- (elt ret i) (/ 1 (float a256)))))
	   ret)))))


#||
(defvar $rr 0.9)
(defvar $gg 0.9)
(defvar $bb 0.9)
(defvar $ramp-type 0)
(defvar $gam nil)
(defvar $lut)
(defvar $new-lut)
(defvar $new-lut1)
(defvar $new-lut2)
(defvar $gamma-size 1024)
||#

(defun set-gammactl-psychopy ($c $rr $gg $bb $ramp-type)
  (let ($gam $gamma-size $lut $new-lut $new-lut1 $new-lut2)
    (with-slots (gamma-size) $c
      (setq $gamma-size gamma-size)
      (setq $gam (numcl:reshape (numcl:asarray (list $rr $gg $bb))
				'(3 1)))
      ;;(shape $gam)
      (setq $lut (tile (linramp $ramp-type $gamma-size) '(3 1)))
      ;;(shape $lut)
      (setq $new-lut (numcl:expt $lut (numcl:/ $gam)))
      ;;(shape $new-lut)
      (setq $new-lut1  (numcl:* $new-lut 65535))
      ;;(shape $new-lut1)
      (setq $new-lut2
	    (make-array (* 3 $gamma-size)
			:element-type '(integer 0 65535)
			:initial-contents
			(numcl:astype (numcl:flatten $new-lut1)
				      '(integer 0 65535))))
      ;;(simple-vector-p $new-lut2)
      ;;(numcl.impl::determine-array-spec $new-lut2 nil)
      (with-ramp (ramp)
	(init-ramp ramp $c)
	(with-slots (data fd) ramp
	  (cffi:with-pointer-to-vector-data (ptr $new-lut2)
	    (cffi:foreign-funcall "memcpy" :pointer data :pointer ptr
				  :int (* 3 $gamma-size 2)))
	  (wayflan-client.wlr-gamma-control:zwlr-gamma-control-v1.set-gamma
	   (get-gamma-control $c)
	   fd))))))
#+nil
(set-gammactl-psychopy $c 0.9 0.9 0.9 1)
