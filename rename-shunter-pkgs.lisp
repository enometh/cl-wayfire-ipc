;;;
;;;   Time-stamp: <>
;;;   Touched: Fri Aug 29 16:12:10 2025 +0530 <enometh@net.meer>
;;;   Bugs-To: enometh@net.meer
;;;   Status: Experimental.  Do not redistribute
;;;   Copyright (C) 2025 Madhu.  All Rights Reserved.
;;;
(in-package "WAYFLAN-EXT")

;;  rename packages to more user friendly variants
;;  makes life in the debugger easier visually

;; cmucl-init.lisp
(defun find-packages-matching (string &key return (test #'equalp))
  (prog (x (string (string string)))
     (map nil (lambda (v)
		(let ((k (package-name v)))
		  (if (search string k :test test)
		      (push (ecase return
			      ((nil :key) k)
			      ((:value) v)
			      ((t :both) (cons k v)))
			    x))))
	  (list-all-packages))
     (return x)))

(defun prefixp (prefix sequence &key (test #'equalp) (start1 0) (start2 0) end1 end2 &aux idx)
  (or (null (setq idx (mismatch prefix sequence :test test :start1 start1 :start2 start2 :end1 end1 :end2 end2)))
      (>= idx (length prefix))))

(defun strip-prefix (prefix string)
  (and (prefixp #1=prefix string)
       (subseq string (length #1#))))

;;madhu 250829 ala zfs-promote.  promote nickname to no longer depend
;; on origin package
(defun package-promote (new)
  (let* ((old-pkg (find-package new))
	 (orig-name (package-name old-pkg))
	 (nicknames (package-nicknames old-pkg)))
    (assert (find new nicknames :test #'equal))
    (assert (not (find orig-name nicknames :test #'equal)))
    (rename-package old-pkg
		    new
		    (adjoin orig-name
			    (remove new nicknames :test #'equal)
			    :test #'equal))))

(defun rename-shunter-package (pkg)
  (let* ((orig-name (package-name pkg))
	 (base (strip-prefix "XYZ.SHUNTER" orig-name)))
    (when base
      (let* ((nicknames (package-nicknames pkg))
	     (nick (car nicknames)))
	(when nicknames
	  (assert (endp (cdr nicknames)))
	  (package-promote nick))))))

;; add a nickname so we can rename it later
(rename-package "XYZ.SHUNTER.WAYFLAN.CLIENT.SCANNER"
		"XYZ.SHUNTER.WAYFLAN.CLIENT.SCANNER"
		'("WAYFLAN-CLIENT.SCANNER"))
(mapcar 'rename-shunter-package (find-packages-matching "XYZ.SHUNTER"))

#||
(setq $a (find-packages-matching "XYZ.SHUNTER"))
(mapcar (lambda (x) (strip-prefix "XYZ.SHUNTER." x)) $a)
(mapcar (lambda (x) (list :base (strip-prefix "XYZ.SHUNTER." x)
			  :nicknames (package-nicknames x)
			  :orig x))
	$a)
(package-name (find-package "XYZ.SHUNTER.WAYFLAN.CLIENT"))
(package-nicknames (find-package "XYZ.SHUNTER.WAYFLAN.CLIENT"))
||#
