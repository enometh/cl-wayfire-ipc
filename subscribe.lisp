;;; -*- Mode: LISP; Package: :cl-user; BASE: 10; Syntax: ANSI-Common-Lisp; -*-
;;;
;;;   Time-stamp: <>
;;;   Touched: Mon Sep 07 19:26:45 2026 +0530 <enometh@net.meer>
;;;   Bugs-To: enometh@net.meer
;;;   Status: Experimental.  Do not redistribute
;;;   Copyright (C) 2026 Madhu.  All Rights Reserved.
;;;
(in-package "WAYFIRE-IPC")

;; event descs taken from /7/gtk/pywayfire/wayfire/ipc.py commit
;; 23ac6bbd
(defvar $known-events
  (let ((ret (make-hash-table :test #'equal))
	(elts
   '((
        "view-focused" .
            "Emitted when input focus changes.
            The `view` field may be a toplevel view or None.
            A None value commonly occurs during switcher or overview plugins.")
   (
        "view-unmapped" .
            "Emitted when a view is hidden or closed.")
   (
        "view-pre-map" .
            "Emitted immediately before a view is mapped.
            Mapping may be delayed until `unblock-map` is called.
            NOTE: This event is only received if explicitly subscribed to.")
   (
        "view-mapped" .
            "Emitted when a view becomes visible on screen.")
   (
        "view-title-changed" .
            "Emitted when a view title changes.")
   (
        "view-app-id-changed" .
            "Emitted when a view application ID changes.")
   (
        "view-set-output" .
            "Emitted when a view is moved to another output (monitor).")
   (
        "view-workspace-changed" .
            "Emitted when a view changes workspace.")
   (
        "view-wset-changed" .
            "Emitted when a view changes workspace set.")
   (
        "view-geometry-changed" .
            "Emitted when a view position or size changes.")
   (
        "view-tiled" .
            "Emitted when a view is tiled or snapped.")
   (
        "view-minimized" .
            "Emitted when a view is minimized or restored.")
   (
        "view-fullscreen" .
            "Emitted when a view enters or exits fullscreen.")
   (
        "view-sticky" .
            "Emitted when a view becomes sticky or unsticky.")
   (
        "view-always-on-top" .
            "Emitted when a view always-on-top state changes.")
   (
        "workspace-activated" .
            "Emitted when a workspace is activated.")
   (
        "wset-workspace-changed" .
            "Emitted when the active workspace inside a workspace set changes.")
   (
        "output-gain-focus" .
            "Emitted when an output gains input focus.")
   (
        "output-wset-changed" .
            "Emitted when an output changes workspace set.")
   (
        "output-layout-changed" .
            "Emitted when output configuration changes.")
   (
        "plugin-activation-state-changed" .
            "Emitted when a plugin is activated or deactivated.")
   (
        "keyboard-modifier-state-changed" .
            "Emitted when keyboard modifier keys change state.
            There could also be other plugin-specific events."))))
    (loop for (k . v) in elts
	  do (setf (gethash k ret) v))
    ret))


(defun check-event-name (s &optional ($known-events $known-events))
  (nth-value 1 (gethash s $known-events)))

(defun subscribe-events (c &rest event-names)
  "EVENT-NAMES is a list of strings to subscribe to. if the list is empty
all events are subscribed to. BEWARE. there is no way to unsubscribe,
you have to close the socket.  Receive events by calling
read-next-event. No part of WAYFIRE-IPC is thread safe. Make sure only
one thread is calling READ-NEXT-EVENT at any point of time."
  (cond ((consp (car event-names))
	 (assert (endp (cadr event-names)))
	 (setq event-names (car event-names))))
  (assert (every 'check-event-name event-names))
  (let* ((m (apply #'get-msg-template
		   "window-rules/events/watch"
		   (append (if event-names
			       (list "events" event-names)))))
	 (ret (send-json c m)))
    (values (ht->x ret) ret)))

(def-simple-f "window-rules/unblock-map" "view-id")

#||
(time (read-next-event $c))
(subscribe-events $c)
(loop for e = (read-next-event $c)
      while e
      do (format t "event: ~S~&" (ht->x e)))
||#