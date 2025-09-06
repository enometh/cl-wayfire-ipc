;;; src/stable-protocols.lisp -- Wayland stable protocols implementations
;;;
;;; Copyright (c) 2022 Samuel Hunter <samuel (at) shunter (dot) xyz>.
;;; This work is licensed under the BSD 3-Clause License.
;;; See LICENSE for more details.

(in-package #:xyz.shunter.wayflan.client.presentation-time)
(xyz.shunter.wayflan.client.scanner:wl-include
 "/usr/share/wayland-protocols/stable/presentation-time/presentation-time.xml"
  :export t)

(in-package #:xyz.shunter.wayflan.client.viewporter)
(xyz.shunter.wayflan.client.scanner:wl-include
 "/usr/share/wayland-protocols/stable/viewporter/viewporter.xml"
  :export t)

(in-package #:xyz.shunter.wayflan.client.xdg-shell)
(xyz.shunter.wayflan.client.scanner:wl-include
 "/usr/share/wayland-protocols/stable/xdg-shell/xdg-shell.xml"
  :export t)
