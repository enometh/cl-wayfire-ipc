(in-package "CL-USER")

(defpackage #:wayflan-client.wlr-gamma-control
  (:use #:cl #:wayflan-client)
  (:nicknames #:xyz.shunter.wayflan.client.wlr-gamma-control)
  (:documentation "WLR gamma control unstable v1 implementation."))

(defpackage #:wayflan-client.hyprctm
  (:use #:cl #:wayflan-client)
  (:nicknames #:xyz.shunter.wayflan.client.hyprland-ctm-control)
  (:documentation "controlling outputs' color transform matrix"))

(in-package #:xyz.shunter.wayflan.client.wlr-gamma-control)
(xyz.shunter.wayflan.client.scanner:wl-include
 (mk::system-relative-pathname :wayflan-client/unstable-protocols
			       "client/protocols/wlr-gamma-control-unstable-v1.xml")
  :export t)

(in-package #:xyz.shunter.wayflan.client.hyprland-ctm-control)
(xyz.shunter.wayflan.client.scanner:wl-include
 (mk::system-relative-pathname :wayflan-client/unstable-protocols
			       "client/protocols/hyprland-ctm-control-v1.xml")
  :export t)
