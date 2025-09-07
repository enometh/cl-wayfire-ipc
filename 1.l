(in-package "CL-USER")
(load "~/cl/extern/Github/cl-xkb/cl-xkb.system")
(load "~/cl/extern/Github/posix-shm/posix-shm.system")
(load "~/cl/extern/Github/wayflan/wayflan.system")
(load "~/cl/extern/cl-cairo2/cl-cairo2.system")
(load "~/cl/extern/Github/xmls/xmls.system")
(load "~/cl/extern/Github/cl-pango/cl-pango.system")
(load "~/cl/extern/Github/input-event-codes/input-event-codes.system")
(load "~/cl/extern/wayflan-ext/wayflan-alt.system")
(load "~/cl/extern/wayflan-ext/wayflan-ext.system")

(unless (cffi:find-foreign-library "libwayland-client")
  (cffi:load-foreign-library "libwayland-client.so"))

#+nil
(mk:oos :wayflan-client-ext :compile)

#+nil
(mk:oos :wayflan-ext :compile)

#+nil
(mk:oos :wayflan-hyprctm :load :compile-during-load t)

#+nil
(mk:oos :wayflan-wlr-gammactl :load :compile-during-load t)


;;; ----------------------------------------------------------------------
;;;
;;; user intf
;;;
(in-package "WAYFLAN-EXT")

#+nil
(defvar $c (make-client-state))
#+nil
(disconnect $c)
#+nil
(connect $c)

;; wlr-gammactl
#+nil
(setup-gamma-control $c)
#+nil
(set-gammactl $c  .9 .98 .9 0.89)
