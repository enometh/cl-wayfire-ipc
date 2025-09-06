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

#+nil
(mk:oos :wayflan-client-ext :compile)

#+nil
(mk:oos :wayflan-ext :compile)