(in-package :cl-user)

(defpackage :symbolist)

(push :symbolist *features*)

(in-package :symbolist)

(cffi:defcfun ("symbolistInit" symbolistInit) :int)
(cffi:defcfun ("symbolistExit" symbolistExit) :int)
(cffi:defcfun ("symbolistInfo" symbolistInfo) :string)


