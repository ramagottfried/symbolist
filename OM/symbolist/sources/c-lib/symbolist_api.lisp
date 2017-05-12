

(in-package :symbolist)

(cffi:defcfun ("symbolistInit" symbolistInit) :int)
(cffi:defcfun ("symbolistExit" symbolistExit) :int)
(cffi:defcfun ("symbolistInfo" symbolistInfo) :string)
(cffi:defcfun ("symbolistNewWindow" symbolistNewWindow) :pointer)
(cffi:defcfun ("symbolistNewWindowWithSymbols" symbolistNewWindowWithSymbols) :pointer (n-symbols :int) (bundle-array :pointer))

