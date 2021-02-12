(define-library (jplankton vendor chibi)
  (export chibi/custom-printer-set!
	  chibi/process*
	  chibi/process-wait
	  chibi/type-of)
  (import (scheme base)
	  (chibi process)
	  (chibi ast))

  (include "chibi.scm"))
