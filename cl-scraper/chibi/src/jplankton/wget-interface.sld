;;;;
;;;; Library definition
(define-library (jplankton wget-interface)
  (export wget-recursive throw-condition &parse-error)
  (import (scheme base)
	  (scheme write)
	  (srfi 1)   ; list library
	  (srfi 23)  ; (error ...)
	  (srfi 35)  ; conditions
	  (srfi 115) ; regex
	  (srfi 130) ; string library (scursor-based)
	  (srfi 166) ; monadic formatting
	  (jplankton line-port)
	  (jplankton process)
	  (chibi ast))

  (include "wget_interface.scm")
  
)
