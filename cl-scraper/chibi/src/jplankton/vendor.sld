(define-library (jplankton vendor)
  (export custom-printer-set!
	  process* process-wait
	  type-of )

  (cond-expand
    (chibi (import (rename (jplankton vendor chibi)
			   (chibi/custom-printer-set! custom-printer-set!)
			   (chibi/process* process*)
			   (chibi/process-wait process-wait)
			   (chibi/type-of type-of))))
    (else (error "This scheme implementation has not been integrated into this system! Now (jplankton vendor XXX) found for this implementation XXX"))))

  
