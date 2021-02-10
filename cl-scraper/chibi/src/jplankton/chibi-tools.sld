;;;;
;;;; Library definition
(define-library (jplankton chibi-tools)
  (export reimport)
  (import (scheme base)
	  (scheme eval)
          (meta))
  (begin
    (define (reimport module)
      (delete-module! module)
      (eval `(repl-import ,module)))))
