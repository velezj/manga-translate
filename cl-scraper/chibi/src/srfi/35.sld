(define-library (srfi 35)

  (export make-condition-type
	  condition-type?
	  make-condition
	  condition?
	  condition-has-type?
	  condition-ref
	  make-compound-condition
	  extract-condition
	  define-condition-type
	  condition
	  &condition
	  &message
	  &serious
	  &error)

  (import (scheme base)
	  (srfi 1)
	  (srfi 9)
	  (srfi 23))

  (include "srfi-35.scm"))
