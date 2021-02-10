;;;;
;;;; Scheme module to create a line-oriented port
;;;; that can "unread" a number of lines
;;;;


;;;;
;;;; Library definition
(define-library (jplankton line-port)
  (import (scheme base)
	  (scheme read)
	  (srfi 1)
	  (srfi 9)
	  (srfi 23) 
	  (srfi 166))
  (export line-input-port
	  line-input-port?
	  line-input-port/read-line
	  line-input-port/unread-line
	  line-input-port/close-port)  
  (include "line_port.scm"))
