(define-library (jplankton line-port test)
  (import (scheme base)
	  (jplankton line-port)
	  (srfi 78))
  (export run-tests)
  
  (include "line_port_test.scm"))
