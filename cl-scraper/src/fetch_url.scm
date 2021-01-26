;;;;
;;;; Scheme module to fetch data  from a url
;;;;


;;;;
;;;; Library definition
(module (jplankton fetch_url) *
  (import (chicken base)
	  scheme
	  r7rs
	  (chicken io)
	  (chicken file)
	  (chicken port)
	  (chicken process)
	  (chicken process-context)
	  http-client
	  ssax
	  (srfi 13))


  ;;
  ;; perform a GET request for a url
  ;; and returns the result as a byte array
  (define (%fetch-bytes url MAX-BYTES)
    (with-input-from-request url #f (lambda () (read-bytevector MAX-BYTES))))

  ;;
  ;; perform a GET request for url
  ;; and return the result as a string
  (define (%fetch-as-string url MAX-BYTES)
    (utf8->string (%fetch-bytes url MAX-BYTES)))

  ;;
  ;; transform HTML to XHTML
  (define (%html->xhtml inport outport)
    (let* ((temp-dir (create-temporary-directory))
	   (htmlpath (string-append temp-dir "/" "input.html"))
	   (xhtmlpath (string-append temp-dir "/" "output.xhtml")))
      (dynamic-wind
	(lambda () #t)
	(lambda ()
	  (with-output-to-file htmlpath
	    (lambda () (copy-port inport (current-output-port))))
	  (let ((args (list "--html"
			    "--xmlout"
			    "--format"
			    "-o" xhtmlpath
			    htmlpath)))
	    (let-values
		(((proc-output proc-input pid) (process "xmllint" args)))
	      (close-input-port proc-output)
	      (close-output-port proc-input)
	      (with-input-from-file xhtmlpath
		(lambda () (copy-port (current-input-port) outport))))))
	(lambda ()  (delete-directory temp-dir #t)))))
  

  (define (fetch-xhtml-string url #!optional (MAX-BYTES 104857600))
    (with-input-from-string (%fetch-as-string url MAX-BYTES)
      (lambda ()
	(with-output-to-string 
	  (lambda () (%html->xhtml (current-input-port) (current-output-port)))))))


  (define *test-parser*
    (ssax:make-parser

     NEW-LEVEL-SEED 
     (lambda (elem-gi attributes namespaces
		      expected-content seed)
       (cons elem-gi seed))
     
     FINISH-ELEMENT
     (lambda (elem-gi attributes namespaces parent-seed seed)
       seed)
     
     CHAR-DATA-HANDLER
     (lambda (string1 string2 seed)
       seed)
     ))
  
  )
