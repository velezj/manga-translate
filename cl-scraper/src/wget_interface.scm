;;;;
;;;; Scheme module to use/parse wget
;;;;


;;;;
;;;; Library definition
(module (jplankton wget_interface) *
  (import (chicken base)
	  scheme
	  r7rs
	  (chicken io)
	  (chicken port)
	  (chicken pretty-print)
	  (chicken irregex)
	  (chicken process)
	  (srfi 1)
	  (srfi 13)
	  (srfi 28)
	  utf8)


  ;;
  ;; create the full wget argument list from the user's wanted
  ;; argumetns and those needed by this module
  (define (%wget-arguments-merge args)
    (concatenate! (list args '("--server-response"))))

  ;;
  ;; parse out a request chunk from the output
  ;; given the header line and a port with the rest
  (define (%parse-wget-request-output headerline port)
    (let ((full-string "")
	  (request-string ""))
      (do (( (line (read-line)) (line (read-line))))
	  ((or (eof-object? line)
	       (string-prefix? line "--"))
	   (values full-string request-string))
	#f)))
	

  ;;
  ;; parse the wget output
  (define (%wget-output-parse port)
    (let ((output-string "")
	  (requested-urls '()))
      (do (( (line (read-line port))
	     (line (read-line port))))
	  ((eof-object? line) (list
			       (cons full: output-string)
			       (cons request-urls: requested-urls)))
	(if (string-prefix? line "--")
	    (let-values (( (full-string request-string)
			   (%parse-wget-request-output line port)))
	      (set! output-string (string-append output-string full-string))
	      (set! requested-urls
		(append requested-urls (list %parse-request-string
					     request-string)))
	      (set! output-string
		(string-append output-string line (string #\newline))))))))
  
  ;;
  ;; runs wget
  (define (%call-wget-and-parse-output args)
    (let ((full-args (%wget-arguments-merge args)))
      (let-values (( (proc-outport proc-inport pid proc-errport)
		     (process* "wget" full-args)))
	(dynamic-wind
	  (lambda () #f)

	  (lambda ()
	    (let ((outres (%wget-output-parse proc-outport)))
	      (copy-port proc-errport (current-output-port))
	      outres))

	  (lambda ()
	    (close-output-port proc-inport)
	    (close-input-port proc-outport)
	    (close-input-port proc-outport))))))

  

  )
