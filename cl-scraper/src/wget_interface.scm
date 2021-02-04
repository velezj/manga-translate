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
	  (jplankton line_port)
	  utf8)


  ;;
  ;; create the full wget argument list from the user's wanted
  ;; argumetns and those needed by this module
  (define (%wget-arguments-merge args)
    (concatenate! (list args '("--server-response"))))

  ;;
  ;; parse out hte request lines from the line port 
  (define (%parse-wget-request-output line-port)
    (let ((header (line-input-port/read-line line-port))
	  (lines '()))
      (do ((line (line-input-port/read-line line-port)
		 (line-input-port/read-line line-port)))
	  ((or (eof-object? line)
	       (string-prefix? line "--"))
	   (if (eof-object? line)
	       (reverse lines)
	       (reverse (cons line lines))))
	(set! lines (cons line lines)))))
    

  ;;
  ;; Parse out the liens from  the request into an alist structure
  (define (%parse-request-lines lines)
    (let ((matches (irregex-match "^--[^-]+--\\s+(.*)\s*$" (first lines))))
      (if (not (irregex-match-valid-index? matches 1))
	  (raise (format (string-append "Failed to parse wget request header "
					"line, line=~a")
			 (first lines)))
	  (list
	   (cons url: (irregex-match-substring matches 1))))))
	  

  ;;
  ;; parse the wget output
  (define (%wget-output-parse port)
    (let ((output-lines '())
	  (requested-urls '())
	  (line-port (line-input-port port 1 0 '())))
      (do ((line (line-input-port/read-line line-port)
		 (line-input-port/read-line line-port)))
	  ((eof-object? line) (list
			       (cons lines: (reverse output-lines))
			       (cons request-urls: requested-urls)))
	(if (string-prefix? line "--")
	    (begin
	      (line-input-port/unread-line line-port)
	      (let ((request-lines (%parse-wget-request-output line-port)))
		(set! output-lines (append (reverse request-lines)
					   output-lines))
		(set! requested-urls
		  (append requested-urls (list %parse-request-lines
					       request-lines)))))
	    (set! output-lines (cons line output-lines))))))
			     
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
