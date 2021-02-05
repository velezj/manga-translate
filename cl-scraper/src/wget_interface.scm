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
	  (chicken condition)
	  (srfi 1)
	  (srfi 13)
	  (srfi 28)
	  ;; (srfi 35)
	  (jplankton line_port)
	  utf8)

  ;;
  ;; condition for failed parsing
  ;; (define-condition-type &parse-error &message
  ;;   parse-error?)

  ;;
  ;; create the full wget argument list from the user's wanted
  ;; argumetns and those needed by this module
  (define (%wget-arguments-merge args)
    (concatenate! (list '("--server-response") args)))

  ;;
  ;; parse out hte request lines from the line port 
  (define (%parse-wget-request-output line-port)
    (let ((result
	   (let ((lines (list (line-input-port/read-line line-port))))
	     (do ((line (line-input-port/read-line line-port)
			(line-input-port/read-line line-port)))
		 ((or (eof-object? line)
		      (string-prefix? "--" line))
		  (if (eof-object? line)
		      (reverse lines)
		      (begin
			(line-input-port/unread-line line-port)
			(reverse lines))))
	       (set! lines (cons line lines))))))
      result))

  ;;
  ;; Parse out the liens from  the request into an alist structure
  (define (%parse-request-lines lines)
    (let ((found-results '()))

      ;; Look over all lines and extract save location(s)
      (for-each
       (lambda (line)
	 (let ((matches (irregex-match "^.*Saving to: ‘(.*)’.*$" line)))
	   (if (and matches
		    (irregex-match-valid-index? matches 1))
	       (begin
		 (set! found-results
		   (alist-cons save-filename:
			       (irregex-match-substring matches 1)
			       found-results))
		 (display (format "SAVED: ~a"
				  (irregex-match-substring matches 1)))
		 (newline)))))
       lines)

      ;; look for errors
      (for-each
       (lambda (line)
	 (let ((matches (irregex-match "^.*ERROR (.*):(.*)$" line)))
	   (if (and matches
		    (irregex-match-valid-index? matches 1)
		    (irregex-match-valid-index? matches 2))
	       (let ((code (irregex-match-substring matches 1))
		     (message (irregex-match-substring matches 2)))
		 (display (format "ERROR: ~a (~a)" code message))
		 (newline)
		 (set! found-results
		   (alist-cons error: (list code message)
			       found-results))))))
       lines)

      ;; Look at the header line for the url accessed
      (let ((matches (irregex-match
		      "^--(\\d+)-(\\d+)-(\\d+)[^-]+--\\s+(.*)\s*$"
		      (first lines))))
	(if (not matches)
	    (begin
	      (let ((message
	    	     (format (string-append
	    		      "Unable to parse request header line."
	    		      "line=~a")
	    		     (first lines))))
		(signal (condition (list 'exn 'message message))))) 
	    (if (not (irregex-match-valid-index? matches 4))
		(begin
		  (let ((message
			 (format (string-append "Failed to parse wget request "
						"header line, line=~a")
				 (first lines))))
		    (signal (list 'exn 'message message))))
		(begin
		  (set! found-results
		    (alist-cons url: (irregex-match-substring matches 4)
				found-results))
		  (display (format "URL: ~a"
				   (irregex-match-substring matches 4)))
		  (newline)))))
	
      found-results))
	  

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
			       (cons request-parameters: requested-urls)))
	(if (string-prefix? "--" line)
	    (begin
	      (line-input-port/unread-line line-port)
	      (let ((request-lines (%parse-wget-request-output line-port)))
		(set! output-lines (append (reverse request-lines)
					   output-lines))
		(set! requested-urls
		  (append requested-urls (list (%parse-request-lines
						request-lines))))))
	    (set! output-lines (cons line output-lines))))))
			     
  ;;
  ;; runs wget
  (define (%call-wget-and-parse-output args)
    (let ((full-args (%wget-arguments-merge args)))
      (display (format "WGET: ~a" full-args))
      (newline)
      (let-values (( (proc-outport proc-inport pid proc-errport)
		     (process* "wget" full-args)))
	(dynamic-wind
	  (lambda () #f)

	  (lambda ()
	    (let ((outres (%wget-output-parse proc-errport)))
	      ;; WGET writes output to STDERR (?!)
	      ;;(copy-port proc-errport (current-output-port))
	      outres))

	  (lambda ()
	    (close-output-port proc-inport)
	    (close-input-port proc-outport)
	    (close-input-port proc-outport))))))


  ;;
  ;; performs a wget recursive request and return the resulting
  ;; files and the list of urls followed
  (define (wget-recursive root-url depth-limit accept-regex
			  #!optional (wait 10) )
    (let ((args (list "--recursive"
		      "--level" (number->string depth-limit)
		      "--regex-type" "posix"
		      "--accept-regex" accept-regex
		      "--span-hosts"
		      "--wait" (number->string wait)
		      "--random-wait"
		      "--trust-server-names"
		      root-url)))
      (let ((wget-results (%call-wget-and-parse-output args)))
	wget-results)))

  )
