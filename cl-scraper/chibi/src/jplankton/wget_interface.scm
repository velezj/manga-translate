;;;;
;;;; Scheme module to use/parse wget
;;;;



;;
;; condition for failed parsing
(define-condition-type &parse-error &message
  parse-error?)
(define (%%write-parse-error parse-err wr out)
  (write-string "{CONDITION <parse-error>: " out)
  (wr (condition-ref parse-err 'message))
  (write-string "}" out))   
(custom-printer-set!
 (type-of (condition (&parse-error (message ""))))
 %%write-parse-error)

;;
;; error throwing that will display the condition using the
;; custom printer
(define (throw-condition c)
  (raise (list (show #f c) c)))


;;
;; create the full wget argument list from the user's wanted
;; argumetns and those needed by this module
(define (%wget-arguments-merge args)
  (concatenate (list '("--server-response") args)))

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
;; parses out the lines given a list of parsers/callbacks.
;; each parser is called in an arbitrary order and each
;; must return two values: ( parser-id, list of results )
;; These lists are concatenated into the final result of
;; this function
(define (%parse-request-lines lines parsers)
  (let ((found-results '()))
    (for-each
     (lambda (parser)
       (let-values (( (name local-results) (parser lines)))
	 ;; (display (show #f "  ran parser "
	 ;; 		"'" (displayed name) "'"
	 ;; 		nl))
	 (if (not (list? local-results))
	     (let ((msg (show #f
			      "parser "
			      "'" (displayed name) "'"
			      " returned non-list results: "
			      (written local-results))))
	       (throw-condition
		(condition (&parse-error (message msg)))))
	     (set! found-results
	       (append local-results found-results)))))
     parsers)
    found-results))


;;
;; Parse out urls being requested by wget
(define (%parser/request-urls lines)
  (let ((found-results '())
	(matches (regexp-search
		  '(: bol "--" (+ num) "-" (+ num) "-"
		      (+ num) (+ (~ "-")) "--" (+ space)
		      ($ (+ any)) (* space) eol)
		  (first lines))))
    (if (not matches)
	(begin
	  (let ((message
	    	 (show #f 
	    	       "Unable to parse request header line."
	    	       "line="
	    	       (displayed (first lines)))))
	    (display message)
	    (newline)
	    (throw-condition (condition (&parse-error (message message))))))
	(begin
	  (set! found-results
	    (alist-cons 'url: (regexp-match-submatch matches 1)
			found-results))
	  (display (show #f "URL: "
			 (displayed (regexp-match-submatch matches 1))
			 nl))))
    (values '%parser/request-urls found-results)))


;;
;; parse out saved files
(define (%parser/saved-files lines)
  (let ((found-results '()))
    (for-each
     (lambda (line)
       (let ((matches (regexp-search
		       '(: bol
			   (* any) "Saving to: "
			   any ($ (* any)) any
			   (* any)
			   eol)
		       line)))
	 (if matches
	     (begin
	       (set! found-results
		 (alist-cons 'save-filename:
			     (regexp-match-submatch matches 1)
			     found-results))
	       (display (show #f "SAVED: "
			      (displayed (regexp-match-submatch matches 1))
			      nl))))))
     lines)
    (values '%parser/saved-files found-results)))


;;
;; parse out errors from wget
(define (%parser/errors lines)
  (let ((found-results '()))
    (for-each
     (lambda (line)
       (let ((matches (regexp-search
		       '(: bol (* any) "ERROR "
			   ($ (* any)) ":" ($ (* any))
			   eol)
		       line)))
	 (if matches
	     (let ((code (regexp-match-submatch matches 1))
		   (message (regexp-match-submatch matches 2)))
	       (display (show #f "ERROR: "
			      (displayed code)
			      "( " (displayed message) ")"
			      nl))
	       (set! found-results
		 (alist-cons 'error: (list code message)
			     found-results))))))
     lines)
    (values '%parser/errors found-results)))





;;
;; parse the wget output
(define (%wget-output-parse port parsers)
  (let ((output-lines '())
	(requested-urls '())
	(line-port (line-input-port port 1 0 '())))
    (do ((line (line-input-port/read-line line-port)
	       (line-input-port/read-line line-port)))
	((eof-object? line) (list
			     (cons 'lines: (reverse output-lines))
			     (cons 'request-parameters: requested-urls)))
      (if (string-prefix? "--" line)
	  (begin
	    (line-input-port/unread-line line-port)
	    (let ((request-lines (%parse-wget-request-output line-port)))
	      (set! output-lines (append (reverse request-lines)
					 output-lines))
	      (set! requested-urls
		(append requested-urls (list (%parse-request-lines
					      request-lines parsers))))))
	  (set! output-lines (cons line output-lines))))))


;;
;; runs wget
(define (%call-wget-and-parse-output args parsers)
  (let ((full-args (%wget-arguments-merge args)))
    (display (show #f "WGET: " (displayed full-args) nl))
    (let-values (( (proc-outport proc-inport proc-errport pid)
		   (process* "wget" full-args)))
      (dynamic-wind
	(lambda () #f)

	(lambda ()
	  (let ((outres (%wget-output-parse proc-errport parsers)))
	    ;; WGET writes output to STDERR (?!)
	    ;;(copy-port proc-errport (current-output-port))
	    outres))

	(lambda ()
	  (process-wait pid)
	  (close-output-port proc-inport)
	  (close-input-port proc-outport)
	  (close-input-port proc-outport))))))

;;
;; Default list of parsers to apply
(define *DEFAULT-PARSERS*
  (list
   %parser/request-urls
   %parser/errors
   %parser/saved-files))

;;
;; performs a wget recursive request and return the resulting
;; files and the list of urls followed
(define (wget-recursive root-url depth-limit accept-regex wait parsers)
  (let ((args (list "--recursive"
		    "--level" (number->string depth-limit)
		    "--regex-type" "posix"
		    "--accept-regex" accept-regex
		    "--span-hosts"
		    "--wait" (number->string wait)
		    "--random-wait"
		    "--trust-server-names"
		    root-url)))
    (let ((wget-results (%call-wget-and-parse-output args parsers)))
      wget-results)))
