;;;;
;;;; Scheme module to fetch data  from a url
;;;;


;;;;
;;;; Library definition
(module (jplankton xml_utils) *
  (import (chicken base)
	  scheme
	  r7rs
	  (chicken io)
	  (chicken file)
	  (chicken port)
	  (chicken process)
	  (chicken irregex)
	  (srfi 13)
	  (srfi 28)
	  (srfi 35))


  ;;
  ;; The separator between Xidel output elements
  (define *XIDEL-SEPARATOR* "<><><><><><><><><>")

  ;;
  ;; The header in the output by xidel
  (define *XIDEL-OUTPUT-HEADER* "**BEGIN-OUTPUT**")

  ;;
  ;; The total maximum size (in bytes) for the output of
  ;; a call to xidel. This limits the amount of data ever
  ;; read from a process stdout to memory
  (define *XIDEL-MAX-OUTPUT-SIZE* (* 1024 1024 100))
  
  ;;
  ;; PERFORMS a generic xpath query and return the results
  ;; as a list of xml strings
  ;;
  ;; The input xml is given as a string and teh xpath query
  ;; is *also* given as a string
  (define (%xpath-query-return-xml-string xml-doc xpath-query-string)
    (let* ((temp-dir (create-temporary-directory))
  	   (docpath (string-append temp-dir "/" "doc.xml")))
      ;;(display (format "\ndocpath: ~a\n\n" docpath))
      (dynamic-wind
  	(lambda () #f)

  	(lambda () 
  	  (with-output-to-file docpath
	    (lambda ()
  	      (write-bytevector (string->utf8 xml-doc))))
  	  (let ((args (list "--xml"
			    "--output-format" "xml"
			    "--output-header" *XIDEL-OUTPUT-HEADER*
			    "--output-footer" ""
  			    "--output-separator" *XIDEL-SEPARATOR*
  			    "--xpath" xpath-query-string
  			    docpath )))
  	    (let-values (( (proc-outport proc-inport pid proc-errport)
  			   (process* "xidel" args)))
  	      (dynamic-wind
  		(lambda () #f)
		
  		(lambda ()
		  (with-output-to-string
		    (lambda ()
		      (copy-port proc-outport (current-output-port)))))
		  
  		(lambda ()
  		  (close-input-port proc-outport)
		  (close-input-port proc-errport)
  		  (close-output-port proc-inport))))))
	
  	(lambda ()  (delete-directory temp-dir #t)))))

  
  ;;
  ;; cheap and dirty "xml" parser used *just* to extract results from
  ;; the xidel calls
  (define (%xidel-xml-result-parse-to-list xidel-xml)
    (with-input-from-string xidel-xml
      (lambda ()
	(let* ((prolog (%parse-xidel-prolog (current-input-port)))
	       (elements (%parse-xidel-elements (current-input-port)))
	       (ending (%parse-xidel-ending (current-input-port))))
	  (values elements (current-input-port))))))

  ;;
  ;; Parse the xidel result xml prolog
  (define (%parse-xidel-prolog inport)
    (%read-line-expect "<?xml version=\"1.0\" encoding=\"UTF-8\"?>" inport)
    (let ((header (read-string (string-length *XIDEL-OUTPUT-HEADER*) inport)))
      (if (eof-object? header)
	  ""
	  (if (equal? header *XIDEL-OUTPUT-HEADER*)
	      ""
	      (raise (format "did not find initial xidel xml prolog: expected '~a' but found '~a'" *XIDEL-OUTPUT-HEADER* header))))))

  ;;
  ;; parse the xidel elements
  (define (%parse-xidel-elements inport)
    (if (eof-object? (peek-char inport))
	'()
	(let ((string-data (read-string *XIDEL-MAX-OUTPUT-SIZE* inport)))
	  (irregex-split *XIDEL-SEPARATOR* string-data))))


  ;;
  ;; parse the xidel ending
  (define (%parse-xidel-ending inport)
    (%read-line-expect (list "</xml>" #!eof) inport))

  ;; condition meaning the expected parsed input was not read
  (define-condition-type &expected-read-failed &error
    expected-read-failed
    (msg expected-read-failed-msg))

  ;;
  ;; read a line from the input port and check that it
  ;; matches the given string, raise error otherwise
  (define (%read-line-expect expected inport)
    (let ((line (read-line inport)))
      (if (or (equal? line expected)
	      (and (string? line)
		   (string? expected)
		   (string= line expected))
	      (and (list? expected)
		   (member line expected)))
	  line
	  (let ((msg
		 (format
		  "unexpected line read: expected='~a' read='~a'"
		  expected
		  line)))
	    ;; (raise (condition (&expected-read-failed (msg msg))))))))
	    (raise msg)))))


  ;;
  ;; The main interface to perform and xpath query
  (define (xpath-query xml-doc query-string)
    (%xidel-xml-result-parse-to-list
     (%xpath-query-return-xml-string xml-doc query-string)))
  
  )
