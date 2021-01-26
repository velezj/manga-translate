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
	  (srfi 13))

  ;;
  ;; performs a generic xpath query and return the results
  ;; as a list of xml strings
  ;;
  ;; The input xml is given as a string and teh xpath query
  ;; is *also* given as a string
  (define (%xpath-query-return-xml-string xml-doc xpath-query-string)
    (let* ((temp-dir (create-temporary-directory))
	   (docpath (string-append temp-dir "/" "doc.xml")))
      (dynamic-wind
	(lambda () #f)

	(lambda () 
	  (with-output-to-file docpath
	    (write-bytevector (string->utf8 xml-doc)))
	  (let ((args (list "--xml"
			    "--xpath" xpath-query-string)))
	    (let-values (( (proc-outport proc-inport oid)
			   (process "xidel" args)))
	      (dynamic-wind
		(lambda () #f)
		
		(lambda ()
		  (with-output-to-string
		    (copy-port proc-outport (current-output-port))))
		  
		(lambda ()
		  (close-input-port proc-outport)
		  (close-output-port proc-inport))))))
	
	(lambda ()  (delete-directory temp-dir #t)))))


  )
