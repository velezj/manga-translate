;;;;
;;;; Interface to the NordVPN vpn service.
;;;;
;;;; This is a wrapper to the command-line 'nordvpn' program that
;;;; hides the commands and makes results scheme-y



;;;;
;;;; Library definition
(module (jplankton nordvpn) *
  (import (chicken base)
	  scheme
	  (chicken process)
	  (chicken io)
	  (chicken random)
	  (chicken condition)
	  (chicken process-context)
	  (chicken pretty-print)
	  (srfi 13)
	  (srfi 14)
	  (srfi 1))

  (define (%nordvpn-command output-parser . rest)
    (let-values
	(((proc-output proc-input pid) (process "nordvpn" rest)))
      (let ((result
	     (with-exception-handler
		 (lambda (ex) 'error)
	       (lambda () (output-parser proc-output)))))
	(close-input-port proc-output)
	(close-output-port proc-input)
	result)))
  
  (define *max-output-buffer-size* (* 1024 1024))
  
  (define (%line-list-parser inport)
    (map
     (lambda (c) (string-trim-both c #\, ))
     (filter
      (lambda (c) (> (string-length c) 2))
      (string-tokenize (read-string *max-output-buffer-size* inport)))))

  (define (%single-string-parser inport)
    (read-string *max-output-buffer-size* inport))
  
  (define (%available-countries)
    (%nordvpn-command
     %line-list-parser
     "countries"))

  (define (%available-cities nordvpn-country-ident)
    (%nordvpn-command
     %line-list-parser
     "cities"
     nordvpn-country-ident))

  (define (%random-list-element lst)
    (list-ref
     lst
     (pseudo-random-integer (length lst))))

  (define (%select-random-nordvpn-country-city)
    (let* ((countries (%available-countries))
	   (chosen-country (%random-list-element countries))
	   (cities (%available-cities chosen-country))
	   (chosen-city (%random-list-element cities)))
      (values chosen-country chosen-city)))

  (define (%nordvpn-connect country city)
    (%nordvpn-command
     %single-string-parser
     "connect"
     country
     city))

  (define (%nordvpn-status)
    (%nordvpn-command
     %single-string-parser
     "status"))

  (define (%parse-lines-and-colons str)
    (let ((lines (string-tokenize
		  str
		  (char-set-complement (char-set #\newline #\return)))))
      (map
       (lambda (kv) (cons (car kv) (cadr kv)))
       (filter
	(lambda (kv) (= (length kv) 2))
	(map
	 (lambda (line)
	   (map
	    (lambda (tok)
	      (string-trim-both tok))
	    (string-tokenize line (char-set-complement (char-set #\:)))))
	 lines)))))
   
  (define (%parse-status-to-alist str)
    (%parse-lines-and-colons str))

  (define (%nordvpn-connection-info)
    (let* ((status-string (%nordvpn-status))
	   (info (%parse-status-to-alist status-string)))
      info))
  
  (define (connect-to-random-city)
    (let-values (( (country city) (%select-random-nordvpn-country-city)))
      (%nordvpn-connect country city)))

  (define (connected-to-vpn?)
    (let ((connection-info (%nordvpn-connection-info)))
      (if (or (null? connection-info)
	      (and
	       (assoc "Status" connection-info)
	       (string= (cdr (assoc "Status" connection-info))
			"Disconnected")))
	  #f
	  #t)))
  

  (if (member "switch" (command-line-arguments))
      (connect-to-random-city))
  (if (member "status" (command-line-arguments))
      (pretty-print (%nordvpn-status)))
  (if (member "countries" (command-line-arguments))
      (pretty-print (%available-countries)))
  (pretty-print (%nordvpn-connection-info))
  
  )


