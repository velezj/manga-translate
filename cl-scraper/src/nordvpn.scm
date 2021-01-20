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
	  (srfi 13)
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
      (list chosen-country chosen-city)))

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
  
  )
