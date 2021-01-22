;;;;
;;;; Core protocol used by Apache Storm for non-java languages
;;;;
;;;; This protocol is a simple JSON messages in STDIN  and results written
;;;; as JSON results on STDOUT


;;;;
;;;; Library definition
(module (jplankton storm_protocol) *
  (import (chicken base)
	  scheme
	  r7rs
	  (chicken io)
	  (chicken pretty-print)
	  (chicken port)
	  json
	  (srfi 13)
	  (srfi 14)
	  (srfi 1)
	  (srfi 35)
	  (srfi 28))


  ;;;;
  ;;;; CORE PROTOCOL (JSON in/out using lines with 'end' demarcation)
  ;;;;

  ;; condition meaning the storm protocol message was not correctly read
  (define-condition-type &malformed-message &error
    malformed-message?
    (msg malformed-message-msg))

  ;; secific messages are malformed
  (define-condition-type &invalid-handshake &malformed-message
    invalid-handshake)

  ;; read the lines that form the next message from port
  ;; This takes care of reading until the "end" token
  ;; from the storm protocol
  (define (%read-message-lines inport buf)
    (let ((line (read-line inport 2056)))
      (if (eof-object? line)
	  (raise (condition (&malformed-message (msg "expected 'end' but got eof"))))
	  (if (string= line "end")
	      (reverse buf)
	      (%read-message-lines inport (cons line buf))))))

  ;; reads the next message, returning the raw string representation
  ;; of the message
  (define (%read-next-message-string inport)
    (let ((lines (%read-message-lines inport '() )))
      (string-concatenate lines)))

  ;; "schemify" the result of json-read.
  ;; we can *only* do this since we know the storm protocol
  (define (%schemify-json obj)
    (if (vector? obj)
	(%json-vector-to-alist obj)
	obj))

  ;; takes a vector returned from json-read and return an
  ;; actual alist for it
  (define (%json-vector-to-alist vec)
    (map
     (lambda (kv)
       (let ((key (car kv))
	     (value (cdr kv)))
	 (if (not (string? key))
	     (raise (condition (&malformed-message (msg "Found json-parsed as vector with non-string key!"))))
	     (cons (string->symbol key)
		   (%schemify-json value)))))
     (vector->list vec)))
    
  ;; reads the next full message from the given port
  ;; and returns the resulting JSON-parsed object
  ;; (so result is not a string but the JSON parsed
  ;;  into a scheme object)
  (define (%read-next-message inport)
    (let ((message-string (%read-next-message-string inport)))
      (%schemify-json
       (call-with-input-string message-string
	 json-read))))


  ;; Taken from srfi-152
  (define (%string-segment str k)
    (if (< k 1) (error "minimum segment size is 1" k))
    (let ((len (string-length str)))
      (let loop ((start 0)
		 (result '()))
	(if (= start len)
            (reverse result)
            (let ((end (min (+ start k) len)))
              (loop end (cons (substring str start end) result)))))))


  ;; write the given message.
  ;; The message must be in a JSON serializable format.
  ;; This will first serialize messave to JSON and then
  ;; send the message according to the storm protocol
  ;; over the gien output port
  (define (%write-message obj outport linesize)
    (let ((json-string
	   (with-output-to-string
	     (lambda () (json-write obj)))))
      (for-each
       (lambda (seg)
	 (write-line seg outport))
       (%string-segment json-string linesize))
      (write-line "end" outport)))




  ;;;;
  ;;;; low-level protocol actions
  ;;;;

  ;; tries to read a setup/handshake message
  (define (%check-read-handshake inport)
    (let ((obj (%read-next-message inport)))
      (if (and
	   (proper-list? obj)
	   (= (length obj) 3)
	   (every pair? obj)
	   (assoc 'conf obj)
	   (assoc 'pidDir obj)
	   (assoc 'context obj))
	  obj
	  (raise (condition
	    	  (&invalid-handshake
	    	   (msg (format "expected json object with 'conf', 'pidDir', and 'context' keys, got ~s" obj)))))
	  )))

  
  )
