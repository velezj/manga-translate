;; ;;;;
;; ;;;; Scheme module to create a line-oriented port
;; ;;;; that can "unread" a number of lines
;; ;;;;



(define-record-type <line-input-port>
  (line-input-port base-inport max-lines-to-buffer buff-finger buffer)
  line-input-port?
  (base-inport line-input-port/%base-inport)
  (max-lines-to-buffer line-input-port/max-lines-to-buffer)
  (buff-finger line-input-port/%buff-finger line-input-port/%set-buff-finger!)
  (buffer line-input-port/%buffer line-input-port/%set-buffer!))

;;
;; Checks if the next read for hte line-port needs to
;; actually pull from the base port
(define (%need-to-read-base line-port)
  (>= (line-input-port/%buff-finger line-port)
      (length (line-input-port/%buffer line-port))))

;;
;; returns the new buffer and finger from adding the given line
(define (%compute-new-buffer line buf finger max-size)
  (let ((added-buf (append buf (list line)))
	(result-finger (+ 1 finger)))
    (let ((result-buf
	   (do ((final-buf added-buf (drop final-buf 1)))
	       ((<= (length final-buf) max-size) final-buf)
	     (set! result-finger (- result-finger 1))
	     (if (< result-finger 0)
		 (error "e0")))))
      ;; (format (string-append
      ;; 	     "Buffer past maximum size. Unable to add line "
      ;; 	     "'~s', buf=~a finger=~a")
      ;;  line buf finger ))))))
      (values result-buf result-finger))))


;;
;; Reads a new line from the *base* port and updates
;; the line port, returns the read line
(define (%read-base-line-and-update line-port)
  (let ((line (read-line (line-input-port/%base-inport line-port))))
    (let-values (( (new-buffer new-finger)
		   (%compute-new-buffer
		    line
		    (line-input-port/%buffer line-port)
		    (line-input-port/%buff-finger line-port)
		    (line-input-port/max-lines-to-buffer line-port))))
      (line-input-port/%set-buff-finger! line-port new-finger)
      (line-input-port/%set-buffer! line-port new-buffer)
      line)))

;;
;; reads the next line in the buffer and updates the stream
(define (%read-buffer-line-and-update line-port)
  (if (>= (line-input-port/%buff-finger line-port)
	  (length (line-input-port/%buffer line-port)))
      (error "e1")
      ;; (raise (format (string-append "Buffer has been fully read, need "
      ;; 			      "to read from base!. buff=~a finger=~a")
      ;; 	       (line-input-port/%buffer line-port)
      ;; 	       (line-input-port/%buff-finger line-port)))
      (let ((line (list-ref (line-input-port/%buffer line-port)
			    (line-input-port/%buff-finger line-port))))
	(line-input-port/%set-buff-finger! line-port
					   (+ 1 (line-input-port/%buff-finger line-port)))
	line)))

;;
;; read a new line from a line input port.
;; this may or may not actually touch the base port depending
;; on what has been buffered by the line port
(define (line-input-port/read-line line-port)
  (if (%need-to-read-base line-port)
      (%read-base-line-and-update line-port)
      (%read-buffer-line-and-update line-port)))

;;
;; "unread" the previously read line.
;; This will make it so that the next line read
;; with line-input-port/read-line will return the
;; same equal? string as previsouly read.
;; A number of "unreads" up to the max buffer size
;; can be done before a read needs to happen.
(define (line-input-port/unread-line line-port)
  (if (<= (line-input-port/%buff-finger line-port) 0)
      (error "e2")
      ;; (raise (format (string-append "Cannot unread line, buffer is full. "
      ;; 			      "buf=~a finger=~a")
      ;; 	       (line-input-port/%buffer line-port)
      ;; 	       (line-input-port/%buff-finger line-port)))
      (begin 
	(line-input-port/%set-buff-finger! line-port
					   (- (line-input-port/%buff-finger line-port) 1))
	line-port)))

;;
;; Close the line port and the base port it wraps
(define (line-input-port/close-port line-port)
  (close-input-port (line-input-port/%base-inport line-port))
  (line-input-port/%set-buff-finger! line-port 0)
  (line-input-port/%set-buffer! line-port '()))

