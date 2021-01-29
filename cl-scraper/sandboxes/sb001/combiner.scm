
(import scheme
	(chicken base)
	r7rs
	(chicken port)
	(chicken irregex)
	(srfi 1)
	(srfi 13))



(define (%compute-sort-key line)
  (let ((numbers (irregex-extract "\\d+" line)))
    (if (null? numbers)
	0
	(string->number (last numbers)))))


(define (%writeout-line-key-pair line outport #!optional (separator "|"))
  (let ((key (%compute-sort-key line)))
    (write-string (number->string key) outport)
    (write-string separator outport)
    (write-string line outport)
    (newline outport)))

(define (lines->line-key-pairs inport outport #!optional (separator "|"))
  (do ((line (read-line inport) (read-line inport)))
      ( (eof-object? line) line )
    (%writeout-line-key-pair line outport separator)))
      
      
