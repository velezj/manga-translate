;;;;
;;;; Test suit for the line_port
;;;;

;; test strings for use throughout the tests
(define (test-string/1)
  "a\nb\nc\nd\ne")

;; test base ports
(define (test-string-port/1)
  (open-input-string (test-string/1)))

;; test line-ports
(define (test-line-port/1)
  (line-input-port
   (test-string-port/1)
   1
   0
   '()))

;;
;; Make sure we can read in lines
(define (test/read-lines/1)
  (let ((line-port (test-line-port/1)))
    (let* ((a (read-line line-port))
	   (b (read-line line-port))
	   (c (read-line line-port))
	   (d (read-line line-port))
	   (e (read-line line-port))
	   (eof (read-line line-port)))
      (check a => "a")
      (check b => "b")
      (check c => "c")
      (check d => "d")
      (check e => "e")
      (check (eof-object? eof) => #t))))

;;;;
;;;; run-tests thunk
(define (run-tests)
  (test-line-port/1))
