;;;;
;;;; Test suit for the line_port
;;;;

;; test strings for use throughout the tests
(define (test-string/1)
  "a\nb\nc\nd\ne")
(define (test-string/2)
  "abcde")
(define (test-string/3)
  "")



;; test base ports
(define (test-string-port/1)
  (open-input-string (test-string/1)))
(define (test-string-port/2)
  (open-input-string (test-string/2)))
(define (test-string-port/3)
  (open-input-string (test-string/3)))

;; test line-ports
(define (test-line-port/1)
  (line-input-port
   (test-string-port/1)
   1
   0
   '()))
(define (test-line-port/2)
  (line-input-port
   (test-string-port/1)
   2
   0
   '()))
(define (test-line-port/3)
  (line-input-port
   (test-string-port/2)
   2
   0
   '()))
(define (test-line-port/4)
  (line-input-port
   (test-string-port/3)
   1
   0
   '()))

;;
;; Make sure we can read in lines
(define (test/read-lines/1)
  (let ((line-port (test-line-port/1)))
    (let* ((a (line-input-port/read-line line-port))
	   (b (line-input-port/read-line line-port))
	   (c (line-input-port/read-line line-port))
	   (d (line-input-port/read-line line-port))
	   (e (line-input-port/read-line line-port))
	   (eof (line-input-port/read-line line-port)))
      (check a => "a")
      (check b => "b")
      (check c => "c")
      (check d => "d")
      (check e => "e")
      (check (eof-object? eof) => #t))))

;;
;; Make sure we can read in "edge" cases of single line
(define (test/read-lines/edge/1)
  (let ((line-port (test-line-port/3)))
    (let* ((a (line-input-port/read-line line-port))
	   (eof (line-input-port/read-line line-port)))
      (check a => "abcde")
      (check (eof-object? eof) => #t))))

;;
;; Make sure we can read in "edge" cases of empty string
(define (test/read-lines/edge/2)
  (let ((line-port (test-line-port/4)))
    (let* ((eof (line-input-port/read-line line-port)))
      (check (eof-object? eof) => #t))))


;;
;; make sure we can unread a single line
(define (test/unread-lines/1)
  (let ((line-port (test-line-port/1)))
    (let* ((l0 (line-input-port/read-line line-port))
	   (l1 (line-input-port/read-line line-port))
	   (l2 (line-input-port/unread-line line-port))
	   (l3 (line-input-port/read-line line-port))
	   (l4 (line-input-port/read-line line-port))
	   (l5 (line-input-port/read-line line-port))
	   (l6 (line-input-port/read-line line-port))
	   (eof (line-input-port/read-line line-port)))
      (check l0 => "a")
      (check l1 => "b")
      ;; unspecified value, (check l2 => #f)
      (check l3 => "b")
      (check l4 => "c")
      (check l5 => "d")
      (check l6 => "e")
      (check (eof-object? eof) => #t))))

;;
;; make sure we can unread a 2 line
(define (test/unread-lines/2)
  (let ((line-port (test-line-port/2)))
    (let* ((l0 (line-input-port/read-line line-port))
	   (l1 (line-input-port/read-line line-port))
	   (l2 (line-input-port/unread-line line-port))
	   (l3 (line-input-port/unread-line line-port))
	   (l4 (line-input-port/read-line line-port))
	   (l5 (line-input-port/read-line line-port))
	   (l6 (line-input-port/read-line line-port))
	   (l7 (line-input-port/read-line line-port))
	   (l8 (line-input-port/read-line line-port))
	   (eof (line-input-port/read-line line-port)))
      (check l0 => "a")
      (check l1 => "b")
      ;; unspecified value, (check l2 => #f)
      ;; unspecified value, (check l3 => #f)
      (check l4 => "a")
      (check l5 => "b")
      (check l6 => "c")
      (check l7 => "d")
      (check l8 => "e")
      (check (eof-object? eof) => #t))))


;;
;; make sure we can interleave read/unread lines
(define (test/unread-lines/3)
  (let ((line-port (test-line-port/1)))
    (let* ((l0 (line-input-port/read-line line-port))
	   (l1 (line-input-port/read-line line-port))
	   (l2 (line-input-port/unread-line line-port))
	   (l3 (line-input-port/read-line line-port))
	   (l4 (line-input-port/read-line line-port))
	   (l5 (line-input-port/unread-line line-port))
	   (l6 (line-input-port/read-line line-port))
	   (l7 (line-input-port/read-line line-port))
	   (l8 (line-input-port/unread-line line-port))
	   (l9 (line-input-port/read-line line-port))
	   (l10 (line-input-port/read-line line-port))
	   (eof (line-input-port/read-line line-port)))
      (check l0 => "a")
      (check l1 => "b")
      ;; unspecified value, (check l2 => #f)
      (check l3 => "b")
      (check l4 => "c")
      ;; unsepcified value, (check l5 => "b")
      (check l6 => "c")
      (check l7 => "d")
      ;; unspecified value, (check l8 => "c")
      (check l9 => "d")
      (check l10 => "e")
      (check (eof-object? eof) => #t))))      



;;;;
;;;; run-tests thunk
(define (run-tests)
  (test/read-lines/1)
  (test/read-lines/edge/1)
  (test/read-lines/edge/2)
  (test/unread-lines/1)
  (test/unread-lines/2)
  (test/unread-lines/3)
  (check-report))
