;;;;
;;;; Scheme module to find patterns in urls/links to follow
;;;;


;;;;
;;;; Library definition
(module (jplankton url_follower) *
  (import (chicken base)
	  scheme
	  r7rs
	  (chicken io)
	  (chicken file)
	  (chicken port)
	  (chicken process)
	  (chicken irregex)
	  (chicken pretty-print)
	  (srfi 1)
	  (srfi 13)
	  (srfi 28)
	  utf8
	  memoize)

  ;;
  ;; Weights for cat/block/bidi differences
  (define *COST/CHAR-WEIGHT* 0.5)
  (define *COST/CAT-WEIGHT* 0.75)
  (define *COST/BLOCK-WEIGHT* 1.0)
  (define *COST/BIDI-WEIGHT* 1.0)

  ;;
  ;; the default gap cost
  (define *GAP* (string->symbol "__gap__"))
  (define *DEFAULT-GAP-COST* 0.25)

  ;;
  ;; try an parse a line from the output of the unicode
  ;; command, returning '() if unable or a
  ;; pair of the category name and value
  (define (%try-parse-command-output-cat line)
    (let ((idx (string-index line #\:)))
      (if (not idx)
	  '()
	  (let ((name (string-downcase
		       (string-trim-both
			(substring/shared
			 line 0 idx))))
		(value (string-downcase
			(string-trim-both
			 (substring/shared
			  line (+ idx 1))))))
	    (cons (cond
		   ( (string= "category" name) category:)
		   ( (string= "unicode block" name) block:)
		   ( (string= "bidi" name) bidi: )
		   ( else other: ))
		  value)))))
	

  ;;
  ;; parse the result of the "unicode" command and
  ;; returns an alist with the properties/categories
  ;; for the input character
  (define (%parse-unicode-command-categories inport)
    (let ((categories '()))
      (do (( line (read-line inport) (read-line inport)))
	  ((eof-object? line) categories)
	(let ((cat (%try-parse-command-output-cat line)))
	  (if (not (null? cat))
	      (set! categories (cons cat categories)))))))

  ;;
  ;; fetch certain catergories for a unicode character
  ;; (define (%char/unicode-categories c)
  ;;   (let-values (( (proc-outport proc-inport pid proc-errport)
  ;; 		   (process* "unicode" (list (string c)))))
  ;;     (dynamic-wind
  ;; 	(lambda () #f)

  ;; 	(lambda ()
  ;; 	  (%parse-unicode-command-categories proc-outport))

  ;; 	(lambda ()
  ;; 	  (close-input-port proc-outport)
  ;; 	  (close-output-port proc-inport)
  ;; 	  (close-output-port proc-errport)))))
  (define (%char/unicode-categories c)
    (list
     (cons category: "1")
     (cons block: "1")
     (cons bidi: "1")))

  ;;
  ;; grab a specific unicpe category by name
  (define (%get-cat id cats)
    (let ((res (assoc id cats)))
      (if (not res)
	  res
	  (cdr res))))

  ;;
  ;; Theunqeighed cost of different characters
  (define (%cost/char->char a b)
    (if (equal? a b)
	0
	1))

  ;;
  ;; The unweighted cost from a unicoe category to another
  (define (%cost/cat->cat a b)
    (if (equal? a b)
	0
	1))

  ;;
  ;; The unweighed cost from a unicode code block to another
  (define (%cost/block->block a b)
    (if (equal? a b)
	0
	(let ((a-numbers (map (lambda (x) (string->number x 16)) (irregex-extract "\\[0-9a-fA-F]+" a)))
	      (b-numbers (map (lambda (x) (string->number x 16)) (irregex-extract "\\[0-9a-fA-F]+" b))))
	  (apply +
		 (map (lambda (xa xb) (abs (- xa xb)))
		      a-numbers
		      b-numbers)))))

  ;;
  ;; The unwiehged cost from a unicode bidirectional orientation (bidi)
  ;; to another
  (define (%cost/bidi->bidi a b)
    (if (equal? a b)
	0
	1))

  ;;
  ;; A cost function for aligning the two given elements
  ;; This returns a pair (cost constring-function)
  ;; The constrint function is a function that maps both elements to
  ;; an equivalent class
  (define (%default-cost-function a b)
    (if (not (and (char? a)
  		  (char? b)))
	(if (or (eq? a *GAP*)
		(eq? b *GAP*))
	    *DEFAULT-GAP-COST*
  	    (if (equal? a b)
  		0
  		1))
  	(if (equal? a b)
  	    0
  	    (let ((a-categories (%char/unicode-categories a))
  		  (b-categories (%char/unicode-categories b)))
  	      (let* ((c0 (%cost/char->char a b))
		     (c1 (%cost/cat->cat (%get-cat category: a-categories) (%get-cat category: b-categories)))
  		     (c2 (%cost/block->block (%get-cat block: a-categories) (%get-cat block: b-categories)))
  		     (c3 (%cost/bidi->bidi (%get-cat bidi: a-categories) (%get-cat bidi: b-categories))))
  		(+ (* *COST/CHAR-WEIGHT* c0)
		   (* *COST/CAT-WEIGHT* c1)
  		   (* *COST/BLOCK-WEIGHT* c2)
  		   (* *COST/BIDI-WEIGHT* c3) ))))))
	  

  (define *DEFAULT-COST-FUNCTION* %default-cost-function)

  ;;
  ;; naive sequence alignment for 2 sequences
  (define (%naive-pair-sequence-alignment seq-a seq-b cost-function depth result)
    ;; (display (format "~s~a: sa=~a sb=~a d=~a res=~a"
    ;; 		     depth (make-string (* 2 depth) #\.)
    ;; 		     seq-a seq-b
    ;; 		     depth result))
    ;; (newline)
    (if (or (< depth 1)
	    (and (null? seq-a)
		 (null? seq-b))
	    (and (null? seq-a)
		 (= (length seq-b) 1)
		 (eq? (car seq-b) *GAP*))
	    (and (null? seq-b)
		 (= (length seq-a) 1)
		 (eq? (car seq-a) *GAP*)))
	(begin
	  ;; (display (format "~s~a  <-- score=0 result=~a depth=~a"
	  ;; 		   depth (make-string (* 2 depth) #\.)
	  ;; 		   result depth))
	  ;; (newline)
	  (values 0 result))
	(let ((a (if (null? seq-a) *GAP* (first seq-a)))
	      (b (if (null? seq-b) *GAP* (first seq-b)))
	      (rest-a (if (null? seq-a) '() (cdr seq-a)))
	      (rest-b (if (null? seq-b) '() (cdr seq-b))))
	  (let-values
	      (( (score-1 result-1)
		 (%naive-pair-sequence-alignment
		  rest-a rest-b
		  cost-function (- depth 1)
		  (cons (list (if (not (eq? a *GAP*)) #t *GAP*)
			      (if (not (eq? b *GAP*)) #t *GAP*)) result)))
	       ( (score-2 result-2)
		 (%naive-pair-sequence-alignment rest-a seq-b cost-function (- depth 1) (cons (list #t *GAP*) result)))
	       ( (score-3 result-3)
		 (%naive-pair-sequence-alignment seq-a rest-b cost-function (- depth 1) (cons (list *GAP* #t) result))))
	    (let (( total-1 (+ score-1 (cost-function a b)))
		  ( total-2 (+ score-2 (cost-function *GAP* b)))
		  ( total-3 (+ score-3 (cost-function a *GAP*))))
	      ;; (display (format "~s~a  1 = (~a , ~a)   2 = (~a , ~a)  3 = (~a , ~a)"
	      ;; 		       depth (make-string (* 2 depth) #\.)
	      ;; 		       score-1 total-1
	      ;; 		       score-2 total-2
	      ;; 		       score-3 total-3))
	      ;; (newline)
	      (cond
	       ((and (< total-1 total-2)
		     (< total-1 total-3))
		(values total-1 result-1))
	       ((and (< total-2 total-1)
		      (< total-2 total-3))
		(values total-2 result-2))
	       (else (values total-3 result-3))))))))


  (define %naive-pair-sequence-alignment/memoized (memoize %naive-pair-sequence-alignment))


  (define (%compute-alignment seq-a seq-b align)
    (if (null? align)
	(values '() '())
	(let* ((a (if (null? seq-a) #f (car seq-a)))
	       (b (if (null? seq-b) #f (car seq-b)))
	       (rest-a (if (null? seq-a) '() (cdr seq-a)))
	       (rest-b (if (null? seq-b) '() (cdr seq-b)))
	       (chose-a (first (first align)))
	       (chose-b (second (first align)))
	       (rest-align (cdr align))
	       (the-a (if (eq? chose-a *GAP*) *GAP* a))
	       (the-b (if (eq? chose-b *GAP*) *GAP* b))
	       (the-rest-a (if (eq? chose-a *GAP*) seq-a rest-a))
	       (the-rest-b (if (eq? chose-b *GAP*) seq-b rest-b)))
	  (let-values (( (al-a al-b) (%compute-alignment the-rest-a the-rest-b rest-align)))
	    ;; (display
	    ;;  (format "seq-a=~a seq-b=~a align=~a a=~a b=~a rest-a=~a rest-b=~a chose-a=~a chose-b=~a rest-align=~a the-a=~a the-b=~a the-rest-a=~a the-rest-b=~a al-a=~a al-b=~a"
	    ;; 	     seq-a seq-b align a b rest-a rest-b chose-a chose-b rest-align the-a the-b the-rest-a the-rest-b al-a al-b))
	    ;; (newline)
	    (values (cons the-a al-a)
		    (cons the-b al-b))))))

  (define (%translate-alignment-values-to-string lst)
    (map
     (lambda (c)
       (if (not c)
	   #\x21a6
	   (if (eq? c *GAP*)
	       #\x21b3
	       c)))
     lst))

  (define (sequence-pair-alignment/direct a b)
    (let-values (( (score ralign) (%naive-pair-sequence-alignment a b *DEFAULT-COST-FUNCTION* (max (length a) (length b)) '())))
      (let ((align (reverse ralign)))
	(let-values (( (aligned-a aligned-b) (%compute-alignment a b align)))
	  (values
	   (list->string (%translate-alignment-values-to-string aligned-a))
	   (list->string (%translate-alignment-values-to-string aligned-b))
	   align
	   score)))))


  (define (sequence-pair-alignment a b)
    (let-values (( (score ralign) (%naive-pair-sequence-alignment/memoized a b *DEFAULT-COST-FUNCTION* (max (length a) (length b)) '())))
      (let ((align (reverse ralign)))
	(let-values (( (aligned-a aligned-b) (%compute-alignment a b align)))
	  (values
	   (list->string (%translate-alignment-values-to-string aligned-a))
	   (list->string (%translate-alignment-values-to-string aligned-b))
	   align
	   score)))))


  (define *test-seq-a-000* (string->list "abcd"))
  (define *test-seq-b-000* (string->list "abd"))
  (let-values (( res (sequence-pair-alignment *test-seq-a-000* *test-seq-b-000*)))
    (pp res)
    (newline))

  (define *test-seq-a-001* (string->list "this is great! no buts"))
  (define *test-seq-b-001* (string->list "this ain't great but no"))
  (let-values (( res (sequence-pair-alignment *test-seq-a-001* *test-seq-b-001*)))
    (pp res)
    (newline))

  
  
  )
