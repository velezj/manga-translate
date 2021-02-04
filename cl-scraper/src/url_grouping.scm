;;;;
;;;; Scheme module to find "groups" in urls
;;;;


;;;;
;;;; Library definition
(module (jplankton url_grouping) *
  (import (chicken base)
	  scheme
	  r7rs
	  (chicken io)
	  (chicken port)
	  (chicken pretty-print)
	  (chicken irregex)
	  (srfi 1)
	  (srfi 13)
	  (srfi 28)
	  (srfi 128)
	  (srfi 132)
	  memoize
	  utf8)

  (define (%normalize url)
    (let* ((normed-url url)
	   (normed-url
	    (irregex-replace/all "^[^/]*://" normed-url ""))
	   (normed-url
	    (irregex-replace/all "\\d+" normed-url ":D:"))
	   (normed-url
	    (irregex-replace/all "\\?.*" normed-url ""))
	   (normed-url
	    (irregex-replace/all "\\#.*" normed-url "")))
      normed-url))

  (define (%normalize-and-sort-urls urls)
    (let* ((normed-urls (map %normalize urls))
	   (string-comp (make-default-comparator))
	   (sorted (list-sort
		    (lambda (a b) (<? string-comp a b))
		    normed-urls)))
      sorted))
      

  (define (%longet-common-sequence a b)
    (if (or (null? a)
	    (null? b))
	'()
	(let ( (elem-a (first a))
	       (elem-b (first b))
	       (rest-a (cdr a))
	       (rest-b (cdr b)))
	  (if (equal? elem-a elem-b)
	      (cons elem-a (%longet-common-sequence rest-a rest-b))
	      (let ((res-1 (%longet-common-sequence a rest-b))
		    (res-2 (%longet-common-sequence rest-a b)))
		(if (>= (length res-1) (length res-2))
		    res-1
		    res-2))))))


  (define (longest xs ys)
    (if (> (length xs)
           (length ys))
	xs ys))

  
  (define lcs
    (memoize
     (lambda (seqx seqy)
       (if (pair? seqx)
           (let ((x (car seqx))
		 (xs (cdr seqx)))
             (if (pair? seqy)
		 (let ((y (car seqy))
                       (ys (cdr seqy)))
                   (if (equal? x y)
                       (cons x (lcs xs ys))
                       (longest (lcs seqx ys)
				(lcs xs seqy))))
		 '()))
           '()))))

  (define (%has-preffix pre list)
    (if (< (length list) (length pre))
	#f
	(equal? pre
		 (take list (length pre)))))

  (define (%first-common-preffix sorted-lists)
    (if (null? sorted-lists)
	(values '() '())
	(if (<= (length sorted-lists) 1)
	    (values (car sorted-lists) '())
	    (let ((target-preffix
		   (map first
			(take-while (lambda (xy) (equal? (first xy)
							 (second xy)))
				    (zip (first sorted-lists)
					 (second sorted-lists))))))
	      (if (null? target-preffix)
		  (values (first sorted-lists)
			  (cdr sorted-lists))
		  (let-values (( (in-group out-group)
				 (partition
				  (lambda (x)
				    (%has-preffix target-preffix x))
				  sorted-lists)))
		    (values target-preffix out-group)))))))
    
  (define (%groupings-using-prefix sorted-normed-urlseqs)
    (if (null? sorted-normed-urlseqs)
	'()
	(let-values (( (group rest)
		       (%first-common-preffix sorted-normed-urlseqs)))
	  (if (not (null? group))
	      (cons group (%groupings-using-prefix rest))
	      (%groupings-using-prefix rest)))))

  (define (%groupings-using-lcs sorted-normed-urlseqs)
    (if (null? sorted-normed-urlseqs)
	'()
	(let-values (( (first-group rest-urls)
		       (%find-first-lcs-group sorted-normed-urlseqs)))
	  (cons first-group (%groupings-using-lcs rest-urls)))))

  (define (%lcs-exists-in lcs seq)
    (if (null? lcs)
	#t
	(if (null? seq)
	    #f
	    (let ((res (member (first lcs) seq)))
	      (if (not res)
		  #f
		  (%lcs-exists-in (cdr lcs) res))))))

  (define (%find-first-lcs-group sorted-normed-urlseqs)
    (if (or (null? sorted-normed-urlseqs)
	    (<= (length sorted-normed-urlseqs) 1))
	(values #f '())
	(let ((target-lcs
	       (lcs (first sorted-normed-urlseqs)
		    (second sorted-normed-urlseqs))))
	  (let-values (( (group rest)
			 (span (lambda (a)
				 (%lcs-exists-in target-lcs a))
			       sorted-normed-urlseqs)))
	    (values target-lcs rest)))))
  )
