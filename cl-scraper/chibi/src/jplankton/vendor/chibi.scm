;;;;
;;;; chibi specific code
;;;;

;;
;; make sure it is chibi running us
(if (not (member 'chibi (features)))
    (error "This code is only CHIBI SCHEME specific!! Don't Run in non-chibi"))


;;
;; Create a custom printer (writer) for the given type
(define (chibi/custom-printer-set! type printer)
  (type-printer-set!
   type
   printer))


;;
;; Creates a new process with cmd (a string) and
;; a list of arguments.
;; Returns four values:
;;    the process's standard output (as an input port)
;;    the process's standard input (as an output port)
;;    the process's standard error (as an input port)
;;    the rocess id (pid, interger)
(define (chibi/process* cmd args)
  (call-with-process-io
   (cons cmd args)
   (lambda (pid proc-stdin proc-stdout proc-stderr)
     (values proc-stdout proc-stdin proc-stderr pid))))

;;
;; wait for a process to finish. by pid (integer)
(define (chibi/process-wait pid)
  (waitpid pid 0))


;;
;; type-of operator
(define (chibi/type-of x)
  (type-of x))
