
;;
;; Creates a new process with cmd (a string) and
;; a list of arguments.
;; Returns four values:
;;    the process's standard output (as an input port)
;;    the process's standard input (as an output port)
;;    the process's standard error (as an input port)
;;    the rocess id (pid, interger)
(define (process* cmd args)
  (call-with-process-io
   (cons cmd args)
   (lambda (pid proc-stdin proc-stdout proc-stderr)
     (values proc-stdout proc-stdin proc-stderr pid))))

;;
;; wait for a process to finish. by pid (integer)
(define (process-wait pid)
  (waitpid pid 0))
