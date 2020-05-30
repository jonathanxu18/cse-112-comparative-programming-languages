#!/afs/cats.ucsc.edu/courses/cse112-wm/usr/racket/bin/mzscheme -qr
;; $Id: sbi.scm,v 1.20 2020-01-16 13:38:52-08 - - $
;;--------------------------------------------------------------
;; PAIR PROGRAMMING
;;    Zain Shafique (zshafiqu@ucsc.edu)
;;    Jonathan Xu (jxu125@ucsc.edu)
;; NAME
;;    sbi.scm - silly basic interpreter
;;
;; SYNOPSIS
;;    sbi.scm filename.sbir
;;
;; DESCRIPTION
;;    The file mentioned in argv[1] is read and assumed to be an SBIR
;;    program, which is the executed.  Currently it is only printed.
;;
;; PRELIMINARY NOTES
;;    - No looping constructs may be used
;;    - Interpreter reads in an intermediate language program
;;    - Parses the program
;;    - Interprets the program
;;
;; PROGRAM REQUIREMENTS
;;    - The SB interpreter reads in an SBIR program from some file
;;    - The filename is specified in argument list
;;    - The SB interpreter stores the program in a list
;;    - Then interprets that immediate representation

;;    - During interpretation, numbers are read from stdin
;;    - Results are printed to stdout
;;    - Errors are printed to stderr and program is terminated on 1st error
;;
;;

;----------Hash Table Constructors----------

;table to store the labels 
(define *label-table* (make-hash))

;table to store the variables 
(define *variable-table* (make-hash))

;table to store the functions
(define *function-table* (make-hash))

;table to store arrays
(define *array-table* (make-hash))


;----------Initializing Set Functions for Hash Tables----------

;set function for label-table
(define (label-put! key value)
        (hash-set! *label-table* key value))

;set function for variable-table
(define (variable-put! key value)
        (hash-set! *variable-table* key value))

;set function for function-table
(define (function-put! key value)
        (hash-set! *function-table* key value))  

;set function for array-table
(define (array-put! key value)
(hash-set! *array-table* key value))  

;----------Inserting Default Values into Hash Tables----------

;inserting key-value pairs into function table
;lambda expression -> parameter (must be a list in for-each): pair, operation: put 
(for-each 
    (lambda (pair) (function-put! (car pair) (cadr pair)))

    ;quasiquote makes this a list
    `(
        ;operators
        (+ ,+)
        (- ,-)
        (* ,*)
        (/ ,/)
        (^ ,expt)

        ;comparisons
        (=  ,=)
        (!= ,(lambda (x y) (not (equal? x y))))
        (<  ,<)
        (>  ,>)
        (>= ,>=)
        (<= ,<=)

        ;Built-in symbols
        (abs      ,abs)
        (acos     ,acos)
        (asin     ,asin)
        (atan     ,atan)
        (cos      ,cos)
        (round    ,round) 
        (sin      ,sin)
        (tan      ,tan)
        (truncate ,truncate)
        (div      ,(lambda (x y) (floor (/ x y))))
        (log10    ,(lambda (x) (/ (log x) (log 10.0))))
        (mod      ,(lambda (x y) (- x (* (div x y) y))))
        (quot     ,(lambda (x y) (truncate (/ x y))))
        (rem      ,(lambda (x y) (- x (* (quot x y) y))))
        (ceiling  ,ceiling)
        (exp      ,exp)
        (floor    ,floor)
        (log      ,log)
        (sqrt     ,sqrt)           
    ))

;inserting key-value pairs into variable table
(for-each 
    (lambda (pair) (variable-put! (car pair) (cadr pair)))

    `(
        (log10_2 0.301029995663981195213738894724493026768189881)
        (sqrt_2  1.414213562373095048801688724209698078569671875)
        (eof 0.0)
        (nan  ,(/ 0.0 0.0))
        (pi   ,(acos -1.0))
        (e    ,(exp 1.0))
        (one  ,1)
        (zero ,0)
 
        
    )
)

(define *stdin* (current-input-port))
(define *stdout* (current-output-port))
(define *stderr* (current-error-port))

;----------Insert-Labels----------
(define (insert-labels program)
    (when (not (null? (car program)))
    (let ((line (car program)))
        
        ;label
        (cond 
          ;skip line number
          ((line_nbr line)) 

          ((label line)

            ;(printf "top level: ~s~n" (car program))
            ;(printf "~s~n" (cadr line)) 
            (label-put! (cadr line) program))
            )
        ))
        ;stop tail recursion if reach EOF 
        (cond  
           ((not (null? (cdr program))) (insert-labels (cdr program)))
        )         
)


;----------Interpret-Statement Functions----------
(define (interpret-statement statement)
    (cond 
        ((equal? (car statement) 'dim)   (interpret-dim (cdr statement)))
        ((equal? (car statement) 'let)   (interpret-let (cdr statement)))
        ((equal? (car statement) 'goto)  (interpret-goto (cadr statement)))
        ((equal? (car statement) 'if)    (interpret-if (cdr statement)))
        ((equal? (car statement) 'print) (interpret-print (cdr statement)))
        ((equal? (car statement) 'input) (interpret-input (cdr statement))))
)

(define (interpret-print statement)
    ;call evaluate-expr on each arg of print 
    ;(printf "print~n")
    ;(printf "print_ ~s~n" statement)
    (map (lambda (expr) (display (evaluate-submit cse112-wm.w20 asg1 sbi.scm README PARTNERexpr expr))) statement)
    ;new line char at the end
    (printf "~n")
    
)

(define (interpret-let statement)
     ;initialize variable in hash-table if new
     ;(printf "let~n")
     (let ((expr (car statement)))
     ;(printf "car statement: ~s~n" (car statement))
        
        (cond  
            ((pair? expr) 
            (let ((array (cdr expr)))
            ;(printf "array~n")
            (vector-set! (hash-ref *array-table* (car array)) 
            (exact-round (evaluate-expr (cadr array))) (evaluate-expr (cadr statement)))
            ;(printf "let_array: ~s~n" (hash-ref *array-table* (car array)))
            )) 
           (else 
            ;(printf "let inner~n")
            ;(variable-put! expr 0.0)
            ;(printf "let_Before:~s~n" (hash-ref *variable-table* expr))
            ;(printf "~s~n" (evaluate-expr (cadr statement)))
            (variable-put! (car statement) (evaluate-expr (cadr statement)))
            ;(printf "let_After:~s~n" (hash-ref *variable-table* expr))
           )
        )
))

(define (interpret-dim statement)
    ;(printf "dim~n")
    ;call evaluate-expr on each arg of print
    ;(printf "~s~n" statement)
    (define array (cdar statement)) 
    ;(printf "cadr array: ~s~n" (cadr array))
    ;(printf "~s~n" ((number? (cdr array)) (+ (cdr array) 0.0)))
    (array-put! (car array) (make-vector (exact-round (evaluate-expr (cadr array))) 0.0 ))
    ;(printf "dim_hash-ref: ~s~n" (hash-ref *array-table* (car array)))
    ;new line char at the end
)

(define (interpret-goto statement)
        ;(printf "goto_statement: ~s~n" statement)
        ;(printf "top-level: ~s~n" (hash-ref *label-table* statement))
        (interpret-program (hash-ref *label-table* statement))

)

(define (interpret-if statement)
    ;(printf "if_statement: ~s~n" statement)
    (cond  
        ((equal? #t (evaluate-expr (car statement))) 
            (interpret-program (hash-ref *label-table* (cadr statement))))
        (else `())
        ) 
    ;(printf "if: ~s~n"(evaluate-expr (car statement)))
)

(define (interpret-input statement)
        (let ((object (read)))
          (cond 
            ((eof-object? object) 
               (variable-put! (car statement) (hash-ref *variable-table* 'nan))
               (variable-put! 'eof 1))
            ((number? object)
                (variable-put! (car statement) (+ object 0.0))
                ;(printf "input: ~s~n" (hash-ref *variable-table* (car statement)))
                )
            (else 
                (variable-put! (car statement) (hash-ref *variable-table* 'nan))
                (hash-ref *variable-table* 'nan)
                ) 
          ))
        (if (not (null? (cdr statement)))
        (interpret-input (cdr statement))
        (void))
   
)

;----------Length Helper Functions----------
(define (line_nbr line)
    (equal? (length line) 1)
)

(define (label line)
    (symbol? (cadr line)) 
)

(define (label_program line)
    (and (symbol? (cadr line)) (equal? (length line) 2))
)

(define (label_statement line)
    (and (symbol? (cadr line)) (equal? (length line) 3))
)

;----------Interpret-Program----------
;takes the sbir file in as the parameter program and checks contents of each line
(define (interpret-program program)
    (when (not (null? (car program)))
        (let ((line (car program)))
        (cond
            ;only line number
            ;(printf "line_nbr: ~s~n" line)
            ((line_nbr line))

            ;label
            ((label_program line))
              ;(printf "line: ~s~n" line))

            ;label and statement 
            ;(printf "label:~s statement:~s~n" (cadr program))
            ((label_statement line) 
                ;(printf "statement: ~s~n" (caddr line)) 
                (interpret-statement (caddr line))) 
            ;statement
            (else (interpret-statement (cadr line)))
        )))
        ;stop tail recursion if reach EOF 
        (if (not (null? (cdr program)))
            (interpret-program (cdr program))
            ;else
            (exit 0)
        ) 
                
)

;----------Evaluate-Expression----------
(define NAN (/ 0.0 0.0))

(define (evaluate-expr expr)
    
    (cond 
        ;statement arg is a number
        ((number? expr) (+ expr 0.0))

        ;statement arg is a string, return arg
        ((string? expr) expr)

        ;statement arg is a symbol
        ((symbol? expr) (hash-ref *variable-table* expr NAN))

        ((and (list? expr) (equal? (car expr) 'asub))        
            (vector-ref (hash-ref *array-table* (cadr expr)) 
            (exact-round (evaluate-expr (caddr expr)))))
        
        ;statement arg is a func
        ((pair? expr) 
        ;create variable func which is the first element (operator) of the statement expression
        ;finds operator in the function hash table
        ;create variable opnds 
        (let (
          (func  (hash-ref *function-table* (car expr) NAN))
          (opnds (map evaluate-expr (cdr expr)))
          )
         (if (null? func) NAN
            (apply func (map evaluate-expr opnds)))))
        (else NAN))
)     



;running the file specified in the terminal call
(define *run-file*
    (let-values
        (((dirpath basepath root?)
            (split-path (find-system-path 'run-file))))
        (path->string basepath))
)


(define (die list)
    (for-each (lambda (item) (display item *stderr*)) list)
    (newline *stderr*)
    (exit 1)
)

(define (usage-exit)
    (die `("Usage: " ,*run-file* " filename"))
)

;the built-in read function of scheme
(define (readlist-from-inputfile filename)
    (let ((inputfile (open-input-file filename)))
         (if (not (input-port? inputfile))
             (die `(,*run-file* ": " ,filename ": open failed"))
             (let ((program (read inputfile)))
                  (close-input-port inputfile)
                         program))))

(define (dump-stdin)
    (let ((token (read)))
         (printf "token=~a~n" token)
         (when (not (eq? token eof)) (dump-stdin))))

;dummy program created to just print the lists/lines of the file out
(define (write-program-by-line filename program)
    (printf "==================================================~n")
    (printf "~a: ~s~n" *run-file* filename)
    (printf "==================================================~n")
    (printf "(~n")

    ;lambda function that just takes in program as a parameter 
    ;and prints out each list/line from the read function
    (for-each (lambda (line) (printf "~s~n" line)) program)
    (printf ")~n"))



;where the list readable is parsed through
(define (main arglist)
    (if (or (null? arglist) (not (null? (cdr arglist))))
        (usage-exit)
        (let* ((sbprogfile (car arglist))
               ;this is where you will put the two functions that you call that both use 'program' as a param

               ;creates the variable 'program' and assigns the function readlist to it
               (program (readlist-from-inputfile sbprogfile)))
               (insert-labels program)
               ;(printf "Label Table: ~s~n" *label-table*)
               (interpret-program program)
               ;currently calls the 'write-program-by-line' method 
               ;which just prints the lists/lines from the file
               ;(write-program-by-line sbprogfile program)
               ))
               )
               

(if (terminal-port? *stdin*)
    (main (vector->list (current-command-line-arguments)))
    (printf "sbi.scm: interactive mode~n"))