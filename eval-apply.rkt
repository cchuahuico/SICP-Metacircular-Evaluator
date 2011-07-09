#lang racket

(require racket/mpair)
(include "mpair-extension.rkt")

(define (eval exp env)
  (cond ((self-evaluating? exp) exp)
        ((variable? exp) (lookup-variable-value exp env))
        ((quoted? exp) (text-of-quotation exp))
        ((assignment? exp) (eval-assignment exp env))
        ((definition? exp) (eval-definition exp env))
        ((if? exp) (eval-if exp env))
        ((lambda? exp)
         (make-procedure (lambda-parameters exp)
                         (lambda-body exp)
                         env))
        ((begin? exp)
         (eval-sequence (begin-actions exp) env))
        ((cond? exp) (eval (cond->if exp) env))
        ((application? exp)
         (metacircular-apply (eval (operator exp) env)
                (list-of-values (operands exp) env)))
        (else
         (error "Unknown expression type -- EVAL" exp))))

(define (metacircular-apply procedure arguments)
  (cond ((primitive-procedure? procedure)
          (apply-primitive-procedure procedure arguments))
         ((compound-procedure? procedure)
          (eval-sequence
           (procedure-body procedure)
           (extend-environment
            (procedure-parameters procedure)
            arguments
            (procedure-environment procedure))))
         (else
          (error "Unknown procedure type -- APPLY" procedure))))

(define (list-of-values exps env)
  (if (no-operands? exps)
      '()
      (mcons (eval (first-operand exps) env)
             (list-of-values (rest-operands exps) env))))

(define (eval-if exp env)
  (if (true? (eval (if-predicate exp) env))
      (eval (if-consequent exp) env)
      (eval (if-alternative exp) env)))

(define (eval-sequence exps env)
  (cond ((last-exp? exps) (eval (first-exp exps) env))
        (else (eval (first-exp exps) env)
              (eval-sequence (rest-exps exps) env))))

(define (eval-assignment exp env)
  (set-variable-value! (assignment-variable exp)
                       (eval (assignment-value exp) env)
                       env)
  'ok)

(define (eval-definition exp env)
  (define-variable! (definition-variable exp)
                    (eval (definition-value exp) env)
                    env)
  'ok)

(define (self-evaluating? exp)
  (or (number? exp) (string? exp)))

(define (variable? exp) (symbol? exp))

(define (quoted? exp)
  (tagged-list? exp 'quote))

(define (text-of-quotation exp) (mcadr exp))

(define (tagged-list? exp tag)
  (if (mpair? exp) 
      (eq? (mcar exp) tag)
      false))

(define (assignment? exp)
  (tagged-list? exp 'set!))

(define (assignment-variable exp)
  (mcadr exp))

(define (assignment-value exp)
  (mcaddr exp))

(define (definition? exp)
  (tagged-list? exp 'define))

(define (definition-variable exp)
  (if (symbol? (mcadr exp))
      (mcadr exp)
      (mcaadr exp)))

(define (definition-value exp)
  (if (symbol? (mcadr exp))
      (mcaddr exp)
      (make-lambda (mcdadr exp)
                   (mcddr exp))))

(define (lambda? exp)
  (tagged-list? exp 'lambda))

(define (lambda-parameters exp) (mcadr exp))
(define (lambda-body exp) (mcddr exp))

(define (make-lambda parameters body)
  (mcons 'lambda (mcons parameters body)))

(define (if? exp) (tagged-list? exp 'if))
(define (if-predicate exp) (mcadr exp))
(define (if-consequent exp) (mcaddr exp))

(define (if-alternative exp)
  (if (not (null? (mcdddr exp)))
      (mcadddr exp)
      'false))

(define (make-if predicate consequent alternative)
  (mlist 'if predicate consequent alternative))

(define (begin? exp) (tagged-list? exp 'begin))

(define (begin-actions exp) (mcdr exp))
(define (last-exp? seq) (null? (mcdr seq)))
(define (first-exp seq) (mcar seq))
(define (rest-exps seq) (mcdr seq))

(define (sequence->exp seq)
  (cond ((null? seq)  seq)
        ((last-exp? seq) (first-exp seq))
        (else (make-begin seq))))

(define (make-begin seq) (mcons 'begin seq))

(define (application? exp) (mpair? exp))
(define (operator exp) (mcar exp))
(define (operands exp) (mcdr exp))
(define (no-operands? ops) (null? ops))
(define (first-operand ops) (mcar ops))
(define (rest-operands ops) (mcdr ops))
    
(define (cond? exp) (tagged-list? exp 'cond))
(define (cond-clauses exp) (mcdr exp))

(define (cond-else-clause? clause)
  (eq? (cond-predicate clause) 'else))

(define (cond-predicate clause) (mcar clause))
(define (cond-actions clause) (mcdr clause))

(define (cond->if exp)
  (expand-clauses (cond-clauses exp)))

(define (expand-clauses clauses)
  (if (null? clauses)
      'false
      (let ((first (mcar clauses))
            (rest (mcdr clauses)))
        (if (cond-else-clause? first)
            (if (null? rest)
                (sequence->exp (cond-actions first))
                (error "ELSE clause isn't last -- COND->IF"
                       clauses))
            (make-if (cond-predicate first)
                     (sequence->exp (cond-actions first))
                     (expand-clauses rest))))))

(define (true? x)
  (not (eq? x false)))

(define (false? x)
  (eq? x false))

(define (make-procedure parameters body env)
  (mlist 'procedure parameters body env))

(define (compound-procedure? p)
  (tagged-list? p 'procedure))

(define (procedure-parameters p) (mcadr p))
(define (procedure-body p) (mcaddr p))
(define (procedure-environment p) (mcadddr p))

; procedures for handling environments
; environments are essentially lists of frames
(define (enclosing-environment env) (mcdr env))
(define (first-frame env) (mcar env))
(define the-empty-environment '())

(define (extend-environment vars vals base-env)
  (if (= (mlength vars) (mlength vals))
      (mcons (make-frame vars vals) base-env)
      (if (< (mlength vars) (mlength vals))
          (error "Too many arguments supplied" vars vals)
          (error "Too few arguments supplied" vars vals))))


; procedures for handling frames containing variable to
; value bindings
(define (make-frame variables values)
  (mcons variables values))

(define (frame-variables frame) (mcar frame))
(define (frame-values frame) (mcdr frame))

(define (add-binding-to-frame! var val frame)
  (set-mcar! frame (mcons var (mcar frame)))
  (set-mcdr! frame (mcons val (mcdr frame))))

; procedures for looking, setting, and defining variables
; in an environment
(define (lookup-variable-value var env)
  (define (env-loop env)
    (define (scan vars vals)
      (cond ((null? vars)
             (env-loop (enclosing-environment env)))
            ((eq? var (mcar vars))
             (mcar vals))
            (else (scan (mcdr vars) (mcdr vals)))))
    (if (eq? env the-empty-environment)
        (error "Unbound variable" var)
        (let ((frame (first-frame env)))
          (scan (frame-variables frame)
                (frame-values frame)))))
  (env-loop env))

(define (set-variable-value! var val env)
  (define (env-loop env)
    (define (scan vars vals)
      (cond ((null? vars)
             (env-loop (enclosing-environment env)))
            ((eq? var (mcar vars))
             (set-mcar! vals val))
            (else (scan (mcdr vars) (mcdr vals)))))
    (if (eq? env the-empty-environment)
        (error "Unbound variable -- SET!" var)
        (let ((frame (first-frame env)))
          (scan (frame-variables frame)
                (frame-values frame)))))
  (env-loop env))

(define (define-variable! var val env)
  (let ((frame (first-frame env)))
    (define (scan vars vals)
      (cond ((null? vars)
             (add-binding-to-frame! var val frame))
            ((eq? var (mcar vars))
             (set-mcar! vals val))
            (else (scan (mcdr vars) (mcdr vals)))))
    (scan (frame-variables frame)
          (frame-values frame))))

(define (primitive-procedure? proc)
  (tagged-list? proc 'primitive))

(define (primitive-implementation proc) (mcadr proc))

(define primitive-procedures
  (mlist (mlist 'car car)
         (mlist 'cdr cdr)
         (mlist 'cons cons)
         (mlist 'null? null?)
         (mlist 'list list)
         (mlist '+ +)
         (mlist '* *)
         (mlist '/ /)
         (mlist '- -)
         (mlist '= =)))

(define (primitive-procedure-names)
  (mmap mcar primitive-procedures))

(define (primitive-procedure-objects)
  (mmap (lambda (proc) (mlist 'primitive (mcar (mcdr proc))))
       primitive-procedures))

(define (apply-primitive-procedure proc args)
  (apply 
   ; The primitive apply only accepts immutable lists
   ; thus, the need to convert
   (primitive-implementation proc) (mlist->list args)))

(define (setup-environment)
  (let ((initial-env
         (extend-environment (primitive-procedure-names)
                             (primitive-procedure-objects)
                             the-empty-environment)))
    (define-variable! 'true true initial-env)
    (define-variable! 'false false initial-env)
    initial-env))

(define the-global-environment (setup-environment))

(define input-prompt ";;; M-Eval input:")
(define output-prompt ";;; M-Eval value:")

(define (driver-loop)
  (prompt-for-input input-prompt)
  (let ((input (read)))
    (if (not (eof-object? input))
        (let ((output (eval (list->mlist-r input) the-global-environment)))
          (announce-output output-prompt)
          (user-print output)
          (driver-loop))
        (display "EOF Encountered. Terminating.."))))

(define (prompt-for-input string)
  (newline) (newline) (display string) (newline))

(define (announce-output string)
  (newline) (display string) (newline))

(define (user-print object)
  (if (compound-procedure? object)
      (display (mlist 'compound-procedure
                      (procedure-parameters object)
                      (procedure-body object)
                      '<procedure-env>))
      (display object)))

(driver-loop)
