(require racket/mpair (for-syntax racket/base))

; This is a recursive version of Racket's list->mlist. Unlike the original, 
; the following version converts all pairs - including nested ones 
; to mutable pairs. This is needed because the read procedure (ie. user input) 
; will return expressions with nested pairs
(define (list->mlist-r exp)
  (cond ((null? exp) null)
        ((not (pair? exp)) exp)
        (else (mcons (list->mlist-r (car exp)) (list->mlist-r (cdr exp))))))


; The following is a macro that allows the use of mcadr, mcaddr, and the like 
; since Racket doesn't have such expressions in the language yet.
;
; This macro is courtesy of Eli Barzilay - http://barzilay.org/
(define-syntax (define-combinations stx)
  (syntax-case stx ()
    [(_ n) (integer? (syntax-e #'n))
     (let ([n (syntax-e #'n)])
       (define options (list (cons "a" #'mcar) (cons "d" #'mcdr)))
       (define (add-options r)
         (apply append
                (map (λ (opt)
                       (map (λ (l) (cons (string-append (car opt) (car l))
                                         (list (cdr opt) (cdr l))))
                            r))
                     options)))
       (define combinations
         (cdddr
          (let loop ([n n] [r '(("" . x))])
            (if (zero? n) r (append r (loop (sub1 n) (add-options r)))))))
       (define (make-name combo)
         (let ([s (string->symbol (string-append "mc" (car combo) "r"))])
           (datum->syntax stx s stx)))
       (with-syntax ([(body ...) (map cdr combinations)]
                     [(name ...) (map make-name combinations)])
         #'(begin (define (name x) body) ...)))]))

(define-combinations 4)