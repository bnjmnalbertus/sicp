;;Exercise 2.53.  What would the interpreter print in response to
;;evaluating each of the following expressions?
;;
;;(list 'a 'b 'c)

'(a b c)

;;(list (list 'george))

'((george))

;;(cdr '((x1 x2) (y1 y2)))

'((y1 y2))

;;(cadr '((x1 x2) (y1 y2)))

'(y1 y2)

;;(pair? (car '(a short list)))

#f

;;(memq 'red '((red shoes) (blue socks)))

#f

;;(memq 'red '(red shoes blue socks))

'(red shoes blue socks)

;;Exercise 2.54.  Two lists are said to be equal? if they contain
;;equal elements arranged in the same order. For example,
;;
;;(equal? '(this is a list) '(this is a list))
;;
;;is true, but
;;
;;(equal? '(this is a list) '(this (is a) list))
;;
;;is false. To be more precise, we can define equal? recursively in
;;terms of the basic eq? equality of symbols by saying that a and b
;;are equal? if they are both symbols and the symbols are eq?, or if
;;they are both lists such that (car a) is equal? to (car b) and (cdr
;;a) is equal? to (cdr b). Using this idea, implement equal? as a
;;procedure.36

(define (equal? a b)
  (cond ((and (pair? a) (pair? b))
         (and (equal? (car a) (car b))
              (equal? (cdr a) (cdr b))))
        ((and (not (pair? a)) (not (pair? b)))
         (eq? a b))
        (else false)))

;;Exercise 2.55.  Eva Lu Ator types to the interpreter the expression
;;
;;(car ''abracadabra)
;;
;;To her surprise, the interpreter prints back quote. Explain.

;; (Note that the interpreter should print 'quote, not quote.) In
;; Scheme, the single quote (') is syntactic sugar for the quote
;; function. So, the expression

(equal? ''abracadabra (quote (quote abracadabra)))

;;returns true. Note that '(quote abracadabra) is a third synonym. So

(equal? (car '(quote abracadabra)) 'quote)

;;and

(equal? (cdr '(quote abracadabra)) '(abracadabra))

;;are both true.
