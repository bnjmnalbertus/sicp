;; This definition is assumed throughout:

(define (accumulate op initial sequence)
           (cond ((null? sequence) initial)
                 (else (op (car sequence)
                           (accumulate op initial (cdr sequence))))))

;; Exercise 2.33.  Fill in the missing expressions to complete the
;; following definitions of some basic list-manipulation operations as
;; accumulations:

(define (map p sequence)
  (accumulate (lambda (x y) (cons (p x) y)) null sequence))
(define (append seq1 seq2)
  (accumulate cons seq2 seq1))
(define (length sequence)
  (accumulate (lambda (x y) (+ 1 y)) 0 sequence))

;; Exercise 2.34.  Evaluating a polynomial in x at a given value of x
;; can be formulated as an accumulation. We evaluate the polynomial
;; 
;; a_nx^n + a_{n-1}x^{n-1} + \dots + a_1x + a_0
;;
;; using a well-known algorithm called Horner's rule, which structures
;; the computation as
;;
;; \(\dots\(a_nx^n + a_{n-1}\)x + \dots + a_1\)x + a_0
;;
;; In other words, we start with a_n, multiply by x, add a_{n-1},
;; multiply by x, and so on, until we reach a_0. Fill in the following
;; template to produce a procedure that evaluates a polynomial using
;; Horner's rule. Assume that the coefficients of the polynomial are
;; arranged in a sequence, from a0 through an.

(define (horner-eval x coefficient-sequence)
  (accumulate (lambda (this-coeff higher-terms) (+ this-coeff 
                                                   (* x higher-terms)))
              0
              coefficient-sequence))

;; For example, to compute 1 + 3x + 5x3 + x5 at x = 2 you would
;; evaluate
;;
;; (horner-eval 2 (list 1 3 0 5 0 1))

;; Exercise 2.35.  Redefine count-leaves from section 2.2.2 as an
;; accumulation:

(define (count-leaves t)
  (accumulate + 0 (map (lambda (x)
                         (cond ((null? x) 0)
                               ((not (pair? x)) 1)
                               (else (count-leaves x)))) t)))

;; Exercise 2.36.  The procedure accumulate-n is similar to accumulate
;; except that it takes as its third argument a sequence of sequences,
;; which are all assumed to have the same number of elements. It
;; applies the designated accumulation procedure to combine all the
;; first elements of the sequences, all the second elements of the
;; sequences, and so on, and returns a sequence of the results. For
;; instance, if s is a sequence containing four sequences, ((1 2 3) (4
;; 5 6) (7 8 9) (10 11 12)), then the value of (accumulate-n + 0 s)
;; should be the sequence (22 26 30). Fill in the missing expressions
;; in the following definition of accumulate-n:

(define (accumulate-n op init seqs)
  (if (null? (car seqs))
      null
      (cons (accumulate op init (map car seqs))
            (accumulate-n op init (map cdr seqs)))))

;; Exercise 2.37.  Suppose we represent vectors v = (v_i) as sequences
;; of numbers, and matrices m = (m_{ij}) as sequences of vectors (the
;; rows of the matrix). For example, the matrix
;;
;; [ [ 1 2 3 4 ]
;;   [ 4 5 6 6 ]
;;   [ 6 7 8 9 ] ]
;;
;; is represented as the sequence ((1 2 3 4) (4 5 6 6) (6 7 8
;; 9)). With this representation, we can use sequence operations to
;; concisely express the basic matrix and vector operations. These
;; operations (which are described in any book on matrix algebra) are
;; the following:
;;
;; (dot-product v w) returns the sum \Sigma_i v_iw_i
;; (matrix-*-vector m v) returns the vector t, where t_i = \Sigma_j m_{ij}v_j
;; (matrix-*-matrix m n) returns the matrix p, where p_{ij} = \Sigma_k m_{ik}n_{kj}
;; (transpose m) returns the matrix n, where n_{ij} = m_{ji}
;;
;; We can define the dot product as:

;; (a footnote here indicates that the built-in 'map' accepts as
;; arguments a function of n arguments and n lists whose elements will
;; be used as arguments)
(require racket/base)

(define (dot-product v w)
  (accumulate + 0 (map * v w)))

;; Fill in the missing expressions in the following procedures for
;; computing the other matrix operations. (The procedure accumulate-n
;; is defined in exercise 2.36.)

(define (matrix-*-vector m v)
  (map (lambda (w) (dot-product w v)) m))
(define (transpose mat)
  (accumulate-n cons null mat))
(define (matrix-*-matrix m n)
  (let ((cols (transpose n)))
    (map (lambda (x) (matrix-*-vector cols x)) m)))

;; Exercise 2.38.  The accumulate procedure is also known as
;; fold-right, because it combines the first element of the sequence
;; with the result of combining all the elements to the right. There
;; is also a fold-left, which is similar to fold-right, except that it
;; combines elements working in the opposite direction:

(define (fold-left op initial sequence)
  (define (iter result rest)
    (if (null? rest)
        result
        (iter (op result (car rest))
              (cdr rest))))
  (iter initial sequence))

;; What are the values of
;;
;; (fold-right / 1 (list 1 2 3))
;; (fold-left / 1 (list 1 2 3))
;; (fold-right list nil (list 1 2 3))
;; (fold-left list nil (list 1 2 3))

;; Give a property that op should satisfy to guarantee that fold-right
;; and fold-left will produce the same values for any sequence.
;;
;; Answer: associativity

;; Exercise 2.39.  Complete the following definitions of reverse
;; (exercise 2.18) in terms of fold-right and fold-left from exercise
;; 2.38:

(define (reverse sequence)
  (fold-right (lambda (x y) <??>) null sequence))
(define (reverse sequence)
  (fold-left (lambda (x y) <??>) null sequence))
