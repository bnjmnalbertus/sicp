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
  (foldr (lambda (x y) (append y (list x))) null sequence))
(define (reverse sequence)
  (foldl (lambda (x y) (cons x y)) null sequence))

;; This definition is assumed:

(define (enumerate-interval low high)
  (if (> low high)
      null
      (cons low (enumerate-interval (+ low 1) high))))

;; Exercise 2.40.  Define a procedure unique-pairs that, given an
;; integer n, generates the sequence of pairs (i,j) with 1<= j< i<=
;; n. Use unique-pairs to simplify the definition of prime-sum-pairs
;; given above.

;; Definition of prime-sum-pairs used above:
;;

(define (flatmap proc seq)
  (accumulate append null (map proc seq)))

(define (prime-sum? pair)
  (prime? (+ (car pair) (cadr pair))))

(define (make-pair-sum pair)
  (list (car pair) (cadr pair) (+ (car pair) (cadr pair))))

;; (define (prime-sum-pairs n)
;;   (map make-pair-sum
;;        (filter prime-sum?
;;                (flatmap
;;                 (lambda (i)
;;                   (map (lambda (j) (list i j))
;;                        (enumerate-interval 1 (- i 1))))
;;                 (enumerate-interval 1 n)))))

(define (unique-pairs n)
  (flatmap (lambda (i)
             (map (lambda (j) (list i j))
                  (enumerate-interval 1 (- i 1))))
           (enumerate-interval 1 n)))

(define (prime-sum-pairs n)
  (map make-pair-sum
       (filter prime-sum?
               (unique-pairs n))))

;; Exercise 2.41.  Write a procedure to find all ordered triples of
;; distinct positive integers i, j, and k less than or equal to a
;; given integer n that sum to a given integer s.

(define (triple-sum-less-than n s)
  (filter (lambda (x) (and (not (= (car x) (cadr x)))
                           (not (= (car x) (caddr x)))
                           (not (= (cadr x) (caddr x)))
                           (< (accumulate + 0 x) s)))
          (flatmap (lambda (i)
                     (flatmap (lambda (j)
                                (map (lambda (k) (list i j k))
                                     (enumerate-interval 1 n)))
                              (enumerate-interval 1 n)))
                   (enumerate-interval 1 n))))


;; Exercise 2.42.  

;; Figure 2.8:  A solution to the eight-queens puzzle.

;; The ``eight-queens puzzle'' asks how to place eight queens on a
;; chessboard so that no queen is in check from any other (i.e., no
;; two queens are in the same row, column, or diagonal). One possible
;; solution is shown in figure 2.8. One way to solve the puzzle is to
;; work across the board, placing a queen in each column. Once we have
;; placed k - 1 queens, we must place the kth queen in a position
;; where it does not check any of the queens already on the board. We
;; can formulate this approach recursively: Assume that we have
;; already generated the sequence of all possible ways to place k - 1
;; queens in the first k - 1 columns of the board. For each of these
;; ways, generate an extended set of positions by placing a queen in
;; each row of the kth column. Now filter these, keeping only the
;; positions for which the queen in the kth column is safe with
;; respect to the other queens. This produces the sequence of all ways
;; to place k queens in the first k columns. By continuing this
;; process, we will produce not only one solution, but all solutions
;; to the puzzle.

;; We implement this solution as a procedure queens, which returns a
;; sequence of all solutions to the problem of placing n queens on an
;; n× n chessboard. Queens has an internal procedure queen-cols that
;; returns the sequence of all ways to place queens in the first k
;; columns of the board.

(define (queens board-size)
  (define (queen-cols k)  
    (if (= k 0)
        (list empty-board)
        (filter
         (lambda (positions) (safe? k positions))
         (flatmap
          (lambda (rest-of-queens)
            (map (lambda (new-row)
                   (adjoin-position new-row k rest-of-queens))
                 (enumerate-interval 1 board-size)))
          (queen-cols (- k 1))))))
  (queen-cols board-size))

In this procedure rest-of-queens is a way to place k - 1 queens in the first k - 1 columns, and new-row is a proposed row in which to place the queen for the kth column. Complete the program by implementing the representation for sets of board positions, including the procedure adjoin-position, which adjoins a new row-column position to a set of positions, and empty-board, which represents an empty set of positions. You must also write the procedure safe?, which determines for a set of positions, whether the queen in the kth column is safe with respect to the others. (Note that we need only check whether the new queen is safe -- the other queens are already guaranteed safe with respect to each other.)

Exercise 2.43.  Louis Reasoner is having a terrible time doing exercise 2.42. His queens procedure seems to work, but it runs extremely slowly. (Louis never does manage to wait long enough for it to solve even the 6× 6 case.) When Louis asks Eva Lu Ator for help, she points out that he has interchanged the order of the nested mappings in the flatmap, writing it as

(flatmap
 (lambda (new-row)
   (map (lambda (rest-of-queens)
          (adjoin-position new-row k rest-of-queens))
        (queen-cols (- k 1))))
 (enumerate-interval 1 board-size))

Explain why this interchange makes the program run slowly. Estimate how long it will take Louis's program to solve the eight-queens puzzle, assuming that the program in exercise 2.42 solves the puzzle in time T. 
