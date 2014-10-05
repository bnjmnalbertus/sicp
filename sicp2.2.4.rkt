;; Exercise 2.44.  Define the procedure up-split used by
;; corner-split. It is similar to right-split, except that it switches
;; the roles of below and beside.
;;
;; Definition for right-split:
;;
;; (define (right-split painter n)
;;   (if (= n 0)
;;       painter
;;       (let ((smaller (right-split painter (- n 1))))
;;         (beside painter (below smaller smaller)))))

(define (up-split painter n)
  (if (= n 0)
      painter
      (let ((smaller (up-split painter (- n 1))))
        (below painter (beside smaller smaller)))))

;; Exercise 2.45.  Right-split and up-split can be expressed as
;; instances of a general splitting operation. Define a procedure
;; split with the property that evaluating

;; (define right-split (split beside below))
;; (define up-split (split below beside))

;; produces procedures right-split and up-split with the same
;; behaviors as the ones already defined.

(define (split op1 op2)
  (define (split-iter first second painter n)
    (if (= n 0)
        painter
        (let ((smaller (split-iter first second painter (- n 1))))
          (first painter (second smaller smaller)))))
  (lambda (pntr m) (split-iter op1 op2 pntr m)))

;; Exercise 2.46.  A two-dimensional vector v running from the origin
;; to a point can be represented as a pair consisting of an
;; x-coordinate and a y-coordinate. Implement a data abstraction for
;; vectors by giving a constructor make-vect and corresponding
;; selectors xcor-vect and ycor-vect. In terms of your selectors and
;; constructor, implement procedures add-vect, sub-vect, and
;; scale-vect that perform the operations vector addition, vector
;; subtraction, and multiplying a vector by a scalar:

(define (make-vect x y)
  (lambda (n) (cond ((= n 0) x)
		    ((= n 1) y)
		    (else null))))

(define (xcor-vect v)
  (v 0))

(define (ycor-vect v)
  (v 1))

(define (add-vect v w)
  (make-vect (+ (xcor-vect v) (xcor-vect w))
	     (+ (ycor-vect v) (ycor-vect w))))

(define (sub-vect v w)
  (make-vect (- (xcor-vect v) (xcor-vect w))
	     (- (ycor-vect v) (ycor-vect w))))

(define (scale-vect s v)
  (make-vect (+ s (xcor-vect v))
	     (+ s (ycor-vect v))))

;; Exercise 2.47.  Here are two possible constructors for frames:

(define (make-frame origin edge1 edge2)
  (list origin edge1 edge2))

(define (make-frame origin edge1 edge2)
  (cons origin (cons edge1 edge2)))

;; For each constructor supply the appropriate selectors to produce an
;; implementation for frames.

(define (origin-frame f)
  (car f))

(define (edge1-frame f)
  (cadr f))

;; First version
(define (edge2-frame f)
  (caddr f))

;; Second version
(define (edge2-frame f)
  (cddr f))
