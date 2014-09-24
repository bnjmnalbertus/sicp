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
