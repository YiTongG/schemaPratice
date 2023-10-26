;;;1. Base Case: L is empty, return 0
;;; Assumption: (count-numbers M) returns a count of the numbers in M, for
;;; any list M smaller than L (including (car L) and (cdr L)).
;;; Step: If (car L) is a list, then return the sum of the count of the
;;; numbers in (car L) and the count of the numbers in (cdr L).
;;; If (car L) is a number, return 1 plus the count of the numbers in
;;; (cdr L). Otherwise, return the count of the numbers in (cdr L).

(define (count-numbers L)
  (cond ((null? L) 0)
        ((list? (car L)) (+ (count-numbers (car L)) (count-numbers (cdr L))))
        ((number? (car L)) (+ 1 (count-numbers (cdr L))))
        (else (count-numbers (cdr L)))))
;2
;;; Base Case: If L is empty or x is smaller than (car L), insert x at the beginning.
;;; Assumption: (insert x M) correctly inserts x into a list M smaller than L.
;;; Step: Compare x with (car L). If x is less, insert x before (car L). 
;;; Otherwise, recursively attempt to insert x into (cdr L).
(define (insert x L)
  (if (or (null? L)
          (< x (car L)))
      (cons x L)
      (cons (car L) (insert x (cdr L)))))
;; 3. Base Case: If L is empty, return M.
;;
;; Assumption:
;; We assume that insert-all can correctly insert the tail of M (cdr M) into L.
;;; Step: Insert the first element of L into M and recurse with the rest of L.
;; Otherwise:
;; This is essentially the inverse of the base case, i.e., when M is not empty, we perform the recursive step.
;;;(insert-all '(3 6 1 5 2 7 4) '())
(define (insert-all L M)
  (if (null? L) M 
      (insert-all (cdr L) (insert (car L) M)))) 

;;; 4.Base Case: If L is empty or x is smaller than (car L), insert x at the beginning.
;;; Assumption: (insert x M) correctly inserts x into a list M smaller than L.
;;; Step: Compare x with (car L). If x is less, insert x before (car L). 
;;; Otherwise, recursively attempt to insert x into (cdr L).
;;; (sort '(3 6 1 5 2 7 4))
(define (sort L)
  (letrec ((insert (lambda (x L)
                     (if (or (null? L) (< x (car L)))
                         (cons x L)
                         (cons (car L) (insert x (cdr L)))))))
    (if (null? L)
        '()
        (insert (car L) (sort (cdr L))))))

;;;5 Translate
(define (translate op)
  (cond 
    ((eq? op '+) +)
    ((eq? op '-) -)
    ((eq? op '*) *)
    ((eq? op '/) /)))
;;;6
;;; Base Case: If exp is a number, return the number directly.
;;; Assumption: (postfix-eval exp) correctly evaluates a postfix expression smaller than the current one.
;;; Step: If exp is a list, recursively evaluate the arguments (arg1 and arg2) and then
;;; apply the operator op to the results.

(define (postfix-eval exp)
  (if (list? exp)
      ((translate (caddr exp)) (postfix-eval (car exp)) (postfix-eval (cadr exp)))
      exp))
;;;7
;;; Base Case: L is empty, return the set containing the empty
;;; set, i.e. ’(()).
;;; Assumption: (powerset M) returns the powerset of M, for any set M
;;; smaller than L (including (cdr L)).
;;; Step: To get the powerset of L, get the powerset of (cdr L) and then
;;; add both the subsets from that powerset and the subsets that include (car L).

(define (powerset L)
  (if (null? L)
      '(()) ; Base case
      (let ((rest-powerset (powerset (cdr L)))) ; Recursively get the powerset of the rest of the list
        (append rest-powerset 
                (map (lambda (subset) (cons (car L) subset)) rest-powerset)))))






































