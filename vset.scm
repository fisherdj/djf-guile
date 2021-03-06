(define-module (djf vset))

(use-modules (srfi srfi-1)
             (srfi srfi-26)
             (ice-9 vlist))

(define-public vset-null vlist-null)
(define-public vset-empty? vlist-null?)

(define-public (vset-cons x s)
  (vhash-cons x #t (vset-remove x s)))

(define-public (vset-cons* a . b)
  (reduce-right vset-cons '() (cons a b)))

(define-public (vset-remove x s)
  (vhash-delete x s))

(define-public (list->vset l)
  (fold vset-cons vlist-null l))

(define-public (vset->list s)
  (vlist->list (vlist-map car s)))

(define-public (vset-in x s)
  (if (vhash-assoc x s) #t #f))

(define-public (vset-subset s1 s2)
  (define (rec l)
    (or (null? l) (and (vset-in (car l) s2) (rec (cdr l)))))
  (rec (vset->list s1)))

(define-public (vset-filter f s)
  (fold (lambda (x s) (if (f x) (vset-cons x s) s))
        vlist-null (vset->list s)))

(define-public (vset-map f s)
  (list->vset (map f (vset->list s))))

(define-public (vset-union . sets)
  (define (union-two s1 s2)
    (fold vset-cons s1 (vset->list s2)))
  (reduce union-two vset-null sets))

(define-public (vset-diff s . rest)
  (define (inter-two s1 s2)
    (vset-filter (lambda (x) (not (vset-in x s1))) s2))
  (fold inter-two s rest))

(define-public (vset-inter s . rest)
  (define (inter-two s1 s2)
    (vset-filter (cut vset-in <> s1) s2))
  (fold inter-two s rest))

(define-public (vset-xor . sets)
  (define (xor-two s1 s2)
    (vset-diff (vset-union s1 s2) (vset-inter s1 s2)))
  (reduce xor-two vlist-null sets))

(define-public (vset-equal? . sets)
  (define (eq-two s1 s2)
    (vset-empty? (vset-xor s1 s2)))
  (reduce eq-two #t sets))

;(define uniq (compose vset->list list->vset))
; useful, but maybe shouldn't be part of the module
