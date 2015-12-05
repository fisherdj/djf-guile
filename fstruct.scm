(define-module (fstruct)
               #:use-module (oop goops)
               #:use-module (srfi srfi-26)
               #:export (define-fstruct make-fstruct fstruct-spec fstruct-ref))

(define-generic fstruct-spec)
(define-syntax-rule (define-fstruct name slots ...)
                    (begin (define-class name () slots ...)
                           (define-method (fstruct-spec (x name)) '(slots ...))))

(define fstruct-ref slot-ref)
(define (make-fstruct class . vals)
  (define struct (make class))
  (for-each (cut slot-set! struct <> <>) (fstruct-spec struct) vals)
  struct)

#|
(use-modules (oop goops))

(define-syntax-rule (define-fstruct name slots ...)
                    (define-class name ()
                      (spec #:allocation #:class #:init-value '(slots ...))
                      obj))

(define (fstruct-raw name spec vals)
  (cond ((not (equal? (null? spec) (null? vals)))
         (error "mismatched slots:" spec vals))
        ((null? spec) (lambda (x) (error "Not in struct: " x name spec)))
        (else
          (let ((l (fstruct-raw name (cdr spec) (cdr vals))))
              (lambda (x)
                (if (eq? x (car spec))
                  (car vals)
                  (l x)))))))
  
(define (make-struct class . vals)
  (define struct (make class))
  (slot-set! struct 'obj
             (fstruct-raw (class-name class) (slot-ref struct 'spec) vals))
  struct)

(define (fstruct-ref struct item)
  ((slot-ref struct 'obj) item))
|#

;(define-fstruct <io-set> in-set out-set)
;(define a (make-struct <io-set> '() '()))

;(export fstruct-ref make-fstruct define-fstruct fstruct-spec)
