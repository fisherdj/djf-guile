(define-module (djf fstruct)
  #:use-module (oop goops)
  #:use-module (srfi srfi-26)
  #:use-module (ice-9 match)
  #:export (fstruct-ref make-fstruct define-fstruct fstruct-spec ; core functionality
	    fstruct-class-spec)
  #:export (fstruct->list fstruct->alist fstruct-map fstruct-map* ; ancillary
	    fstruct-destruct fstruct-destruct* fstruct-adestruct
	    fstruct-adestruct* fstruct-map-slot fstruct-let)
  #:export (define-box make-box unbox box-map)
  #:export (define-unit)) ; box functions

(export fstruct-ref make-fstruct define-fstruct fstruct-spec)
(export	fstruct->list fstruct->alist fstruct-map fstruct-map*
	fstruct-destruct fstruct-adestruct fstruct-adestruct* fstruct-map-slot)

; core functionality
(define-generic fstruct-spec)
(define-syntax-rule (define-fstruct name slots ...)
                    (begin (define-class name () slots ...)
                           (define-method (fstruct-spec (x name)) '(slots ...))))

(define fstruct-ref slot-ref)
(define (make-fstruct class . vals)
  (define struct (make class))
  (for-each (cut slot-set! struct <> <>) (fstruct-spec struct) vals)
  struct)
(define fstruct-class class-of)
(define (fstruct-class-spec class)
  (fstruct-spec (make class)))

; ancillary
(define (fstruct->list struct)
  (map (λ (slot) (fstruct-ref struct slot))
       (fstruct-spec struct)))

(define (fstruct->alist struct)
  (map cons (fstruct-spec struct) (fstruct->list struct)))

#;(define (fstruct-map f . structs)
(define spec (fstruct-spec (car structs)))
(define class (class-of (car structs)))
(apply make-fstruct class
       (map (λ (slot)
	      (apply f (map (cut fstruct-ref <> slot) structs)))
	    spec)))

(define (fstruct-map* f . structs)
  (apply make-fstruct (class-of (car structs))
	 (apply map f (fstruct-spec (car structs))
		(map fstruct->list structs))))

(define (fstruct-map f . structs)
  (apply fstruct-map* (λ args (apply f (cdr args))) structs))

(define (fstruct-destruct . structs)
  (map fstruct->list structs))

(define (fstruct-destruct* . structs)
  (apply map list (apply fstruct-destruct structs)))

(define (fstruct-adestruct . structs)
  (map fstruct->alist structs))

(define (fstruct-adestruct* . structs)
  (apply map list (fstruct-spec (car structs))
	 (apply fstruct-destruct structs)))

(define (fstruct-map-slot slot f struct)
  (fstruct-map* (λ (s arg)
		  (if (eqv? s slot)
		      (f arg)
		      arg))
		struct))

(define-syntax fstruct-let
  (syntax-rules ()
    ((_ ((struct vars ...) rest ...) body ...)
     (match (fstruct->list struct)
       ((vars ...) (fstruct-let (rest ...) body ...))))
    ((_ () body ...) (let () body ...))))

; "box" supplementary fstructs
(define-syntax-rule (define-box cname) (define-fstruct cname obj))
(define (make-box class obj)
  (make-fstruct class obj))

(define unbox (cut fstruct-ref <> 'obj))
;(define (box-map f box) (make-box (class-of box) (f (unbox box))))
;(define (box-map f box) (fstruct-map f box))
(define (box-map f . boxes) (apply fstruct-map f boxes))

; "unit" supplementary fstructs
(define-syntax-rule (define-unit cname elm)
  (begin (define-fstruct cname)
	 (define elm (make-fstruct cname))))
;(define (make-unit class) (make-fstruct class))
