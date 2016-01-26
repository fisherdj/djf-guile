(define-module (djf vhash)
  #:use-module (ice-9 vlist)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-26)
  #:use-module (djf vset)
  #:re-export (alist->vhash))

(define-public (vhash-over key value vhash)
  (vhash-cons key value (vhash-delete key vhash)))

(define-public (vhash->alist vhash)
  (vhash-fold (lambda (k v r) (cons (cons k v) r)) '() vhash))

(define-public (vhash-keys vhash)
  (vhash-fold (lambda (k v r) (cons k r)) '() vhash))

(define-public (vhash-vals vhash)
  (vhash-fold (lambda (k v r) (cons v r)) '() vhash))

(define-public (vhash-update key proc vhash)
  (vhash-over key (proc (and=> (vhash-assoc key vhash) cdr)) vhash))

(define-public (vhash-merge f . vhashes)
  (let ((keys (vset->list
	       (apply vset-union
		      (map (compose list->vset vhash-keys)
			   vhashes)))))
    (fold-right
     (lambda (key vhash)
       (vhash-over key
		   (apply f (filter identity
				    (map (cut vhash-assoc key <>)
					 vhashes)))
		   vhash))
     vlist-null keys)))

(define-public (vhash-merge-val f . vhashes)
  (apply vhash-merge (lambda x (apply f (map cdr x))) vhashes))

(define-public (alist-merge f . alists)
  (vhash->alist (apply vhash-merge f (map alist->vhash alists))))

(define-public (alist-merge-val f . alists)
  (vhash->alist
   (apply vhash-merge-val
	  (cons f (map alist->vhash
		       alists)))))

(define-public (vhash-map f . vhashes)
  (let ((keys (vset->list
	       (apply vset-inter
		      (map (compose list->vset vhash-keys)
			   vhashes)))))
    (fold-right
     (lambda (key vhash)
       (vhash-over key
		   (apply f
			  (map (cut vhash-assoc key <>)
			       vhashes))
		   vhash))
     vlist-null
     keys)))

(define-public (vhash-map-val f . vhashes)
  (apply vhash-map (lambda x (apply f (map cdr x))) vhashes))

(define-public (alist-map f . alists)
  (vhash->alist (apply vhash-map f (map alist->vhash alists))))

(define-public (alist-map-val f . alists)
  (vhash->alist (apply vhash-map-val f (map alist->vhash alists))))

(define-public (vhash-filter f vhash)
  (vhash-fold
   (lambda (k v h) (if (f k v) (vhash-over k v h) h))
   vlist-null
   vhash))

(define-public (vhash-filter-val f vhash)
  (vhash-filter (compose f (lambda (x y) y)) vhash))

(define-public (vhash-ref vhash key)
  (and=> (vhash-assoc key vhash) cdr))
