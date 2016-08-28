(use-modules (srfi srfi-1)) ;; for append-map
(use-modules (srfi srfi-11)) ;; for let-values
(use-modules (ice-9 receive)) ;; for receive

;; memoize functions
(define (memoize f)
  (let ((table (make-hash-table)))
    (lambda (x)
      (let ((prev-result (hash-ref table x)))
        (or prev-result (let ((result (f x)))
                          (hash-set! table x result) result))))))

;; returns a list of permutations of a given list or string
(define (permutations lst)
  (define (_permutations lst)
    (cond ((null? lst) lst)
          ((= (length lst) 1) (list lst))
          ;; for each element in lst, cons it to all permutations of (list - element)
          (else (append-map (lambda (x) (map (lambda (y) (cons x y)) (_permutations (delq x lst)))) lst))))
  (set! _permutations (memoize _permutations))
  ;; cast strings to a list of chars, and convert back to string at end
  (if (string? lst) (map (lambda (x) (list->string x)) (_permutations (string->list lst)))
    (_permutations lst)))

(define (k-element-subsets set k)
  (cond ((= 0 k) '())
        ((= 1 k) (map (lambda (x) (list x)) set))
        (else (get-k-subsets (map (lambda (x) (list x)) set) (gen-tuples set 2) k 2))))

(define (get-k-subsets orig-set cur-sets k n)
  (cond ((= k n) cur-sets)
        (else (get-k-subsets
                orig-set
                (append-map
                  (lambda (x)
                    (filter (lambda (x) x)
                            (map (lambda (y)
                                   (if (not (member (car y) x)) (append x y) #f))
                                 orig-set)))
                  cur-sets) k (1+ n)))))

;; used to compare lists deeply (compare by length first, then by contents)
(define* (deep-less #:optional (less <) (e =))
  (define (func x y) (cond ((null? x) #t)
                           ((null? y) #f)
                           ((not (= (length x) (length y))) (< (length x) (length y)))
                           ((e (car x) (car y)) (func (cdr x) (cdr y)))
                           (else (less (car x) (car y)))))
  func)

(define (gen-tuples lst n)
  (if (not (>= (length lst) n)) '()
    (receive (head tail) (split-at lst (1- n))
      (sort-list (append
         (map (lambda (x) (sort (lset-adjoin equal? head x) <)) tail)
         (gen-tuples (cdr lst) n)
         ) (deep-less)))))

(define* (subsets lst #:optional size upto)
 (define* (_subsets lst #:optional size)
   (cond ((null? lst) (list lst))
         (else
           (let* ((set (list->set lst))
                  (results (subsets (cdr set)))
                  (results (append results (map (lambda (y) (cons (car lst) y)) results))))
             ; (format #t "~s\n" results)
             (if size (filter (lambda (x) (<= (length x) size)) results) results)))))
 (let ((results (_subsets lst size)))
   (if (and size (not upto)) (filter (lambda (x) (= (length x) size)) results) results)))

(define (list->set lst)
  (if (null? lst) lst
    (apply lset-adjoin = '() lst)))
  ; (let* ((sorted (sort lst <)) (set (list (car sorted))))
  ;   (for-each (lambda (x) (if (not (= x (last set))) (set! set (append set (list x))))) (cdr sorted))
  ;   set)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; From "Teach Yourself Scheme in Fixnum Days"
(define amb-fail '*)

(define initialize-amb-fail
  (lambda ()
    (set! amb-fail
      (lambda ()
        (error "amb tree exhausted")))))

(initialize-amb-fail)

(define-macro amb
  (lambda alts...
    `(let ((+prev-amb-fail amb-fail))
       (call/cc
        (lambda (+sk)

          ,@(map (lambda (alt)
                   `(call/cc
                     (lambda (+fk)
                       (set! amb-fail
                         (lambda ()
                           (set! amb-fail +prev-amb-fail)
                           (+fk 'fail)))
                       (+sk ,alt))))
                 alts...)

          (+prev-amb-fail))))))

(define assert
  (lambda (pred)
    (if (not pred) (amb))))

(define (choose lst)
  (assert (not (null? lst)))
  (amb (car lst) (choose (cdr lst))))

(define-macro bag-of
  (lambda (e)
    `(let ((+prev-amb-fail amb-fail)
           (+results '()))
       (if (call/cc
             (lambda (+k)
               (set! amb-fail (lambda () (+k #f)))
               (let ((+v ,e))
                 (set! +results (cons +v +results))
                 (+k #t))))
         (amb-fail))
       (set! amb-fail +prev-amb-fail)
       (reverse! +results))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (fact n)
  (if (< n 2) 1 (* n (fact (1- n)))))

(define (n-choose-r n r)
  (/ (fact n) (fact (- n r)) (fact r)))

(define (group lst n) ;; split lst into groups of size at most n
  (if (<= (length lst) n) (list lst)
    (let-values (((head tail) (split-at lst n)))
                (cons head (group tail n)))))

(define (flatten lst)
  (cond ((null? lst) lst)
        ((not (list? lst)) (list lst))
        (else (append (flatten (car lst)) (flatten (cdr lst))))))
