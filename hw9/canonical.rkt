#lang racket

(require rackunit)

;; a sentence φ is :
;; (p_i) | (¬ φ) | (φ ∧ φ) | (φ ∨ φ) | (★ φ) | (◇ φ) 

(define (powerset xs)
  (foldr (λ (i rec)
           (append (map (λ (set) (cons i set)) rec) rec)) '(()) xs))

(check-equal? (powerset '(p (¬ p)))
              '((p (¬ p)) (p) ((¬ p)) ()))
(check-equal? (powerset '()) '(()))

(define C01 '((p1) (¬ p1)))


;; conj-◇ :: [φ] -> [φ]
;; returns the conjunction of every sentence in the given set, with (★ _) wrapped around it
(define (conj-◇ sents)
  (cond ((empty? sents) '())
        ((empty? (cdr sents)) `((◇ ,(car sents))))
        (else `((◇ ,(car sents)) ∧ ,@(conj-◇ (cdr sents))))))

(check-equal? (conj-◇ '((p) (¬ p)))
              '((◇ (p)) ∧ (◇ (¬ p))))
(check-equal? (conj-◇ '((p2) (¬ p2) (p1) (¬ p1)))
              '((◇ (p2)) ∧ (◇ (¬ p2)) ∧
                (◇ (p1)) ∧ (◇  (¬ p1))))


;; disj :: [φ] -> [φ]
(define (disj sents)
  (cond ((empty? sents) '())
        ((empty? (cdr sents))  (car sents))
        (else `(,(car sents) ∨ ,(disj (cdr sents))))))


(check-equal? (disj '((p) (¬ p)))
              '((p) ∨ (¬ p)))
(check-equal? (disj '()) '())
(check-equal? (disj '((p))) '(p))


#;; actual:  '((p2) ∨ ((¬ p2) ∨ ((p1) ∨ (¬ p1))))
(check-equal? (disj '((p2) (¬ p2) (p1) (¬ p1)))
              '((p2) ∨ (¬ p2) ∨ (p1) ∨ (¬ p1)))

;; nabla :: [[φ]] -> [φ]
;; for ψ_i ∈ S, ((conj (◇ ψ_i)) ∧ ★( disj(ψ_i)))=
(define (nabla sents)
  (cond
    ((empty? sents) `(★ F))
    ((= (length sents) 2) `(,@(conj-◇ sents)))
    (else `(,@(conj-◇ sents) ∧ (★ (,@(disj sents)))))))

(check-equal? (nabla '()) '(★ F))
(check-equal? (nabla '((p))) '((◇ (p)) ∧ (★ (p))))



;; atomify :: Num -> Symbol
;; creates an atomic sentence of order n
(define (atomify n)
  (string->symbol
   (string-append "p" (number->string n))))

(check-equal? (atomify 2) 'p2)
(check-equal? (atomify 33) 'p33)

;; SD :: Num -> [φ]
;; returns the set of state descriptions of order n
(define (SD n)
  (cond
    ((zero? n) '())
    (else (let ([p (atomify n)])
            `(,p (¬ ,p) . ,(SD (sub1 n)))))))

(check-equal? (SD 0) '())
(check-equal? (SD 1) '(p1 (¬ p1)))
(check-equal? (SD 2)
              '(p2 (¬ p2) p1 (¬ p1)))
(check-equal? (SD 3)
              `(p3 (¬ p3) . ,(SD 2)))

(define one? (compose zero? sub1))

;; canon : Num -> Num -> [φ]
;; (canon (add1 h) n) = TODO
;; invariant: n >= 1
(define (canon h n)
  (cond
    ((zero? h) (SD n))
    (else
     (foldr append '()
            (map (λ (S)
                   (map (λ (σ)
                          `(,(nabla S) ∧ ,σ))
                        (SD n)))
                 (powerset (canon (sub1 h) n)))))))

;; (length (canon 2 2)) = 262144
(length (nabla (SD 1)))

(define (canon-length h n)
  (cond
    ((zero? h) (expt 2 n))
    (else
     (let [(S (expt 2 (canon-length (sub1 h) n)))
           (SDn (expt 2 n))]
       (* S SDn)))))


#|

(foldr append '()
            (map (λ (S)
                   (map (λ (σ)
                          `(,(nabla S) ∧ ,σ))
                        (SD n)))
                 (powerset (canon (sub1 h) n))))





(SD n) := 2*n











|#