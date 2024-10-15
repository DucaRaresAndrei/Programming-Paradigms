#lang racket
(require "suffix-tree-stream.rkt")
(require "collection.rkt")

(provide (all-defined-out))

;; Vom prelua toate funcțiile din etapele 1-3 (exceptând
;; longest-common-substring, care nu beneficiază de 
;; reprezentarea ca flux întrucât parcurge tot arborele)
;; și le vom adapta la noua reprezentare a unui ST.
;;
;; Pentru că un ST este construit pornind de la o colecție
;; de sufixe și pentru că ne dorim să nu calculăm toate
;; sufixele decât dacă este nevoie, vom modifica toate
;; funcțiile care prelucrau liste de sufixe pentru a
;; prelucra fluxuri de sufixe.
;;
;; Obs: fără această modificare a listelor de sufixe în
;; fluxuri de sufixe, și presupunând că am manipulat
;; arborii de sufixe doar prin interfața definită în
;; fișierul suffix-tree (respectând astfel bariera de 
;; abstractizare), ar trebui să alterăm doar funcția 
;; suffixes->st care este practic un constructor pentru
;; tipul ST.
;; Din cauza transformării listelor de sufixe în fluxuri,
;; avem mult mai multe implementări de modificat.
;; Puteam evita acest lucru? Da, utilizând conceptul de
;; colecție de sufixe de la început (în loc să presupunem
;; că ele vor fi prelucrate ca liste). În loc de cons,
;; car, cdr, map, filter, etc. am fi folosit de fiecare
;; dată collection-cons, collection-first, ... etc. -
;; aceste funcții fiind definite într-o bibliotecă
;; inițială ca fiind echivalentele lor pe liste, și
;; redefinite ulterior în stream-cons, stream-first,
;; ... etc. Operatorii pe colecții de sufixe ar fi 
;; folosit, desigur, doar funcții de tip collection-.
;;
;; Am ales să nu procedăm astfel pentru că ar fi provocat
;; confuzie la momentul respectiv (când chiar operatorii
;; pe liste erau o noutate) și pentru a vă da ocazia să
;; faceți singuri acest "re-design".


; TODO
; Copiați din etapele anterioare implementările funcțiilor
; de mai jos și modificați-le astfel:
; - Toate funcțiile care lucrează cu liste de sufixe vor
;   lucra cu un nou tip de date Collection, ai cărui
;   constructori și operatori vor fi definiți de voi
;   în fișierul collection.rkt.
; - Pentru toate funcțiile, trebuie să vă asigurați că
;   este respectată bariera de abstractizare (atât în 
;   cazul tipului ST cât și în cazul tipului Collection).
; Obs: cu cât mai multe funcții rămân nemodificate, cu atât
; este mai bine (înseamnă că design-ul inițial a fost bun).


(define (longest-common-prefix w1 w2)
  (define (prefix-queue prefix rest-w1 rest-w2)
    (cond
      ((or (null? rest-w1) (null? rest-w2)) (list (reverse prefix) rest-w1 rest-w2))
      ((equal? (car rest-w1) (car rest-w2)) (prefix-queue (cons (car rest-w1) prefix) (cdr rest-w1) (cdr rest-w2)))
      (else (list (reverse prefix) rest-w1 rest-w2))))
  (prefix-queue '() w1 w2))


; am schimbat, în numele funcției, cuvântul list în
; cuvântul collection
(define (longest-common-prefix-of-collection words)
  (define (prefix-collection prefix words)
    (cond
      ((collection-empty? words) prefix)
      ((= (length prefix) 1) prefix)
      (else (prefix-collection (car (longest-common-prefix prefix (collection-car words))) (collection-cdr words)))))
  (prefix-collection (collection-car words) (collection-cdr words)))


(define (match-pattern-with-label st pattern)
  (let ((result (get-ch-branch st (car pattern))))
    (if result
      (let ((longest-prefix (longest-common-prefix pattern (car result))))
        (cond
          ((null? (cadr longest-prefix)) #t)
          ((and (null? (caddr longest-prefix)) (not (null? (first-branch (cdr result)))))
            (list (car result) (cadr longest-prefix) (cadr result)))
          (else (list #f (car longest-prefix)))))
      (list #f '()))))



(define (st-has-pattern? st pattern)
  (let ((result (match-pattern-with-label st pattern)))
    (cond
      ((eq? #t result) #t)
      ((equal? #f (car result)) #f)
      (else (st-has-pattern? (caddr result) (cadr result))))))


(define (get-suffixes text)
  (if (null? text)
    '()
    (collection-cons text (get-suffixes (cdr text)))))


(define (get-ch-words words ch)
  (if (collection-empty? words)
    '()
    (collection-filter (lambda (word) (and (not (null? word)) (equal? (car word) ch))) words)))


(define (ast-func suffixes)
  (if (collection-empty? suffixes)
    '()
    (cons (cons (car (collection-car suffixes)) '()) (collection-map (lambda (word) (drop word 1)) suffixes))))


(define (cst-func suffixes)
  (if (collection-empty? suffixes)
    '()
    (let ((label (longest-common-prefix-of-collection suffixes)))
      (cons label (collection-map (lambda (word)
        (if (> (length word) (length label))
          (drop word (length label))
          '()
          )) suffixes)))))


; considerați că și parametrul alphabet este un flux
; (desigur, și suffixes este un flux, fiind o colecție
; de sufixe)
(define (suffixes->st labeling-func suffixes alphabet)
  (if (stream-empty? suffixes)
    empty-stream
    (let* ((real-alphabet (collection-filter (lambda (character) (not (stream-empty? (get-ch-words suffixes character)))) alphabet))
           (sufixes-per-ch (collection-map (lambda (ch) (get-ch-words suffixes ch)) real-alphabet)))
      (collection-map (lambda (sufixes) 
        (let ((label-subtree (labeling-func sufixes)))
          (cons (car label-subtree) (suffixes->st labeling-func (cdr label-subtree) alphabet)))
      ) sufixes-per-ch))))



; nu uitați să convertiți alfabetul într-un flux
(define text->st
  (lambda (text)
    (lambda (function)
      (let* ((text-with-$ (append text '(#\$)))
            (suffixes (get-suffixes text-with-$))
            (alphabet (sort (remove-duplicates text-with-$) char<?))
            (alphabet-as-stream (collection-list->stream alphabet)))
        (suffixes->st function suffixes alphabet-as-stream)))))


(define (text->ast text)
  ((text->st text) ast-func))



(define (text->cst text)
  ((text->st text) cst-func))


; dacă ați respectat bariera de abstractizare,
; această funcție va rămâne nemodificată.
(define (substring? text pattern)
  'your-code-here)



; dacă ați respectat bariera de abstractizare,
; această funcție va rămâne nemodificată.
(define (repeated-substring-of-given-length text len)
  'your-code-here)
