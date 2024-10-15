#lang racket
(require "suffix-tree.rkt")
(require "etapa1.rkt")
(require "etapa2.rkt")

(provide (all-defined-out))

;; Această etapă este dedicată aplicațiilor 
;; arborelui de sufixe:
;; - găsirea unui șablon într-un text
;; - cel mai lung subșir comun a două texte
;; - găsirea unui subșir de lungime dată care se
;;   repetă în text
;; Conform convenției din etapele anterioare, un text
;; este întotdeauna reprezentat ca listă de caractere.
;; Rezultatele funcțiilor de mai jos sunt de asemenea
;; reprezentate ca liste de caractere.


; TODO 1
; Implementați funcția substring? care primește un text și
; un șablon nevid și întoarce true dacă șablonul apare în 
; text, respectiv false în caz contrar.
; Observație: ați implementat deja logica principală a
; acestei căutări în etapa 1, în funcția st-has-pattern?,
; care este un operator al tipului ST. Acum aveți toate
; uneltele necesare implementării operatorului corespunzător
; pentru tipul text (pentru că în etapa 2 ați implementat
; construcția arborelui de sufixe asociat unui text).
(define (substring? text pattern)
  (let* ((st (text->cst text))
        (result (st-has-pattern? st pattern)))
    result))


; TODO 2
; Implementați funcția longest-common-substring care primește
; două texte și determină cel mai lung subșir comun al
; acestora, folosind algoritmul următor:
; 1. Construiește arborele de sufixe ST1 pentru primul text.
; 2. Pentru fiecare sufix din al doilea text (de la cel mai
;    lung la cel mai scurt), găsește cea mai lungă potrivire 
;    cu sufixele din primul text, urmând căile relevante în ST1.
; 3. Rezultatul final este cel mai lung rezultat identificat
;    la pasul 2 (în caz de egalitate a lungimii, păstrăm
;    primul șir găsit).
; Folosiți named let pentru a parcurge sufixele.
; Observație: pentru sufixele din al doilea text nu dorim 
; marcajul de final $ pentru a nu crește artificial lungimea 
; șirului comun cu acest caracter.
; Hint: Revizitați funcția match-pattern-with-label (etapa 1).
(define (longest-common-substring text1 text2)
  (let ((ST1 (text->cst text1))
        (S (get-suffixes text2)))
    (let find-longest ((suffixes S) (longest '()))
      (if (null? suffixes)
        longest
        (let* ((current-suffix (car suffixes))
               (result-finding (match-pattern-with-label ST1 current-suffix))
               (longets-length (length longest))
               (cdr-suffixes (cdr suffixes)))
          (cond
            ((eq? #t result-finding) (if (> (length current-suffix) longets-length)
                                        (find-longest cdr-suffixes current-suffix)
                                        (find-longest cdr-suffixes longest)))
            ((equal? #f (car result-finding)) (if (> (length (cdr result-finding)) longets-length)
                                                (find-longest cdr-suffixes (cadr result-finding))
                                                (find-longest cdr-suffixes longest)))
            (else (let ((substring (append (car result-finding) (let find-substring ((st (caddr result-finding)) (pattern (cadr result-finding)))
                    (let* ((result-substring (match-pattern-with-label st pattern)))
                      (cond
                        ((eq? #t result-substring) pattern)
                        ((equal? #f (car result-substring)) (cadr result-substring))
                        (else (append (car result-substring) (find-substring (caddr result-substring) (cadr result-substring))))))))))
                    (if (> (length substring) longets-length)
                      (find-longest cdr-suffixes substring)
                      (find-longest cdr-suffixes longest))
                    ))
            ))))))


; TODO 3
; Implementați funcția repeated-substring-of-given-length
; care primește un text și un număr natural len și
; parcurge arborele de sufixe al textului până găsește un
; subșir de lungime len care se repetă în text.
; Dacă acest subșir nu există, funcția întoarce false.
; Obs: din felul în care este construit arborele de sufixe
; (pe baza alfabetului sortat), rezultatul va fi primul 
; asemenea subșir din punct de vedere alfabetic.
; Ideea este următoarea: orice cale în arborele de sufixe
; compact care se termină cu un nod intern (un nod care 
; are copii, nu este o frunză) reprezintă un subșir care
; se repetă, pentru că orice asemenea cale reprezintă un
; prefix comun pentru două sau mai multe sufixe ale textului.
; Folosiți interfața definită în fișierul suffix-tree
; atunci când manipulați arborele.
(define (repeated-substring-of-given-length text len)
  (let ((cst (text->cst text)))
    (letrec 
      [(find-in-branches
        (lambda (st)
          (if (st-empty? st)
            #f
            (let ((branch (list (first-branch st))))
              (or (find-in-subtree branch '())
                  (find-in-branches (other-branches st)))))))
                  
      (find-in-subtree
        (lambda (tree prefixx)
          (if (st-empty? tree)
            #f
            (let* ((branch (first-branch tree))
                   (label-with-$ (get-branch-label branch))
                   (label (if (equal? '(#\$) (drop label-with-$ (- (length label-with-$) 1))) (take label-with-$ (- (length label-with-$) 1)) label-with-$))
                   (subtree (get-branch-subtree branch))
                   (original-prefix prefixx)
                   (prefix (if (equal? '(#\$) label) prefixx (append prefixx label))))
              (if (and (not (null? label)) (>= (length prefix) len))
                (let search-substring ((prefix prefix))
                  (if (>= (length prefix) len)
                    (let* ((substring (take prefix len))
                           (rest-of-prefix (drop prefix len))
                           (subtree-concatenate (if (null? rest-of-prefix) subtree (cons (list rest-of-prefix) subtree))))
                      (let through-subtree ((thr-subtree subtree-concatenate))
                        (if (st-has-pattern? thr-subtree substring)
                          substring
                          (if (st-empty? thr-subtree)
                            (search-substring (cdr prefix))
                            (through-subtree (if (st-empty? (caar thr-subtree)) (cdr thr-subtree) (cons (list (cdaar thr-subtree)) (cdr thr-subtree))))))))
                    (or (find-in-subtree subtree prefix)
                        (find-in-subtree (other-branches tree) original-prefix))))
                (or (find-in-subtree subtree prefix)
                    (find-in-subtree (other-branches tree) original-prefix)))))))]
      (find-in-branches cst))))