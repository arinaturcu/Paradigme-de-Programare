#lang racket
(require racket/match)

(provide (all-defined-out))

(define ITEMS 5)

;; C1, C2, C3, C4 sunt case într-un supermarket.
;; C1 acceptă doar clienți care au cumparat maxim ITEMS produse (ITEMS este definit mai sus).
;; C2 - C4 nu au restricții.
;; Considerăm că procesarea fiecărui produs la casă durează un minut.
;; Casele pot suferi întarzieri (delay).
;; La un moment dat, la fiecare casă există 0 sau mai mulți clienți care stau la coadă.
;; Timpul total (tt) al unei case reprezintă timpul de procesare al celor aflați la coadă,
;; adică numărul de produse cumpărate de ei + întârzierile suferite de casa respectivă (dacă există).
;; Ex:
;; la C3 sunt Ana cu 3 produse și Geo cu 7 produse,
;; și C3 nu are întârzieri => tt pentru C3 este 10.


; Definim o structură care descrie o casă prin:
; - index (de la 1 la 4)
; - tt (timpul total descris mai sus)
; - queue (coada cu persoanele care așteaptă)
(define-struct counter (index tt queue) #:transparent)


; TODO
; Implementați o functie care intoarce o structură counter goală.
; tt este 0 si coada este vidă.
; Obs: la definirea structurii counter se creează automat o funcție make-counter pentru a construi date de acest tip
(define (empty-counter index)
  (make-counter index 0 '()))


; TODO
; Implementați o funcție care crește tt-ul unei case cu un număr dat de minute.
(define (tt+ C minutes)
  (struct-copy counter C
               [tt (+ minutes (match C [(counter index tt queue) tt]))]
               ))

; TODO
; Implementați o funcție care primește o listă de case și intoarce o pereche dintre:
; - indexul casei (din listă) care are cel mai mic tt
; - tt-ul acesteia
; Obs: când mai multe case au același tt, este preferată casa cu indexul cel mai mic
(define (min-tt-helper counters min-counter)
  (if (null? counters)
      min-counter
      (if (< (counter-tt (car counters)) (cdr min-counter))
          (min-tt-helper (cdr counters) (cons (counter-index (car counters)) (counter-tt (car counters))))
          (min-tt-helper (cdr counters) min-counter)
          )
      )
  )

(define (min-tt counters)
  (min-tt-helper (cdr counters) (cons (counter-index (car counters)) (counter-tt (car counters))))
  )

; TODO
; Implementați o funcție care adaugă o persoană la o casă.
; C = casa, name = numele persoanei, n-items = numărul de produse cumpărate
; Veți întoarce o nouă structură obținută prin așezarea perechii (name . n-items)
; la sfârșitul cozii de așteptare.
(define (add-to-counter C name n-items)
  (struct-copy counter C
               [tt (+ (counter-tt C) n-items)]
               [queue (append (counter-queue C) (list (cons name n-items)))]
               )
  )

; TODO
; Implementați funcția care simulează fluxul clienților pe la case.
; requests = listă de cereri care pot fi de 2 tipuri:
; - (<name> <n-items>) - persoana <name> trebuie așezată la coadă la o casă
; - (delay <index> <minutes>) - casa <index> este întârziată cu <minutes> minute
; C1, C2, C3, C4 = structuri corespunzătoare celor 4 case
; Sistemul trebuie să proceseze aceste cereri în ordine, astfel:
; - persoanele vor fi distribuite la casele cu tt minim (dintre casele la care au voie)
; - când o casă suferă o întârziere, tt-ul ei crește
(define (serve requests C1 C2 C3 C4)
  (define (get-counter index)
    (cond
      [(eq? index 1) C1]
      [(eq? index 2) C2]
      [(eq? index 3) C3]
      [(eq? index 4) C4]
      )
    )
  
  (define (delay index minutes)
    (tt+ (get-counter index) minutes)
    )

  (define (find-best n-items)
    (if (<= n-items ITEMS)
        (min-tt (list C1 C2 C3 C4))
        (min-tt (list C2 C3 C4))
        ))

  (define (add-to-counter-wrapper index name n-items)
    (cond
      [(eq? index 1) (serve (cdr requests) (add-to-counter C1 name n-items) C2 C3 C4)]
      [(eq? index 2) (serve (cdr requests) C1 (add-to-counter C2 name n-items) C3 C4)]
      [(eq? index 3) (serve (cdr requests) C1 C2 (add-to-counter C3 name n-items) C4)]
      [(eq? index 4) (serve (cdr requests) C1 C2 C3 (add-to-counter C4 name n-items))]
      [else index]
      )
    )

  (define (serve-helper name n-items)
    (add-to-counter-wrapper (car (find-best n-items)) name n-items))

  ; puteți să vă definiți aici funcții ajutătoare (define în define)
  ; - avantaj: aveți acces la variabilele requests, C1, C2, C3, C4 fără a le retrimite ca parametri
  ; puteți de asemenea să vă definiți funcțiile ajutătoare în exteriorul lui "serve"
  ; - avantaj: puteți să vă testați fiecare funcție imediat ce ați implementat-o

  (if (null? requests)
      (list C1 C2 C3 C4)
      (match (car requests)
        [(list 'delay index minutes)
         (cond
           [(eq? index 1) (serve (cdr requests) (delay 1 minutes) C2 C3 C4)]
           [(eq? index 2) (serve (cdr requests) C1 (delay 2 minutes) C3 C4)]
           [(eq? index 3) (serve (cdr requests) C1 C2 (delay 3 minutes) C4)]
           [(eq? index 4) (serve (cdr requests) C1 C2 C3 (delay 4 minutes))]
           )]
        [(list name n-items)
         (serve-helper name n-items)
         ])))
