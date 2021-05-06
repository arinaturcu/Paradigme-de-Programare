#lang racket
(require racket/match)
(require "queue.rkt")

(provide (all-defined-out))

(define ITEMS 5)

(define-struct counter (index tt et queue) #:transparent)

; TODO
; Aveți libertatea să vă structurați programul cum doriți (dar cu restricțiile
; de mai jos), astfel încât funcția serve să funcționeze conform specificației.
; 
; Restricții (impuse de checker):
; - trebuie să existe în continuare funcția (empty-counter index)
; - cozile de la case trebuie implementate folosind noul TDA queue

(define (empty-counter index)
  (make-counter index 0 0 empty-queue))
  
(define (update f counters index) (map (λ (C) (if (= (counter-index C) index) (f C) C)) counters))

(define (tt+ minutes) (λ (C) (struct-copy counter C [tt (+ minutes (counter-tt C))])))

(define (et+ minutes) (λ (C) (struct-copy counter C [et (+ minutes (counter-et C))])))

(define (add-to-counter name items)     ; testată de checker
  (λ (C)                                ; nu modificați felul în care funcția își primește argumentele
        (if (queue-empty? (counter-queue C))
        (struct-copy counter C
                 [queue (enqueue (cons name items) (counter-queue C))]
                 [et (+ items (counter-et C))]
                 [tt (+ items (counter-tt C))])
        (struct-copy counter C
                 [queue (enqueue (cons name items) (counter-queue C))]
                 [tt (+ items (counter-tt C))])
        )))

(define (min-t field-getter counters)
  (if (null? counters)
      (cons 0 0)
      (foldl (λ(C best)
         (if (< (field-getter C) (cdr best))
             (cons (counter-index C) (field-getter C))
             best))
       (cons (counter-index (car counters)) (field-getter (car counters)))
       (cdr counters))))

(define (min-tt counters) (min-t counter-tt counters)) ; folosind funcția de mai sus
(define (min-et counters) (min-t counter-et (filter
                                             (λ(C) (not(null? (counter-queue C))))
                                             counters))) ; folosind funcția de mai sus

(define (remove-first-from-counter C)   ; testată de checker
  (struct-copy counter C
               [tt (- (counter-tt C) (counter-et C))]
               [et (if (= 1 (+ (queue-size-r (counter-queue C)) (queue-size-l (counter-queue C)))) 0 (cdr (top (dequeue (counter-queue C)))))]
               [queue (dequeue (counter-queue C))]))


; TODO
; Implementați o funcție care calculează starea unei case după un număr dat de minute.
; Funcția presupune, fără să verifice, că în acest timp nu a ieșit nimeni din coadă, 
; deci va avea efect doar asupra câmpurilor tt și et.
; (cu alte cuvinte, este responsabilitatea utilizatorului să nu apeleze această funcție
; cu minutes > timpul până la ieșirea primului client din coadă)
; Atenție: casele fără clienți nu trebuie să ajungă la timpi negativi!
(define (pass-time-through-counter minutes)
  (λ (C)
    (struct-copy counter C
                 [tt (max 0 (- (counter-tt C) minutes))]
                 [et (max 0 (- (counter-et C) minutes))])))


 (define (get-ttmed counters)
  (/ (apply + (map counter-tt counters)) (foldl (λ(C number) (+ number 1)) 0 counters)))

; TODO
; Implementați funcția care simulează fluxul clienților pe la case.
; ATENȚIE: Față de etapa 3, apare un nou tip de cerere, așadar
; requests conține 5 tipuri de cereri (cele moștenite din etapa 3 plus una nouă):
;   - (<name> <n-items>) - persoana <name> trebuie așezată la coadă la o casă              (ca înainte)
;   - (delay <index> <minutes>) - casa <index> este întârziată cu <minutes> minute         (ca înainte)
;   - (ensure <average>) - cât timp tt-ul mediu al caselor este mai mare decât
;                          <average>, se adaugă case fără restricții (case slow)           (ca înainte)
;   - <x> - trec <x> minute de la ultima cerere, iar starea caselor se actualizează
;           corespunzător (cu efect asupra câmpurilor tt, et, queue)                       (ca înainte)
;   - (close <index>) - casa index este închisă                                            (   NOU!   )
; Sistemul trebuie să proceseze cele 5 tipuri de cereri în ordine, astfel:
; - persoanele vor fi distribuite la casele DESCHISE cu tt minim; nu se va întâmpla
;   niciodată ca o persoană să nu poată fi distribuită la nicio casă                       (mică modificare)
; - când o casă suferă o întârziere, tt-ul și et-ul ei cresc (chiar dacă nu are clienți);
;   nu aplicați vreun tratament special caselor închise                                    (ca înainte)
; - tt-ul mediu (ttmed) se calculează pentru toate casele DESCHISE, 
;   iar la nevoie veți adăuga case slow una câte una, până când ttmed <= <average>         (mică modificare)
; - când timpul prin sistem avansează cu <x> minute, tt-ul, et-ul și queue-ul tuturor 
;   caselor se actualizează pentru a reflecta trecerea timpului; dacă unul sau mai mulți 
;   clienți termină de stat la coadă, ieșirile lor sunt contorizate în ordine cronologică. (ca înainte)
; - când o casă se închide, ea nu mai primește clienți noi; clienții care erau deja acolo
;   avansează normal, până la ieșirea din supermarket                                    
; Rezultatul funcției serve va fi o pereche cu punct între:
; - lista sortată cronologic a clienților care au părăsit supermarketul:
;   - fiecare element din listă va avea forma (index_casă . nume)
;   - dacă mai mulți clienți ies simultan, sortați-i crescător după indexul casei
; - lista cozilor (de la case DESCHISE sau ÎNCHISE) care încă au clienți:
;   - fiecare element va avea forma (index_casă . coadă) (coada este de tip queue)
;   - lista este sortată după indexul casei
(define (serve-helper requests fast-counters slow-counters closed-counters clients)
(define (handle-delay index minutes)
  (if (< index (counter-index (car slow-counters)))
      (serve-helper (cdr requests) (update (tt+ minutes) (update (et+ minutes) fast-counters index) index) slow-counters closed-counters clients)
      (serve-helper (cdr requests) fast-counters (update (tt+ minutes) (update (et+ minutes) slow-counters index) index) closed-counters clients)
      )
  )

  (define (handle-add name n-items)
    (if (and (<= n-items ITEMS) (< (car (min-tt (append fast-counters slow-counters))) (counter-index (car slow-counters))))
        (serve-helper (cdr requests)
               (update (add-to-counter name n-items) fast-counters (car (min-tt fast-counters)))
               slow-counters closed-counters clients)
        (serve-helper (cdr requests)
               fast-counters
               (update (add-to-counter name n-items) slow-counters (car (min-tt slow-counters))) closed-counters clients)
        )
    )

  (define (get-counter-by-index index counters)
    (if (null? counters)
        (cons 0 0)
        (if (= (counter-index (car counters)) index)
            (car counters)
            (get-counter-by-index index (cdr counters)))))
  
  (define (handle-close index)
    (serve-helper (cdr requests)
                  (filter (λ(C) (not (equal? (counter-index C) index))) fast-counters)
                  (filter (λ(C) (not (equal? (counter-index C) index))) slow-counters)
                  (let ([to-add (get-counter-by-index index (append fast-counters slow-counters))])
                    (if (not (equal? (cons 0 0) to-add))
                      (cons to-add closed-counters)
                      closed-counters))
                  clients))
  
  (define (handle-ensure avg slow-counters)
    (if (>= avg (get-ttmed (append fast-counters slow-counters)))
        (serve-helper (cdr requests) fast-counters slow-counters closed-counters clients)
        (handle-ensure avg (append slow-counters (list (empty-counter (add1 (length (append fast-counters slow-counters closed-counters))))))))
    )

  (define (pass minutes)
    (λ (C)
      (if (< minutes (counter-et C))
          ((pass-time-through-counter minutes) C)
          (let go ([remained-minutes minutes] [C C])
            (if (or (< remained-minutes (counter-et C)) (queue-empty? (counter-queue C)))
                ((pass-time-through-counter remained-minutes) C)
                (go (- remained-minutes (counter-et C)) (remove-first-from-counter C)))))))

  (define (cmp-function c1 c2)
    (if (not (equal? (cdr c1) (cdr c2)))
        (< (cdr c1) (cdr c2))
        (< (caar c1)  (caar c2))))

  ; Adaug la fiecare client scos timpul la care a plecat si apoi sortez in functie de asta
  (define (clients-to-go minutes)
    (λ (C clients)
      (if (< minutes (counter-et C))
          (sort clients cmp-function)
          (let go ([remained-minutes minutes] [C C] [clients clients])
            (if (or (< remained-minutes (counter-et C)) (queue-empty? (counter-queue C)))
                (sort clients cmp-function)
                (go (- remained-minutes (counter-et C))
                    (remove-first-from-counter C)
                    (append clients (list (cons (cons (counter-index C) (car (top (counter-queue C)))) (- minutes (- remained-minutes (counter-et C))))))))))))
  
  (define (handle-pass-time minutes)
    (serve-helper (cdr requests)
                  (map (pass minutes) fast-counters)
                  (map (pass minutes) slow-counters)
                  (map (pass minutes) closed-counters)
                  (append clients (map car (foldl (clients-to-go minutes) '() (append fast-counters slow-counters closed-counters))))))
  
  (if (null? requests)
      (cons clients (map (λ(C) (cons (counter-index C) (counter-queue C)))
                               (filter (λ(C) (not (queue-empty? (counter-queue C)))) (sort (append fast-counters slow-counters closed-counters) #:key counter-index <))))
      (match (car requests)
        [(list 'ensure avg)          (handle-ensure avg slow-counters)]
        [(list 'close index)         (handle-close index)]
        [(list name n-items)         (handle-add name n-items)]
        [(list 'delay index minutes) (handle-delay index minutes)]
        [minutes                     (handle-pass-time minutes)]
        )))

(define (serve requests fast-counters slow-counters)
  (serve-helper requests fast-counters slow-counters '() '()))
