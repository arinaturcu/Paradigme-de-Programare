:- ensure_loaded('checker.pl').

% test_mode(detailed).

% Considerăm următoarele reprezentări:
%
% O integramă este reprezentată prin structura (compusul)
% integ(H, W, Lista, Vocab), unde:
% H este înălțimea integramei
% W este lățimea integramei
% Lista este o listă de tupluri (Poz, Valoare), unde
%   Poz este un tuplu (R, C) conținând rândul și coloana (0-based)
%   Valoare este una dintre:
%     x - dacă celula este neagră (nu poate fi completată cu litere)
%     o literă, dacă celula este completată cu o literă
%     o listă de întrebări, reprezentate ca tupluri (Text, Dir, ID), cu
%       Text - un literal, textul întrebării
%       Dir - una dintre valorile j sau d, indicând direcția întrebării
%       ID - un identificator numeric al întrebării
% Vocab este o listă de literali reprezentând cuvinte disponibile
% pentru a rezolva întrebarea.
%
% În ieșirea predicatului intrebări, o întrebare este reprezentată ca
% ((R, C), Text, Dir, ID), unde
% R este rândul căsuței cu întrebarea (0-based)
% C este coloana căsuței cu întrebarea (0-based)
% Text este textul întrebării (un literal)
% Dir este j sau d, reprezentând direcția în care trebuie să fie plasat
% răspunsul (jos sau dreapta)
% ID este un identificator numeric al întrebării.

% Puteți vizualiza integramele cu:
% integrama(0, W), print_integrama(W).
% integrama(1, W), print_integrama(W).
% integrama(2, W), print_integrama(W).
% integrama(3, W), print_integrama(W).
%
% Testați cu
% vmtest.
% Testați teste individuale (vedeți predicatul tt din checker.pl) cu
% vmtest(Test).
% de exemplu cu vmtest(intrebari).


% intrebari/2
% intrebari(integ(+H, +W, +Lista, +Vocab), -Lista_intrebari)
% Este adevărat atunci când Lista_intrebari este o lista de tupluri
% ((R, C), Text, Dir, ID), fiecare tuplu corespunzând unei întrebări din
% integramă (rândul, coloana, textul întrebării, direcția (j/d),
% identificatorul).
%
% BONUS: intrebari are o singură soluție (o singură listă) pentru o
% anumită integramă.
intrebariHelper(_, _) :- false.
intrebariHelper([], []) :- true.
intrebariHelper([ ((R, C), [(Text, Dir, ID) | T1]) | T3 ], [((R, C), Text, Dir, ID) | T2]) :- intrebariHelper([((R, C), T1) | T3], T2).
intrebariHelper([ (_, _) | T3], L) :- intrebariHelper(T3, L).

intrebari(_, _) :- false.
intrebari(integ(_, _, List, _), L) :- intrebariHelper(List, L), !.

% id_intrebare/2
% id_intrebare(+Integ, ?Intrebare, ?Q_ID)
% Este adevărat dacă în integrama reprezentată ca integ(...), Intrebare
% este un text iar Q_ID este un identificator care corespund aceleași
% întrebări.
id_intrebare(_, _, _) :- false.
id_intrebare(integ(H, W, List, Vocab), Q_Text, Q_ID) :- intrebari(integ(H, W, List, Vocab), L), member((_, Q_Text, _, Q_ID), L), !.

% completare/3
% completare(+Integ, +Sol, -Integrama)
% Predicatul produce Integrama, o structură de forma integ(...),
% pornind de la Integ, în care au fost completate celule conform cu
% soluția Sol.
% Soluția este reprezentată ca o listă de perechi (Întrebare, Răspuns),
% unde Întrebarea este textul unei întrebări, iar Răspuns este un cuvânt
% de completat; ambele sunt atomi (literali).
% De exemplu, o soluție parțială pentru integrama 0 poate fi:
% [('Din care plouă', 'NOR'), ('Al doilea număr', 'DOI')]
%
% BONUS: lungime_spatiu are o singură soluție pentru o anumită
% întrebare.
% Puteți testa manual predicatul cu o interogare de forma:
% integrama(0, W), solutie(0, Sol), completare(W, Sol, W2),
%   print_integrama(W2).
getIntrebare(integ(H, W, List, Vocab), Text, ((R, C), Text, Dir, ID)) :- intrebari(integ(H, W, List, Vocab), L), member(((R, C), Text, Dir, ID), L).

transformInIntegramaCel([], []).
transformInIntegramaCel([(_, _, []) | Tail], L ) :- transformInIntegramaCel(Tail, L).
transformInIntegramaCel([((R, C), d, [FirstChar | Rest]) | T1], [((R, NewC), FirstChar) | T2]) :- NewC is C + 1, transformInIntegramaCel([((R, NewC), d, Rest) | T1], T2), !.
transformInIntegramaCel([((R, C), j, [FirstChar | Rest]) | T1], [((NewR, C), FirstChar) | T2]) :- NewR is R + 1, transformInIntegramaCel([((NewR, C), j, Rest) | T1], T2), !.

detailedSol(_, [], []).
detailedSol(Integ, [(Text, Ans) | T1], [((R, C), Dir, AnsChars) | T2]) :- atom_chars(Ans, AnsChars), getIntrebare(Integ, Text, ((R, C), Text, Dir, _)), detailedSol(Integ, T1, T2), !.

completare(_, _, _) :- false.
completare(integ(H, W, List, Vocab), Sol, integ(H, W, ComplList, Vocab)) :- 
    detailedSol(integ(H, W, List, Vocab), Sol, DetailedSol),
    transformInIntegramaCel(DetailedSol, CellsToAdd),
    append(List, CellsToAdd, TmpList),
    sort(TmpList, ComplList), !.

% lungime_spatiu/3
% lungime_spatiu(integ(+H, +W, +Lista, +Vocab), +Intrebare, -Lungime)
% pentru Bonus:
% lungime_spatiu(integ(+H, +W, +Lista, +Vocab), ?Intrebare, ?Lungime)
%
% Returnează lungimea spațiului asociat întrebării date.
% Întrebarea este indicată prin textul ei. De exemplu:
% lungime_spatiu pentru integrama 0 și întrebarea 'Al doilea număr'
% trebuie să lege Lungime la 3.
%
% BONUS: lungime_spatiu are o singură soluție pentru o anumită
% întrebare.
% Puteți testa manual predicatul cu o interogare de forma:
% integrama(0, W), id_intrebare(W, Text, 3), lungime_spatiu(W, Text, X).
% isValueAt(List, (R, C), Val) :- member(((R, C), Val), List), !.

countLength(_, W, List, ((R, C), _, d, _), 0) :- NewC is C + 1, ( member(((R, NewC), _), List) ; NewC =:= W).
countLength(H, _, List, ((R, C), _, j, _), 0) :- NewR is R + 1, ( member(((NewR, C), _), List) ; NewR =:= H).

countLength(H, W, List, ((R, C), _, d, _), Lungime) :- 
    NewC is C + 1, 
    \+member(((R, NewC), _), List),
    countLength(H, W, List, ((R, NewC), _, d, _), NewLungime),
    Lungime is NewLungime + 1.

countLength(H, W, List, ((R, C), _, j, _), Lungime) :- 
    NewR is R + 1,
    \+member(((NewR, C), _), List),
    countLength(H, W, List, ((NewR, C), _, j, _), NewLungime),
    Lungime is NewLungime + 1.

lungime_spatiu(_, _, _) :- false.
lungime_spatiu(integ(H, W, List, Vocab), Text, Lungime) :- 
    getIntrebare(integ(H, W, List, Vocab), Text, Intrebare), 
    countLength(H, W, List, Intrebare, Lungime).

% intersectie/5
% intersectie(integ(+H, +W, +Lista, +Voc), +I1, -Poz1, +I2, -Poz2)
% pentru Bonus:
% intersectie(integ(+H, +W, +Lista, +Voc), ?I1, ?Poz1, ?I2, ?Poz2)
%
% Pentru o integramă și două întrebări date prin textul lor (I1 și I2),
% al căror răspunsuri se intersectează, întoarce în Poz1 indicele din
% răspunsul la I1 la care este intersecția, și în Poz2 indicele din
% răspunsul la I2 la care este intersecția. Indecșii incep de la 0.
%
% De exemplu, în integrama 0:
%  █       █       2↓      3↓      █
%  █       0↓,1→   -       -       █
%  4→      -       -       -       █
%  5→      -       -       -       █
%  █       █       █       █       █
%
%  Întrebările 'Primii 3 din artă' și 'Afirmativ' (3, respectiv 1) se
%  intersectează la pozițiile 0, respectiv 1 (va fi litera A, de la
%  ART, respectiv DA).

seIntersecteaza(((R1, C1), d), ((R2, C2), j), L1, L2) :- 
    R1 > R2, C1 < C2,
    C1 + L1 >= C2,
    R2 + L2 >= R1.

seIntersecteaza(((R1, C1), j), ((R2, C2), d), L1, L2) :-
    R2 > R1, C2 < C1,
    C2 + L2 >= C1,
    R1 + L1 >= R2.

intersectie(_, _, _, _, _) :- false.
intersectie(I, Intreb1, Poz1, Intreb2, Poz2) :- 
    getIntrebare(I, Intreb1, ((R1, C1), _, Dir1, _)),
    getIntrebare(I, Intreb2, ((R2, C2), _, Dir2, _)),
    lungime_spatiu(I, Intreb1, L1), 
    lungime_spatiu(I, Intreb2, L2),
    seIntersecteaza(((R1, C1), Dir1), ((R2, C2), Dir2), L1, L2),
    (Dir1 = d -> 
        (Rint is R1, Cint is C2, Poz1 is Cint - C1 - 1, Poz2 is Rint - R2 - 1); 
        (Rint is R2, Cint is C1, Poz1 is Rint - R1 - 1, Poz2 is Cint - C2 - 1)).

% solutii_posibile/2
% solutii_posibile(integ(+H, +W, +Lista, +Vocabular), -Solutii)
% Formează o listă Solutii, conținând perechi de forma
% (Întrebare, Cuvinte), unde
% Întrebare este textul unei întrebări din integramă, iar Cuvinte este o
% listă de cuvinte sunt din Vocabular și au lungimea corectă pentru a fi
% răspuns la întrebare. Solutii conține câte o pereche pentru fiecare
% întrebare din integramă.
% Cuvintele sunt reprezentate ca liste de atomi, fiecare atom
% având lungime 1 (o singură literă).
% De exemplu, pentru integrama 0, Solutii conține 6 perechi, două dintre
% ele fiind:
% ('Afirmativ', [['D', 'A'], ['N', 'U']])
% ('Din care plouă',
% [['N','O','R'],['A','R','T'],['U','I','T'],['D','O','I']])
solutii_posibile(_, _) :- false.

% rezolvare/2
% rezolvare(+Integ, -Solutie)
% Rezolvare produce în Solutie soluția integramei Integ. Soluția este
% reprezentată ca o listă de perechi de literali, fiecare pereche
% conținând textul unei întrebări și cuvântul (ca literal) care este
% răspunsul la întrebare.
%
% BONUS: rezolvare nu oferă soluții duplicate - numărul de soluții ale 
% predicatului este chiar numărul de completări posibile ale integramei.
rezolvare(_, _) :- false.
