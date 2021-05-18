{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE EmptyDataDecls, MultiParamTypeClasses,
             TypeSynonymInstances, FlexibleInstances,
             InstanceSigs #-}

module Basics where
{-
    Expune funcțiile necesare reprezentării jocului.
-}

import ProblemState
import Data.List

{-
    Sinonim tip de date pentru reprezetarea unei perechi (Int, Int)
    care va reține coordonatele celulelor de pe tabla de joc.
    Colțul stânga-sus este (0, 0).
-}
type Position = (Int, Int)

{-
    Tip de date pentru reprezentarea Target-urilor.
    Acestea conțin informații atât despre poziția curentă a
    Target-ului cât și despre comportamentul acestuia.
    Tipul Behavior este definit mai jos.
-}
data Target = Target {
    position :: Position,
    behavior :: Behavior
}

instance Eq Target where
    Target p1 _ == Target p2 _ = p1 == p2

instance Ord Target where
    Target p1 _ <= Target p2 _ = p1 <= p2

{-
    Tip de date pentru reprezentarea comportamentului unui Target.
    Tipul Behavior este utilizat pentru a modela tranziția Target-urilor
    din starea curentă în starea următoare. Primul parametru este poziția
    actuală a target-ului, iar al doilea, starea curentă a jocului.
    Tipul Game este definit mai jos.
    
    Observați că, din moment ce un Behavior produce un Target nou,
    acesta din urmă ar putea fi caracterizat de un alt Behavior
    decât cel anterior.
-}
type Behavior = Position -> Game -> Target

{-
    Direcțiile de deplasare pe tablă
-}
data Direction = North | South | West | East
    deriving (Eq, Show)

{-
    *** DONE ***
    
    Tip de date pentru reprezentarea stării jocului, la un anumit
    moment. Completați-l cu orice informație aveți nevoie pentru
    stocarea stării jocului (hunter, target, obstacole, gateways).
-}

data Game = Game {
    hunter           :: Position,
    targets          :: [Target],
    obstacles        :: [Position],
    gateways         :: [(Position, Position)],
    terLines         :: Int,
    terColumns       :: Int
} deriving (Eq, Ord)

{-
    *** Optional *** 
  
    Dacă aveți nevoie de o funcționalitate particulară,
    instantiați explicit clasele Eq și Ord pentru Game.
    În cazul acesta, eliminați deriving (Eq, Ord) din Game.
-}

{-
    *** DONE ***

    Reprezentați starea jocului ca șir de caractere, pentru afișarea
    la consolă.
    
    Atenție! Fiecare linie, mai puțin ultima, este urmată de \n.
    Celule goale vor fi reprezentate ca ' '.
    Hunter-ul va fi reprezentat ca '!'.
    Target-urile vor fi reprezentate ca '*'
    Gateways-urile vor fi reprezentate ca '#'.
    Obstacolele vor fi reprezentate de '@'.

    Hint: S-ar putea să vă fie utile list comprehensions,
    precum și funcțiile elem, any și intercalate din Data.List.
-}
gameAsString :: Game -> String
gameAsString game = init $ intercalate ['\n'] board
    where
        board           = [line x | x <- [0 .. terLines game]]
        line x          = foldl f [] (elementsOnX x)
        elementsOnX x   = filter (\p -> fst p == x) [(i, j)| i <- [0 .. terLines game - 1], j <- [0 .. terColumns game - 1]]
        f resString pos = resString ++ [charPos pos]
        charPos pos
            | pos == hunter game = '!'
            | pos `elem` obstacles game = '@'
            | pos `elem` map position (targets game) = '*'
            | pos `elem` concatMap (\gate -> [fst gate, snd gate]) (gateways game) = '#'
            | otherwise  = ' '

instance Show Game where
    show = gameAsString

{-
    *** DONE ***
    
    Primește numărul de linii și numărul de coloane ale tablei de joc.
    Intoarce un obiect de tip Game în care tabla conține spații goale în interior, fiind
    împrejmuită de obstacole pe toate laturile. Implicit, colțul din stânga sus este (0,0),
    iar Hunterul se găsește pe poziția (1, 1).
-}
emptyGame :: Int -> Int -> Game
emptyGame lin col = Game hunt targs obs gates lin col
    where
        hunt  = (1, 1) -- hunter
        targs = [] -- targets
        obs   = [(0, y) | y <- [0 .. col - 1]]
            ++ [(x, y) | x <- [1 .. lin - 2], y <- [0, col - 1]]
            ++ [(lin - 1, y) | y <- [0 .. col - 1]] -- obstacles
        gates = [] -- gateways

-- functii suplimentare

{-
    Verifica daca pozitia este in interiorul jocului si
    nu are un obstacol
-}
isPositionValid :: Position -> Game -> Bool 
isPositionValid (x, y) game
    | (x >= terLines game) || (x < 0) || (y >= terColumns game) || (y < 0) = False 
    | (x, y) `elem` obstacles game = False
    | otherwise = True

{-
    Verifica daca pe pozitia data exista ceva
-}
isPositionBusy :: Position -> Game -> Bool
isPositionBusy pos game
    | pos == hunter game = True
    | pos `elem` map position (targets game) = True -- map este folosita ca sa optin o lista de pozitii ale target-urilor
    | pos `elem` concatMap (\gate -> [fst gate, snd gate]) (gateways game) = True
    | pos `elem` obstacles game = True
    | otherwise  = False -- pozitie goala
    
-- end of functii suplimentare

{-
    *** DONE ***

    Primește o poziție și un joc și întoarce un nou joc, cu Hunter-ul pus
    pe poziția specificată.
    Parametrul Position reprezintă poziția de pe hartă la care va fi adaugat Hunter-ul
    Daca poziția este invalidă (ocupată sau în afara tablei de joc) se va întoarce
    același joc.
-}
addHunter :: Position -> Game -> Game
addHunter p game
    | not (isPositionValid p game) || isPositionBusy p game = game
    | otherwise = game { hunter = p }

{-
    *** DONE ***

    Primește un comportament, o poziție și un joc și întoarce un nou joc, în care a fost
    adăugat Target-ul descris de comportament și poziție.
    Parametrul Behavior reprezintă comportamentul Hunter-ului care va fi adăugat.
    Parametrul Position reprezintă poziția de pe hartă la care va fi adăugat Target-ul.
-}
addTarget :: Behavior -> Position -> Game -> Game
addTarget beh p game
    | not (isPositionValid p game) = game
    | otherwise = game { targets = Target p beh : targets game }

{-
    *** DONE *** 

    Primește o pereche de poziții și un joc și întoarce un nou joc, în care au fost adăugate
    cele două gateway-uri interconectate.
    Parametrul (Position, Position) reprezintă pozițiile de pe hartă la care vor fi adăugate 
    cele două gateway-uri interconectate printr-un canal bidirecțional.
-}

addGateway :: (Position, Position) -> Game -> Game
addGateway g@(src, dest) game
    | not (isPositionValid src game) || not (isPositionValid dest game) = game
    | otherwise = game { gateways = g : gateways game }

{-
    *** DONE ***

    Primește o poziție și un joc și întoarce un nou joc, în care a fost adăugat un obstacol
    la poziția specificată.
    Parametrul Position reprezintă poziția de pe hartă la care va fi adăugat obstacolul.
-}
addObstacle :: Position -> Game -> Game
addObstacle p game
    | not (isPositionValid p game) = game
    | otherwise = Game (hunter game) (targets game) (p : obstacles game) (gateways game) (terLines game) (terColumns game)

{-
    *** DONE ***
    
    Primește o poziție destinație înspre care vrea să se deplaseze o entitate (Hunter sau Target)
    și verifică daca deplasarea este posibilă, întorcând noua poziție, luând în considerare
    și Gateway-urile.
    Avem următoarele cazuri:
    - dacă poziția corespunde unui spațiu gol, se întoarce acea poziție;
    - dacă poziția corespunde unui gateway, se întoarce poziția gateway-ului pereche;
    - dacă poziția corespunde unui obstacol, se întoarce Nothing.
    Parametrul Position reprezintă poziția destinație.
-}
attemptMove :: Position -> Game -> Maybe Position
attemptMove p game
    | any (\g -> (fst g == p) || (snd g == p)) (gateways game) = findPair p (gateways game) -- gateway
    | not(isPositionValid p game) = Nothing -- obstacol
    | otherwise = Just p -- spatiu gol

findPair :: Eq t => t -> [(t, t)] -> Maybe t
findPair pos list 
    | null list = Nothing 
    | pos == fst (head list) = Just $ snd (head list)
    | pos == snd (head list) = Just $ fst (head list)
    | otherwise = findPair pos (tail list)

{-
    *** DONE ***

    Comportamentul unui Target de a se deplasa cu o casuță înspre est. 
    Miscarea se poate face doar daca poziția este validă (se află pe tabla de
    joc) și nu este ocupată de un obstacol. In caz contrar, Target-ul va rămâne 
    pe loc.
    
    Conform definiției, tipul Behavior corespunde tipului funcție
    Position -> Game -> Target.
    
    Având în vedere că cele patru funcții definite în continuare (goEast, goWest,
    goNorth, goSouth) sunt foarte similare, încercați să implementați o funcție
    mai generală, pe baza căreia să le definiți apoi pe acestea patru.
-}

fromJust :: Maybe Position -> Position
fromJust (Just pos) = pos
fromJust Nothing = error "error: fromJust called on Nothing"

isJust :: Maybe a -> Bool 
isJust (Just _) = True
isJust Nothing = False

isNothing :: Maybe a -> Bool
isNothing Nothing = True
isNothing _ = False

go :: Int -> Int -> Position -> Behavior -> Game -> Target
go x y p beh game
    | isJust nextMove && any (\g -> (fst g == nextP) || (snd g == nextP)) (gateways game) = Target nextP beh
    | isJust nextMove = Target nextP beh
    | isNothing nextMove && any (\g -> (fst g == nextP) || (snd g == nextP)) (gateways game) = Target (fromJust (findPair nextP (gateways game))) beh
    | otherwise = Target p beh 
    where
        nextMove = attemptMove (fst p + x, snd p + y) game
        nextP
            | isJust nextMove = fromJust nextMove
            | otherwise = p

goEast :: Behavior
goEast p = go 0 1 p goEast

{-
    *** DONE ***

    Comportamentul unui Target de a se deplasa cu o casuță înspre vest. 
    Miscarea se poate face doar daca poziția este validă (se află pe tabla de
    joc) și nu este ocupată de un obstacol. In caz contrar, Target-ul va rămâne 
    pe loc.
-}
goWest :: Behavior
goWest p = go 0 (-1) p goWest

{-
    *** DONE ***

    Comportamentul unui Target de a se deplasa cu o casuță înspre nord. 
    Miscarea se poate face doar daca poziția este validă (se află pe tabla de
    joc) și nu este ocupată de un obstacol. In caz contrar, Target-ul va rămâne 
    pe loc.
-}
goNorth :: Behavior
goNorth p = go (-1) 0 p goNorth

{-
    *** DONE ***

    Comportamentul unui Target de a se deplasa cu o casuță înspre sud. 
    Miscarea se poate face doar daca poziția este validă (se află pe tabla de
    joc) și nu este ocupată de un obstacol. In caz contrar, Target-ul va rămâne 
    pe loc.
-}
goSouth :: Behavior
goSouth p = go 1 0 p goSouth

{-
    *** DONE ***

    Comportamentul unui Target de a-și oscila mișcarea, când înspre nord, când înspre sud. 
    Mișcarea se poate face doar dacă poziția este validă (se află pe tablă de
    joc) și nu este ocupată de un obstacol. In caz contrar, Target-ul iși va schimba
    direcția de mers astfel:
    - daca mergea inspre nord, își va modifica direcția miscării înspre sud;
    - daca mergea inspre sud, își va continua mișcarea înspre nord.
    Daca Target-ul întâlneste un Gateway pe traseul său, va trece prin acesta,
    către Gateway-ul pereche conectat și își va continua mișcarea în același sens la ieșire
    din acesta.
    Puteți folosit parametrul Int pentru a surprinde deplasamentul Target-ului (de exemplu,
    1 pentru sud, -1 pentru nord).
-}
bounce :: Int -> Behavior
bounce dir = bounceBehavior
    where
        bounceBehavior pos game
            | not (isPositionValid (nextP pos dir) game) = Target (nextP pos (- dir)) (bounce (- dir))
            | otherwise = go dir 0 pos (bounce dir) game
        nextP pos d = (fst pos + d, snd pos)

{-
    *** DONE ***
    Funcție care mută toate Target-urile din Game-ul dat o poziție, în functie
    de behavior-ul fiecăreia și întoarce noul Game în care pozițiile Target-urilor
    sunt actualizate.
    
-}
moveTargets :: Game -> Game
moveTargets game = game {targets = newTargets}
    where
        newTargets = map modif (targets game)
        modif targ = behavior targ (position targ) game

{-
    *** DONE ***

    Verifică dacă Targetul va fi eliminat de Hunter.
    Un Target este eliminat de Hunter daca se află pe o poziție adiacentă
    cu acesta.
    Parametrul Position reprezintă poziția Hunterului pe tabla
    de joc.
    Parametrul Target reprezintă Targetul pentru care se face verificarea.
-}
isTargetKilled :: Position -> Target -> Bool
isTargetKilled (x, y) (Target (i, j) _) = (abs (x - i) == 1 && y == j) || (abs (y - j) == 1 && x == i) 


{-
    *** DONE ***

    Avansează starea jocului curent, rezultând starea următoare a jocului.
    Parametrul Direction reprezintă direcția în care se va deplasa Hunter-ul.
    Parametrul Bool specifică dacă, după mutarea Hunter-ului, vor fi
    mutate și Target-urile sau nu, și dacă vor fi eliminate din joc sau nu.
    Este folosit pentru a distinge între desfășurarea reală a jocului (True)
    și planificarea „imaginată” de hunter (False) în partea a doua a temei.

    Avansarea stării jocului respectă următoarea ordine:
    1. Se deplasează Hunter-ul.
    2. În funcție de parametrul Bool, se elimină Target-urile omorâte de către Hunter.
    3. In funcție de parametrul Bool, se deplasează Target-urile rămase pe tablă.
    4. Se elimină Targeturile omorâte de către Hunter și după deplasarea acestora.
    
    Dubla verificare a anihilării Target-urilor, în pașii 2 și 4, îi oferă Hunter-ului
    un avantaj în prinderea lor.
-}

advanceGameState :: Direction -> Bool -> Game -> Game
advanceGameState direction kill game = game { hunter = newHunter, targets = newTargets}
    where 
        newHunter
            | isJust (nextPos direction) = fromJust (nextPos direction)
            | otherwise = hunter game
        nextPos dir
            | dir == East  = attemptMove (fst hpos, snd hpos + 1) game
            | dir == West  = attemptMove (fst hpos, snd hpos - 1) game
            | dir == North = attemptMove (fst hpos - 1, snd hpos) game
            | otherwise    = attemptMove (fst hpos + 1, snd hpos) game
        hpos = hunter game
        afterKillTargets targs
            | kill = filter (not . isTargetKilled newHunter) targs
            | otherwise    = targs
        afterMoveTargets
            | kill = targets (moveTargets game { targets = afterKillTargets (targets game)})
            | otherwise    = targets game
        newTargets
            | kill = afterKillTargets afterMoveTargets
            | otherwise    = targets game
        

{-
    ***  DONE ***

    Verifică dacă mai există Target-uri pe table de joc.
-}
areTargetsLeft :: Game -> Bool
areTargetsLeft game = not $ null $ targets game; 


{-
    *** BONUS TODO ***

    Comportamentul unui Target de a se deplasa în cerc, în jurul unui Position, având
    o rază fixată.
    Primul parametru, Position, reprezintă centrul cercului.
    Parametrul Int reprezintă raza cercului.
    Puteți testa utilizând terenul circle.txt din directorul terrains, în conjuncție
    cu funcția interactive.
-}
circle :: Position -> Int -> Behavior
circle = undefined


instance ProblemState Game Direction where
    {-
        *** DONE ***
        
        Generează succesorii stării curente a jocului.
        Utilizați advanceGameState, cu parametrul Bool ales corespunzător.
    -}
    successors game = concatMap (\d -> [(d, advanceGameState d False game)]) [East, West, North, South]

    {-
        *** DONE ***
        
        Verifică dacă starea curentă este un în care Hunter-ul poate anihila
        un Target. Puteți alege Target-ul cum doriți, în prezența mai multora.
    -}
    isGoal game = any (isTargetKilled (hunter game)) (targets game)

    {-
        *** DONE ***
        
        Euristica euclidiană (vezi hEuclidian mai jos) până la Target-ul ales
        de isGoal.
        (minimul dintre toate euristicile)
    -}
    h game = foldl1 (\m t -> if t < m then t else m) euclidians -- cauta minimul
        where
            -- calculeaza euclidianul pentru fiecare target
            euclidians = map (\t -> hEuclidean (position t) (hunter game)) (targets game)
            

{-
     ** NU MODIFICATI **
-}
hEuclidean :: Position -> Position -> Float
hEuclidean (x1, y1) (x2, y2) = fromIntegral $ ((x1 - x2) ^ pow) + ((y1 - y2) ^ pow)
  where
    pow = 2 :: Int

{-
    *** BONUS ***

    Acesta reprezintă un artificiu necesar pentru testarea bonusului,
    deoarece nu pot exista două instanțe diferite ale aceleiași clase
    pentru același tip.

    OBSERVAȚIE: Testarea bonusului pentru Seach este făcută separat.
-}

newtype BonusGame = BonusGame Game
    deriving (Eq, Ord, Show)

{-
    *** BONUS TODO ***

    Folosind wrapper-ul peste tipul Game de mai sus instanțiați
    ProblemState astfel încât să fie folosită noua euristică. 
-}
instance ProblemState BonusGame Direction where
    {-
        *** BONUS TODO ***

        Pentru a ne asigura că toțî succesorii unei stări sunt de tipul
        BonusGame și folosesc noua euristică trebuie să aplicăm wrapper-ul
        definit mai sus peste toți succesorii unei stări.

        Hint: Puteți să folosiți funcția fmap pe perechi pentru acest lucru.
        https://wiki.haskell.org/Functor
    -}
    successors = undefined

    {-
        *** BONUS TODO ***

        Definiți funcția isGoal pentru BonusGame.

        Hint: Folosiți funcția isGoal deja implementată pentru tipul Game.
    -}
    isGoal = undefined

    {-
        *** BONUS TODO ***

        Definiți o funcție euristică care este capabilă să găsească un drum mai scurt
        comparativ cu cel găsit de euristica implementată pentru Game.

        ATENȚIE: Noua euristică NU trebuie să fie una trivială.

        OBSERVAȚIE: Pentru testare se va folosi fișierul terrains/game-6.txt.
    -}
    h = undefined
