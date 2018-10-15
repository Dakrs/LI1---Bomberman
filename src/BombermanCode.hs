module BombermanCode where

import System.Random
import Data.Char
import Data.List
import Data.Char (isDigit)
import System.Environment
import Text.Read
import Data.Maybe

type Dim = Int
type Epicentro = (Int,Int)
type Iplayer  = String
type Idplayer = Int
type Ipower = String
type Mapa  = [String]
type Ibomb = String
type World = (Mapa,[Ipower],[Ibomb],[Iplayer])
type Time = Int
type Raio = Int
type Lexplo = [(Int,Int)]
 

    
avanca :: [String] -> Int -> [String]
avanca mapaT time | timeCheck time (length (head mapaT)) = comCaracol mapaT time
                  | otherwise = semCaracol mapaT

semCaracol :: [String] -> [String]
semCaracol mapaT = let (a,b,c,d) = splitMap mapaT
                       k = checkBombs(slowTime c)
                       w = checkBombs2(slowTime c)
                   in if k == [] then groupMap (a,b,(slowTime c),d) else groupMap(explodeTotal (a,b,w,d) k)

comCaracol :: [String] -> Time -> [String]
comCaracol mapaT time = let (a,b,c,d) = splitMap (mapaT)
                            dim = length (head mapaT)
                            tpas = timeplay time dim
                            (w,k) = head(drop tpas (criaT dim (1,1) 2))
                            nm = altotal2 a [(w,k)]
                            nb = killplayer b [(w,k)]
                            nc = killplayer c [(w,k)]
                            nd = killplayer d [(w,k)]
                        in semCaracol(groupMap (nm,nb,nc,nd))


explodeTotal :: World -> [Ibomb] -> World
explodeTotal mapa [] = mapa
explodeTotal mapa (h:t) = let raio = read ((words h) !! 4) :: Int
                              (x,y) = coordPLayer h
                          in explodeTotal (explode mapa raio (x,y)) t


splitMap :: [String] -> World
splitMap jogo = let mapa    = filter (\jogo -> head jogo == '#') jogo
                    poderes = filter (\jogo -> head jogo == '+' || head jogo == '!') jogo
                    bombs   = filter (\jogo -> head jogo == '*') jogo
                    players  = filter (\jogo -> isDigit(head jogo) ) jogo
                in (mapa,poderes,bombs,players)

groupMap :: World -> [String]
groupMap (mapa,poderes,bombs,players) = mapa ++ poderes ++ bombs ++ players
-- diminui os tempos
slowTime :: [Ibomb] -> [Ibomb]
slowTime [] = []
slowTime (i:bombs) = let timeb = read ((words i) !! 5) :: Int
                         splitb = words i
                         ntimer = timeb - 1
                         nbomb = map1 (\splitb k z -> if k == z then show ntimer else splitb) splitb 5 0
                     in unwords (nbomb) : slowTime bombs
--ve bombas com timer 0
checkBombs :: [Ibomb] -> [Ibomb]
checkBombs [] = []
checkBombs (i:bombs) = let timeb = read ((words i) !! 5) :: Int
                       in if timeb == 0 then i : checkBombs bombs else checkBombs bombs

checkBombs2 :: [Ibomb] -> [Ibomb]
checkBombs2 [] = []
checkBombs2 (i:bombs) = let timeb = read ((words i) !! 5) :: Int
                        in if timeb /= 0 then i : checkBombs2 bombs else checkBombs2 bombs
-- vai buscar coordenadas
coordPLayer :: Iplayer -> (Int,Int)
coordPLayer iplayer = let i = words iplayer
                          x = read ( i !! 1) :: Int
                          y = read ( i !! 2) :: Int
                      in (x,y)

-- info de um ponto
coordMapa :: Mapa -> (Int,Int) -> Char
coordMapa mapa (a,b) = (mapa !! b) !! a

-- mata o jogador

killplayer :: [Iplayer] -> [(Int,Int)] -> [Iplayer]
killplayer ijogadores [] = ijogadores
killplayer ijogadores (h:t) = let alt = tiraj ijogadores h
                              in killplayer alt t
-- aux de matar o jogador
tiraj :: [Iplayer] -> (Int,Int) -> [Iplayer]
tiraj iplayer a = filter(\iplayer -> coordPLayer iplayer /= a) iplayer

--let u = splitexplosion (x,y) raio a

explode :: World -> Raio -> Epicentro -> World
explode (a,b,c,d) raio (x,y) = let coo = map (coordPLayer) b
                                   u = splitexplosion (x,y) raio a coo
                                   u2 = splitexplosion2 (x,y) raio a coo
                                   mapa = altotal a u2
                                   b2 = killplayer b u
                                   d2 = killplayer d u
                                   c2 = newBombs c u2
                               in (mapa,b2,c2,d2)

newBombs :: [Ibomb] -> [Epicentro] -> [Ibomb]
newBombs [] _ = []
newBombs (h:t) exp = (showtime2 h exp) : newBombs t exp

showtime2 :: Ibomb -> [Epicentro] -> Ibomb
showtime2 bomba [] = bomba
showtime2 bomba ((a,b):t) = let nbomb = words bomba
                            in if coordPLayer bomba == (a,b) then (unwords((take 5 nbomb) ++ ["1"])) else showtime2 bomba t

checkraid :: Lexplo -> Mapa -> Lexplo -> Lexplo
checkraid [] mapa _ = []
checkraid ((a,b):t) mapa coo | (coordMapa mapa (a,b) == '?') || (coordMapa mapa (a,b) == '#') = []
                             | elem (a,b) coo = [(a,b)]
                             | otherwise = (a,b) : checkraid t mapa coo

checkraid2 :: Lexplo -> Mapa -> Lexplo -> Lexplo
checkraid2 [] mapa _ = []
checkraid2 ((a,b):t) mapa coo | (coordMapa mapa (a,b) == '?') = [(a,b)]
                              | (coordMapa mapa (a,b) == '#') || elem (a,b) coo = []
                              | otherwise = checkraid2 t mapa coo


checkraid3 :: Lexplo -> Mapa -> [Ipower] -> Lexplo
checkraid3 [] mapa _ = []
checkraid3 ((a,b):t) mapa poder | (coordMapa mapa (a,b) == '#') = []
                                | (coordMapa mapa (a,b) == '?') || (elem (a,b) y) = [(a,b)]
                                | otherwise = (a,b) : checkraid3 t mapa poder
                                where
                                  y = map (coordPLayer) poder

--altmapa
altotal :: Mapa -> [Epicentro] -> Mapa
altotal mapa [] = mapa
altotal mapa (x:xs) = altotal u xs  where u = altmapa mapa x ' '

altmapa :: Mapa -> Epicentro -> Char -> Mapa
altmapa mapa (w,k) u = (take k mapa) ++ (( alteraL (head (drop k mapa)) w u ):[]) ++ drop (k+1) mapa

alteraL :: String -> Int -> Char -> String
alteraL y w u = map1 (\y w z -> if w /= z then y else u ) y w 0

altotal2 :: Mapa -> [Epicentro] -> Mapa
altotal2 mapa [] = mapa
altotal2 mapa (x:xs) = altotal2 u xs  where u = altmapa mapa x '#'


--linhas de explusão sem destruir os ?
splitexplosion :: Epicentro -> Raio -> Mapa -> Lexplo -> Lexplo
splitexplosion centro raio mapa coo = let eu = checkraid (explosionU centro (raio+1) 0) mapa coo
                                          ed = checkraid (explosionD centro raio 0) mapa coo
                                          el = checkraid (explosionL centro raio 0) mapa coo
                                          er = checkraid (explosionR centro raio 0) mapa coo
                                      in eu ++ ed ++ el ++ er

splitexplosion2 :: Epicentro -> Raio -> Mapa -> Lexplo -> Lexplo
splitexplosion2 centro raio mapa coo = let eu = checkraid2 (explosionU centro (raio+1) 0) mapa coo
                                           ed = checkraid2 (explosionD centro raio 0) mapa coo
                                           el = checkraid2 (explosionL centro raio 0) mapa coo
                                           er = checkraid2 (explosionR centro raio 0) mapa coo
                                       in eu ++ ed ++ el ++ er

explosionU :: Epicentro -> Raio -> Int -> Lexplo
explosionU (a,b) 0 conta = []
explosionU (a,b) raio conta = (a,b-conta) : explosionU (a,b) (raio - 1) (conta+1)

explosionD :: Epicentro -> Raio -> Int -> Lexplo
explosionD (a,b) 0 conta = []
explosionD (a,b) raio conta = (a,k+conta) : explosionD (a,b) (raio - 1) (conta+1) where k = b+1

explosionL :: Epicentro -> Raio -> Int -> Lexplo
explosionL (a,b) 0 conta = []
explosionL (a,b) raio conta = (k-conta,b) : explosionL (a,b) (raio - 1) (conta+1) where k = a-1

explosionR :: Epicentro -> Raio -> Int -> Lexplo
explosionR (a,b) 0 conta = []
explosionR (a,b) raio conta = (k+conta,b) : explosionR (a,b) (raio - 1) (conta+1) where k = a+1

map1 :: (a->Int->Int->b) -> [a] -> Int -> Int -> [b]
map1 f [] k z = []
map1 f (y:ys) k z  = f y k z  : map1 f ys k (z+1)


timeCheck :: Time -> Int -> Bool
timeCheck time tam = (time <= (tam-2)^2)

timeplay :: Time -> Int -> Int
timeplay time tam = ((tam-2)^2) - time

criaCoord :: Dim -> (Int,Int) -> Int-> (Int,Int) -> [(Int,Int)]
criaCoord dim (a,b) cont (c,d) 
                               | cont == 0 = cria1 y (a,b) ++ criaCoord dim (u,b) 1 (c,d)
                               | cont == 1 = cria2 y (a,b) ++ criaCoord dim (u,u) 2 (c,d)
                               | cont == 2 = cria3 y (a,b) ++ criaCoord dim (c,b) 3 (c,d)
                               | cont == 3 = cria4 y (a,b)
                               where
                                u = (c + dim - 3)
                                y = dim - 3


cria1 :: Dim -> (Int,Int) -> [(Int,Int)]
cria1 0 _ = []
cria1 size (a,b) = (a,b) : cria1 (size-1) (a+1,b)

cria2 :: Dim -> (Int,Int) -> [(Int,Int)]
cria2 0 _ = []
cria2 size (a,b) = (a,b) : cria2 (size-1) (a,b+1)

cria3 :: Dim -> (Int,Int) -> [(Int,Int)]
cria3 0 _ = []
cria3 size (a,b) = (a,b) : cria3 (size-1) (a-1,b)

cria4 :: Dim -> (Int,Int) -> [(Int,Int)]
cria4 0 _ = []
cria4 size (a,b) = (a,b) : cria4 (size-1) (a,b-1)



criaT :: Dim -> (Int,Int) -> Int -> [(Int,Int)]
criaT 3 (a,b) w = [(a,b)]
criaT dim (a,b) w = (criaCoord dim (a,b) 0 (a,b)) ++ criaT (dim-2) (w,w) (w+1) 

shieldBot :: Dim -> (Int,Int) -> Int -> [(Int,Int)] -> [(Int,Int)]
shieldBot _ _ 4 acu = acu
shieldBot dim (a,b) cont acu 
                            | cont == 0 = shieldBot dim (a1+1,b1) 1 (cria1 y (a,b))
                            | cont == 1 = shieldBot dim (a2,b2+1) 2 (acu ++ (cria2 y (a,b)))
                            | cont == 2 = shieldBot dim (a3-1,b3) 3 (acu ++ (cria3 y (a,b)))
                            | cont == 3 = shieldBot dim (a4,b4+1) 4 (acu ++ (cria4 y (a,b)))
                            where
                             y = dim - 3
                             (a1,b1) = last (acu ++ (cria1 y (a,b)))
                             (a2,b2) = last (acu ++ (cria2 y (a,b))) 
                             (a3,b3) = last (acu ++ (cria3 y (a,b))) 
                             (a4,b4) = last (acu ++ (cria4 y (a,b)))  




conv :: String -> Int
conv y = read y :: Int

dec :: String -> (Int,Int)
dec (y:t:ys) = aux ys 0 (0,0) where 
    aux ys z (w,k) | z== 0 = aux (drop 1 b) (z+1) (conv a,k)
                   | z== 1 = aux ys (z+1) (w, conv c)
                   | otherwise = (w,k)
    (a,b) = span (\ys -> isDigit ys == True) ys 
    u = drop 1 b
    (c,d) = span (\u -> isDigit u == True) u

rdec :: String -> (Int,Int) -> String
rdec (y:ys) (a,b) = (y:[]) ++ aux ys (a,b) 0 where
    aux [] (a,b) z  = []
    aux (x:xs) (a,b) z 
                       | x == ' ' = " " ++ aux xs (a,b) (z+1)
                       | z == 1    = (show a) ++ aux (dropWhile (\xs -> isDigit xs == True) xs) (a,b) z
                       | z == 2    = (show b) ++ aux (dropWhile (\xs -> isDigit xs == True) xs) (a,b) z
                       | otherwise = (x:[]) ++ aux xs (a,b) z

dbomb :: String -> Int
dbomb (y:ys) = aux ys 0 where
  aux ys 3 = digitToInt(head ys)
  aux ys z | head ys == ' ' = aux (drop 1 ys) (z+1)
           | otherwise = aux (dropWhile (\ys -> isDigit ys == True) ys) z

coord :: Char -> [String] -> (Int,Int)
coord x y = dec $ head $ filter (\y -> x == head y) y

coordPUP :: Char -> [String] -> [(Int,Int)]
coordPUP x y = map dec  (filter (\y -> x == head y) y)

coordBombas :: [String] -> [(Int,Int)]
coordBombas y = map dec (filter (\y-> '*' == head y) y)

coordCompare :: (Int,Int) -> (Int,Int) -> Bool
coordCompare (a,b) (w,k) | b > k      = True
                         | b == k &&  a > w = True
                         | otherwise = False


checka :: String -> (Int,Int) -> Bool
checka y (w,k) = aux y w 0 where
    aux [] w z = False
    aux (y:ys) w z | (y == '#' || y== '?') && w == z = False
                   |  w == z                         = True
                   | otherwise                       = aux ys w (z+1)

procLinha :: Int -> [String] -> String
procLinha x (y:ys) | x==0 = y
                   | otherwise = procLinha (x-1) ys

moveRight :: (Int,Int) -> [String] -> (Int,Int)
moveRight (w,k) list | checka (procLinha k list) (w+1,k) == True = (w+1,k)
                     | otherwise = (w,k)

moveLeft :: (Int,Int) -> [String] -> (Int,Int)
moveLeft (w,k) list | checka (procLinha k list) (w-1,k) == True = (w-1,k)
                    | otherwise = (w,k)

moveUP :: (Int,Int) -> [String] -> (Int,Int)
moveUP (w,k) list | checka (procLinha (k-1) list) (w,k) == True = (w,k-1)
                  | otherwise = (w,k)

moveDown :: (Int,Int) -> [String] ->  (Int,Int)
moveDown (w,k) list | checka (procLinha (k+1) list) (w,k) == True = (w,k+1)
                    | otherwise = (w,k)

checkCoord :: (Int,Int) -> [(Int,Int)] -> Bool
checkCoord x y = elem x y

takePUP :: [String] -> Int -> (Int,Int) -> [String]
takePUP x y  (w,k) = filter (\x -> (head x == '#') || (dec x /= (w,k) ) ||isDigit (head x)) x 


testPUP :: String -> [Char] -> Bool
testPUP y [] = False
testPUP y (x:xs) | elem x y = True
                 | otherwise = testPUP y xs


finalPUP :: String -> String -> String -> String
finalPUP y x z | z == []                 = y
               | (testPUP y x) == False  = y ++ " " ++ z
               | (testPUP y x) == True && z == "+"  = a ++ z ++ b
               | otherwise = y ++ z
               where (a,b) = span (\y -> y /= '!') y


verNumeB :: [String] -> Int -> Int
verNumeB x y =length(filter (\u -> u == y) u ) where u = map dbomb (filter (\x-> '*' == head x) x)


firePOWER :: [String] -> Int -> Int
firePOWER x y = 1 + length (filter(\u -> u == '!') u) where u = head (filter (\x -> intToDigit(y) == head x) x)


bombPOWER :: [String] -> Int -> Int
bombPOWER x y = 1 + length (filter(\u -> u == '+') u) where u = head (filter (\x -> intToDigit(y) == head x) x)


allowBomba :: [String] -> Int -> String
allowBomba x y   | checkCoord (coord (intToDigit y) x) (coordBombas x) = []
                 | (bombPOWER x y) == verNumeB x y = []
                 |  otherwise = stringBomba x y (coord (intToDigit y) x)

stringBomba :: [String] -> Int -> (Int,Int) -> String
stringBomba x y (w,k) = "*" ++ " " ++ show w ++ " " ++ show k ++ " " ++ show y ++ " " ++ show (firePOWER x y) ++ " " ++ "10"

colocaBom :: [String] -> String -> [String]
colocaBom [] y = y : []
colocaBom x [] = x
colocaBom (x:xs) y | coordCompare (dec x) (dec y) = y : x : xs
                   | otherwise = x : colocaBom xs y

powerUP :: [String] -> Int -> Char -> String
powerUP x y l | l == 'R' = checkpowerR x y
              | l == 'L' = checkpowerL x y
              | l == 'U' = checkpowerU x y
              | l == 'D' = checkpowerD x y


checkpowerU :: [String] -> Int -> String
checkpowerU x y | checkCoord (moveUP (coord (intToDigit y) x) x) (coordPUP '+' x)  = "+"
                | checkCoord (moveUP (coord (intToDigit y) x) x) (coordPUP '!' x)  = "!"
                | otherwise = []

checkpowerL :: [String] -> Int -> String
checkpowerL x y | checkCoord (moveLeft (coord (intToDigit y) x) x) (coordPUP '+' x)  = "+"
                | checkCoord (moveLeft (coord (intToDigit y) x) x) (coordPUP '!' x)  = "!"
                | otherwise = []


checkpowerR :: [String] -> Int -> String
checkpowerR x y | checkCoord (moveRight (coord (intToDigit y) x) x) (coordPUP '+' x) = "+"
                | checkCoord (moveRight (coord (intToDigit y) x) x) (coordPUP '!' x) = "!"
                | otherwise = []


checkpowerD :: [String] -> Int -> String
checkpowerD x y | checkCoord (moveDown (coord (intToDigit y) x) x) (coordPUP '+' x)  = "+"
                | checkCoord (moveDown (coord (intToDigit y) x) x) (coordPUP '!' x)  = "!"
                | otherwise = []


newStr :: [String] -> Int -> Char -> String -> String
newStr [] y l  b = []
newStr  x y l  b | l== 'R' = rdec b (moveRight (coord (intToDigit y) x) x)
                 | l== 'L' = rdec b (moveLeft  (coord (intToDigit y) x) x)
                 | l== 'U' = rdec b (moveUP (coord (intToDigit y) x) x)
                 | l== 'D' = rdec b (moveDown  (coord (intToDigit y) x) x)


move :: [String] -> Int -> Char -> [String]
move x y l  | p == [] = x
            | l == 'B' = e ++ (colocaBom f (allowBomba x y)) ++ d
            | l == 'R' = (takePUP o y  (moveRight (coord (intToDigit y) x) x)) ++ i ++ ( tool :[]) ++ tail b                             
            | l == 'L' = (takePUP o y  (moveLeft  (coord (intToDigit y) x) x)) ++ i ++ ( tool :[]) ++ tail b
            | l == 'U' = (takePUP o y  (moveUP  (coord (intToDigit y) x) x)) ++ i ++ ( tool :[]) ++ tail b
            | l == 'D' = (takePUP o y  (moveDown (coord (intToDigit y) x) x)) ++ i ++ ( tool :[]) ++ tail b
            where (a,b) = span (\x -> (intToDigit (y) /= head x) ) x
                  (o,i) = span (\a -> head a /= '*') a
                  tool = finalPUP (newStr x y l (head b)) "+!" (powerUP x y l)
                  (c,d) = span (\x -> isDigit (head x) == False ) x
                  (e,f) = span (\c -> head c /= '*') c
                  p = filter (\h -> head h == (intToDigit y)) b


cal :: Int -> Int
cal x | x == 5        = 0
      | x > 5         = (x-2)^2  -  u  where
                                            u = quot (((x-5)+2)^2) 4 + 12

valr :: Int -> Int -> [Int]
valr x y = take ((cal x)+1) $ randomRs (0,99) (mkStdGen y)

rand :: Int -> Char
rand x 
        | x <= 39  = '?'
        | x >= 40  = ' '

geraLinha :: Int -> (Int,Int) -> [(Int,Int)]
geraLinha x (a,b) | a <= x - 1  = (a,b) : geraLinha x (a+1,b)
                  | a > x - 1  = []



fillMap :: Int -> [(Int,Int)] -> [Int] -> String
fillMap  x [] y = []
fillMap  x ((a,b):ys) (t:t') | b == 0   || b == x - 1                                 = replicate x '#'
                             | a == 0   || a== x - 1                                  = '#' : fillMap x ys (t:t')
                             | (b == 1 || b == x-2) && ( a >= (x-3) || a <=  2 )      = ' ' : fillMap x ys (t:t')  
                             | (b == 2 || b == x-3) && ( a == 1 || a == x-2)          = ' ' : fillMap x ys (t:t')
                             | linha (a,b) == True                                    = '#' : fillMap x ys (t:t')
                             | otherwise                                              = rand t : fillMap x ys t'

linha :: (Int,Int) -> Bool
linha (w,k) | odd (w+1) && odd (k+1) = True
            | otherwise = False

mapa1 :: Int -> Int -> [String]
mapa1 x y = aux x  y 0 where
  aux x y z | x < 5           = error "x tem de ser superior ou igual a 5"
            | even x == True  = error "x tem de ser impar"
            | z > x - 1       = []
            | otherwise       = fillMap x (geraLinha x (0,z))  o : aux x y (z+1) where o = drop (soma x (z-1)) (valr x y)

alpha :: Int -> Int
alpha x = x - ((div x 2) + 1)

soma :: Int -> Int -> Int
soma 5 z = 0
soma x 0 = 0
soma x z 
         | z < 0 = 0
         | z == x - 1            = 0 + soma x (z-1)
         | z == 1 || z == x-2 = (x - 6) + soma x (z-1)
         | z == 2 || z == x-3 = (alpha x - 2) + soma x (z-1)
         | even (z) == True  =  (alpha x) + soma x (z-1)
         | otherwise          = (x - 2) + soma x (z-1)


geraParesR :: Int -> (Int,Int) -> [(Int,Int)]
geraParesR x (a,b)  
                  | b == x  = []
                  | b == 0   || b == x - 1 || a == x = geraParesR x (0,b+1)
                  | a == 0   || a == x - 1 = geraParesR x (a+1,b)
                  | (b == 1  || b == x-2) &&  a <=  2  = geraParesR x (3,b)
                  | (b == 1  || b == x-2) && a >= (x - 3) = geraParesR x (0,b+1)
                  | (b == 2 || b == x-3) && ( a == 1 || a == x-2) = geraParesR x (a+1,b)
                  | linha (a,b) == True = geraParesR x (a+1,b)
                  | otherwise = (a,b) : geraParesR x (a+1,b)


powerUP2 :: Int -> [(Int,Int)] -> [Int] -> [String]
powerUP2 x [] _ = []
powerUP2 x t y = aux x t y [] [] where
     aux x [] y w k = w ++ k
     aux x ((a,b):t') (y:ys) w k | y <= 1 && w == []   = aux x t' ys (("+" ++ " " ++ show (a) ++ " " ++ show (b)):[]) k
                                 | y <= 1              = aux x t' ys (w ++ ("+" ++ " " ++ show (a) ++ " " ++ show (b)):[]) k
                                 | y <= 3 && k == []   = aux x t' ys w (("!" ++ " " ++ show (a) ++ " " ++ show (b)):[])
                                 | y <= 3              = aux x t' ys w (k ++ ("!" ++ " " ++ show (a) ++ " " ++ show (b)):[])
                                 | otherwise = aux x t' ys w k


mapa :: Int -> Int -> [String]
mapa x y = (mapa1 x y) ++ powerUP2 x (geraParesR x (0,0)) (valr x y)