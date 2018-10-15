{- | 
   Module      : Main 
   Description : Função Responsável pela Passagem do Tempo.
   Copyright   : Diogo José Cruz Sobral
                 João Pedro Rodrigues Gomes

   Com esta documentação pretendemos que se torne mais fácil a leitura e o entendimento do nosso código para fazer o jogo "Bomberman".
   Aqui fica um pequeno resumo da lógica usada para fazer o jogo, e mais à frente abordamos todas as funções uma a uma. 
   Nesta tarefa a função principal , avanca, divide-se em dois tipos de diferentes, uma responsável por executar a passagem do tempo sem o caracol ativado e outra responsável por executar a passagem do tempo com o caracol.
-}


module Main where

import Data.Char
import Data.List
import Data.Char (isDigit)
import System.Environment
import Text.Read
import Data.Maybe

{- | Dim é a Dimensão do Mapa.
-}

type Dim = Int

{- | Epicentro é um Ponto.
-}
type Epicentro = (Int,Int)

{- | String com a informação do Jogador.
-}

type Iplayer  = String

{- | Número de Jogador.
-}
type Idplayer = Int

{- | String com a informção de um powerup.
-}

type Ipower = String

{- | Mapa.
-}

type Mapa  = [String]

{- | String com a informação de uma bomba.
-}
type Ibomb = String

{- | Mapa dividido em (Mapa,poderes,bombas,jogadores).
-}

type World = (Mapa,[Ipower],[Ibomb],[Iplayer])

{- | Número de Ticks.
-}

type Time = Int

{- | Raio de bomba.
-}

type Raio = Int

{- | Lista de coordenadas.
-}
type Lexplo = [(Int,Int)]




{- | A função avanca é a função responsável pela passagem do tempo.

  == Exemplos de utilização:

  >>> avanca ["#########","#       #","# #?#?# #","#  ?  ? #","#?# # #?#","# ?  ?  #","# #?#?# #","#  ??   #","#########","+ 5 2","+ 3 3","! 5 5","* 1 1 0 1 2","0 2 1"] 200
  ["#########","#       #","# #?#?# #","#  ?  ? #","#?# # #?#","# ?  ?  #","# #?#?# #","#  ??   #","#########","+ 5 2","+ 3 3","! 5 5","* 1 1 0 1 1","0 2 1"]

  >>> avanca ["#########","#       #","# #?#?# #","#  ?  ? #","#?# # #?#","# ?  ?  #","# #?#?# #","#  ??   #","#########","+ 5 2","+ 3 3","! 5 5","* 1 1 0 1 1","0 2 1"] 200
  ["#########","#       #","# #?#?# #","#  ?  ? #","#?# # #?#","# ?  ?  #","# #?#?# #","#  ??   #","#########","+ 5 2","+ 3 3","! 5 5"]
-}
avanca :: [String] -> Int -> [String]
avanca mapaT time | timeCheck time (length (head mapaT)) = comCaracol mapaT time
                  | otherwise = semCaracol mapaT


{- | A função semCaracol é a função responsavel por executar a passagem do tempo sem o caracol.

  == Exemplos de utilização:

    >>> semCaracol ["#########","#       #","# #?#?# #","#  ?  ? #","#?# # #?#","# ?  ?  #","# #?#?# #","#  ??   #","#########","+ 5 2","+ 3 3","! 5 5","* 1 1 0 1 1","0 2 1"]
    ["#########","#       #","# #?#?# #","#  ?  ? #","#?# # #?#","# ?  ?  #","# #?#?# #","#  ??   #","#########","+ 5 2","+ 3 3","! 5 5"]
-}
semCaracol :: [String] -> [String]
semCaracol mapaT = let (a,b,c,d) = splitMap mapaT
                       k = checkBombs(slowTime c)
                       w = checkBombs2(slowTime c)
                   in if k == [] then groupMap (a,b,(slowTime c),d) else groupMap(explodeTotal (a,b,w,d) k)

{- | A função comCaracol é a função responsavel por executar a passagem do tempo com o caracol.

  == Exemplos de utilização:

  >>> comCaracol  ["#########","#       #","# #?#?# #","#  ?  ? #","#?# # #?#","# ?  ?  #","# #?#?# #","#  ??   #","#########","+ 5 2","+ 3 3","! 5 5","* 1 1 0 1 1","0 2 1"] 2
  ["#########","#       #","# #?#?# #","#  ?  ? #","#?### #?#","# ?  ?  #","# #?#?# #","#  ??   #","#########","+ 5 2","+ 3 3","! 5 5"]
-}


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

{- | Função responsável por efetuar as alterações necessárias ao para resultantes das explosões.
    
      == Exemplos de utilização:

      >>> explodeTotal (["#########","#       #","# #?#?# #","#  ?  ? #","#?# # #?#","# ?  ?  #","# #?#?# #","#  ??   #","#########"],["+ 5 2","+ 3 3","! 5 5"],[],["0 2 1"]) ["* 1 1 0 1 1"]
      (["#########","#       #","# #?#?# #","#  ?  ? #","#?# # #?#","# ?  ?  #","# #?#?# #","#  ??   #","#########"],["+ 5 2","+ 3 3","! 5 5"],[],[])
-}

explodeTotal :: World -> [Ibomb] -> World
explodeTotal mapa [] = mapa
explodeTotal mapa (h:t) = let raio = read ((words h) !! 4) :: Int
                              (x,y) = coordPLayer h
                          in explodeTotal (explode mapa raio (x,y)) t

{- | Função responsável por transformar a lista de Strings do mapa num tipo novo World que divide a lista de string em (Mapa,Poderes,Bombas,Jogadores) .
    
      == Exemplos de utilização:

      >>> splitMap ["#########","#       #","# #?#?# #","#  ?  ? #","#?# # #?#","# ?  ?  #","# #?#?# #","#  ??   #","#########","+ 5 2","+ 3 3","! 5 5","* 1 1 0 1 1","0 2 1"]
      (["#########","#       #","# #?#?# #","#  ?  ? #","#?# # #?#","# ?  ?  #","# #?#?# #","#  ??   #","#########"],["+ 5 2","+ 3 3","! 5 5"],["* 1 1 0 1 1"],["0 2 1"])

-}

splitMap :: [String] -> World
splitMap jogo = let mapa    = filter (\jogo -> head jogo == '#') jogo
                    poderes = filter (\jogo -> head jogo == '+' || head jogo == '!') jogo
                    bombs   = filter (\jogo -> head jogo == '*') jogo
                    players  = filter (\jogo -> isDigit(head jogo) ) jogo

                in (mapa,poderes,bombs,players)

{- | Função responsável por transformar um tipo World novamente em lista de strings .
    
    == Exemplos de utilização:

     >>> groupMap (["#########","#       #","# #?#?# #","#  ?  ? #","#?# # #?#","# ?  ?  #","# #?#?# #","#  ??   #","#########"],["+ 5 2","+ 3 3","! 5 5"],["* 1 1 0 1 1"],["0 2 1"])
     ["#########","#       #","# #?#?# #","#  ?  ? #","#?# # #?#","# ?  ?  #","# #?#?# #","#  ??   #","#########","+ 5 2","+ 3 3","! 5 5","* 1 1 0 1 1","0 2 1"]
     
-}

groupMap :: World -> [String]
groupMap (mapa,poderes,bombs,players) = mapa ++ poderes ++ bombs ++ players

{- | Função responsável por reduzir o tempo das bombas.
    
    == Exemplos de utilização:
     >>> slowTime ["* 1 1 0 2 5","* 1 4 1 2 3"]
     ["* 1 1 0 2 4","* 1 4 1 2 2"]
-}


slowTime :: [Ibomb] -> [Ibomb]
slowTime [] = []
slowTime (i:bombs) = let timeb = read ((words i) !! 5) :: Int
                         splitb = words i
                         ntimer = timeb - 1
                         nbomb = map1 (\splitb k z -> if k == z then show ntimer else splitb) splitb 5 0
                     in unwords (nbomb) : slowTime bombs

{- | Função responsável por retirar as bombas com tick 0 , ou seja, que vao expludir.
    
    == Exemplos de utilização:
     >>> checkBombs ["* 1 1 0 2 5","* 1 4 1 2 3"]
     []
-}

checkBombs :: [Ibomb] -> [Ibomb]
checkBombs [] = []
checkBombs (i:bombs) = let timeb = read ((words i) !! 5) :: Int
                       in if timeb == 0 then i : checkBombs bombs else checkBombs bombs

{- | Função responsável por retirar as bombas com tick diferente de  0 , ou seja, as que ficam no mapa.
    
    == Exemplos de utilização:
     >>> checkBombs2 ["* 1 1 0 2 5","* 1 4 1 2 3"]
     ["* 1 1 0 2 5","* 1 4 1 2 3"]
-}

checkBombs2 :: [Ibomb] -> [Ibomb]
checkBombs2 [] = []
checkBombs2 (i:bombs) = let timeb = read ((words i) !! 5) :: Int
                        in if timeb /= 0 then i : checkBombs2 bombs else checkBombs2 bombs

{- | Função que dada um String respondes as coordenadas que estao na String.
    
    == Exemplos de utilização:
     >>> coordPlayer "* 1 1 0 2 5"
     (1,1)
-}

coordPLayer :: Iplayer -> (Int,Int)
coordPLayer iplayer = let i = words iplayer
                          x = read ( i !! 1) :: Int
                          y = read ( i !! 2) :: Int
                      in (x,y)

{- | Função que dada umas coordenadas do Mapa responde o informação sobre o que esta nessa coordenada.
    
    == Exemplos de utilização:
     >>> coordMapa ["#########","#       #","# #?#?# #","#  ?  ? #","#?# # #?#","# ?  ?  #","# #?#?# #","#  ??   #","#########","+ 5 2","+ 3 3","! 5 5","* 1 1 0 1 1","0 2 1"] (2,1)
     ' '
-}

coordMapa :: Mapa -> (Int,Int) -> Char
coordMapa mapa (a,b) = (mapa !! b) !! a

{- | Função que dada as Strings do jogadores e as coordenadas das explusões retira os jogadores que estao nessas coordenadas.

    == Exemplos de utilização:
     >>> killplayer ["0 1 1","1 3 1"] [(1,1),(5,2),(8,9)]
     ["1 3 1"]
-}

killplayer :: [Iplayer] -> [(Int,Int)] -> [Iplayer]
killplayer ijogadores [] = ijogadores
killplayer ijogadores (h:t) = let alt = tiraj ijogadores h
                              in killplayer alt t


{- | Função auxiliar da ´killplayer´ que dada um coordenada retira os jogadores nessa coordenada.

    == Exemplos de utilização:
     >>> tiraj ["0 1 1","1 3 1"] (1,1)
     ["1 3 1"]
-}

tiraj :: [Iplayer] -> (Int,Int) -> [Iplayer]
tiraj iplayer a = filter(\iplayer -> coordPLayer iplayer /= a) iplayer

{- | Função que dado um Estado World, o raio da bomba e o seu centro faz a explusão da mesma.

    == Exemplos de utilização:
     >>> explode (["#########","#       #","# #?#?# #","#  ?  ? #","#?# # #?#","# ?  ?  #","# #?#?# #","#  ??   #","#########"],["+ 5 2","+ 3 3","! 5 5"],[],["0 2 1"]) 1 (1,1)
     (["#########","#       #","# #?#?# #","#  ?  ? #","#?# # #?#","# ?  ?  #","# #?#?# #","#  ??   #","#########"],["+ 5 2","+ 3 3","! 5 5"],[],[])
-}

explode :: World -> Raio -> Epicentro -> World
explode (a,b,c,d) raio (x,y) = let coo = map (coordPLayer) b
                                   u = splitexplosion (x,y) raio a coo
                                   u2 = splitexplosion2 (x,y) raio a coo
                                   mapa = altotal a u2
                                   b2 = killplayer b u
                                   d2 = killplayer d u
                                   c2 = newBombs c u2
                               in (mapa,b2,c2,d2)

{- | Função altera tick das bombas para 1 se ela estiver numa das coordenadas das explusões.

    == Exemplos de utilização:
     >>> newBombs ["* 1 1 0 1 10","* 4 1 1 2 10"] [(1,1),(2,3)]
     ["* 1 1 0 1 1","* 4 1 1 2 10"]
-}

newBombs :: [Ibomb] -> [Epicentro] -> [Ibomb]
newBombs bombas epc = map (\y -> showtime2 y epc) bombas

{- | Função auxiliar da newBombs que altera o tick de uma bomba para 1 se ela estiver numa das coordenadas das explusões.

    == Exemplos de utilização:
     >>> showtime2 "* 1 1 0 1 10" [(1,1),(2,3)]
     "* 1 1 0 1 1"
-}

showtime2 :: Ibomb -> [Epicentro] -> Ibomb
showtime2 bomba [] = bomba
showtime2 bomba ((a,b):t) = let nbomb = words bomba
                            in if coordPLayer bomba == (a,b) then (unwords((take 5 nbomb) ++ ["1"])) else showtime2 bomba t

{- | Função que dada as todas as coordenadas possivel da explusão, o mapa e as coordenadas dos powerups devolve as coordenas de que não destroem '?' no mapa.

    == Exemplos de utilização:
     >>> checkraid [(3,2),(3,3)] ["#########","#       #","# #?#?# #","#  ?  ? #","#?# # #?#","# ?  ?  #","# #?#?# #","#  ??   #","#########"] [(5,2),(3,3),(5,5)]
     []
-}

checkraid :: Lexplo -> Mapa -> Lexplo -> Lexplo
checkraid [] mapa _ = []
checkraid ((a,b):t) mapa coo | (coordMapa mapa (a,b) == '?') || (coordMapa mapa (a,b) == '#') = []
                             | elem (a,b) coo = [(a,b)]
                             | otherwise = (a,b) : checkraid t mapa coo

{- | Função que dada as todas as coordenadas possivel da explusão, o mapa e as coordenadas dos powerups devolve as coordenas de que não destroem '?' no mapa.

    == Exemplos de utilização:
     >>> checkraid2 [(3,2),(3,3)] ["#########","#       #","# #?#?# #","#  ?  ? #","#?# # #?#","# ?  ?  #","# #?#?# #","#  ??   #","#########"] [(5,2),(3,3),(5,5)]
     [(3,2)]
-}

checkraid2 :: Lexplo -> Mapa -> Lexplo -> Lexplo
checkraid2 [] mapa _ = []
checkraid2 ((a,b):t) mapa coo | (coordMapa mapa (a,b) == '?') = [(a,b)]
                              | (coordMapa mapa (a,b) == '#') || elem (a,b) coo = []
                              | otherwise = checkraid2 t mapa coo

{- | Função que dada as todas as coordenadas a alterar no mapa, altera essas coordenadas.

    == Exemplos de utilização:
     >>> altotal ["#########","#       #","# #?#?# #","#  ?  ? #","#?# # #?#","# ?  ?  #","# #?#?# #","#  ??   #","#########"] [(5,2),(3,3)]
     ["#########","#       #","# #?# # #","#     ? #","#?# # #?#","# ?  ?  #","# #?#?# #","#  ??   #","#########"]
-}

altotal :: Mapa -> [Epicentro] -> Mapa
altotal mapa [] = mapa
altotal mapa (x:xs) = altotal u xs  where u = altmapa mapa x ' '

{- | Função que dada uma coordenada altera no mapa essa coordenada.

    == Exemplos de utilização:
     >>> almapa ["#########","#       #","# #?#?# #","#  ?  ? #","#?# # #?#","# ?  ?  #","# #?#?# #","#  ??   #","#########"] (5,2) ' '
     ["#########","#       #","# #?# # #","#  ?  ? #","#?# # #?#","# ?  ?  #","# #?#?# #","#  ??   #","#########"]
-}

altmapa :: Mapa -> Epicentro -> Char -> Mapa
altmapa mapa (w,k) u = (take k mapa) ++ (( alteraL (head (drop k mapa)) w u ):[]) ++ drop (k+1) mapa

{- | Função que dado um Int altera a o elemento com essa posição na string.

    == Exemplos de utilização:
     >>> alteraL "#########" 2 '!'
     "##!######"
-}   

alteraL :: String -> Int -> Char -> String
alteraL y w u = map1 (\y w z -> if w /= z then y else u ) y w 0

{- | Função igual à altotal2 mas neste caso utilizada para a expiral.

    == Exemplos de utilização:
     >>> altotal2 ["#########","#       #","# #?#?# #","#  ?  ? #","#?# # #?#","# ?  ?  #","# #?#?# #","#  ??   #","#########"] [(5,2),(3,3)]
     ["#########","#       #","# #?### #","#  #  ? #","#?# # #?#","# ?  ?  #","# #?#?# #","#  ??   #","#########"]
-}

altotal2 :: Mapa -> [Epicentro] -> Mapa
altotal2 mapa [] = mapa
altotal2 mapa (x:xs) = altotal2 u xs  where u = altmapa mapa x '#'

{- | Função que dado o centro, o raio, o mapa e os powerups produz uma lista com as coordenadas que alteram os powerups, os jogadores e as bombas.

    == Exemplos de utilização:
     >>> splitexplosion (1,1) 1 ["#########","#       #","# #?#?# #","#  ?  ? #","#?# # #?#","# ?  ?  #","# #?#?# #","#  ??   #","#########"] [(5,2),(3,3)]
     [(1,1),(1,2),(2,1)]
-}

splitexplosion :: Epicentro -> Raio -> Mapa -> Lexplo -> Lexplo
splitexplosion centro raio mapa coo = let eu = checkraid (explosionU centro (raio+1) 0) mapa coo
                                          ed = checkraid (explosionD centro raio 0) mapa coo
                                          el = checkraid (explosionL centro raio 0) mapa coo
                                          er = checkraid (explosionR centro raio 0) mapa coo
                                      in eu ++ ed ++ el ++ er

{- | Função que dado o centro, o raio, o mapa e os powerups produz uma lista com as coordenadas que alteram o mapa numeadamente os '?' destruidos.

    == Exemplos de utilização:
     >>> splitexplosion2 (1,1) 1 ["#########","#       #","# #?#?# #","#  ?  ? #","#?# # #?#","# ?  ?  #","# #?#?# #","#  ??   #","#########"] [(5,2),(3,3)]
     []
-}

splitexplosion2 :: Epicentro -> Raio -> Mapa -> Lexplo -> Lexplo
splitexplosion2 centro raio mapa coo = let eu = checkraid2 (explosionU centro (raio+1) 0) mapa coo
                                           ed = checkraid2 (explosionD centro raio 0) mapa coo
                                           el = checkraid2 (explosionL centro raio 0) mapa coo
                                           er = checkraid2 (explosionR centro raio 0) mapa coo
                                       in eu ++ ed ++ el ++ er

{- | Função que dado o centro e o raio gera as coordenadas para cima com o centro.

    == Exemplos de utilização:
     >>> explosionU (3,1)  2 0
     [(3,1),(3,0)]
-}

explosionU :: Epicentro -> Raio -> Int -> Lexplo
explosionU (a,b) 0 conta = []
explosionU (a,b) raio conta = (a,b-conta) : explosionU (a,b) (raio - 1) (conta+1)

{- | Função que dado o centro e o raio gera as coordenadas para baixo.

    == Exemplos de utilização:
     >>> explosionD (3,1)  2 0
     [(3,2),(3,3)]
-}

explosionD :: Epicentro -> Raio -> Int -> Lexplo
explosionD (a,b) 0 conta = []
explosionD (a,b) raio conta = (a,k+conta) : explosionD (a,b) (raio - 1) (conta+1) where k = b+1

{- | Função que dado o centro e o raio gera as coordenadas para a esquerda.

    == Exemplos de utilização:
     >>> explosionL (3,1)  2 0
     [(2,1),(1,1)]
-}

explosionL :: Epicentro -> Raio -> Int -> Lexplo
explosionL (a,b) 0 conta = []
explosionL (a,b) raio conta = (k-conta,b) : explosionL (a,b) (raio - 1) (conta+1) where k = a-1

{- | Função que dado o centro e o raio gera as coordenadas para cima com o centro

    == Exemplos de utilização:
     >>> explosionR (3,1)  2 0
     [(4,1),(5,1)]
-}

explosionR :: Epicentro -> Raio -> Int -> Lexplo
explosionR (a,b) 0 conta = []
explosionR (a,b) raio conta = (k+conta,b) : explosionR (a,b) (raio - 1) (conta+1) where k = a+1

{- | Função auxiliar.
-}

map1 :: (a->Int->Int->b) -> [a] -> Int -> Int -> [b]
map1 f [] k z = []
map1 f (y:ys) k z  = f y k z  : map1 f ys k (z+1)


{- | Função que dado um tempo responde se o caracol esta ativo.

    == Exemplos de utilização:
     >>> timeCheck 100 10
     False
-}

timeCheck :: Time -> Int -> Bool
timeCheck time tam = (time <= (tam-2)^2)

{- | Função que dado um tempo responde em que altura do caracol esta.

    == Exemplos de utilização:
     >>> timeplay 100 10
     6
-}

timeplay :: Time -> Int -> Int
timeplay time tam = ((tam-2)^2) - time

{- | Função uma dimensão e o ponto em que começa dá uma volta com as coordenadas utilizadas para a expiral.

    == Exemplos de utilização:
     >>> criaCoord 7 (1,1) 0 (1,1)
     [(1,1),(2,1),(3,1),(4,1),(5,1),(5,2),(5,3),(5,4),(5,5),(4,5),(3,5),(2,5),(1,5),(1,4),(1,3),(1,2)]
-}

criaCoord :: Dim -> (Int,Int) -> Int-> (Int,Int) -> [(Int,Int)]
criaCoord dim (a,b) cont (c,d) 
                               | cont == 0 = criaG (\(a,b) -> (a+1,b)) (a,b) y ++ criaCoord dim (u,b) 1 (c,d)
                               | cont == 1 = criaG (\(a,b) -> (a,b+1)) (a,b) y ++ criaCoord dim (u,u) 2 (c,d)
                               | cont == 2 = criaG (\(a,b) -> (a-1,b)) (a,b) y ++ criaCoord dim (c,b) 3 (c,d)
                               | cont == 3 = criaG (\(a,b) -> (a,b-1)) (a,b) y
                               where
                                u = (c + dim - 3)
                                y = dim - 3

{- | Função que cria coordenadas para a espiral.
-}

criaG :: (Epicentro -> Epicentro) -> Epicentro -> Int -> [(Int,Int)]
criaG _ _ 0 = []
criaG f (a,b) r = (a,b) : criaG f (f (a,b)) (r-1)

{- | Função que cria as coordenadas da espiral.

    == Exemplos de utilização:
     >>> criaT 7 (1,1) 2
     [(1,1),(2,1),(3,1),(4,1),(5,1),(5,2),(5,3),(5,4),(5,5),(4,5),(3,5),(2,5),(1,5),(1,4),(1,3),(1,2),(2,2),(3,2),(4,2),(4,3),(4,4),(3,4),(2,4),(2,3),(3,3)]
-}

criaT :: Dim -> (Int,Int) -> Int -> [(Int,Int)]
criaT 3 (a,b) w = [(a,b)]
criaT dim (a,b) w = (criaCoord dim (a,b) 0 (a,b)) ++ criaT (dim-2) (w,w) (w+1) 

{- | ’main’: função que dá print ao mapa.
-}
main :: IO ()
main = do
    a <- getArgs
    let ticks = readMaybe (a !! 0)
    w <- getContents
    if isJust ticks
        then putStr $ unlines $ avanca (lines w) (fromJust ticks)
        else putStrLn "Parâmetros inválidos"
