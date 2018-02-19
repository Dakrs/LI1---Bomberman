{- |
   Module : Main 
   Description : Desenvolvimento de um bot.
   Copyright   : Diogo José Cruz Sobral ; João Pedro Rodrigues Gomes

    c

 Com esta documentação pretendemos que se torne mais fácil a leitura e o entendimento do nosso código para fazer o jogo "Bomberman".
 Aqui fica um pequeno resumo da lógica usada para fazer o jogo, e mais à frente abordamos todas as funções uma a uma. 
 Esta parte do trabalho teve o objetivo de criar um bot ,ou seja, um programa que dado o estado do jogo, tempo e o número de um jogador conseguisse tomar as suas decisões.

-}

module Tarefa6_li1g048 where

import BombermanCode
import Data.Char
import Data.List


{- | Coordenadas de um ponto.
-}


type Local = (Int,Int)

{- | Função principal bot que dado o estado atual do jogo e o tempo, toda a decisão por um jogador.

     == Exemplos de utilização:
     >>> bot ["#########","#       #","# #?#?# #","#  ?  ? #","#?# # #?#","# ?  ?  #","# #?#?# #","#  ??   #","#########","+ 5 2","+ 3 3","! 5 5","0 1 1","1 5 1"] 0 100
     Just 'R'
-}


bot :: [String] -> Int -> Int -> Maybe Char
bot mapa player ticks = let (a,b,c,d) = splitMap mapa
                            expl = explodeSum (a,b,c,d)
                            (x,y) = coordPLayer $ head $ filter (\d -> head d == intToDigit player) d
                        in if elem (x,y) expl
                          then ativarShield (a,b,c,d) expl (x,y) ticks player
                          else movimento (a,b,c,d) player ticks (x,y) expl

{- | A função vaiBelem é a função responsável por dar os dois elementos diferente pertencentes a um string para movimentar o bot.

     == Exemplos de utilização:
     >>> vaiBelem "UUUL"
     ('U','L')
-}

vaiBelem :: String -> (Char,Char)
vaiBelem (h:t) = if b == [] then (h,h) else (h,head b) where b = (dropWhile (\y -> y == h) t)

{- | A função invertCaminho é a função responsável passar o string que faça primeiros os movimentos horizontais e depois os verticais numa que faça primeiro os verticais e só depois os horizontais.

     == Exemplos de utilização:
     >>> invertCaminho "UUUL"
     "LLLU"
-}

invertCaminho :: String -> String
invertCaminho (h:t) = let u = map (\y -> if y == h then 0 else 1) (h:t)
                          (a,b) = vaiBelem (h:t)
                          nStr = map (\y -> if y == 1 then a else b ) u
                      in nStr

{- | A função caminho é a função responsável por dar uma string com os movimento para o bot se deslocar de um sitio para outro.

     == Exemplos de utilização:
     >>> caminho (1,1) (4,1)
     "RRR"
-}

caminho :: Local -> Local -> String
caminho (x,y) (x1,y1) = comHorizontal ++ comVertical 
    where comHorizontal | x1 - x >= 0 = replicate (x1 -x) 'R'
                        | otherwise = replicate (abs(x1-x)) 'L'
          comVertical | y1 - y >= 0 = replicate (y1-y) 'D'
                      | otherwise = replicate (abs(y1-y)) 'U'


{- | A função alteralcaminho é a função responsável por tornar o caminho mais eficiente e muitas vezes possivel.

     == Exemplos de utilização:
     >>> alteralcaminho (1,1) "RR" "LL"
     "RRLL"
-}

alteralcaminho :: Local -> String -> String -> String
alteralcaminho _ [] [] = []
alteralcaminho _ x [] = x
alteralcaminho _ [] y = y
alteralcaminho (x,y) (h:ori) (v:ert) = let lhf = length (h:ori) + x
                                           lvf = length (v:ert) + y
                                       in if even lhf 
                                            then (ori ++ (v:ert)) ++ [h]
                                            else if even lvf
                                                then (ert ++ (h:ori)) ++ [v]
                                                else (h:ori) ++ (v:ert)

{- | A função caminhof é a função responsável criar o caminho melhor entre duas posições.

     == Exemplos de utilização:
     >>> caminhof (1,1) (3,3)
     "RRLL"
-}

caminhof :: Local -> Local -> String
caminhof (x,y) (x1,y1) = let str = caminho (x,y) (x1,y1)
                             (a,b) = span (\y -> y == 'L' || y == 'R') str
                         in alteralcaminho (x,y) a b

{- | A função objetivo é a função responsável por estabelecer o objetivo do bot.

     == Exemplos de utilização:
     >>> objetivo 10 9 (["#########","#       #","# #?#?# #","#  ?  ? #","#?# # #?#","# ?  ?  #","# #?#?# #","#  ??   #","#########"],[],["1 2 1","0 1 1"]) 0
     (3,4)
-}

objetivo :: Time -> Dim -> (Mapa,[Ipower],[Iplayer]) -> Int -> Local
objetivo tempo size (mapa,ipower,ip) id | tempo  <= (size-2)^2 + 3 = h 
                                        | not cPO = minimumADA $ map (\y -> (y, length (caminhof cstp y ))) dPO
                                        | length ip == 1 = coordIni id size
                                        | otherwise = matarJogador id ip
                                        where          
                                         h = head(dropWhile (\y -> coordMapa mapa y == '#') (reverse (criaT size (1,1) 2)))
                                         stp = head $ filter (\y -> head y == intToDigit id) ip
                                         cstp = coordPLayer stp
                                         cPO = all (\y -> coordMapa mapa y == '?') (map coordPLayer ipower)
                                         dPO = filter (\y -> coordMapa mapa y == ' ') (map coordPLayer ipower)



{- | A função nMovimentos conta o numero de ticks que o bot demora a chegar a outro sítio

     == Exemplos de utilização:
     >>> nMovimentos "DDRR" (1,1) ["#########","#       #","# #?#?# #","#  ?  ? #","#?# # #?#","# ?  ?  #","# #?#?# #","#  ??   #","#########","+ 5 2","+ 3 3","! 5 5"]
     13
-}


nMovimentos :: String -> Local -> Mapa -> Int
nMovimentos [] _ _ = 0
nMovimentos (h:mo) (x,y) mapa = let nh = coordCaminho (x,y) h
                                    u = coordMapa mapa nh
                                in if u == '?'
                                     then 10 + nMovimentos mo (x,y) mapa
                                     else 1 + nMovimentos mo (x,y) mapa

{- | A função escolherCam escolhe o caminho entre dois movimentos diferentes.

     == Exemplos de utilização:
     >>> escolherCam "RRDD" "DDRR" (1,1) ["#########","#       #","# #?#?# #","#  ?  ? #","#?# # #?#","# ?  ?  #","# #?#?# #","#  ??   #","#########","+ 5 2","+ 3 3","! 5 5"]
     "DDRR"
-}

escolherCam :: String -> String -> Local -> Mapa -> String
escolherCam mov mov2 (x,y) mapa = if nMovimentos mov (x,y) mapa <= nMovimentos mov2 (x,y) mapa
  then mov
  else mov2

{- | A função fazObjtivo decide o que fazer uma vez na posição pretendida.

     == Exemplos de utilização:
     >>> fazObjtivo 10 9 0 ["#########","#       #","# #?#?# #","#  ?  ? #","#?# # #?#","# ?  ?  #","# #?#?# #","#  ??   #","#########","+ 5 2","+ 3 3","! 5 5","0 1 1"]
     Nothing
-}

fazObjtivo :: Time -> Dim -> Int -> Mapa -> Maybe Char
fazObjtivo time size nj mapa | time <= (size-2)^2 + 3 = Nothing
                             | not cPO = Nothing
                             | allowBomba mapa nj /= [] = Just 'B'
                             | otherwise = Nothing
                             where 
                              stp = head $ filter (\y -> head y == intToDigit nj) mapa
                              ipower = filter (\y -> head y == '+' || head y == '!') mapa
                              cPO = all (\y -> coordMapa mapa y == '?') (map coordPLayer ipower) 

{- | A função matarJogador decide que jogador tem que matar conforme o seu identificador e responde as suas coordenadas.

     == Exemplos de utilização:
     >>> matarJogador 2 ["0 1 1","1 3 1","2 2 3", "3 5 5"]
     (1,1)
-}

matarJogador :: Int -> [Iplayer] -> Local
matarJogador z ip | z == 0 = mataZ z ip
                  | z == 1 = mata1 z ip
                  | z == 2 = mata2 z ip
                  | otherwise = mata3 z ip

{- | A função iden é responsável por dizer se o jogardor esta vivo e responder um par com as suas coordenadas e o ser estado.

     == Exemplos de utilização:
     >>> iden 1 ["0 1 1","1 3 1","2 2 3", "3 5 5"]
     ((3,1),True)
-}

iden :: Int -> [Iplayer] -> (Local,Bool)
iden z id = let u = filter (\h -> head h == intToDigit z) id
                cP = coordPLayer $ head u
            in if u /= []
                  then (cP,True)
                  else ((1,1),False)

{- | A função mataZ tem os alvos se o jogador do bot for o 0.
-}


mataZ :: Int -> [Iplayer] -> Local
mataZ z id | snd $ iden 1 id = fst $ iden 1 id
           | snd $ iden 3 id = fst $ iden 3 id
           | snd $ iden 2 id = fst $ iden 2 id

{- | A função mataZ tem os alvos se o jogador do bot for o 1.
-}


mata1 :: Int -> [Iplayer] -> Local
mata1 z id | snd $ iden 3 id = fst $ iden 3 id
           | snd $ iden 2 id = fst $ iden 2 id
           | snd $ iden 0 id = fst $ iden 0 id

{- | A função mataZ tem os alvos se o jogador do bot for o 2.
-}


mata2 :: Int -> [Iplayer] -> Local
mata2 z id | snd $ iden 0 id = fst $ iden 0 id
           | snd $ iden 1 id = fst $ iden 1 id
           | snd $ iden 3 id = fst $ iden 3 id

{- | A função mataZ tem os alvos se o jogador do bot for o 3.
-}


mata3 :: Int -> [Iplayer] -> Local
mata3 z id | snd $ iden 2 id = fst $ iden 2 id
           | snd $ iden 0 id = fst $ iden 0 id
           | snd $ iden 1 id = fst $ iden 1 id

{- | A função coordIni devolve as coordenadas iniciais do bot.

      == Exemplos de utilização:
     >>> coordIni 1 9
     (7,1)
-}


coordIni :: Int -> Int -> Local
coordIni nj size | nj == 0 = (1,1)
                 | nj == 1 = (size-2,1)
                 | nj == 2 = (1,size-2)
                 | otherwise = (size-2,size-2)

{- | A função movimento é a função responsável por levar o bot a executar o seu objetivo.
-}

movimento :: World -> Int -> Int -> Local -> [Local] -> Maybe Char
movimento (a,b,c,d) player ticks (x,y) lexp | (x,y) == (x1,y1) = fazObjtivo ticks (length $ head a) player (groupMap (a,b,c,d))
                                            | timeCheck ticks size && ncor == (x,y) = movimentar a (head mov) (x,y)
                                            | escolherCam mov mov2 (x,y) a == mov = if elem nc lexp || elem nc1 lexp
                                                                                        then Nothing
                                                                                        else if coordMapa a (coordCaminho (x,y) (head mov)) == '#' && coordMapa a (coordCaminho (x,y) (head mov2)) == '#' && allowBomba (groupMap (a,b,c,d)) player /= []
                                                                                            then Just 'B'
                                                                                            else if coordMapa a (coordCaminho (x,y) (head mov)) == '#' && coordMapa a (coordCaminho (x,y) (head mov2)) == '#'
                                                                                                then Nothing
                                                                                                else if coordMapa a (coordCaminho (x,y) (head mov)) == '#'
                                                                                                        then movimentar a (head mov2) (x,y)
                                                                                                        else movimentar a (head mov) (x,y)
                                            | escolherCam mov mov2 (x,y) a == mov2 = if elem nc1 lexp || elem nc lexp
                                                                                          then Nothing
                                                                                          else if coordMapa a (coordCaminho (x,y) (head mov2)) == '#' && coordMapa a (coordCaminho (x,y) (head mov2)) == '#' && allowBomba (groupMap (a,b,c,d)) player /= []
                                                                                            then Just 'B'
                                                                                            else if coordMapa a (coordCaminho (x,y) (head mov)) == '#' && coordMapa a (coordCaminho (x,y) (head mov2)) == '#'
                                                                                                then Nothing
                                                                                                else if coordMapa a (coordCaminho (x,y) (head mov2)) == '#'
                                                                                                        then movimentar a (head mov) (x,y)
                                                                                                        else movimentar a (head mov2) (x,y)
                                            | otherwise = Nothing
                                            where (x1,y1) = objetivo ticks (length $ head a) (a,b,d) player
                                                  (mov) = caminhof (x,y) (x1,y1)
                                                  (mov2) = invertCaminho (mov)
                                                  nc = coordCaminho (x,y) (head mov)
                                                  nc1 = coordCaminho (x,y) (head mov2)
                                                  size = length $ head a
                                                  ncor = (head $ (drop ((timeplay ticks size) +1) (criaT size (1,1) 2)))       

{- | A função movimentar é a função que movimenta o bom
-}

movimentar :: Mapa -> Char -> Local -> Maybe Char
movimentar mapa char (x,y) | char == 'R' = mov mapa char (x+1,y)
                           | char == 'L' = mov mapa char (x-1,y)
                           | char == 'U' = mov mapa char (x,y-1)
                           | char == 'D' = mov mapa char (x,y+1)

{- | A função mov é uma função auxiliar da movimentar.
-}

mov :: Mapa -> Char -> Local -> Maybe Char
mov mapa char (x,y) = if coordMapa mapa (x,y) == '?'
  then Just 'B'
  else if coordMapa mapa (x,y) == ' '
    then Just char
    else Nothing

{- | A função verRaid ve o raio de uma bomba.
-}

verRaid :: Ibomb -> Raio
verRaid bomba = read ((words bomba) !! 4) :: Int

{- | A função inforBomb transforma um lista de bombas numca lista do tipo [(Local,Int)].

     == Exemplos de utilização:
     >>> infoBomb ["* 1 1 0 2 10","* 2 3 1 1 10"]
     [((1,1),2),((2,3),1)]
-}

infoBomb :: [Ibomb] -> [((Int,Int),Int)] 
infoBomb ibombas = map (\y -> ((coordPLayer y),verRaid y)) ibombas
                 
{- | A função splitexplosionBot dado o centro de uma bomba e o seu raid devolver as coordenadas das explusões.
-}

splitexplosionBot :: Epicentro -> Raio -> Mapa -> [Ipower] -> Lexplo
splitexplosionBot centro raio mapa coo = let eu = checkraid3 (explosionU centro (raio+1) 0) mapa coo
                                             ed = checkraid3 (explosionD centro raio 0) mapa coo
                                             el = checkraid3 (explosionL centro raio 0) mapa coo
                                             er = checkraid3 (explosionR centro raio 0) mapa coo
                                         in eu ++ ed ++ el ++ er

{- | A função explosaoTotal é responsável por calcular as coordenadas das explusões de todas as bombas no mapa.
-}

explosaoTotal :: Mapa -> [Ipower] -> Lexplo -> [Raio] -> Lexplo
explosaoTotal _ _ [] [] = []
explosaoTotal mapa power (bomba:bombas) (ratual:raios) = splitexplosionBot bomba ratual mapa power ++ explosaoTotal mapa power bombas raios

{- | A função explosaoSum é responsável por calcular as coordenadas das explusões de todas as bombas no mapa.
-}

explodeSum :: World -> [(Int,Int)]
explodeSum (a,b,c,d) = let infoB = infoBomb c
                           raios = map (snd) infoB
                           coord = map (fst) infoB
                       in explosaoTotal a b coord raios

{- | A função ativarShield é a função que toma decisão sobre o que fazer quando o bot esta dentro do raio de uma explusão.
-}

ativarShield :: World -> [Local] -> Local -> Time -> Int -> Maybe Char
ativarShield (mapa,b,c,d) lexplo (x,y) ticks nj = let size = length $ head mapa
                                                      lprot = shieldBot 5 (x-1,y-1) 0 []
                                                      h = head(dropWhile (\y -> coordMapa mapa y == '#') (reverse (criaT size (1,1) 2)))
                                                      (mov) = caminhof (x,y) h
                                                      (mov2) = invertCaminho (mov)
                                                  in if ticks <= 5 && (x,y) /= h
                                                       then movimentar mapa (head(escolherCam mov mov2 (x,y) mapa)) (x,y)
                                                       else if ticks <= 5 && allowBomba (groupMap (mapa,b,c,d)) nj /= []
                                                             then Just 'B'
                                                             else if ticks <= 5
                                                              then Nothing
                                                              else if timeCheck ticks size == False
                                                                   then solve mapa lexplo lprot (x,y) 1
                                                                   else solve2  mapa (criaT size (1,1) 2)  lexplo lprot (x,y) 1 ticks

{- | A função minimumADA calcula o par com a minima segunda componemte e devolve a primeira componente desse par.

     == Exemplos de utilização:
     >>> minimumADA [((1,1),2),((2,2),4)]
     (1,1)
-}

minimumADA :: [(Local,Int)] -> Local
minimumADA [(a,b)] = a
minimumADA (h:t:l) = if snd h <= snd t then minimumADA (h:l) else minimumADA (t:l)

{- | A função maximumADA calcula o par com a maxima segunda componemte e devolve a primeira componente desse par.

     == Exemplos de utilização:
     >>> maximumADA [((1,1),2),((2,2),4)]
     (2,2)
-}

maximumADA :: [(Local,Int)] -> Local
maximumADA [(a,b)] = a
maximumADA (h:t:l) = if snd h >= snd t then maximumADA (h:l) else maximumADA (t:l)


{- | A função solve toma a decisão para o bot se desviar da bomba sem a espiral influenciar essa decisão.
-}

solve :: Mapa -> [Local] -> [Local] -> Local -> Raio -> Maybe Char
solve mapa lexplo lprot (x,y) linha = let lfree = filter (\u -> (not(elem u lexplo)) && (coordMapa mapa u) == ' ' ) lprot
                                          lfree2 = filter (\k -> elem k (verificaTotal mapa (x,y) k) ) lfree
                                          dist = minimumADA $ map (\c -> (c,length (caminhof (x,y) c))) lfree2
                                          size = length (head mapa)
                                          (h:mov) = caminhof (x,y) dist
                                          shield2 = filter (\(a,b) -> a >= 0 && b >= 0 && a <= (size - 1) && b <= (size -1)) (shieldBot (5+2*linha) (x-1-linha,y-1-linha) 0 [])
                                      in if (1+2*linha) >= size
                                            then Nothing
                                            else if  lfree == [] || lfree2 == [] 
                                                    then solve mapa lexplo shield2 (x,y) (linha+1)
                                                    else movimentar mapa h (x,y)

{- | A função solve2 toma a decisão para o bot se desviar da bomba com a espiral influenciar essa decisão.
-}

solve2 :: Mapa -> [Local] -> [Local] -> [Local] -> Local -> Raio -> Time -> Maybe Char
solve2 mapa esp lexplo lprot (x,y) linha ticks = let lfree = filter (\u -> (not(elem u lexplo)) && (coordMapa mapa u) == ' ' ) lprot
                                                     lfree2 = filter (\k -> elem k (verificaTotal mapa (x,y) k) ) lfree
                                                     size = length (head mapa)
                                                     lespiral = (drop (timeplay ticks size) esp)
                                                     valor = maximumADA $ map (\y -> ( y , maybeToNumber (elemIndex y lespiral))) lfree2
                                                     (h:mov) = caminhof (x,y) valor
                                                     shield2 = filter (\(a,b) -> a >= 0 && b >= 0 && a <= (size - 1) && b <= (size -1)) (shieldBot (5+2*linha) (x-1-linha,y-1-linha) 0 [])
                                                 in if (1+2*linha) >= size
                                                       then Nothing
                                                       else if  lfree == [] || lfree2 == [] 
                                                               then solve2 mapa esp lexplo shield2 (x,y) (linha+1) ticks
                                                               else movimentar mapa h (x,y)

{- | A função verificaTotal calcula as coordenadas pelas quais o bot vai passar, juntando todas as suas auxiliares.
-}

verificaTotal :: Mapa -> Local -> Local -> [Local]
verificaTotal mapa (x,y) (x1,y1) = let u = verLinhas (x,y) (x1,y1)
                                       cf = lastver mapa u
                                   in cf

{- | A função lastver pega nas coordenadas originadas pelo caminho e ve até onde o bot pode chegar.

     == Exemplos de utilização:
     >>> lastver ["#########","#       #","# #?#?# #","#  ?  ? #","#?# # #?#","# ?  ?  #","# #?#?# #","#  ??   #","#########","+ 5 2","+ 3 3","! 5 5"] [(2,1),(3,1),(2,1)]
     [(2,1),(3,1),(2,1)]
-}

lastver :: Mapa -> [Local] -> [Local]
lastver mapa [] = []
lastver mapa (h:t) = if coordMapa mapa h == ' '
  then h : lastver mapa t
  else []

{- | A função verLinha calcula a coordenadas dos movimentos entre duas posições

     == Exemplos de utilização:
     >>> verLinhas (1,1) (3,2)
     [(2,1),(3,1),(3,2)]
-}

verLinhas :: Local -> Local -> [Local]
verLinhas a b = let cam = caminhof a b
                in transCam a cam

{- | A função coordCaminho calcula as novas coordenadas após um movimento.

     == Exemplos de utilização:
     >>> coordCaminho (1,1) 'R'
     (2,1)
-}

coordCaminho :: Local -> Char -> Local
coordCaminho (x,y) char | char == 'R' = (x+1,y) 
                        | char == 'L' = (x-1,y)
                        | char == 'U' = (x,y-1)
                        | char == 'D' = (x,y+1)

{- | A função transCam calcula as coordenadas por onde o bot vai passar dado um caminho.

     == Exemplos de utilização:
     >>> transCam (1,1) "RRL"
     [(2,1),(3,1),(2,1)]
-}

transCam :: Local -> String -> [Local]
transCam _ [] = []
transCam (x,y) (h:t) = coordCaminho (x,y) h : transCam (coordCaminho (x,y) h) t

{- | A função maybeToNumber transforma um Maybe Int num Int

     == Exemplos de utilização:
     >>> maybeToNumber (Just 2)
     2
-}

maybeToNumber :: Maybe Int -> Int
maybeToNumber (Just a) = a




