{- |
   Module      : Main
   Description : Função pela representação gráfica do jogo.
   Copyright   : Diogo José Cruz Sobral
                 João Pedro Rodrigues Gomes

   Com esta documentação pretendemos que se torne mais fácil a leitura e o entendimento do nosso código para fazer o jogo "Bomberman".
   Aqui fica um pequeno resumo da lógica usada para fazer o jogo, e mais à frente abordamos todas as funções uma a uma.
   Esta tarefa é a responsável pela componente gráfica do bot e por isso tem um papel muito importante no jogo sendo que esta componemte é muito importante.
-}

module Main where

import Tarefa6_li1g048
import System.Random
import BombermanCode
import Data.Char
import Graphics.Gloss
import Graphics.Gloss.Data.Picture
import Graphics.Gloss.Interface.Pure.Game

{- | Lista de Strings do Mapa.
-}
type Map = [String]
{- | Lista de Strings de poderes.
-}
type Poderes = [String]
{- | Lista de Strings de bombas.
-}
type Bombs = [String]
{- | Lista de Strings de Jogadores.
-}
type Jodadores = [String]
{- | Estado de jogo utilizado pela função play.
-}
type Estado = (Map,Int,Int,Int,Picture,Picture,Picture,Picture,Picture,Picture,Picture,Picture,Picture,Picture,(Picture,Picture,Picture,Picture,Picture,Picture,Picture),(Picture,Picture,Picture,Picture,Picture),(Picture,Picture,Picture,Picture,Picture),(Picture,Picture,Picture,Picture,Picture),(Picture,Picture,Picture,Picture,Picture),[String],[String],(Bool,Picture),(Bool,Picture),Int,(Bool,Picture),Int,(Picture,Picture,Picture,Picture,Picture,Picture,Picture,Picture,Picture,Picture),Int,(Bool,Picture),Picture,(Picture,Picture,Picture,Picture),Map,Int)
{- | Novo tipo de coordenada com (Char,Int,Int)
-}
type Char1 = (Char,Int,Int)


{- | A função estado inicial é a função responsável por criar o estado inicial do jogo e recebe como inputs uma seed para o jogo e todos os BMP que foram usados.
-}
estadoInicial :: Picture -> Picture -> Picture -> Picture -> Picture -> Picture -> Picture -> Picture -> Picture -> Picture -> Picture -> Picture -> Picture -> Picture -> Picture -> Picture -> Picture -> Picture -> Picture -> Picture -> Picture -> Picture -> Picture -> Picture -> Picture -> Picture -> Picture -> Picture -> Picture -> Picture -> Picture -> Picture -> Picture -> Picture -> Picture -> Picture -> Picture -> Picture -> Picture -> Picture -> Picture -> Picture -> Picture -> Picture -> Picture -> Picture -> Picture -> Picture -> Picture -> Picture -> Picture -> Picture -> Picture -> Picture -> Picture -> Picture -> Int -> Estado
estadoInicial car bomb tijol back pbomb pfire n1 n2 n3 n4 u1 l1 l2 d1 r1 e1 c1 n1d n1l n1r n1u n1b n2d n2l n2r n2u n2b n3d n3l n3r n3u n3b n4d n4l n4r n4u n4b menu1 menu1aux menu2 num0 num1 num2 num3 num4 num5 num6 num7 num8 num9 menu3 menugame str1 str2 str3 str4 seed = ([],0,0,0,car,bomb,tijol,back,pbomb,pfire,n1,n2,n3,n4,(u1,l1,l2,d1,r1,e1,c1),(n1d,n1l,n1r,n1u,n1b),(n2d,n2l,n2r,n2u,n2b),(n3d,n3l,n3r,n3u,n3b),(n4d,n4l,n4r,n4u,n4b),[],[],(True,menu1),(True,menu1aux),0,(True,menu2),0,(num0,num1,num2,num3,num4,num5,num6,num7,num8,num9),0,(True,menu3),menugame,(str1,str2,str3,str4),[],seed)


{- | A função stringjogadores é a funçao que dado um numero de jogado e o tamanho do map responde as strings iniciais desses jogadores.
  	== Exemplos de utilização:

  	>>> stringjogadores 4 0 9
  	["0 1 1","1 7 1","2 1 7","3 7 7"]
-}

stringjogadores :: Int -> Int -> Int -> Jodadores
stringjogadores nj ja size | nj == ja = []
                           | otherwise = escrevestr ja size : stringjogadores nj (ja+1) size

{- | A função auxiliar da stringjogadores que dado o tamnho do mapa e o numero do jogador cria a string inicial desse jogador.
  	== Exemplos de utilização:

  	>>> escrevestr 0 9
  	"0 1 1"
-}

escrevestr :: Int -> Int -> String
escrevestr nj size | nj == 0 = "0" ++ " 1 1"
                   | nj == 1 = "1 " ++ (show (size-2)) ++ " 1"
                   | nj == 2 = "2 1 " ++ (show (size-2))
                   | otherwise = "3" ++ " " ++ (show (size-2)) ++ " " ++ (show (size-2))

{- | A função responsável por criar o retângolo onde é mostrado o tempo do jogo.
-}

retangolo :: Float -> Picture
retangolo size = let a = (800 - size)
                 in Polygon (rectanglePath a 70)

{- | Cor usada para o painel do Tempo.
-}

color1 :: Color
color1 = (makeColorI 253 225 0 255)

{- | A função tampic calculo o tamanho maximo que um elemento do mapa pode ocupar.
  	== Exemplos de utilização:

  	>>> tampic 9
  	66.0
-}

tampic :: Int -> Float
tampic size = fromIntegral(div 600 size)

{- | A função transformT é a responsável por transformar as listas de strings do mapa numa nova lista com o tipo (Char,Int,Int).
  	== Exemplos de utilização:

  	>>> transformaT ["#########","#       #","# #?#?# #","#  ?  ? #","#?# # #?#","# ?  ?  #","# #?#?# #","#  ??   #","#########"] 0
  	[('#',0,0),('#',1,0),('#',2,0),('#',3,0),('#',4,0),('#',5,0),('#',6,0),('#',7,0),('#',8,0),('#',0,1),(' ',1,1),(' ',2,1),(' ',3,1),(' ',4,1),(' ',5,1),(' ',6,1),(' ',7,1),('#',8,1),('#',0,2),(' ',1,2),('#',2,2),('?',3,2),('#',4,2),('?',5,2),('#',6,2),(' ',7,2),('#',8,2),('#',0,3),(' ',1,3),(' ',2,3),('?',3,3),(' ',4,3),(' ',5,3),('?',6,3),(' ',7,3),('#',8,3),('#',0,4),('?',1,4),('#',2,4),(' ',3,4),('#',4,4),(' ',5,4),('#',6,4),('?',7,4),('#',8,4),('#',0,5),(' ',1,5),('?',2,5),(' ',3,5),(' ',4,5),('?',5,5),(' ',6,5),(' ',7,5),('#',8,5),('#',0,6),(' ',1,6),('#',2,6),('?',3,6),('#',4,6),('?',5,6),('#',6,6),(' ',7,6),('#',8,6),('#',0,7),(' ',1,7),(' ',2,7),('?',3,7),('?',4,7),(' ',5,7),(' ',6,7),(' ',7,7),('#',8,7),('#',0,8),('#',1,8),('#',2,8),('#',3,8),('#',4,8),('#',5,8),('#',6,8),('#',7,8),('#',8,8)]
-}

transformaT :: Map -> Int -> [Char1]
transformaT [] _ = []
transformaT (h:t) lin = tranforma h (0,lin) ++ transformaT t (lin+1)

{- | A função tranforma é auxiliar da transforMat que transformar um linha do mapa numa nova lista com o tipo (Char,Int,Int).
  	== Exemplos de utilização:

  	>>> tranforma "#######" (0,0)
  	[('#',0,0),('#',1,0),('#',2,0),('#',3,0),('#',4,0),('#',5,0),('#',6,0)]
-}

tranforma :: String -> (Int,Int) -> [Char1]
tranforma [] _ = []
tranforma (h:t) (a,b) = (h,a,b) : tranforma t (a+1,b)

{- | A função topicT é a responvável por tranforma o tipo Char1 em (Picture,Int,Int) conforme o char da primeira compunente.
-}

topicT :: [Char1] -> Picture -> Picture -> Picture -> [(Picture,Int,Int)]
topicT l car tijol back = map (\l -> charTopic l car tijol back) l

{- | A função charTopic é a auxiliar da topicT e é a responvável por tranformar um tipo Char1 em (Picture,Int,Int) conforme o char da primeira compunente.
-}

charTopic :: (Char1) -> Picture -> Picture -> Picture -> (Picture,Int,Int)
charTopic (a,b,c) car t ba | a == '#' = (car,b,c)
                           | a == ' ' = (ba,b,c)
                           | otherwise = (t,b,c)

{- | A função todaPic é a responvável por colocar as figuras na sua devida posição conforme as suas coordenadas
-}

todaPic :: [(Picture,Int,Int)] -> Float -> [Picture]
todaPic [] _ = []
todaPic ((a,b,c):t) size = let u = size / 2
                               b1 = (-400 + u) + (size*(fromIntegral b))
                               c1 = (300-u) - (size * (fromIntegral c))
                           in (Translate b1 c1 a) : todaPic t size

{- | A função explosionUfla é a responvável por transformar as coordenadas das explusões para cima das bombas em figuras.
-}

explosionUfla :: Lexplo -> Picture -> Picture -> Picture -> Int -> [(Picture,Int,Int)]
explosionUfla [] _ _ _ _ = []
explosionUfla [(a,b)] u1 l1 c1 0 = [(c1,a,b)]
explosionUfla [(a,b)] u1 l1 c1 z = [(u1,a,b)]
explosionUfla ((a,b):t) u1 l1 c1 z | z == 0 = (c1,a,b) : explosionUfla t u1 l1 c1 (z+1)
                                   | otherwise = (l1,a,b) : explosionUfla t u1 l1 c1 (z+1)

{- | A função explosionLfla é a responvável por transformar as coordenadas das explusões das bombas em figuras.
-}

explosionLfla :: Lexplo -> Picture -> Picture -> [(Picture,Int,Int)]
explosionLfla [] _ _  = []
explosionLfla [(a,b)] e1 l2 = [(e1,a,b)]
explosionLfla ((a,b):t) e1 l2 = (l2,a,b) : explosionLfla t e1 l2

{- | A função splitexplosionad é a responvável criar a explusão de uma bomba dado o raio e o centro da explusão.
-}

splitexplosionad :: Epicentro -> Raio -> Mapa -> Picture -> Picture -> Picture -> Picture -> Picture -> Picture -> Picture -> Poderes -> [(Picture,Int,Int)]
splitexplosionad centro raio mapa u1 l1 l2 d1 r1 e1 c1 poder = let eu = explosionUfla (checkraid3 (explosionU centro (raio+1) 0) mapa poder) u1 l1 c1 0
                                                                   ed = explosionLfla (checkraid3 (explosionD centro raio 0) mapa poder) d1 l1
                                                                   el = explosionLfla (checkraid3 (explosionL centro raio 0) mapa poder) e1 l2
                                                                   er = explosionLfla (checkraid3 (explosionR centro raio 0) mapa poder) r1 l2
                                                                in eu ++ ed ++ el ++ er

{- | A função singleflame é a responsável por passar à splitexplosionad toda a informação necessária para criar o efeito da explusão.
-}

singleflame :: String -> [String] -> Picture -> Picture -> Picture -> Picture -> Picture -> Picture -> Picture -> Poderes -> [(Picture,Int,Int)]
singleflame bomba mapa u1 l1 l2 d1 r1 e1 c1 poder = let raio = read ((words bomba) !! 4) :: Int
                                                        x = read ((words bomba) !! 1) :: Int
                                                        y = read ((words bomba) !! 2) :: Int
                                                    in splitexplosionad (x,y) raio mapa u1 l1 l2 d1 r1 e1 c1 poder

{- | A função flamesbombas é a responvável criar a explusão todas as bombas.
-}

flamesbombas :: [String] -> [String] -> Picture -> Picture -> Picture -> Picture -> Picture -> Picture -> Picture -> Poderes -> [(Picture,Int,Int)]
flamesbombas [] _ _ _ _ _ _ _ _ _ = []
flamesbombas (h:t) mapa u1 l1 l2 d1 r1 e1 c1 poder = singleflame h mapa u1 l1 l2 d1 r1 e1 c1 poder ++ flamesbombas t mapa u1 l1 l2 d1 r1 e1 c1 poder

{- | A função tranformaBomba é a responvável por separar as bombas que vão expludir das que vão ficar paradas.
-}

tranformaBomba :: [String] -> [String] -> Picture -> Picture -> Picture -> Picture -> Picture -> Picture -> Picture -> Picture -> Poderes -> [(Picture,Int,Int)]
tranformaBomba bombas mapa ibomba u1 l1 l2 d1 r1 e1 c1 poder = let w = filter (\h -> (words h !! 5) /= "0" ) bombas
                                                                   k = filter (\h -> (words h !! 5) == "0" ) bombas
                                                                   u = map (coordPLayer) w
                                                               in  (map (\(a,b) -> (ibomba,a,b)) u) ++ flamesbombas k mapa u1 l1 l2 d1 r1 e1 c1 poder

{- | A função drawPower é a responvável verficiar se o powerup deve ser representado ou não conforme esta tapado ou não.
-}

drawPower :: Map -> [(Int,Int)] -> Picture -> [(Picture,Int,Int)]
drawPower a [] pic = []
drawPower a ((c,b):t) pic = let u = coordMapa a (c,b)
                            in if u == ' ' then (pic,c,b) : drawPower a t pic else drawPower a t pic

{- | A função drawPlayer é a responvável transformar as coordenadas de todos os jogadores nos respetivos tipos (Picture,Int,Int).
-}

drawPlayer :: Jodadores -> Picture -> Picture -> Picture -> Picture -> Picture -> Picture -> Picture -> Picture -> Bombs -> [(Picture,Int,Int)]
drawPlayer [] _ _ _ _ _ _ _ _ _ = []
drawPlayer (h:t) p1 p2 p3 p4 p1b p2b p3b p4b bombs = let (a,b) = coordPLayer h
                                                         u = head h
                                                         op = elem (coordPLayer h) (map (coordPLayer) bombs)
                                                     in if op == True
                                                      then drawSingle u (a,b) p1b p2b p3b p4b : drawPlayer t p1 p2 p3 p4 p1b p2b p3b p4b bombs
                                                      else drawSingle u (a,b) p1 p2 p3 p4 : drawPlayer t p1 p2 p3 p4 p1b p2b p3b p4b bombs

{- | A função drawSingle é a responvável transformar as coordenadas de um jogador no respetivo tipo (Picture,Int,Int).
-}

drawSingle :: Char -> (Int,Int) -> Picture -> Picture -> Picture -> Picture -> (Picture,Int,Int)
drawSingle u (a,b) p1 p2 p3 p4 | u == '0' = (p1,a,b)
                               | u == '1' = (p2,a,b)
                               | u == '2' = (p3,a,b)
                               | otherwise = (p4,a,b)

{- | A função scoreboard é a responsável por representar as imagens dos jogadores que ainda estão vivos debaixo do tempo do jogo.
-}

scoreboard :: [String] -> Int -> (Picture,Picture,Picture,Picture) -> [Picture]
scoreboard [] _ _ = []
scoreboard (h:t) z img = let player = digitToInt (head h)
                             imgn = dono player img
                             alt = fromIntegral(140 - (50 * z))
                         in  (Translate 275 alt (scale 1.25 1.25 imgn)) : scoreboard t (z+1) img

{- | A função dono é a responvável identificar qual a imagem a usar no scoreboard.
-}

dono :: Int -> (Picture,Picture,Picture,Picture) -> Picture
dono z (a,b,c,d) | z == 0 = a
                 | z == 1 = b
                 | z == 2 = c
                 | otherwise = d

{- | A função desenhaNumero é a responvável colocar os números do tempo na posição correta para serem representados.
-}

desenhaNumero :: [Picture] -> Int -> [Picture]
desenhaNumero [] _ = []
desenhaNumero (h:t) x = (Translate a 0 h) : desenhaNumero t (x+1) where a =fromIntegral ((35)*x - 30)

{- | A função charToNumber é a responvável por transformar um número na sua respetiva imagem.
-}

charToNumber :: Char -> Picture -> Picture -> Picture -> Picture -> Picture -> Picture -> Picture -> Picture -> Picture -> Picture -> Picture
charToNumber '0' num0 num1 num2 num3 num4 num5 num6 num7 num8 num9 = num0
charToNumber '1' num0 num1 num2 num3 num4 num5 num6 num7 num8 num9 = num1
charToNumber '2' num0 num1 num2 num3 num4 num5 num6 num7 num8 num9 = num2
charToNumber '3' num0 num1 num2 num3 num4 num5 num6 num7 num8 num9 = num3
charToNumber '4' num0 num1 num2 num3 num4 num5 num6 num7 num8 num9 = num4
charToNumber '5' num0 num1 num2 num3 num4 num5 num6 num7 num8 num9 = num5
charToNumber '6' num0 num1 num2 num3 num4 num5 num6 num7 num8 num9 = num6
charToNumber '7' num0 num1 num2 num3 num4 num5 num6 num7 num8 num9 = num7
charToNumber '8' num0 num1 num2 num3 num4 num5 num6 num7 num8 num9 = num8
charToNumber '9' num0 num1 num2 num3 num4 num5 num6 num7 num8 num9 = num9

{- | A função desenhaTempo é a responvável desenhar o número escrito pelo o utilizador no menu de escolher o tamanho do mapa.
-}

desenhaTempo :: Estado -> Picture
desenhaTempo (gera,size,time,segp,car,bomb,tijol,back,pbomb,pfire,p1,p2,p3,p4,flames,player1,player2,player3,player4,exbom,expower,(menu1status,menu1),(menu1auxstatus,menu1aux),timermenu,(menu2status,menu2),dim,(num0,num1,num2,num3,num4,num5,num6,num7,num8,num9),nj,(menu3status,menu3),menugame,str,mapa2,seed) = pictures (desenhaNumero (map (\y -> charToNumber y num0 num1 num2 num3 num4 num5 num6 num7 num8 num9 ) (show dim)) 0)

{- | A função desenhaTempo2 é a responvável desenhar o número escrito pelo o utilizador no menu de escolher o número de jogadores
-}

desenhaTempo2 :: Estado -> Picture
desenhaTempo2 (gera,size,time,segp,car,bomb,tijol,back,pbomb,pfire,p1,p2,p3,p4,flames,player1,player2,player3,player4,exbom,expower,(menu1status,menu1),(menu1auxstatus,menu1aux),timermenu,(menu2status,menu2),dim,(num0,num1,num2,num3,num4,num5,num6,num7,num8,num9),nj,(menu3status,menu3),menugame,str,mapa2,seed) = pictures (desenhaNumero (map (\y -> charToNumber y num0 num1 num2 num3 num4 num5 num6 num7 num8 num9 ) (show nj)) 0)


{- | A função desenhaMenu é a responvável desenhar os menus e alterar os estados dos mesmo para o jogo começar.
-}

desenhaMenu1 :: Estado -> Picture
desenhaMenu1 (gera,size,time,segp,car,bomb,tijol,back,pbomb,pfire,p1,p2,p3,p4,flames,player1,player2,player3,player4,exbom,expower,(menu1status,menu1),(menu1auxstatus,menu1aux),timermenu,(menu2status,menu2),dim,numeros,nj,(menu3status,menu3),menugame,str,mapa2,seed)  | menu1status && menu1auxstatus = pictures ([menu1] ++ [(Translate 0 (-100) menu1aux)])
                                                                                                                                                                                                                                                                            | menu1status = menu1
                                                                                                                                                                                                                                                                            | menu3status = pictures ([menu3] ++ [desenhaTempo2 (gera,size,time,segp,car,bomb,tijol,back,pbomb,pfire,p1,p2,p3,p4,flames,player1,player2,player3,player4,exbom,expower,(menu1status,menu1),(menu1auxstatus,menu1aux),timermenu,(menu2status,menu2),dim,numeros,nj,(menu3status,menu3),menugame,str,mapa2,seed)] )
                                                                                                                                                                                                                                                                            | menu2status = pictures ([menu2] ++ [desenhaTempo (gera,size,time,segp,car,bomb,tijol,back,pbomb,pfire,p1,p2,p3,p4,flames,player1,player2,player3,player4,exbom,expower,(menu1status,menu1),(menu1auxstatus,menu1aux),timermenu,(menu2status,menu2),dim,numeros,nj,(menu3status,menu3),menugame,str,mapa2,seed)] )
                                                                                                                                                                                                                                                                            | otherwise = desenhaEstado (gera,size,time,segp,car,bomb,tijol,back,pbomb,pfire,p1,p2,p3,p4,flames,player1,player2,player3,player4,exbom,expower,(menu1status,menu1),(menu1auxstatus,menu1aux),timermenu,(menu2status,menu2),dim,numeros,nj,(menu3status,menu3),menugame,str,mapa2,seed)

{- | A função desenhaEstado é a função mais importante desta tarefa e é a responsável por dar vida à parte grafica do jogo.
-}

desenhaEstado :: Estado -> Picture
desenhaEstado (gera,size,time,segp,car,bomb,tijol,back,pbomb,pfire,p1,p2,p3,p4,(u1,l1,l2,d1,r1,e1,c1),(n1d,n1l,n1r,n1u,n1b),(n2d,n2l,n2r,n2u,n2b),(n3d,n3l,n3r,n3u,n3b),(n4d,n4l,n4r,n4u,n4b),exbom,expower,(menu1status,menu1),(menu1auxstatus,menu1aux),timermenu,(menu2status,menu2),dim,(num0,num1,num2,num3,num4,num5,num6,num7,num8,num9),nj,(menu3status,menu3),menugame,str,mapa2,seed) = let (a,b,c,d) = splitMap gera
                                                                                                                                                                                                                                                                                                                                                                                                      tpic = tampic size
                                                                                                                                                                                                                                                                                                                                                                                                      sca = (tpic / 45)
                                                                                                                                                                                                                                                                                                                                                                                                      carscaled = scale sca sca car
                                                                                                                                                                                                                                                                                                                                                                                                      bombscaled = scale sca sca bomb
                                                                                                                                                                                                                                                                                                                                                                                                      tijolscaled = scale sca sca tijol
                                                                                                                                                                                                                                                                                                                                                                                                      backscaled = scale sca sca back
                                                                                                                                                                                                                                                                                                                                                                                                      pbombscaled = scale sca sca pbomb
                                                                                                                                                                                                                                                                                                                                                                                                      pfirescaled = scale sca sca pfire
                                                                                                                                                                                                                                                                                                                                                                                                      p1scaled = scale sca sca p1
                                                                                                                                                                                                                                                                                                                                                                                                      p2scaled = scale sca sca p2
                                                                                                                                                                                                                                                                                                                                                                                                      p3scaled = scale sca sca p3
                                                                                                                                                                                                                                                                                                                                                                                                      p4scaled = scale sca sca p4
                                                                                                                                                                                                                                                                                                                                                                                                      n1bscaled = scale sca sca n1b
                                                                                                                                                                                                                                                                                                                                                                                                      n2bscaled = scale sca sca n2b
                                                                                                                                                                                                                                                                                                                                                                                                      n3bscaled = scale sca sca n3b
                                                                                                                                                                                                                                                                                                                                                                                                      n4bscaled = scale sca sca n4b
                                                                                                                                                                                                                                                                                                                                                                                                      nu1 = scale sca sca u1
                                                                                                                                                                                                                                                                                                                                                                                                      nl1 = scale sca sca l1
                                                                                                                                                                                                                                                                                                                                                                                                      nl2 = scale sca sca l2
                                                                                                                                                                                                                                                                                                                                                                                                      nd1 = scale sca sca d1
                                                                                                                                                                                                                                                                                                                                                                                                      nr1 = scale sca sca r1
                                                                                                                                                                                                                                                                                                                                                                                                      ne1 = scale sca sca e1
                                                                                                                                                                                                                                                                                                                                                                                                      nc1 = scale sca sca c1
                                                                                                                                                                                                                                                                                                                                                                                                      na =  transformaT a 0
                                                                                                                                                                                                                                                                                                                                                                                                      switchpit = topicT na carscaled tijolscaled backscaled
                                                                                                                                                                                                                                                                                                                                                                                                      mapafigurado = pictures (todaPic switchpit tpic)
                                                                                                                                                                                                                                                                                                                                                                                                      bombasfiguradas = pictures (todaPic (tranformaBomba exbom mapa2 bombscaled nu1 nl1 nl2 nd1 nr1 ne1 nc1 expower) tpic)
                                                                                                                                                                                                                                                                                                                                                                                                      (k,w) = span (\y -> head y == '+') b
                                                                                                                                                                                                                                                                                                                                                                                                      bombfigurada = pictures (todaPic (drawPower a (map coordPLayer k) pbombscaled) tpic)
                                                                                                                                                                                                                                                                                                                                                                                                      firefigurada = pictures (todaPic (drawPower a (map coordPLayer w) pfirescaled) tpic)
                                                                                                                                                                                                                                                                                                                                                                                                      playerdraw = pictures (todaPic (drawPlayer d p1scaled p2scaled p3scaled p4scaled n1bscaled n2bscaled n3bscaled n4bscaled c) tpic)
                                                                                                                                                                                                                                                                                                                                                                                                      drawTime = (Translate 300 200 (pictures (desenhaNumero (map (\y -> charToNumber y num0 num1 num2 num3 num4 num5 num6 num7 num8 num9 ) (show (div (time-segp) 5))) 0)))
                                                                                                                                                                                                                                                                                                                                                                                                      drawTimeRestart = (Translate 300 200 (pictures (desenhaNumero (map (\y -> charToNumber y num0 num1 num2 num3 num4 num5 num6 num7 num8 num9 ) (show (10 - (div timermenu 5)))) 0)))
                                                                                                                                                                                                                                                                                                                                                                                                      drawWRestart = Translate 300 265 menugame
                                                                                                                                                                                                                                                                                                                                                                                                      drawTimef = Translate 300 200  (color color1 (retangolo ((fromIntegral size) * tpic)))
                                                                                                                                                                                                                                                                                                                                                                                                      drawScoreboard = pictures (scoreboard d 0 str)
                                                                                                                                                                                                                                                                                                                                                                                                  in if length d > 1
                                                                                                                                                                                                                                                                                                                                                                                                   then pictures([mapafigurado]++[bombasfiguradas]++[bombfigurada]++[firefigurada]++[playerdraw]++[drawTimef]++[drawTime]++[drawScoreboard])
                                                                                                                                                                                                                                                                                                                                                                                                   else if mod timermenu 5 == 0
                                                                                                                                                                                                                                                                                                                                                                                                     then pictures([mapafigurado]++[bombasfiguradas]++[bombfigurada]++[firefigurada]++[playerdraw]++[drawTimef]++[drawTimeRestart]++[drawScoreboard])
                                                                                                                                                                                                                                                                                                                                                                                                     else pictures([mapafigurado]++[bombasfiguradas]++[bombfigurada]++[firefigurada]++[playerdraw]++[drawTimef]++[drawTimeRestart]++[drawWRestart]++[drawScoreboard])

{- | A função newImagem é a responvável alterar a representação do jogador conforme o comando utilizado.
-}

newImage :: Char -> Picture -> Picture -> Picture -> Picture -> Picture -> Picture
newImage com n1d n1l n1r n1u n1b | com == 'U' = n1u
                                 | com == 'R' = n1r
                                 | com == 'L' = n1l
                                 | com == 'D' = n1d
                                 | otherwise = n1b

{- | A função newfram é a responvável alterar a representação de todos os jogadores conforme o comando utilizado.
-}

newfram :: Estado -> Char -> Int -> Estado
newfram (gera,size,time,segp,car,bomb,tijol,back,pbomb,pfire,p1,p2,p3,p4,flames,(n1d,n1l,n1r,n1u,n1b),(n2d,n2l,n2r,n2u,n2b),(n3d,n3l,n3r,n3u,n3b),(n4d,n4l,n4r,n4u,n4b),exbom,expower,(menu1status,menu1),(menu1auxstatus,menu1aux),timermenu,(menu2status,menu2),dim,numeros,nnj,(menu3status,menu3),menugame,str,mapa2,seed) k nj | nj == 0 = (gera,size,time,segp,car,bomb,tijol,back,pbomb,pfire,npic,p2,p3,p4,flames,(n1d,n1l,n1r,n1u,n1b),(n2d,n2l,n2r,n2u,n2b),(n3d,n3l,n3r,n3u,n3b),(n4d,n4l,n4r,n4u,n4b),exbom,expower,(menu1status,menu1),(menu1auxstatus,menu1aux),timermenu,(menu2status,menu2),dim,numeros,nj,(menu3status,menu3),menugame,str,mapa2,seed)
                                                                                                                                                                                                                                                                                                                                    | nj == 1 = (gera,size,time,segp,car,bomb,tijol,back,pbomb,pfire,p1,npic2,p3,p4,flames,(n1d,n1l,n1r,n1u,n1b),(n2d,n2l,n2r,n2u,n2b),(n3d,n3l,n3r,n3u,n3b),(n4d,n4l,n4r,n4u,n4b),exbom,expower,(menu1status,menu1),(menu1auxstatus,menu1aux),timermenu,(menu2status,menu2),dim,numeros,nj,(menu3status,menu3),menugame,str,mapa2,seed)
                                                                                                                                                                                                                                                                                                                                    | nj == 2 = (gera,size,time,segp,car,bomb,tijol,back,pbomb,pfire,p1,p2,npic3,p4,flames,(n1d,n1l,n1r,n1u,n1b),(n2d,n2l,n2r,n2u,n2b),(n3d,n3l,n3r,n3u,n3b),(n4d,n4l,n4r,n4u,n4b),exbom,expower,(menu1status,menu1),(menu1auxstatus,menu1aux),timermenu,(menu2status,menu2),dim,numeros,nj,(menu3status,menu3),menugame,str,mapa2,seed)
                                                                                                                                                                                                                                                                                                                                    | otherwise = (gera,size,time,segp,car,bomb,tijol,back,pbomb,pfire,p1,p2,p3,npic4,flames,(n1d,n1l,n1r,n1u,n1b),(n2d,n2l,n2r,n2u,n2b),(n3d,n3l,n3r,n3u,n3b),(n4d,n4l,n4r,n4u,n4b),exbom,expower,(menu1status,menu1),(menu1auxstatus,menu1aux),timermenu,(menu2status,menu2),dim,numeros,nj,(menu3status,menu3),menugame,str,mapa2,seed)
                                                                                                                                                                                                                                                                                                                                    where npic = newImage k n1d n1l n1r n1u p1
                                                                                                                                                                                                                                                                                                                                          npic2 = newImage k n2d n2l n2r n2u p2
                                                                                                                                                                                                                                                                                                                                          npic3 = newImage k n3d n3l n3r n3u p3
                                                                                                                                                                                                                                                                                                                                          npic4 = newImage k n4d n4l n4r n4u p4

{- | A função reageMenu2 é a responsável por fazer com que o menu2 funcione.
-}

reageMenu2 :: Event -> Estado -> Estado
reageMenu2 (EventKey (Char '0') Down _ _ ) s = alterDim 0 s
reageMenu2 (EventKey (Char '1') Down _ _ ) s = alterDim 1 s
reageMenu2 (EventKey (Char '2') Down _ _ ) s = alterDim 2 s
reageMenu2 (EventKey (Char '3') Down _ _ ) s = alterDim 3 s
reageMenu2 (EventKey (Char '4') Down _ _ ) s = alterDim 4 s
reageMenu2 (EventKey (Char '5') Down _ _ ) s = alterDim 5 s
reageMenu2 (EventKey (Char '6') Down _ _ ) s = alterDim 6 s
reageMenu2 (EventKey (Char '7') Down _ _ ) s = alterDim 7 s
reageMenu2 (EventKey (Char '8') Down _ _ ) s = alterDim 8 s
reageMenu2 (EventKey (Char '9') Down _ _ ) s = alterDim 9 s
reageMenu2 (EventKey (SpecialKey KeyEnter) Down _ _ ) (gera,size,time,segp,car,bomb,tijol,back,pbomb,pfire,p1,p2,p3,p4,flames,player1,player2,player3,player4,exbom,expower,(menu1status,menu1),(menu1auxstatus,menu1aux),timermenu,(menu2status,menu2),dim,numeros,nj,(menu3status,menu3),menugame,str,mapa2,seed) = if not (even dim) && dim >= 5
  then (gera2 ++ strj,dim,3*(dim-2)^2,segp,car,bomb,tijol,back,pbomb,pfire,p1,p2,p3,p4,flames,player1,player2,player3,player4,exbom,expower,(menu1status,menu1),(menu1auxstatus,menu1aux),timermenu,(False,menu2),dim,numeros,nj,(menu3status,menu3),menugame,str,gera2++strj,seed)
  else (gera,size,time,segp,car,bomb,tijol,back,pbomb,pfire,p1,p2,p3,p4,flames,player1,player2,player3,player4,exbom,expower,(menu1status,menu1),(menu1auxstatus,menu1aux),timermenu,(True,menu2),0,numeros,nj,(menu3status,menu3),menugame,str,mapa2,seed) where gera2 = mapa dim seed
                                                                                                                                                                                                                                                                  strj = stringjogadores nj 0 dim
reageMenu2 _ s = s

{- | A função alterDim é a responvável alterar a dimensão do mapa no menu2 sempre que utilizador pressiona uma tecla.
-}

alterDim :: Int -> Estado -> Estado
alterDim x (gera,size,time,segp,car,bomb,tijol,back,pbomb,pfire,p1,p2,p3,p4,flames,player1,player2,player3,player4,exbom,expower,(menu1status,menu1),(menu1auxstatus,menu1aux),timermenu,(menu2status,menu2),dim,numeros,nj,(menu3status,menu3),menugame,str,mapa2,seed) = let dima = show dim
                                                                                                                                                                                                                                                                               dimb = show x
                                                                                                                                                                                                                                                                               dimc = read (dima ++ dimb) :: Int
                                                                                                                                                                                                                                                                           in (gera,size,time,segp,car,bomb,tijol,back,pbomb,pfire,p1,p2,p3,p4,flames,player1,player2,player3,player4,exbom,expower,(menu1status,menu1),(menu1auxstatus,menu1aux),timermenu,(menu2status,menu2),dimc,numeros,nj,(menu3status,menu3),menugame,str,mapa2,seed)

{- | A função movePre é a um função auxiliar da reageEvento e é a responsável pelo movimento dos jogadores
-}

movePre :: Estado -> Char -> Int -> Estado
movePre (gera,size,time,segp,car,bomb,tijol,back,pbomb,pfire,p1,p2,p3,p4,flames,player1,player2,player3,player4,exbom,expower,(menu1status,menu1),(menu1auxstatus,menu1aux),timermenu,(menu2status,menu2),dim,numeros,nnj,(menu3status,menu3),menugame,str,mapa2,seed) k nj = let nmapa = move gera nj k
                                                                                                                                                                                                                                                                              in newfram (nmapa,size,time,segp,car,bomb,tijol,back,pbomb,pfire,p1,p2,p3,p4,flames,player1,player2,player3,player4,exbom,expower,(menu1status,menu1),(menu1auxstatus,menu1aux),timermenu,(menu2status,menu2),dim,numeros,nnj,(menu3status,menu3),menugame,str,mapa2,seed) k nj

{- | A função reageMenu é a função que decide como o jogo deve responder quando o utilizador pressiona um tecla conforme o seu estado atual.
-}

reageMenu :: Event -> Estado -> Estado
reageMenu (EventKey key Down y1 y2) (gera,size,time,segp,car,bomb,tijol,back,pbomb,pfire,p1,p2,p3,p4,flames,player1,player2,player3,player4,exbom,expower,(menu1status,menu1),(menu1auxstatus,menu1aux),timermenu,(menu2status,menu2),dim,numeros,nj,(menu3status,menu3),menugame,str,mapa2,seed) | menu1status = (gera,size,time,0,car,bomb,tijol,back,pbomb,pfire,p1,p2,p3,p4,flames,player1,player2,player3,player4,exbom,expower,(False,menu1),(menu1auxstatus,menu1aux),timermenu,(menu2status,menu2),dim,numeros,nj,(menu3status,menu3),menugame,str,mapa2,seed)
                                                                                                                                                                                                                                                                                                  | menu3status = reageMenu3 (EventKey key Down y1 y2) (gera,size,time,segp,car,bomb,tijol,back,pbomb,pfire,p1,p2,p3,p4,flames,player1,player2,player3,player4,exbom,expower,(menu1status,menu1),(menu1auxstatus,menu1aux),0,(menu2status,menu2),dim,numeros,nj,(menu3status,menu3),menugame,str,mapa2,seed)
                                                                                                                                                                                                                                                                                                  | menu2status = reageMenu2 (EventKey key Down y1 y2) (gera,size,time,segp,car,bomb,tijol,back,pbomb,pfire,p1,p2,p3,p4,flames,player1,player2,player3,player4,exbom,expower,(menu1status,menu1),(menu1auxstatus,menu1aux),timermenu,(menu2status,menu2),dim,numeros,nj,(menu3status,menu3),menugame,str,mapa2,seed)
                                                                                                                                                                                                                                                                                                  | otherwise = reageEvento (EventKey key Down y1 y2) (gera,size,time,segp,car,bomb,tijol,back,pbomb,pfire,p1,p2,p3,p4,flames,player1,player2,player3,player4,exbom,expower,(menu1status,menu1),(menu1auxstatus,menu1aux),timermenu,(menu2status,menu2),dim,numeros,nj,(menu3status,menu3),menugame,str,mapa2,seed)
reageMenu _ s = s

{- | A função reageEvento é a responvável por responder ,já no jogo, quando os utilizadores pressionam as teclas
-}

reageEvento :: Event -> Estado -> Estado
reageEvento (EventKey (Char 'a') Down _ _ ) s = movePre s 'L' 0
reageEvento (EventKey (Char 'd') Down _ _ ) s = movePre s 'R' 0
reageEvento (EventKey (Char 'w') Down _ _ ) s = movePre s 'U' 0
reageEvento (EventKey (Char 's') Down _ _ ) s = movePre s 'D' 0
reageEvento (EventKey (Char 'q') Down _ _ ) s = movePre s 'B' 0
reageEvento (EventKey (Char 'f') Down _ _ ) s = movePre s 'L' 1
reageEvento (EventKey (Char 'h') Down _ _ ) s = movePre s 'R' 1
reageEvento (EventKey (Char 't') Down _ _ ) s = movePre s 'U' 1
reageEvento (EventKey (Char 'g') Down _ _ ) s = movePre s 'D' 1
reageEvento (EventKey (Char 'r') Down _ _ ) s = movePre s 'B' 1
reageEvento (EventKey (Char 'j') Down _ _ ) s = movePre s 'L' 3
reageEvento (EventKey (Char 'l') Down _ _ ) s = movePre s 'R' 3
reageEvento (EventKey (Char 'i') Down _ _ ) s = movePre s 'U' 3
reageEvento (EventKey (Char 'k') Down _ _ ) s = movePre s 'D' 3
reageEvento (EventKey (Char 'u') Down _ _ ) s = movePre s 'B' 3
reageEvento (EventKey (SpecialKey KeyLeft) Down _ _ ) s = movePre s 'L' 2
reageEvento (EventKey (SpecialKey KeyRight) Down _ _ ) s = movePre s 'R' 2
reageEvento (EventKey (SpecialKey KeyUp) Down _ _ ) s = movePre s 'U' 2
reageEvento (EventKey (SpecialKey KeyDown) Down _ _ ) s = movePre s 'D' 2
reageEvento (EventKey (SpecialKey KeyEnter) Down _ _ ) s = movePre s 'B' 2
reageEvento _ s = s

{- | A função reageTempo é a responvável pela para passagem do tempo ja quando o jogo foi iniciado.
-}
playBot :: [String] -> Int -> Maybe Char -> [String]
playBot mapa z Nothing  = mapa
playBot mapa z (Just x)  = if (filter (\y -> head y == intToDigit z ) mapa) == []
  then mapa
  else move mapa z x


reageTempo :: Float -> Estado -> Estado
reageTempo f (gera,size,time,segp,car,bomb,tijol,back,pbomb,pfire,p1,p2,p3,p4,flames,player1,player2,player3,player4,exbom,expower,(menu1status,menu1),(menu1auxstatus,menu1aux),timermenu,(menu2status,menu2),dim,numeros,nj,(menu3status,menu3),menugame,str,mapa2,seed) = let nmapa = avanca gera (time-segp)
                                                                                                                                                                                                                                                                                 nmapa2 = playBot nmapa 3 (bot nmapa 3 (time-segp))
                                                                                                                                                                                                                                                                                 (a4,b4,c4,d4) = splitMap gera
                                                                                                                                                                                                                                                                                 exbom2 = slowTime c4
                                                                                                                                                                                                                                                                                 (a,b,c,d) = splitMap nmapa
                                                                                                                                                                                                                                                                                 f1 = 1 + segp
                                                                                                                                                                                                                                                                                 stop = f1 - 1
                                                                                                                                                                                                                                                                             in if (time-segp) == 0 || length d <= 1 && timermenu == 50
                                                                                                                                                                                                                                                                               then ([],0,0,0,car,bomb,tijol,back,pbomb,pfire,p1,p2,p3,p4,flames,player1,player2,player3,player4,[],[],(True,menu1),(True,menu1aux),0,(True,menu2),0,numeros,0,(True,menu3),menugame,str,[],seed)
                                                                                                                                                                                                                                                                               else if (time-segp) == 0 || length d <= 1
                                                                                                                                                                                                                                                                                 then (nmapa2,size,time,stop,car,bomb,tijol,back,pbomb,pfire,p1,p2,p3,p4,flames,player1,player2,player3,player4,exbom2,b4,(menu1status,menu1),(menu1auxstatus,menu1aux),timermenu+1,(menu2status,menu2),dim,numeros,nj,(menu3status,menu3),menugame,str,gera,seed)
                                                                                                                                                                                                                                                                                 else  (nmapa2,size,time,f1,car,bomb,tijol,back,pbomb,pfire,p1,p2,p3,p4,flames,player1,player2,player3,player4,exbom2,b4,(menu1status,menu1),(menu1auxstatus,menu1aux),timermenu,(menu2status,menu2),dim,numeros,nj,(menu3status,menu3),menugame,str,a4,seed)


{- | A função reageMenu3 é a responsável por fazer com que o menu3 funcione ,ou seja, é responsável por permitir ao utilizador introduzir o número de utilizadores pretendidos.
-}

reageMenu3 :: Event -> Estado -> Estado
reageMenu3 (EventKey (Char '1') Down _ _ ) s = alteraNum 1 s
reageMenu3 (EventKey (Char '2') Down _ _ ) s = alteraNum 2 s
reageMenu3 (EventKey (Char '3') Down _ _ ) s = alteraNum 3 s
reageMenu3 (EventKey (Char '4') Down _ _ ) s = alteraNum 4 s
reageMenu3 (EventKey (SpecialKey KeyEnter) Down _ _ ) (gera,size,time,segp,car,bomb,tijol,back,pbomb,pfire,p1,p2,p3,p4,flames,player1,player2,player3,player4,exbom,expower,(menu1status,menu1),(menu1auxstatus,menu1aux),timermenu,(menu2status,menu2),dim,numeros,nj,(menu3status,menu3),menugame,str,mapa2,seed) = if nj >=1 && nj <= 4
  then (gera,size,time,segp,car,bomb,tijol,back,pbomb,pfire,p1,p2,p3,p4,flames,player1,player2,player3,player4,exbom,expower,(menu1status,menu1),(menu1auxstatus,menu1aux),timermenu,(menu2status,menu2),dim,numeros,nj,(False,menu3),menugame,str,mapa2,seed)
  else (gera,size,time,segp,car,bomb,tijol,back,pbomb,pfire,p1,p2,p3,p4,flames,player1,player2,player3,player4,exbom,expower,(menu1status,menu1),(menu1auxstatus,menu1aux),timermenu,(menu2status,menu2),dim,numeros,0,(True,menu3),menugame,str,mapa2,seed)
reageMenu3 _ s = s

{- | A função alteraNum é a responsável alterar o numero que vai ser exibido no Menu de escolher o número de utilizadores.
-}

alteraNum :: Int -> Estado -> Estado
alteraNum x (gera,size,time,segp,car,bomb,tijol,back,pbomb,pfire,p1,p2,p3,p4,flames,player1,player2,player3,player4,exbom,expower,(menu1status,menu1),(menu1auxstatus,menu1aux),timermenu,(menu2status,menu2),dim,numeros,nj,(menu3status,menu3),menugame,str,mapa2,seed) = (gera,size,time,segp,car,bomb,tijol,back,pbomb,pfire,p1,p2,p3,p4,flames,player1,player2,player3,player4,exbom,expower,(menu1status,menu1),(menu1auxstatus,menu1aux),timermenu,(menu2status,menu2),dim,numeros,x,(menu3status,menu3),menugame,str,mapa2,seed)

{- | A função reagemTempo2 é a responsável por indicar ao jogo como este deve reagir à passagem do tempo conforme o estado atual.
-}

reageTempo2 :: Float -> Estado -> Estado
reageTempo2 f (gera,size,time,segp,car,bomb,tijol,back,pbomb,pfire,p1,p2,p3,p4,flames,player1,player2,player3,player4,exbom,expower,(menu1status,menu1),(menu1auxstatus,menu1aux),timermenu,(menu2status,menu2),dim,numeros,nj,(menu3status,menu3),menugame,str,mapa2,seed) | menu1status = reageTMenu1 (gera,size,time,segp,car,bomb,tijol,back,pbomb,pfire,p1,p2,p3,p4,flames,player1,player2,player3,player4,exbom,expower,(menu1status,menu1),(menu1auxstatus,menu1aux),timermenu,(menu2status,menu2),dim,numeros,nj,(menu3status,menu3),menugame,str,mapa2,seed)
                                                                                                                                                                                                                                                                            | menu3status = (gera,size,time,segp,car,bomb,tijol,back,pbomb,pfire,p1,p2,p3,p4,flames,player1,player2,player3,player4,exbom,expower,(menu1status,menu1),(menu1auxstatus,menu1aux),timermenu,(menu2status,menu2),dim,numeros,nj,(menu3status,menu3),menugame,str,mapa2,seed)
                                                                                                                                                                                                                                                                            | menu2status = (gera,size,time,segp,car,bomb,tijol,back,pbomb,pfire,p1,p2,p3,p4,flames,player1,player2,player3,player4,exbom,expower,(menu1status,menu1),(menu1auxstatus,menu1aux),timermenu,(menu2status,menu2),dim,numeros,nj,(menu3status,menu3),menugame,str,mapa2,seed)
                                                                                                                                                                                                                                                                            | otherwise = reageTempo f (gera,size,time,segp,car,bomb,tijol,back,pbomb,pfire,p1,p2,p3,p4,flames,player1,player2,player3,player4,exbom,expower,(menu1status,menu1),(menu1auxstatus,menu1aux),timermenu,(menu2status,menu2),dim,numeros,nj,(menu3status,menu3),menugame,str,mapa2,seed)

{- | A função reageTMenu1 é a pela passagem do tempo no menu1
-}

reageTMenu1 :: Estado -> Estado
reageTMenu1 (gera,size,time,segp,car,bomb,tijol,back,pbomb,pfire,p1,p2,p3,p4,flames,player1,player2,player3,player4,exbom,expower,(menu1status,menu1),(menu1auxstatus,menu1aux),timermenu,(menu2status,menu2),dim,numeros,nj,(menu3status,menu3),menugame,str,mapa2,seed) = if mod timermenu 4 == 0
  then (gera,size,time,segp,car,bomb,tijol,back,pbomb,pfire,p1,p2,p3,p4,flames,player1,player2,player3,player4,exbom,expower,(menu1status,menu1),(False,menu1aux),timermenu + 1,(menu2status,menu2),dim,numeros,nj,(menu3status,menu3),menugame,str,mapa2,seed)
  else (gera,size,time,segp,car,bomb,tijol,back,pbomb,pfire,p1,p2,p3,p4,flames,player1,player2,player3,player4,exbom,expower,(menu1status,menu1),(True,menu1aux),timermenu + 1,(menu2status,menu2),dim,numeros,nj,(menu3status,menu3),menugame,str,mapa2,seed)

{- | fr indica a FrameRate que vai ser utilizada.
-}

fr :: Int
fr = 5

{- | dm indica todas as informações sobre a janela do jogo.
-}

dm :: Display
dm = InWindow "BomberMan by Group 48" (800, 600) (0, 0)

{- | A função main é a responsável por juntar todas as pequenas peças de modo a tornar o jogo jogável.
-}
main :: IO ()
main = do car <- (loadBMP "img/cardinal.bmp")
          bomb <- (loadBMP "img/bomb45.bmp")
          tijol <- (loadBMP "img/rand45.bmp")
          back <- (loadBMP "img/back45pix.bmp")
          pbomb <- (loadBMP "img/newpowerbomb.bmp")
          pfire <- (loadBMP "img/newpowerfire.bmp")
          n1 <- (loadBMP "img/player45pix.bmp")
          n1b <- (loadBMP "img/player1bomb45.bmp")
          n2 <- (loadBMP "img/player2back.bmp")
          n3 <- (loadBMP "img/player3back.bmp")
          n4 <- (loadBMP "img/player5back.bmp")
          u1 <- (loadBMP "img/up.bmp")
          l1 <- (loadBMP "img/lateralback.bmp")
          l2 <- (loadBMP "img/lateral2back.bmp")
          d1 <- (loadBMP "img/down.bmp")
          r1 <- (loadBMP "img/rigth.bmp")
          e1 <- (loadBMP "img/left.bmp")
          c1 <- (loadBMP "img/explosionback.bmp")
          n1l <- (loadBMP "img/lframep1.bmp")
          n1r <- (loadBMP "img/rframep1.bmp")
          n1u <- (loadBMP "img/uframep1.bmp")
          n2b <- (loadBMP "img/player2bomb.bmp")
          n2l <- (loadBMP "img/lframep2.bmp")
          n2r <- (loadBMP "img/rframep2.bmp")
          n2u <- (loadBMP "img/uframep2.bmp")
          n3b <- (loadBMP "img/player3bomb.bmp")
          n3l <- (loadBMP "img/lframep3.bmp")
          n3r <- (loadBMP "img/rframep3.bmp")
          n3u <- (loadBMP "img/uframep3.bmp")
          n4b <- (loadBMP "img/player5bomb.bmp")
          n4l <- (loadBMP "img/lframep5.bmp")
          n4r <- (loadBMP "img/rframep5.bmp")
          n4u <- (loadBMP "img/uframep5l.bmp")
          menu1 <- (loadBMP "img/menu1.bmp")
          menu1ax <- (loadBMP "img/menu1presskey.bmp")
          menu2 <- (loadBMP "img/menuCM.bmp")
          num0 <- (loadBMP "img/n1.bmp")
          num1 <- (loadBMP "img/n2.bmp")
          num2 <- (loadBMP "img/n3.bmp")
          num3 <- (loadBMP "img/n4.bmp")
          num4 <- (loadBMP "img/n5.bmp")
          num5 <- (loadBMP "img/n6.bmp")
          num6 <- (loadBMP "img/n7.bmp")
          num7 <- (loadBMP "img/n8.bmp")
          num8 <- (loadBMP "img/n9.bmp")
          num9 <- (loadBMP "img/n10.bmp")
          menu3 <- (loadBMP "img/menubuilder.bmp")
          menugame <- (loadBMP "img/letterRestart.bmp")
          str1 <- (loadBMP "img/p1str.bmp")
          str2 <- (loadBMP "img/p2str.bmp")
          str3 <- (loadBMP "img/p3str.bmp")
          str4 <- (loadBMP "img/p4str.bmp")
          let rand1 = 0 :: Int
          let rand2 = 1000 :: Int
          seed <- randomRIO (rand1,rand2)
          play dm              -- display mode
               (greyN 0.8)     -- côr do fundo da janela
               fr              -- frame rate
               (estadoInicial car bomb tijol back pbomb pfire n1 n2 n3 n4 u1 l1 l2 d1 r1 e1 c1 n1 n1l n1r n1u n1b n2 n2l n2r n2u n2b n3 n3l n3r n3u n3b n4 n4l n4r n4u n4b menu1 menu1ax menu2 num0 num1 num2 num3 num4 num5 num6 num7 num8 num9 menu3 menugame str1 str2 str3 str4 seed)   -- estado inicial
               desenhaMenu1    -- desenha o estado do jogo
               reageMenu     -- reage a um evento
               reageTempo2     -- reage ao passar do tempo
