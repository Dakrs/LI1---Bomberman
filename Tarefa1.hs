{- | 
   Module      : Main 
   Description : Gerador de Mapas para o Bomberman.
   Copyright   : Diogo José Cruz Sobral
                 João Pedro Rodrigues Gomes

   Com esta documentação pretendemos que se torne mais fácil a leitura e o entendimento do nosso código para fazer o jogo "Bomberman".
   Aqui fica um pequeno resumo da lógica usada para fazer o jogo, e mais à frente abordamos todas as funções uma a uma. 
   O nosso mapa é gerado recursivamente linha a linha. 
   Para tal, damos mais importância a três funçoes usadas. Uma primeira que gera todos os pontos duma linha, "linha".
   Uma segunda, "fillMap", que usando a linha gerada pela função "linha", preenche todos os pontos dessa linha, recursivamente, com o Char devido.
   E a função principal, "mapa", que utiliza a função "fillMap" e a função linha para preencher todas as linhas do mapa recursivamente. 
-}


module Main where

import System.Environment
import Text.Read
import Data.Maybe
import Data.Char
import System.Random




{- | Esta função, dada a dimensão do mapa, calcula o número de valores aleatórios necessários gerar.

  == Exemplos de utilização:
  >>> cal 9
  28
-}
cal :: Int -> Int
cal x | x == 5        = 0
      | x > 5         = (x-2)^2  -  u  where
                                            u = quot (((x-5)+2)^2) 4 + 12

{- | Esta função, utilizando a função acima, cal, gera uma lista com o numero de valores aleatórios necessarios, de acordo com a dimensao do mapa. 

  == Exemplos de utilização:
  >>> val 9 0
  [83,93,63,38,0,87,81,1,61,86,13,50,32,80,54,25,90,31,65,92,2,76,70,25,6,29,10,99,81]

-}
valr :: Int -> Int -> [Int]
valr x y = take ((cal x)+1) $ randomRs (0,99) (mkStdGen y)

{- | Função criada com o intuito de ser usada para atribuir a cada um dos numeros inteiros que se encontram na lista dos valores aleatórios um Char.


== Exemplos de utilização:
  >>> rand 83
  ' '

-}
rand :: Int -> Char
rand x 
        | x <= 39  = '?'
        | x >= 40  = ' '

{- | Dado a dimensão do mapa e o primeiro ponto duma Linha, esta função devolve os pares de coordenadas de todos os pontos duma Linha.

== Exemplos de utilização:
  >>> geraLinha 5 (0,0)
  [(0,0),(1,0),(2,0),(3,0),(4,0)] 

-}
geraLinha :: Int -> (Int,Int) -> [(Int,Int)]
geraLinha x (a,b) | a <= x - 1  = (a,b) : geraLinha x (a+1,b)
                  | a > x - 1  = []

{- | Esta função preenche uma linha, ponto a ponto, com os seus determinados caracteres.
-}
fillMap :: Int -> [(Int,Int)] -> [Int] -> String
fillMap  x [] y = []
fillMap  x ((a,b):ys) (t:t') | b == 0   || b == x - 1                                 = replicate x '#'
                             | a == 0   || a== x - 1                                  = '#' : fillMap x ys (t:t')
                             | (b == 1 || b == x-2) && ( a >= (x-3) || a <=  2 )      = ' ' : fillMap x ys (t:t')  
                             | (b == 2 || b == x-3) && ( a == 1 || a == x-2)          = ' ' : fillMap x ys (t:t')
                             | linha (a,b) == True                                    = '#' : fillMap x ys (t:t')
                             | otherwise                                              = rand t : fillMap x ys t'

{- | Esta função é usada para testar se um ponto do mapa, excepto os exteriores, terá uma pedra ou nao 
-}
linha :: (Int,Int) -> Bool
linha (w,k) | odd (w+1) && odd (k+1) = True
            | otherwise = False

{- | Esta função irá gerar a primeira parte do mapa, ou seja, todos os pontos do mapa.
     Irá fazê-lo usando a função fillMap que, como dito anteriormente, preenche uma e uma só linha, ponto a ponto.
     Usamos então uma função auxiliar, que use a função fillMap para a primeira linha e depois a invoque recursivamente para preencher as linhas restantes.

     == Exemplos de utilização:
     >>> mapa1 7 0
     ["#######","#     #","# # # #","# ??  #","# #?# #","#     #","#######"]
-}
mapa1 :: Int -> Int -> [String]
mapa1 x y = aux x  y 0 where
  aux x y z | x < 5           = error "x tem de ser superior ou igual a 5"
            | even x == True  = error "x tem de ser impar"
            | z > x - 1       = []
            | otherwise       = fillMap x (geraLinha x (0,z))  o : aux x y (z+1) where o = drop (soma x (z-1)) (valr x y)


{- | Função auxilidar para tornar a função soma menos complexa.
-}
alpha :: Int -> Int
alpha x = x - ((div x 2) + 1)


{- | Esta função conta o numero de valores aleatórios utilizados até à linha em questao. 

     == Exemplos de utilização:
     >>> soma 9 4
     16

-}
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

{- | Função que gera devolve as coordenadas de todos os pontos em que é preciso utilizar valores aleatórios.

     == Exemplos de utilização:
     >>> geRaParesR 7 (0,0)
     [(3,1),(3,2),(1,3),(2,3),(3,3),(4,3),(5,3),(3,4),(3,5)] 
-}
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

{- | Função que adiciona, após os mapa, as Strings com a informação acerca da posição do jogador e dos powerups.

    == Exemplos de utilização:
    >>> powerUP 9 (geraParesR 9 (0,0) (valr 9 0)
    ["+ 5 2","+ 3 3","! 5 5"]

-}
powerUP :: Int -> [(Int,Int)] -> [Int] -> [String]
powerUP x [] _ = []
powerUP x t y = aux x t y [] [] where
    aux x [] y w k = w ++ k
    aux x ((a,b):t') (y:ys) w k | y <= 1 && w == []   = aux x t' ys (("+" ++ " " ++ show (a) ++ " " ++ show (b)):[]) k
                                | y <= 1              = aux x t' ys (w ++ ("+" ++ " " ++ show (a) ++ " " ++ show (b)):[]) k
                                | y <= 3 && k == []   = aux x t' ys w (("!" ++ " " ++ show (a) ++ " " ++ show (b)):[])
                                | y <= 3              = aux x t' ys w (k ++ ("!" ++ " " ++ show (a) ++ " " ++ show (b)):[])
                                | otherwise = aux x t' ys w k

{- | Função principal, que utiliza tanto a função que gera o mapa em si, como a função que gera as linhas com as coordenadas dos power ups.
    
    == Exemplos de utilização:
     >>> mapa 9 0
     ["#########","#       #","# #?#?# #","#  ?  ? #","#?# # #?#","# ?  ?  #","# #?#?# #","#  ??   #","#########","+ 5 2","+ 3 3","! 5 5"]

-}
mapa :: Int -> Int -> [String]
mapa x y = (mapa1 x y) ++ powerUP x (geraParesR x (0,0)) (valr x y)





{- | ’main’: função que dá print à ’mapa’ na forma correta
-}
main :: IO ()
main = do a <- getArgs
          let s = readMaybe (a !! 0)
          let l = readMaybe (a !! 1)
          if length a == 2 && isJust s && isJust l && fromJust s >= 5 && odd (fromJust s)
             then putStr $ unlines $ mapa (fromJust s) (fromJust l)
             else putStrLn "Parâmetros inválidos"