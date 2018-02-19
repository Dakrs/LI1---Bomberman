{- |
   Module : Main 
   Description : Função movimento do jogador.
   Copyright   : Diogo José Cruz Sobral ; João Pedro Rodrigues Gomes

    

 Com esta documentação pretendemos que se torne mais fácil a leitura e o entendimento do nosso código para fazer o jogo "Bomberman".
 Aqui fica um pequeno resumo da lógica usada para fazer o jogo, e mais à frente abordamos todas as funções uma a uma. 
 Esta parte do trabalho teve o objetivo de realizar movimentos dado o estado do jogo, um jogador e um comando.

-}

module Main where

import Data.Char (isDigit)
import System.Environment
import Data.List
import Data.Char


{- | Função que converte Strings para Inteiros. 

     == Exemplos de utilização:
     >>> conv "120"
     120
-}
conv :: String -> Int
conv y = read y :: Int


{- | Função usada para ir buscar as coordenadas de alguma coisa que necessitamos. 

     == Exemplos de utilização:
     >>> dec "0 1 1"
     (1,1)

     >>> dec "1 3 2 ++!"
     (3,2)

-}
dec :: String -> (Int,Int)
dec (y:t:ys) = aux ys 0 (0,0) where 
    aux ys z (w,k) | z== 0 = aux (drop 1 b) (z+1) (conv a,k)
                   | z== 1 = aux ys (z+1) (w, conv c)
                   | otherwise = (w,k)
    (a,b) = span (\ys -> isDigit ys == True) ys 
    u = drop 1 b
    (c,d) = span (\u -> isDigit u == True) u

{- | Função usada para alterar as coordenadas de uma String. 
    
    == Exemplos de utilização:
    >>> rdec "0 1 1 ++!" (2,1)
    "0 2 1 ++!"

-}
rdec :: String -> (Int,Int) -> String
rdec (y:ys) (a,b) = (y:[]) ++ aux ys (a,b) 0 where
    aux [] (a,b) z  = []
    aux (x:xs) (a,b) z 
                       | x == ' ' = " " ++ aux xs (a,b) (z+1)
                       | z == 1    = (show a) ++ aux (dropWhile (\xs -> isDigit xs == True) xs) (a,b) z
                       | z == 2    = (show b) ++ aux (dropWhile (\xs -> isDigit xs == True) xs) (a,b) z
                       | otherwise = (x:[]) ++ aux xs (a,b) z


{- | Função que dado a string com a informação da bomba devolve o número do jogador detentor da bomba. 

     == Exemplos de utilização:
     >>> dbomb "* 1 1 2 1 10"
     2

-}
dbomb :: String -> Int
dbomb (y:ys) = aux ys 0 where
  aux ys 3 = digitToInt(head ys)
  aux ys z | head ys == ' ' = aux (drop 1 ys) (z+1)
           | otherwise = aux (dropWhile (\ys -> isDigit ys == True) ys) z

{- | Função usada para ir buscar as coordenadas do ponto onde se encontra o jogador. -}
coord :: Char -> [String] -> (Int,Int)
coord x y = dec $ head $ filter (\y -> x == head y) y

{- | Função usada para ir buscar as coordenadas dos pontos se têm power ups de um certo tipo. 

    == Exemplos de utilização:
    >>> coordPUP '+' ["+ 5 2","+ 3 3","! 5 5"]
    [(5,2),(3,3)]

-}
coordPUP :: Char -> [String] -> [(Int,Int)]
coordPUP x y = map dec  (filter (\y -> x == head y) y)

{- | Função usada para ir buscar as coordenadas de bombas as bombas colocadas no mapa. 

     == Exemplos de utilização:
     >>> coordBombas ["* 1 1 0 2 10","* 2 1 2 3 10"]
    [(1,1),(2,1)]

-}
coordBombas :: [String] -> [(Int,Int)]
coordBombas y = map dec (filter (\y-> '*' == head y) y)

{- | Função usada para calcular se um coordena vem depois de outra necessária para organizar as bombas.

    == Exemplos de utilização:
    >>> coordCompare (0,0) (1,1)
    False
-}
coordCompare :: (Int,Int) -> (Int,Int) -> Bool
coordCompare (a,b) (w,k) | b > k      = True
                         | b == k &&  a > w = True
                         | otherwise = False  


{- | Função que verifica se é possível ou não o jogador movimentar-se para uma determinada coordenada. -}
checka :: String -> (Int,Int) -> Bool
checka y (w,k) = aux y w 0 where
    aux [] w z = False
    aux (y:ys) w z | (y == '#' || y== '?') && w == z = False
                   |  w == z                         = True
                   | otherwise                       = aux ys w (z+1)

{- | Função que vai buscar a linha em que a função 'checka' vai realizar a verificação. -}
procLinha :: Int -> [String] -> String
procLinha x (y:ys) | x==0 = y
                   | otherwise = procLinha (x-1) ys

{- | Função que calcula as novas coordenadas se o jogador se poder mover ou não quando este se movimenta para a direita. -}
moveRight :: (Int,Int) -> [String] -> (Int,Int)
moveRight (w,k) list | checka (procLinha k list) (w+1,k) == True = (w+1,k)
                     | otherwise = (w,k)

{- | Função que calcula as novas coordenadas se o jogador se poder mover ou não quando este se movimenta para a esquerda. -}
moveLeft :: (Int,Int) -> [String] -> (Int,Int)
moveLeft (w,k) list | checka (procLinha k list) (w-1,k) == True = (w-1,k)
                    | otherwise = (w,k)
{- | Função que calcula as novas coordenadas se o jogador se poder mover ou não quando este se movimenta para cima. -}
moveUP :: (Int,Int) -> [String] -> (Int,Int)
moveUP (w,k) list | checka (procLinha (k-1) list) (w,k) == True = (w,k-1)
                  | otherwise = (w,k)
{- | Função que calcula as novas coordenadas se o jogador se poder mover ou não quando este se movimenta para baixo. -}
moveDown :: (Int,Int) -> [String] ->  (Int,Int)
moveDown (w,k) list | checka (procLinha (k+1) list) (w,k) == True = (w,k+1)
                    | otherwise = (w,k)

{- | Função que verifica se um power up esta a ser usado. -}
checkCoord :: (Int,Int) -> [(Int,Int)] -> Bool
checkCoord x y = elem x y

{- | Função que retira um power up do mapa quando este é apanhado. 
-}
takePUP :: [String] -> Int -> (Int,Int) -> [String]
takePUP x y  (w,k) = filter (\x -> (head x == '#') || (dec x /= (w,k) ) ||isDigit (head x)) x 


{- | Função verifica se um jogador tem power ups. 

    == Exemplos de utilização:
    >>> testPUP "0 1 1 +"
    True

    == Exemplos de utilização:
    >>> testPUP "0 1 1"
    False
-}
testPUP :: String -> [Char] -> Bool
testPUP y [] = False
testPUP y (x:xs) | elem x y = True
                 | otherwise = testPUP y xs 

{- | Função que finaliza a string de um jogador com a informação dos power ups. 

    == Exemplos de utilização:
    >>> finalPUP "0 1 1" "+!" "+"
    "0 1 1 +"

    == Exemplos de utilização:
    >>> finalPUP "0 1 1 +" "+!" "!"
    "0 1 1 +!"

-}
finalPUP :: String -> String -> String -> String
finalPUP y x z | z == []                 = y
               | (testPUP y x) == False  = y ++ " " ++ z
               | (testPUP y x) == True && z == "+"  = a ++ z ++ b
               | otherwise = y ++ z
               where (a,b) = span (\y -> y /= '!') y

{- | Função que calcula o numero de bombas que um jogador tem no mapa. 

    == Exemplos de utilização:
    >>> verNumeB ["* 1 1 0 1 4","* 1 2 0 1 4","* 3 1 1 1 4"] 0
    2
-}
verNumeB :: [String] -> Int -> Int
verNumeB x y =length(filter (\u -> u == y) u ) where u = map dbomb (filter (\x-> '*' == head x) x)

{- | Função que calcula a potência das bombas de um jogador. 

    == Exemplos de utilização:
    >>> firePOWER ["0 1 1 !!","1 3 2","2 4 4"] 0
    3
-}
firePOWER :: [String] -> Int -> Int
firePOWER x y = 1 + length (filter(\u -> u == '!') u) where u = head (filter (\x -> intToDigit(y) == head x) x)

{- | Função que calcula o número de bombas que um jogador pode colocar.

    == Exemplos de utilização:
    >>> bombPOWER ["0 1 1","1 3 2 +","2 4 4"] 1
    2

 -}
bombPOWER :: [String] -> Int -> Int
bombPOWER x y = 1 + length (filter(\u -> u == '+') u) where u = head (filter (\x -> intToDigit(y) == head x) x)

{- | Função que dado um jogador verifica se este pode colocar uma bomba na sua posição. -}
allowBomba :: [String] -> Int -> String
allowBomba x y   | checkCoord (coord (intToDigit y) x) (coordBombas x) = []
                 | (bombPOWER x y) == verNumeB x y = []
                 |  otherwise = stringBomba x y (coord (intToDigit y) x)

{- | Função que origina um string com a informação de um bomba. -}
stringBomba :: [String] -> Int -> (Int,Int) -> String
stringBomba x y (w,k) = "*" ++ " " ++ show w ++ " " ++ show k ++ " " ++ show y ++ " " ++ show (firePOWER x y) ++ " " ++ "10"

{- | Função que coloca uma bomba no mapa na sua posição correta. -}
colocaBom :: [String] -> String -> [String]
colocaBom [] y = y : []
colocaBom x [] = x
colocaBom (x:xs) y | coordCompare (dec x) (dec y) = y : x : xs
                   | otherwise = x : colocaBom xs y

{- | Função que verifica se o jogador apanhou um power up. -}
powerUP :: [String] -> Int -> Char -> String
powerUP x y l | l == 'R' = checkpowerR x y
              | l == 'L' = checkpowerL x y
              | l == 'U' = checkpowerU x y
              | l == 'D' = checkpowerD x y


{- | Função que verifica se o jogador apanhou um power up quando se move para cima. -}
checkpowerU :: [String] -> Int -> String
checkpowerU x y | checkCoord (moveUP (coord (intToDigit y) x) x) (coordPUP '+' x)  = "+"
                | checkCoord (moveUP (coord (intToDigit y) x) x) (coordPUP '!' x)  = "!"
                | otherwise = []

{- | Função que verifica se o jogador apanhou um power up quando se move para a esquerda. -}
checkpowerL :: [String] -> Int -> String
checkpowerL x y | checkCoord (moveLeft (coord (intToDigit y) x) x) (coordPUP '+' x)  = "+"
                | checkCoord (moveLeft (coord (intToDigit y) x) x) (coordPUP '!' x)  = "!"
                | otherwise = []

{- | Função que verifica se o jogador apanhou um power up quando se move para a direita. -}
checkpowerR :: [String] -> Int -> String
checkpowerR x y | checkCoord (moveRight (coord (intToDigit y) x) x) (coordPUP '+' x) = "+"
                | checkCoord (moveRight (coord (intToDigit y) x) x) (coordPUP '!' x) = "!"
                | otherwise = []

{- | Função que verifica se o jogador apanhou um power up quando se move para baixo. -}
checkpowerD :: [String] -> Int -> String
checkpowerD x y | checkCoord (moveDown (coord (intToDigit y) x) x) (coordPUP '+' x)  = "+"
                | checkCoord (moveDown (coord (intToDigit y) x) x) (coordPUP '!' x)  = "!"
                | otherwise = []


{- | Função que altera as coordenadas de um jogador para as suas novas coordenadas. -}
newStr :: [String] -> Int -> Char -> String -> String
newStr [] y l  b = []
newStr  x y l  b | l== 'R' = rdec b (moveRight (coord (intToDigit y) x) x)
                 | l== 'L' = rdec b (moveLeft  (coord (intToDigit y) x) x)
                 | l== 'U' = rdec b (moveUP (coord (intToDigit y) x) x)
                 | l== 'D' = rdec b (moveDown  (coord (intToDigit y) x) x)

{- | A função move é a funçao principal e executa todos os comandos de movimentos e de bombas. -}
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




main :: IO ()
main = do a <- getArgs
          let p = a !! 0
          let c = a !! 1
          w <- getContents
          if length a == 2 && length p == 1 && isDigit (head p) && length c == 1 && head c `elem` "UDLRB"
             then putStr $ unlines $ move (lines w) (read p) (head c)
             else putStrLn "Parâmetros inválidos"
