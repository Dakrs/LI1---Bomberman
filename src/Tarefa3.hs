{- |
   Module : Main 
   Description : Comprimir o estado do jogo.
   Copyright   : Diogo José Cruz Sobral ; João Pedro Rodrigues Gomes

    

 Com esta documentação pretendemos que se torne mais fácil a leitura e o entendimento do nosso código para fazer o jogo "Bomberman".
 Aqui fica um pequeno resumo da lógica usada para fazer o jogo, e mais à frente abordamos todas as funções uma a uma. 
 Esta parte do trabalho teve o objetivo de o tamanha do jogo para que este ocupe menos espaço quando guardado.

-}

module Main where

import System.Environment
import Data.List
import Data.Char

{- | Função que transforma o estado do jogo comprimido novamente no jogo original. 

     == Exemplos de utilização:
     >>> decode "9 @ic,~.nb&* 1 1 0 2 10#0 2 1 !"
     ["#########","#       #","# #?#?# #","#  ?  ? #","#?# # #?#","# ?  ?  #","# #?#?# #","#  ??   #","#########","+ 5 2","+ 3 3","! 5 5","* 1 1 0 2 10","0 2 1 !"]


-}

decode :: String -> [String]
decode l = mapa2 (conv l1) (decodeLRand l3) ++ decodeLBomb l4   where
    (l1,l2) = span(\l -> l /= ' ') l
    r = drop 1 l2
    (l3,l4) = span(\r -> r /= '*' && r /= '#') r



{- | Função que transforma o estado das bombas e dos jogadores do jogo comprimidas novamente nos valores originais. 

     == Exemplos de utilização:
     >>> decodeLBomb "* 1 1 0 2 10#0 2 1 !"
     ["* 1 1 0 2 10","0 2 1 !"]
     
-}

decodeLBomb :: String -> [String]
decodeLBomb [] = []
decodeLBomb (y:ys) | y == '*' = ((cString y) ++ a) : decodeLBomb b
                   | otherwise = a : decodeLBomb b
                   where
                    (a,b) = span (\ys -> ys /= '*' && ys /= '#' ) ys


{- | Função que transforma os valores do mapa que não são fixo comprimidos nos seu valores originais. 

     == Exemplos de utilização:
     >>> decodeLRand "9 @ic,~.nb&"
     "            ?+  +  ? ?  ? ?  !  ???? "
     
-}

decodeLRand :: String -> String
decodeLRand [] = []
decodeLRand x = decodeLowerL(decodeSimbol(decodeUpperL((decodeNum x))))


{- | Função que descomprime as letras maiusculas. 

     == Exemplos de utilização:
     >>> decodeUpperL "A@iB,~.nb&"
     "aa@ibb,~.nb&"
     
-}

decodeUpperL :: String -> String
decodeUpperL [] = []
decodeUpperL (x:xs) | isUpper x = replicate 2 (toLower x) ++ decodeUpperL xs
                    | otherwise = x : decodeUpperL xs


{- | Função que descomprime os numeros na string comprimida. 

     == Exemplos de utilização:
     >>> decodeNum "A@iB3a,~.nb&"
     "A@iBaaa,~.nb&"
     
-}


decodeNum :: String -> String
decodeNum [] = []
decodeNum p@(x:xs) | isDigit x = (replicate (conv a) r) ++ decodeNum k
                   | otherwise = x : decodeNum xs
                   where
                    (a,b) = span (\p -> isDigit p) p
                    r = head b
                    k = drop 1 b


{- | Função que descomprime os simbolos. 

     == Exemplos de utilização:
     >>> decodeSimbol "A@iB,~.nb&"
     "AabiBaeebbanbfe"
     
-}


decodeSimbol :: String -> String
decodeSimbol [] = []
decodeSimbol (x:xs) | elem x "@,;" = decodePatternA x ++ decodeSimbol xs
                    | elem x ".:-" = decodePatternB x ++ decodeSimbol xs
                    | elem x "_~^" = decodePatternE x ++ decodeSimbol xs
                    | elem x "%$&" = decodePatternF x ++ decodeSimbol xs
                    | otherwise = (x:[]) ++ decodeSimbol xs


{- | Função que descomprime um simbolo em duas letras. 

     == Exemplos de utilização:
     >>> decodePatternA '@'
     "ab"
     
-}


decodePatternA :: Char -> String
decodePatternA x | x == '@' = "ab"
                 | x == ',' = "ae"
                 | otherwise = "af"

{- | Função que descomprime um simbolo em duas letras. 

     == Exemplos de utilização:
     >>> decodePatternB ':'
     "be"
     
-}

decodePatternB :: Char -> String
decodePatternB x | x == '.' = "ba"
                 | x == ':' = "be"
                 | otherwise = "bf"

{- | Função que descomprime um simbolo em duas letras. 

     == Exemplos de utilização:
     >>> decodePatternE '~'
     "eb"
     
-}

decodePatternE :: Char -> String
decodePatternE x | x == '_' = "ea"
                 | x == '~' = "eb"
                 | otherwise = "ef"

{- | Função que descomprime um simbolo em duas letras. 

     == Exemplos de utilização:
     >>> decodePatternF '$'
     "fb"
     
-}

decodePatternF :: Char -> String
decodePatternF x | x == '%' = "fa"
                 | x == '$' = "fb"
                 | otherwise = "fe"

{- | Função que descomprime as letras minusculas. 

     == Exemplos de utilização:
     >>> decodeLowerL "aa@ibb,~.nb&"
     "    @+  ? ?,~.!  ?&"
     
-}

decodeLowerL :: String -> String
decodeLowerL [] = []
decodeLowerL (x:xs) | isLetter x = decodePattern x ++ decodeLowerL xs
                    | otherwise  = x : decodeLowerL xs

{- | Função que descomprime as letras minusculas.   
-}

decodePattern :: Char -> String
decodePattern c | c <= 'd'  = patternVazio c 
                | c <= 'h'  = patternPedra c 
                | c <= 'm'  = patternPlus  c 
                | otherwise = patternExcl  c

{- | Função que descomprime as letras minusculas.   
-}

patternVazio :: Char -> String
patternVazio c | c == 'a'  = "  "
               | c == 'b'  = " ?"
               | c == 'c'  = " +"
               | otherwise = " !"

{- | Função que descomprime as letras minusculas.   
-}

patternPedra :: Char -> String
patternPedra c | c == 'e'  = "? "
               | c == 'f'  = "??"
               | c == 'g'  = "?+"
               | otherwise = "?!"

{- | Função que descomprime as letras minusculas.   
-}

patternPlus :: Char -> String
patternPlus c | c == 'i'  = "+ "
              | c == 'j'  = "+?"
              | c == 'k'  = "++"
              | otherwise = "+!"

{- | Função que descomprime as letras minusculas.   
-}

patternExcl :: Char -> String
patternExcl c | c == 'n'  = "! "
              | c == 'o'  = "!?"
              | c == 'p'  = "!+"
              | otherwise = "!!"


{- | Função que converte um string num inteiro. 

     == Exemplos de utilização:
     >>> conv "120"
     120
-}

conv :: String -> Int
conv y = read y :: Int

{- | Função que converte um string num inteiro. 

     == Exemplos de utilização:
     >>> cString '+'
     "+"
-}

cString :: Char -> String
cString y = y:[]

{- | Função que altera um elemento de uma lista. 
-}

map1 :: (a->Int->Int->b) -> [a] -> Int -> Int -> [b]
map1 f [] k z = []
map1 f (y:ys) k z  = f y k z  : map1 f ys k (z+1)

{- | Função que comprime o mapa. 

     == Exemplos de utilização:
     >>> encode ["#########","#       #","# #?#?# #","#  ?  ? #","#?# # #?#","# ?  ?  #","# #?#?# #","#  ??   #","#########","+ 5 2","+ 3 3","! 5 5","* 1 1 0 2 10","0 2 1 !"]
     "9 @ic,~.nb&* 1 1 0 2 10#0 2 1 !"
-}

encode :: [String] -> String
encode mapa = show (length (head mapa)) ++ " " ++ nSeguidosSTR(nSeguidosSTR2(strcomp2)) ++ concat (mapa3) ++ junta(mapa4) where
  (mapa1,mapa2) = span (\mapa -> head mapa /= '*' && isDigit(head(mapa)) /= True) mapa
  strcomp1 = concat (compAlpha (parteList ((takeRandValue (newMapWPUP mapa1) (length (head mapa1)) 0))))
  strcomp2 = concat (compBeta (parteList strcomp1))
  (mapa3,mapa4) = span (\mapa2 -> head mapa2 == '*') mapa2

{- | Função que comprime a informação dos jogadores. 

     == Exemplos de utilização:
     >>> junta ["0 1 1","1 2 2"]
     "#0 1 1#1 2 2"
-}

junta :: [String] -> String
junta [] = []
junta (mapa4:mapa5) = "#" ++ mapa4 ++ junta mapa5

{- | Função usada para ir buscar as coordenadas dos pontos se têm power ups de um certo tipo. 

    == Exemplos de utilização:
    >>> coordPUP '+' ["+ 5 2","+ 3 3","! 5 5"]
    [(5,2),(3,3)]

-}

coordPUP :: Char -> [String] -> [(Int,Int)]
coordPUP x mapa = map dec  (filter (\mapa -> x == head mapa) mapa)

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

{- | Função usada para colocar os powerups no mapa. 
-}

coloPUPnMplusT :: [String] -> [(Int,Int)] -> [(Int,Int)]  -> [String]
coloPUPnMplusT mapa [] []     = mapa
coloPUPnMplusT mapa [] (p:ps) = coloPUPnMplusT (coloPUPnMplus mapa p '!' '<') [] ps
coloPUPnMplusT mapa (x:xs) p  = coloPUPnMplusT (coloPUPnMplus mapa x '+' '>') xs p

{- | Função usada para colocar um powerup no mapa. 
-}

coloPUPnMplus :: [String] -> (Int,Int) -> Char -> Char -> [String]
coloPUPnMplus mapa (w,k) u i = (take k mapa) ++ (( alteraL (head (drop k mapa)) w u i ):[]) ++ drop (k+1) mapa

{- | Função usada para alterar a informaçao de uma linha do mapa.
    
    >>> alteraL "#  ?     #" 3 '+' '>'
    "#  +     #"

-} 

alteraL :: String -> Int -> Char -> Char -> String
alteraL y w u i = map1 (\y w z -> if w /= z then y else if y == '?' then u else i ) y w 0

{- | Função retira os power da parte de baixo do mapa. 

     == Exemplos de utilização:
     >>> newMapWPUP ["#########","#       #","# #?#?# #","#  ?  ? #","#?# # #?#","# ?  ?  #","# #?#?# #","#  ??   #","#########","+ 5 2","+ 3 3","! 5 5"]
     ["#########","#       #","# #?#+# #","#  +  ? #","#?# # #?#","# ?  !  #","# #?#?# #","#  ??   #","#########"]

-}

newMapWPUP :: [String] -> [String]
newMapWPUP mapa = takeWhile (\r -> head r == '#') r  where 
    (a,b) = span (\mapa -> head mapa /= '*') mapa
    r     = coloPUPnMplusT a (coordPUP '+' mapa) (coordPUP '!' mapa)

{- | Função que retira os valores aleatórios de um mapa.
    
    >>> takeRandValue ["#########","#       #","# #?#+# #","#  +  ? #","#?# # #?#","# ?  !  #","# #?#?# #","#  ??   #","#########"] 9 0
    "   ?+  +  ? ?  ? ?  !  ???? "

-}

takeRandValue :: [String] -> Int -> Int -> String
takeRandValue [] size linha = []
takeRandValue (m:mapa) size linha = randPerLine m size linha ++ takeRandValue mapa size (linha+1)

{- | Função que retira os valores aleatórios de uma linha do mapa.
    
    >>> randPerLine "# #?#+# #" 9 2
    "?+"

-}

randPerLine :: String -> Int -> Int -> String
randPerLine y size linha | linha == 0 || linha == size - 1 = linha1 y
                         | linha == 1 || linha == size - 2 = linha2 y 0 size
                         | linha == 2 || linha == size - 3 = linha3 y 0 size
                         | otherwise = linha4 y

{- | Função que retira os valores aleatórios da primeira ou última linha do mapa.
-}

linha1 :: String -> String
linha1 y = []

{- | Função que retira os valores aleatórios da segunda linha e da penúltima linha do mapa.
    
    >>> linha2 "#       #" 0 9
    "   "

-}

linha2 :: String -> Int -> Int  -> String
linha2 (y:ys) k size | k <= 2 = linha2 ys (k+1) size
                     | k >= size - 3 = []
                     | otherwise = y : linha2 ys (k+1) size

{- | Função que retira os valores aleatórios da terceira e antepenúltima linha do mapa.
    
    >>> linha3 "# #?#+# #" 0 9
    "?+"

-}

linha3 :: String -> Int -> Int -> String
linha3  (y:ys) k size | k <= 1 = linha3 ys (k+1) size
                      | k >= size - 2 = []
                      | y == '#' = linha3 ys (k+1) size
                      | otherwise = y : linha3 ys (k+1) size

{- | Função que retira os valores aleatórios das restantes linhas do mapa.
-}                     

linha4 :: String -> String
linha4 y = filter (\y -> y /= '#') y

{- | Função calcula o número de elementos iguais seguidos sendo que o char inicial pertence à lista.
    
    >>> nSeguidos '#' "#######"
    8

-}

nSeguidos :: Char -> String -> Int
nSeguidos k [] = 1
nSeguidos k (x:xs) | k == x = 1 + nSeguidos k xs
                   | otherwise = 1

{- | Função que se tiver duas letras minusculas iguais seguidas transforma-as numa maiuscula.
    
    >>> nSeguidosSTR2 "aabkbb"
    "AbkB"

-}

nSeguidosSTR2 :: String -> String 
nSeguidosSTR2 [] = []
nSeguidosSTR2 (x:xs) | (nSeguidos x xs) == 2 && isLetter x = ((toUpper x) :[]) ++ nSeguidosSTR2 (drop (nSeguidos x xs) (x:xs) )
                     | otherwise = replicate (nSeguidos x xs) x ++  nSeguidosSTR2 (drop (nSeguidos x xs) (x:xs) )

{- | Função que transforma um número de carateres seguidos numa string em que a cabeça é o numero de vezes que esse carater aparece seguido.
    
    >>> nSeguidosSTR "ab????a"
    "ab4?a"

-}

nSeguidosSTR :: String -> String
nSeguidosSTR [] = []
nSeguidosSTR (x:xs) | (nSeguidos x xs) >= 3 = show ((nSeguidos x xs)) ++ (x:[]) ++ nSeguidosSTR (drop (nSeguidos x xs) (x:xs) )
                    | otherwise = replicate (nSeguidos x xs) x ++  nSeguidosSTR (drop (nSeguidos x xs) (x:xs) )

{- | Função que se tiver duas letras minusculas iguais seguidas transforma-as numa maiuscula.
    
    >>> parteList "3? ?+!?"
    ["3?"," ?","+!","?"]

-}

parteList :: String -> [String]
parteList [] = []
parteList p@ (y:ys) | isDigit y == True = aux1 p : parteList (drop (length (aux1 p)) p)
                    | otherwise = aux2 p 0 : parteList (drop (length (aux2 p 0)) p)

{- | Função auxiliar da 'parteList'.
-}

aux1 :: String -> String
aux1 str = takeWhile (\str -> isDigit str == True) str ++ take 1 (drop (length (takeWhile (\str -> isDigit str == True) str)) str)

{- | Função auxiliar da 'parteList'.
-}

aux2 :: String -> Int -> String
aux2 [] z = []
aux2 (y:ys) z | isDigit y == True || z==2 = []
              | otherwise = y : aux2 ys (z+1) 


{- | Função que transforma o resultado de 'parteList' em letras.

    >>> compAlpha ["3?"," ?","+!","?"]
    ["3?","b","m","?"]
-}

compAlpha :: [String] -> [String]
compAlpha [] = []
compAlpha (y:ys) | isDigit (head y) || elem '<' y || elem '>' y = y : compAlpha ys
                 | head y == ' '    = subEsp y : compAlpha ys
                 | head y == '?'    = subInt y : compAlpha ys
                 | head y == '+'    = subPlus y : compAlpha ys
                 | head y == '!'    = subEx y : compAlpha ys

{- | Função auxiliar da 'comAlpha' se ve o padrão.
-}

subEsp :: String -> String
subEsp [y] = [y]
subEsp (y:t:ys) | t == ' '  = "a"
                | t == '?'  = "b"
                | t == '+'  = "c"
                | otherwise = "d"

{- | Função auxiliar da 'comAlpha' se ve o padrão.
-}

subInt :: String -> String
subInt [y] = [y]
subInt (y:t:ys) | t == ' '  = "e"
                | t == '?'  = "f"
                | t == '+'  = "g"
                | otherwise = "h"

{- | Função auxiliar da 'comAlpha' se ve o padrão.
-}

subPlus :: String -> String
subPlus [y] = [y]
subPlus (y:t:ys) | t == ' '  = "i"
                 | t == '?'  = "j"
                 | t == '+'  = "k"
                 | otherwise = "m"

{- | Função auxiliar da 'comAlpha' se ve o padrão.
-}

subEx :: String -> String
subEx [y] = [y]
subEx (y:t:ys)  | t == ' '  = "n"
                | t == '?'  = "o"
                | t == '+'  = "p"
                | otherwise = "q"

{- | Função auxiliar da 'comBeta' se ve o padrão.
-}

subPLa :: String -> String
subPLa [y] = [y]
subPLa p@(y:t:ys)  | t == 'b'  = "@"
                   | t == 'e'  = ","
                   | t == 'f'  = ";"
                   | otherwise = p

{- | Função auxiliar da 'comBeta' se ve o padrão.
-}

subPLb :: String -> String
subPLb [y] = [y]
subPLb p@(y:t:ys)  | t == 'a'  = "."
                   | t == 'e'  = ":"
                   | t == 'f'  = "-"
                   | otherwise = p

{- | Função auxiliar da 'comBeta' se ve o padrão.
-}

subPLe :: String -> String
subPLe [y] = [y]
subPLe p@(y:t:ys)  | t == 'a'  = "_"
                   | t == 'b'  = "~"
                   | t == 'f'  = "^"
                   | otherwise = p

{- | Função auxiliar da 'comBeta' se ve o padrão.
-}

subPLf :: String -> String
subPLf [y] = [y]
subPLf p@(y:t:ys)  | t == 'a'  = "%"
                   | t == 'b'  = "$"
                   | t == 'e'  = "&"
                   | otherwise = p

{- | Função que transforma duas letras num simbolo.

     >>> compBeta ["?a","ab","ba","ik"]
     ["?a","@",".","ik"]
-}

compBeta :: [String] -> [String]
compBeta [] = []
compBeta (y:ys)  | isDigit (head y) || elem '<' y || elem '>' y = y : compBeta ys
                 | head y == 'a'    = subPLa y : compBeta ys
                 | head y == 'b'    = subPLb y : compBeta ys
                 | head y == 'e'    = subPLe y : compBeta ys
                 | head y == 'f'    = subPLf y : compBeta ys
                 | otherwise        = y : compBeta ys

{- | Função criada com o intuito de ser usada para atribuir a cada um dos numeros inteiros que se encontram na lista dos valores aleatórios um Char.


     == Exemplos de utilização:
     >>> rand 83
     ' '

-}

rand :: Char -> Char
rand x 
        | x == '+' || x == '!' || x == '?'  = '?'
        | otherwise = ' '

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

fillMap2 :: Int -> [(Int,Int)] -> String -> String
fillMap2  x [] y = []
fillMap2  x ((a,b):ys) (t:t') | b == 0   || b == x - 1                                 = replicate x '#'
                              | a == 0   || a== x - 1                                  = '#' : fillMap2 x ys (t:t')
                              | (b == 1 || b == x-2) && ( a >= (x-3) || a <=  2 )      = ' ' : fillMap2 x ys (t:t')  
                              | (b == 2 || b == x-3) && ( a == 1 || a == x-2)          = ' ' : fillMap2 x ys (t:t')
                              | linha (a,b) == True                                    = '#' : fillMap2 x ys (t:t')
                              | otherwise                                              = rand t : fillMap2 x ys t'

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

mapa1 :: Int -> String -> [String]
mapa1 x y = aux x  y 0 where
  aux x y z 
            | z > x - 1       = []
            | otherwise       = fillMap2 x (geraLinha x (0,z))  o : aux x y (z+1) where o = drop (soma x (z-1)) y

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
         | z == x - 1         = 0 + soma x (z-1)
         | z == 1 || z == x-2 = (x - 6) + soma x (z-1)
         | z == 2 || z == x-3 = (alpha x - 2) + soma x (z-1)
         | even (z) == True   =  (alpha x) + soma x (z-1)
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

powerUP :: Int -> [(Int,Int)] -> String -> [String]
powerUP x [] _ = []
powerUP x t y = aux x t y [] [] where
    aux x [] y w k = w ++ k
    aux x ((a,b):t') (y:ys) w k | (y == '+' || y == '>') && w == []  = aux x t' ys (("+" ++ " " ++ show (a) ++ " " ++ show (b)):[]) k
                                | (y == '+' || y == '>')             = aux x t' ys (w ++ ("+" ++ " " ++ show (a) ++ " " ++ show (b)):[]) k
                                | (y == '!' || y == '<') && k == []  = aux x t' ys w (("!" ++ " " ++ show (a) ++ " " ++ show (b)):[])
                                | (y == '!' || y == '<')             = aux x t' ys w (k ++ ("!" ++ " " ++ show (a) ++ " " ++ show (b)):[])
                                | otherwise = aux x t' ys w k

{- | Função principal, que utiliza tanto a função que gera o mapa em si, como a função que gera as linhas com as coordenadas dos power ups.
    
     == Exemplos de utilização:
     >>> mapa2 9 0
     ["#########","#       #","# #?#?# #","#  ?  ? #","#?# # #?#","# ?  ?  #","# #?#?# #","#  ??   #","#########","+ 5 2","+ 3 3","! 5 5"]

-}

mapa2 :: Int -> String -> [String]
mapa2 x y = (mapa1 x r) ++ powerUP x (geraParesR x (0,0)) r where
  r = y ++ "1"

           
main :: IO ()
main = do a <- getArgs
          let p = a !! 0
          w <- getContents
          if length a == 1 && length p == 2 && (p=="-e" || p=="-d")
             then if p=="-e" then putStr $ encode $ lines w
                             else putStr $ unlines $ decode w
             else putStrLn "Parâmetros inválidos"