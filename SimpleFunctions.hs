-- 1. Construa um programa que ordena em ordem ascendente uma lista de listas a partir do tamanho das sublistas.

import Data.List
mysort xs = sortOn length xs

-- 2. Defina a função myMap :: (a -> b) -> (a -> b) -> [a] -> [b] que aplica de forma alternada duas funções passadas como argumentos aos elementos de uma lista. 

myMap :: (a -> b) -> (a -> b) -> [a] -> [b]
myMap _ _ [] = []  
myMap f1 _ (x:[]) = f1 x : []  
myMap f1 f2 (x:y:xs) = f1 x : f2 y : myMap f1 f2 xs  

-- 3. A partir da função myMap, defina um função luhn :: [Int] -> Bool que implemente o algoritmo de Luhn para a validações de números de cartão de crédito para códigos de cartão de qualquer tamanho. 

-- função auxiliar para subtrair 9 caso o número seja maior que 9
fa :: Int -> Int
fa n = if n < 10 then n else n-9

-- primeira parte da função Luhn que usa a função myMap 
luhna :: [Int] -> [Int]
luhna xs = myMap (*2) (+0) xs

-- segunda parte da função Luhn que aplica a função auxiliar a cada um dos números gerados pela primeira parte
luhnb :: [Int] -> Int
luhnb xs = sum (map fa (luhna xs))

-- função Luhn propriamente dita. Para cartões com 0 ou apenas 1 número a função retorna falso
luhn :: [Int] -> Bool
luhn [] = False
luhn [_] = False
luhn xs = mod (luhnb xs) 10 == 0

-- 4. Construa um programa em Haskell capaz de converter um número octal (na forma forma de string) 
-- em um número decimal. Trate uma entrada inválida com 0 octal. Não use funções prontas de conversão, 
-- construa a sua própria versão usando suas próprias funções ou as funções disponíveis no prelude.hs.

-- Função principal que deve ser chamada. Chama a função auxiliar que calcula o número decimal
oct2dec :: [Char] -> Int
oct2dec xs =  if (invalido xs) == True then 0 else oct2decAux xs  

-- Função auxiliar que calcula o número decimal
oct2decAux :: [Char] -> Int
oct2decAux n = sum [x*(8^y) | (x,y) <- zip (reverse(toVector n)) [0..]]

-- Função auxiliar que transforma uma String em vetor de inteiros
toVector :: [Char] -> [Int]
toVector [] = []
toVector (x:xs) = (toIntAux x) : (toVector xs) 


-- Função auxiliar que mapeia Char para Int
toIntAux :: Char -> Int
toIntAux x | x == '0' = 0
toIntAux x | x == '1' = 1
toIntAux x | x == '2' = 2
toIntAux x | x == '3' = 3
toIntAux x | x == '4' = 4
toIntAux x | x == '5' = 5
toIntAux x | x == '6' = 6
toIntAux x | x == '7' = 7
           | otherwise = -1

-- Função auxiliar que testa se a string é inválida ou não
invalido :: [Char] -> Bool
invalido [] = False
invalido (x:xs) =  if (toIntAux x) == -1 then True else (False || (invalido xs))


