module MillerRabin (
    millerRabin,
    getr,
    getr',
    getd,
    witnessLoop,
    bucler,
    getRandPrime,
    generateRandomList,
    xkgt
) where

import System.Random

--El algoritmo de millerRabin es un algoritmo genial ya que nos dice si un numero es primo o no con una probabilidad de 1/4^k
--siendo k el numero de iteraciones que usaremos en el algoritmo y lo hace en el orden de O(klog^3n)
--Hemos tenido que usar System.Random ya que este juega con la probabilidad y la aleatoriedad
--El pseudocódigo de este es como sigue:

--Input #1: n > 3, el entero a probar
--Input #2: k, las iteraciones que haremos
--Output: el output debería ser probablemente primo y compuesto, pero usaremos True si es probablemente primo y False si es compuesto

--Escribir n como 2^r·d + 1 con d impar (obligar a que sea única)
--WitnessLoop: es el primer bucle que se repite k veces
--  coger un entero 'a' con valor entre: [2, n − 2]
--   x ← a^d mod n
--   si x = 1 o x = n − 1 entonces
--      hacer WitnessLoop
--   si no bucler: repetir r-1 veces:
--      x ← x^2 mod n
--      si x = n − 1 entonces
--         hacer WitnessLoop
--      si no bucler
--      fsi
--   return False
--return True

--Esta es la función principal, en ella descomponemos n y llamamos al bucle witnessLoop
--Nosotros vamos a suponer que nos dan una lista de enteros igual al número de iteraciones que realizaremos
--esa lista serán nuestros números aleatorios entre 2 y n-2

millerRabin :: Integer -> Integer -> [Integer] -> Bool
millerRabin n k xs = witnessLoop n k d r xs
    where r = getr n
          d = getd n
    

getr :: Integer -> Integer
getr m = getr' (m - 1)

getr' :: Integer -> Integer
getr' n
    |mod n 2 == 0 = 1 + getr' (div n 2)
    |otherwise = 0

getd n = getd' (n - 1)

getd' :: Integer -> Integer
getd' n = div n (2^(getr' n))

--En este bucle tenemos que usar random, y si cumple lo que le pedimos se repite, si no pasamos al siguiente bucle: bucler
--aunque el algoritmo pide n > 3, le especificamos el 1 y el 2

witnessLoop :: Integer -> Integer -> Integer -> Integer -> [Integer] -> Bool
witnessLoop 1 k d r (a:xs) = False
witnessLoop 2 k d r (a:xs) = True
witnessLoop n 0 d r [] = True
witnessLoop n k d r (a:xs) = if x==1 || x==(n-1) then witnessLoop n (k-1) d r xs else bucler n k d r r x xs
                                where x = mod (a^d) n

--el bucler: si este bucle se repite r-1 veces y no se consigue lo que queremos entonces es compuesto
--para hacer este bucle usé un contador r' que simplemente tiene el valor de r inicialmente pero cambia a lo largo de repetir el bucle, asi guardamos el valor de r

bucler :: Integer -> Integer -> Integer -> Integer -> Integer -> Integer -> [Integer] -> Bool
bucler n k d r 0 x xs = False
bucler n k d r r' x xs = if mod (x^2) n == (n-1) then witnessLoop n (k-1) d r xs else bucler n k d r (r'-1) (mod (x^2) n) xs

--Ahora voy a hacer una función que te da un número primo al azar cogiendo numeros al azar y viendo is son primos
--La haremos de rango entre 10 y 299 para no demorar el programa y solo iteramos 10 veces
--A pesar de iterar solo 10 veces la posibilidad de error es 1 entre 4^10

getRandPrime :: IO Integer
getRandPrime = do
    lista <- generateRandomList 10 []
    gen <- getStdGen 
    let (a, _) = randomR (10,299) gen :: (Integer, StdGen)
    newStdGen
    if millerRabin a 10 lista then return a else getRandPrime

--funcion que genera una lista de n numeros aleatorios entre 3 y 9999

generateRandomList :: Integer -> [Integer] -> IO [Integer]
generateRandomList 0 xs = do
    return xs
generateRandomList n xs = do
    gen <- getStdGen 
    let (a, _) = randomR (3,299) gen :: (Integer, StdGen)
    newStdGen
    generateRandomList (n-1) (a:xs)

--Función para probar si esto de arriba funciona

xkgt :: IO()
xkgt = do
    a <- getRandPrime
    putStrLn ("El primo es " ++ show a)














