module RsaAlgorithm (
    cript,
    decrip,
    doBoth,
    prodN,
    phi,
    calculaD2,
    calculaE2,
    cript2,
    decrip2,
    genClaves,
    getClaves
) where

import System.Random

--El algoritmo de cifrado asimétrico RSA es un algoritmo que trabaja con dos claves, una pública y otra privada. 
--Con el podremos tanto cifrar como descifrar información.
--El pseudocódigo de nuestro algoritmo viene dado por:

--para la generación de mis claves:

--Una vez obtenidos mis dos primos p, q de manera aleatoria,
--denominaremos n al producto de los dos primos (calculado en prodN) y será la segunda componente de nuestras claves tanto pública como privada, 
--y phi a la función de Euler del producto de estos dos primos (p-1)(q-1);
--así procederemos a calcular e y d que serán las componentes primeras de nuestra clave pública y privada respectivamente.
-- e por un lado deberá cumplir: 1<e<phi y mcd(phi, e) = 1;
-- d por otro lado deberá cumplir: inv(e, phi) = d y (e*d ´mod´ phi ==1);
-- asi para calcular e y d (calculaE):

--  genero un valor aleatorio aleat que sea entero
--  asigno aux <- abs(aleat) mod phi + 2
--  si MCD(phi, aux) == 1 entonces
--     aux es un candidato a ser mi primer elemento e de mi clave publica.
--     genero mi primer elemento d de mi clave privada (llamando a calculaD), 
--       si e*aux2 mod phi == 1 entonces
--           aux2 es mi primer componente de mi clave privada
--           de llegar aqui ya tendría generados mis pares (e, n) y (d, n) que se corresponden  con mi clave publica y privada respectivamente.
--  si no llamo de nuevo a mi función para que haga lo mismo pero a partir de otro entero generado aleatoriamente como antes.

--para el cifrado del mensaje (cript):                                          (C = M^e mod n)

--primero pasaremos cada uno de los caracteres de nuestra cadena a encriptar 
--  a su correspondiente entero del codigo ascii (strAEntero, strAListaInt);
--  ahora encriptaremos esta lista de enteros (cifradoRSA) a partir de nuestra clave publica (e, n);
--  aplicando a cada entero x <- lista enteros, x^e mod n (con expMod).
--  finalmente pasaremos esta lista de enteros a String (listaIntAStr) para poder realizar un print.

--para el descifrado del mensaje (decrip):                                      (M = C^d mod n)

--  primero pasaremos nuestra String a la correspondiente lista de enteros previa (con strAEntero),
--  desencriptaremos ahora nuestra (descifradoRSA) a partir de nuestra clave privada (d, n);
--  aplicando a cada entero x <- lista enteros, x^d mod n (con expMod).
--  Finalmente convertiremos cada uno de los enteros de mi lista en su correspondiente caracter (vía código ascii con intACh).

--para cifrar y descifrar:

--  Simplemente se ejecutarán con simultaneidad el cifrado y el descifrado del mensaje tal y como hemos descrito anteriormente.

--Ejemplo de prueba (Aitor porfi prueba para el caso que quieras y simplemente sustituye)

--  sean los primos p = , q = tales que n = p*q =  y phi = (p-1)*(q-1) entonces
--      e =  y d =      (por ejemplo) (calculados a partir de calculaE phi)
--  sea mi mensaje a encriptar ''Hola Mundo'', con (e, n) =   clave pública; y (d, n) =  clave privada:
--      cript ''Hola Mundo'' poner e  poner n  --> 

--      decrip 


import Data.Char as Chard

expMod :: (Integer, Integer, Integer) -> Integer
expMod (x, y, z) = (x ^ y) `mod` z

--cifrado --> C = M^e mod n donde M es mi mensaje
cifradoRSA :: [Integer] -> Integer -> Integer -> [Integer]
cifradoRSA msg e n = [expMod(x, e, n) | x <- msg]

--descifrado --> C^d mod n = M
--primero desencripto y luego lo paso a caracter
--[f x | x <- xs] es equivalente a aplicar map de f sobre toda xs
--fromIntegral
descifradoRSA :: [Integer] -> Integer -> Integer -> [Char]
descifradoRSA msgEncriptado d n = [intACh (fromIntegral(expMod (x, d, n))) | x <- msgEncriptado]

--splitOn "x" "axbxcx" -->["a","b","c",""]
--read "12" ::Double --> 12
-- ¿puedo split.splitOn " " xs?
strAEntero :: String -> [Integer]
strAEntero xs = map read (words xs)

--(toInteger) Integral a => a -> Integer
--para cada x caracter de mi msg convierto a un entero formando una lista de enteros
--ord:: Char -> Int
strAListaInt :: String -> [Integer]
strAListaInt msg = [toInteger (Chard.ord x) | x <- msg]

--show [1,2,3] --> "[1,2,3]"
--uno todos los enteros de mi lista formando una string.
listaIntAStr :: [Integer] -> String
listaIntAStr [] = ""
listaIntAStr (x : xs) = show x ++ " " ++ listaIntAStr xs

--pasamos de entero a caracter 
--chr:: Int -> Char
intACh :: Int -> Char
intACh val = Chard.chr val

--putStrLn devuelve String seguido de un linea en blanco
cript :: String -> Integer -> Integer -> IO()
cript xs e n = do
    let cifrar = strAListaInt xs -- Recibe tu mensaje a encriptar y le pasa ascii
    putStrLn ("\n Cifrando...")
    putStr ("\n Tu mensaje Encriptado: ")
    print (listaIntAStr (cifradoRSA cifrar e n));

decrip :: String -> Integer -> Integer -> IO()
decrip ys d n = do
    let descifrar = strAEntero ys -- trnsforma tu mensaje cifrado en una lista de enteros
    putStrLn ("\n Descifrando...")
    putStr ("\n Mensaje descifrado: ")
    print (descifradoRSA descifrar d n);    

doBoth :: String -> Integer -> Integer -> Integer -> IO()
doBoth ys e n d = do
    let cifrar = strAListaInt ys 
    putStrLn ("\n Cifrando...")
    putStr ("\n Mensaje cifrado: ")
    let encriptadoMSG = cifradoRSA cifrar e n 
    print (listaIntAStr(encriptadoMSG));    
    putStrLn ("\n desencriptando...")
    putStr ("\n Mensaje desencriptado: ")
    print (descifradoRSA encriptadoMSG d n);    




prodN :: Integer -> Integer -> Integer
prodN p q = p * q

phi :: Integer -> Integer -> Integer
phi p q = (p-1) * (q-1) 

--newRand = randomIO :: IO Int te da un primo nuevo siempre.
newRand = randomIO :: IO Integer

--para calcular e de mi clave publica este debera ser 1<e<phi y ademas mcd(phi, e) = 1
--n = C = phi --> si aux modulo entre mi num aleatorio y mi phi + 2 (si el modulo es 0 vale 2 y el mcd entre phi y aux es 1 ese me vale. (del dos en adelante)
calculaE :: Integer -> IO()
calculaE phi = 
    do 
        aleat <- newRand
        let aux = (abs(aleat)) `mod` phi + 2
        if (gcd phi aux) == 1 
            then
                do
                    putStr ("\n  e: " ++ (show aux) ++ "\n")
                    calculaD phi aux 0
                    else calculaE phi 
					
--deberá cuplirse que inv(e, phi) = d (e*d ´mod´ phi ==1) y para ser d --> nuestra aux es d (B), e (A) y to (C = phi) serán nuestros e y phi; 
--el inverso de A mod C es B tal que A*B mod C = 1
calculaD :: Integer -> Integer -> Integer -> IO()
calculaD to e aux = 
    if ((e * aux) `mod` to) == 1
        then putStr ("\n Tu Clave Privada: \n  d: "++ show (aux))
        else calculaD to e (aux + 1)

genClaves :: Integer -> Integer -> IO()
genClaves p q = do
    putStr ("\n Tu Clave pública: \n  MiN: "++show(n))
    calculaE t   
    putStr ("\n  Primo 1: "++show(p)++"\n  Primo 2: "++show(q)++ "\n\n")
    where   
        n = prodN p q
        t = phi p q
        
calculaE2 :: Integer -> IO Integer
calculaE2 phi = do
    aleat <- newRand
    let aux = (abs(aleat)) `mod` phi + 2
    if (gcd phi aux) == 1
        then
            do
                return aux
                else calculaE2 phi
                
calculaD2 :: Integer -> Integer -> Integer -> IO Integer
calculaD2 to e aux =
    if ((e * aux) `mod` to) == 1
        then return aux
        else calculaD2 to e (aux + 1)

cript2 :: String -> Integer -> Integer -> String
cript2 xs e n = listaIntAStr (cifradoRSA cifrar e n)
    where cifrar = strAListaInt xs
    
decrip2 :: String -> Integer -> Integer -> String
decrip2 ys d n = descifradoRSA descifrar d n
    where descifrar = strAEntero ys 

getClaves :: Integer -> Integer -> Integer -> IO ()
getClaves e p q = do
    putStrLn ("\n Tu Clave Publica: \n M: " ++ show n)
    putStrLn (" e: " ++ show e)
    d <- calculaD2 ((p-1)*(q-1)) e 0
    putStrLn ("\n Tu clave Privada: \n d: " ++ show d)
    putStrLn (" Primo 1: " ++ show p)
    putStrLn (" Primo 2: " ++ show q)
    where n = prodN p q


    
    
    
    
    
    