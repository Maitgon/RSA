import System.Random
import MillerRabin
import RsaAlgorithm
import Tutorial
import MenuSecreto

--Funciones auxiliares para coger un entero:
getInt :: IO Int
getInt = do linea <- getLine
            return (read linea)

main :: IO ()
main = do putStrLn("Menu inicial: ¿Que desea hacer?")
               putStrLn("(1) Encriptar un mensaje.")
               putStrLn("(2) Desencriptar un mensaje.")
               putStrLn("(3) Tutorial del programa.")
               putStrLn("(4) Terminar.")
               opcion <- getInt
               case opcion of
                1 -> do encriptar
                2 -> do desencriptar
                3 -> do tutorial
                        main
                4 -> do putStrLn("Programa terminado.")
                _ -> do putStrLn("Número incorrecto. Tu ordenador se apagará en 3,")
                        putStrLn("2,")
                        putStrLn("1,")
                        putStrLn("Que no, que es broma.")
                        main


encriptar :: IO ()
encriptar = do putStr("Por favor, diga el nombre del fichero en el que se encuentra su mensaje.")
               nombre <- getLine
               p <- getRandPrime
               q <- getRandPrime
               let phi = (p-1)*(q-1)
               e <- calculaE2 phi
               getClaves e p q
               texto <- readFile nombre
               let m = (p*q)
               let rsa = cript2 texto e m
               writeFile "msgencriptado.txt" rsa
               putStrLn ("Su mensaje ha sido encriptado correctamente.")
               putStrLn("¿Que desea hacer ahora?")
               putStrLn("(1) Leer mi mensaje encriptado")
               putStrLn("(2) Volver al menu")
               opcion <- getInt
               case opcion of
                1 -> do putStrLn("Su mensaje encriptado es: " ++ show(rsa) ++ " Su mensaje se ha encriptado 1 vez.")
                        main
                2 -> do main
                _ -> do putStr("¿Pensabas que no habíamos considerado los casos incorrectos?")
                        putStrLn("Pues no.")
                        main

desencriptar :: IO () -- Aunque dependiendo de Alba y su función desencriptar, puede ser IO Int
desencriptar = do   putStrLn("Por favor, diga el nombre del fichero en el que se encuentra su mensaje.")
                    nombre <- getLine
                    texto <- readFile nombre
                    putStrLn("Dame la m.")
                    m <- getInt
                    putStrLn("Por favor, diga introduzca su clave privada.")
                    d <- getInt
                    let msgOriginal = decrip2 texto (toInteger d) (toInteger m)
                    writeFile "msgdesencriptado.txt" msgOriginal
                    putStr ("Su mensaje ha sido desencriptado correctamente.")
                    putStrLn("¿Que desea hacer ahora?")
                    putStrLn("(1) Leer mi mensaje desencriptado")
                    putStrLn("(2) Volver al menu")
                    opcion <- getInt
                    case opcion of
                        1 -> do putStrLn("Su mensaje desencriptado es: " ++ show(msgOriginal))
                                main
                        2 -> do main
                        _ -> do putStr("¿Pensabas que no habíamos considerado los casos incorrectos?")
                                putStrLn("Pues no.")
                                main

--Funciones auxiliares para desencriptar:
-- while m n f x es la función while (m < n) haz f empezando en x que actualiza m = m+1
while :: Int -> Int -> (a -> a) -> a -> a
while m n f x
    |m<n = while (m+1) n f (f x)
    |otherwise = x
--

