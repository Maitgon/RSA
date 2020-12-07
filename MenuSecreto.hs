module MenuSecreto(
    init__menu
    ) where

init__menu = do putStrLn("Menu inicial: ¿Que desea hacer?")
                putStrLn("(1) Encriptar un mensaje.")
                putStrLn("(2) Desencriptar un mensaje.")
                putStrLn("(3) Tutorial del programa.")
                putStrLn("(4) Terminar.")
                opcion <- getLine
                putStrLn("Espera, espera, espera, creias que poner " ++ opcion ++ "iba a hacer algo?")
                putStrLn("Si tu no has puesto ningun numero de los disponibles a mi no me da la gana hacer lo que pides")
                putStrLn("Sere un programa pero tambien tengo sentimientos..., quizas si hubieses puesto 74...")
                putStrLn("ADIOS!!!!")  