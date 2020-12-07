module Tutorial (
    tutorial) where

tutorial:: IO()
tutorial = do 
		   putStr("Hola amiguito! ")
		   putStrLn("Vamos a aprender como encriptar con RSA.")
		   putStrLn("Primero generaremos dos primos p y q gracias al maravilloso algoritmo Miller Rabin")
		   putStrLn("que nos dira si un numero aleatorio es primo con una probabilidad altisima.")
		   putStrLn("\nAhora procederemos a denominar n al producto de nuestros numeros primos")
		   putStrLn("(en nuestro algoritmo llamada a prodN, que recibe como parametros a p y a q)")
		   putStrLn("\ny phi a la funcion de Euler de n, que por ser p y q primos se correspondera con (p-1)*(q-1).")
		   putStrLn("(en nuestro algoritmo llamada a phi, que recibe como parametros p y q)")
		   putStrLn("\nYa podemos proceder a generar nuestras claves publica y privada.")
		   putStrLn("(en nuestro algoritmo llamada a calculaE, que recibe como parametro a phi)")
		   putStrLn("\nPor un lado mi clave publica (n, e) estara compuesta de:")
		   putStrLn("en primer lugar un entero e que debera verificar: 1<e<phi y ademas mcd(phi, e) = 1")
		   putStrLn("y en segundo, por nuestro numero n (que te recuerdo, es el producto de mis dos primos iniciales).")
		   putStrLn("Por otro lado mi clave privada (n,d) estará compuesta de:")
		   putStrLn("en primer lugar un entero d que debera verificar: inv(e, phi) = d (e*d ´mod´ phi ==1)")
		   putStrLn("y en segundo lugar, de nuevo, nuestro numero n.")
		   putStrLn("\nYa tenemos todo lo que necesitamos para entrar en faena!!")
		   putStrLn("\nCOMENCEMOS")
		   putStrLn("\nUna vez seleccionado el mensaje que queremos encriptar, seleccionaremos un codigo de caracteres (en nuestro caso, el codigo ascii),")
		   putStrLn("y con el representaremos cada uno de los caracteres de nuestro mensaje numericamente;")
		   putStrLn("\nEjemplo 'h' 'o' 'l' 'a' --> (codigo ascii)--> '104' '111' '108' '97'")
		   putStrLn("\nAhora procederemos a encriptar nuestro mensaje haciendo uso de nuestra clave publica:")
		   putStrLn("(en nuestro algoritmo llamada a la función cript, que recibe como parámetros nuestro mensaje e y n)")
		   putStrLn("\nEl algoritmo RSA nos dice que C = (M^e) mod n; siendo M nuestro mensaje y C nuestro mensaje ya encriptado.")
		   putStrLn("Aplicado esto a cada uno de los dígitos de nuestro mensaje... ")
		   putStrLn("\n\nENHORABUENA, HAS ENCRIPTADO TU MENSAJE")
		   putStrLn("\n\nVeamos ahora como procedeer si queremos desencriptar un mensaje del que disponemos la clave privada")
		   putStrLn("(en nuestro algoritmo llamada a decrip, que recibe como parámetros a nuestro mensaje encriptado, d y n")
		   putStrLn("\nTe recuerdo que la clave privada estaba compuesta por (n, d) calculados anteriormente")
		   putStrLn("El mensaje a desencriptar, por ser un mensaje previamente encriptado nos llegara como una secuencia numerica.")
		   putStrLn("El algoritmo RSA nos dice que M = (C^d) mod n; donde C es el mensaje encriptado y M el mensaje ya desencriptado.")
		   putStrLn("Ahora simplemente procederemos a aplicarle lo anterior a cada uno de los dígitos de mi secuencia numérica")
		   putStrLn("\nPero, Como es posible que el mensaje que recibo sean siempre y unicamente numeros?")
		   putStrLn("\nNo se me estara olvidando a mi algo?")
		   putStrLn("\nBingo! Sabia que me pillarias, se trata del codigo ascii del principio!!")
		   putStrLn("Ahora simplemente encontrando la referencia de mi digito en el codigo ascii podre hacer la conversion de los numeros a los caracteres correspondientes")
		   putStrLn("y ...")
		   putStrLn("ENHORABUENA, YA SABES ENCRIPTAR Y DESENCRITAR A PARTIR DEL MARAVILLOSO ALGORITMO DE CLAVE PUBLICA Y CLAVE PRIVADA RSA")
		   putStrLn("\nAhora si que si...")
		   putStrLn("Comencemos a usar el codigo")

