import System.Directory
import Text.Printf
import Data.Fixed

-- ///////////////////////////////// RETO 1 DE HENU /////////////////////////////////////////--

type Matrix =[[(Int,Int,Int)]]
type Fila =[(Int,Int,Int)]

--Llamada de la funcion
graficar funcion x0 x1 w h = createFile (crear funcion x0 x1 w h)

-- Crea y escribe en el archivo
createFile datos = writeFile "salida.pbm" ("P3 "++datos) 

-- Redondea a X decimales
roundToStr :: (PrintfArg a, Floating a) => Int -> a -> String
roundToStr = printf "%0.*f"

--Reversar una lista
reversa :: [b] -> [b]
reversa [] = []
reversa [x] = [x]
reversa xs = last xs : reversa (init xs)

-- Reemplaza los caracteres de una cadena
reemplazar :: [Char] -> [Char]
reemplazar a = map repl a

repl :: Char -> Char
repl ',' = ' '
repl '['   = ' '
repl ')'   = ' '
repl '('   = ' '
repl ']'   = ' '
repl '"'   = ' '
repl c = c

--Devuelve un valor a escala de un pixel en un eje de coordenadas			
coordenadaValor :: (Integral a) => a -> a -> Float
coordenadaValor x1 x2 = ((fromIntegral x2)-((fromIntegral x1)/2))/10

-- Crea ek contenido del archivo y limpia las listas, quitandole los caracteres innecesarios
crear :: (Floating a, Integral c, Show c, PrintfArg b, Floating b, Ord a)=>(a -> b) -> a -> a -> c -> c -> [Char]
crear f x0 x1 w h = (show w ++ " "++ show h ++ " 255 " ++reemplazar (show (crearColumna f x0 x1 w h h))) 

--  Va creando las columnas de pixeles
crearColumna :: (Integral  a, Floating b, PrintfArg c, Floating c, Ord b) => (b -> c) -> b -> b -> a -> a -> a -> Matrix
crearColumna f x0 x1 x 0 y2 = []	
crearColumna f x0 x1 x y y2 = reversa (crearFila f x0 x1 x y x y2):crearColumna f x0 x1 (x) (y-1) y2

-- Aqui se pintan los pixeles de cada fila, primero se evalua si 
-- el valor del pixel en el eje de coordenadas pertenece a la 
-- funcion, si pertenece se pinta de azul, y si no se pinta de
-- blanco, a demas se pintan los ejes de coordenas para una mejor
-- visualizacion de las funciones resultantes.

crearFila :: (Integral  a, Floating b, Floating c, PrintfArg c, Ord b) => (b -> c) -> b -> b -> a -> a -> a -> a -> Fila
crearFila f x0 x1 0 y x2 y2 = []
crearFila f x0 x1 x y x2 y2 = 				

					if ( fx == expr) then 
						if ((realx >= x0) && (realx<=x1)) then
							azul:crearFila f x0 x1 (x-1) y x2 y2
						else
							 if((fromIntegral y2)/2==(fromIntegral y))then	

					    		if ((mod' valorx 1)==0) then					    		
					    			gris_oscuro:crearFila f x0 x1 (x-1) y x2 y2
					    		else
					    			gris_claro:crearFila f x0 x1 (x-1) y x2 y2

					   		 else
					    		if((fromIntegral x2)/2==(fromIntegral x))then
					    			if ((mod' realy 1)==0) then					    		
					    				gris_oscuro:crearFila f x0 x1 (x-1) y x2 y2
					    			else
					    				gris_claro:crearFila f x0 x1 (x-1) y x2 y2	
					    		else				   	
									blanco:crearFila f x0 x1 (x-1) y x2 y2
					else 					   	
					    if((fromIntegral y2)/2==(fromIntegral y))then		

					    	if ((mod' valorx 1)==0) then					    		
					    			gris_oscuro:crearFila f x0 x1 (x-1) y x2 y2
					    		else
					    			gris_claro:crearFila f x0 x1 (x-1) y x2 y2

					    else
					    	if((fromIntegral x2)/2==(fromIntegral x))then
					    		if ((mod' realy 1)==0) then					    		
					    			gris_oscuro:crearFila f x0 x1 (x-1) y x2 y2
					    		else
					    			gris_claro:crearFila f x0 x1 (x-1) y x2 y2	
					    	else				   	
								blanco:crearFila f x0 x1 (x-1) y x2 y2		


	where
		blanco = (255,255,255)
		azul = (26,0,255)
		gris_claro = (192,192,192)
		gris_oscuro = (64,64,64)
		fx = (roundToStr 1 (coordenadaValor y2 y))
		realy = (((fromIntegral y)-((fromIntegral y2)/2))/10)
		realx = (((fromIntegral x)-((fromIntegral x2)/2))/10)
		valorx = (((fromIntegral x)-((fromIntegral x2)/2))/10)		
		expr = (roundToStr 1 (f realx))		




-- /////////////////////////// Reto 2 /////////////////////////////////////


-- valida que se pueda aplicar el metodo a la funcion
cero:: (Fractional a, Eq a, Ord a,Show a) => (a->a)->a->a->String
cero f a b = if ((f a)*(f b)<0) then   show(resolv f a b) else "No se puede aplicar a la funcion"

-- Busca un valor que hace que la funcion sea cero utilizando
resolv :: (Fractional a, Eq a, Ord a) => (a->a)->a->a->a
resolv f a b 
    | (b-a)/2 < 0.001 = (a+b)/2
    | f(c) == 0        = c
    | f(a)*f(c) < 0    = resolv f a c
    | otherwise        = resolv f c b
       
       where 
       c = (a+b)/2



-- /////////////////////////// Reto 3 /////////////////////////////////////


---Le aplica el metodo de integracion Simpson 1/3 con repeticion a una funcion
integrar :: (Fractional a, Eq a) => (a->a) -> a -> a -> a -> a
integrar f x0 xn n = h/3 * ( (f (calcularXn x0 x0 h)) + (f (calcularXn x0 n h)) + 4*(sumatoria f 0 ((n-2)/2) x0 h 1) + 2*(sumatoria f 1 ((n-2)/2) x0 h 0) )
	where
		h = ((xn - x0)/n)

-- Calcula el valor de Xn utilizado para el metodo de Simpson
calcularXn :: (Fractional a, Eq a) => a -> a -> a -> a
calcularXn x0 n h = x0 + (n*h)

-- Realiza una sumatoria ultilizada para el metodo
sumatoria :: (Fractional a, Eq a) =>  (a->a) -> a -> a -> a -> a -> a -> a
sumatoria f k kn x0 h num 
		| k == kn = f (calcularXn x0 (2*k+num) h)
		| otherwise = (f (calcularXn x0 (2*k+num) h)) + (sumatoria f (k+1) kn x0 h num)

