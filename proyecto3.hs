import System.Directory
import Text.Printf

type Matrix =[[(Int,Int,Int)]]
type Fila =[(Int,Int,Int)]

roundToStr :: (PrintfArg a, Floating a) => Int -> a -> String
roundToStr = printf "%0.*f"

reversa :: [b] -> [b]
reversa [] = []
reversa [x] = [x]
reversa xs = last xs : reversa (init xs)

ponerFila :: (Integral  a) => a -> a -> a -> a -> Fila
ponerFila 0 y x2 y2 = []
ponerFila x y x2 y2 = if ( fx == expr)	then 
						(26,0,255):ponerFila (x-1) y x2 y2
					else 
						(255,255,255):ponerFila (x-1) y x2 y2

	where
		fx = (roundToStr 1 (coordenadaValor y2 y))
		expr = (roundToStr 1 ((cos(coordenadaValor x2 x))))


pintarCuadro :: (Integral  a) => a -> a -> a -> Matrix
pintarCuadro x 0 y2 = []	
pintarCuadro x y y2 = reversa (ponerFila x y x y2):pintarCuadro (x) (y-1) y2

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

funcion :: (Floating a) => a -> a
funcion x = sin(x)

mostrarImagen :: (Integral  a, Show a) => a -> a -> [Char]
mostrarImagen x y = (show x ++ " "++ show y ++ " 255 " ++reemplazar (show (pintarCuadro x y y)))
					
coordenadaValor :: (Integral a) => a -> a -> Float
coordenadaValor x1 x2 = ((fromIntegral x2)-((fromIntegral x1)/2))/10

createFile datos = writeFile "salida.pbm" ("P3 "++datos) 
 
