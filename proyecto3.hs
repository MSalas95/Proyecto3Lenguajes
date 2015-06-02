import System.Directory
import Text.Printf
import Data.Fixed

type Matrix =[[(Int,Int,Int)]]
type Fila =[(Int,Int,Int)]

graficar funcion x0 x1 w h = createFile (crear funcion x0 x1 w h)

createFile datos = writeFile "salida.pbm" ("P3 "++datos) 

roundToStr :: (PrintfArg a, Floating a) => Int -> a -> String
roundToStr = printf "%0.*f"

reversa :: [b] -> [b]
reversa [] = []
reversa [x] = [x]
reversa xs = last xs : reversa (init xs)

myabs :: Float -> Float
myabs n = if n >= 0 then n else -n

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
					
coordenadaValor :: (Integral a) => a -> a -> Float
coordenadaValor x1 x2 = ((fromIntegral x2)-((fromIntegral x1)/2))/10


crear :: (Floating a, Integral c, Show c, PrintfArg b, Floating b, Ord a)=>(a -> b) -> a -> a -> c -> c -> [Char]
crear f x0 x1 w h = (show w ++ " "++ show h ++ " 255 " ++reemplazar (show (crearColumna f x0 x1 w h h))) 

crearColumna :: (Integral  a, Floating b, PrintfArg c, Floating c, Ord b) => (b -> c) -> b -> b -> a -> a -> a -> Matrix
crearColumna f x0 x1 x 0 y2 = []	
crearColumna f x0 x1 x y y2 = reversa (crearFila f x0 x1 x y x y2):crearColumna f x0 x1 (x) (y-1) y2

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
		gris_claro = (128,128,128)
		gris_oscuro = (64,64,64)
		fx = (roundToStr 1 (coordenadaValor y2 y))
		realy = (((fromIntegral y)-((fromIntegral y2)/2))/10)
		realx = (((fromIntegral x)-((fromIntegral x2)/2))/10)
		valorx = (((fromIntegral x)-((fromIntegral x2)/2))/10)		
		expr = (roundToStr 1 (f realx))
		--expr = (roundToStr 1 ( f 2.8))
		--expr = (roundToStr 1 ( cos(coordenadaValor x2 x)))

-- createFile (crear(\x -> x) 1 1 100 100)


