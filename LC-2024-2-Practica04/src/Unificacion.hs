module Unificacion where

import Data.List

-- Representa las cadenas.
type Nombre = String

-- Un término es una variable o un símbolo de función seguido de una
-- lista de términos.
data Termino = V Nombre
             | T Nombre [Termino]
             deriving Eq

-- Instancia show para terminos.
instance Show Termino where
  show (V nombre)    = nombre
  show (T nombre []) = nombre
  show (T nombre ts) = nombre ++ concat [show ts]

-- Una variable es un termino.
type Variable = Termino

-- Una sustitución es un par formado por una variable y un término.
type Sustitucion = [(Variable, Termino)]

-- Función que nos dice si un un termino es una variable.
esVariable :: Termino -> Bool
esVariable (V nombre) = True
esVariable nombre = False


-- Función que dado un término, regresa su lista de variables.
variables :: Termino -> [Variable]
variables (V i) = [(V i)]
variables (T i ts) = elim(variablesEnLista(ts))


-- Función que regresa la lista de variables dada una lista de términos.
variablesEnLista :: [Termino] -> [Variable]
variablesEnLista [] = []
variablesEnLista [x] = if esVariable(x) then [x] else variables x
variablesEnLista (x:xs) = if esVariable(x) then elim([x] ++ variablesEnLista xs) else elim(variables x ++ variablesEnLista xs)

-- Función que representa a la sustitución identidad.
epsilon :: Sustitucion
epsilon = []

-- Función que dada una sustitución, obtenemos su dominio.
dominio :: Sustitucion -> [Variable]
dominio [] = [] 
dominio (x:xs) = dominioAux x ++ dominio xs

-- Función que dada una sustitución y una variable, regresa la
-- aplicación de la sustitución a la variable.
aplicaVar :: Sustitucion -> Variable -> Termino
aplicaVar [] var = var
aplicaVar (x:xs) v = if  fst x == v then snd x else aplicaVar xs v

-- Función que dada una sustitución y un término, regresa la
-- aplicación de la sustitución al término.
aplicaT :: Sustitucion -> Termino -> Termino
aplicaT [] x = x
aplicaT s (V nombre) = aplicaVar s (V nombre)
aplicaT s (T nombre xs) = T nombre [aplicaT s x | x <- xs]

-- Función que regresa la sustitución obtenida, eliminando los pares
-- cuyos elementos son iguales.
reduce :: Sustitucion -> Sustitucion
reduce [] = []
reduce (x:xs) = (reduceAux x) ++ reduce xs


-- Función que dadas dos sustituciones, regresa su composición.
composicion :: Sustitucion -> Sustitucion -> Sustitucion
composicion xs ys = 
  (reduce [((fst x),(aplicaT ys (snd x))) | x <-  xs ]) ++ -- de s1 tomamos sus elementos y lo aplicamos a s2 sin repetidos
  [ a | a <- ys, (fst a) `notElem` (dominio xs) ] -- verificamos que las sustiticiones no esten en el dom

-- Función que dados dos términos, regresa la lista formada por el
-- unificador más general de ambos términos. Si no son unificables,
-- regresa la lista vacía.
unifica :: Termino -> Termino -> [Sustitucion]
unifica (V x) (V y) = if x == y then [epsilon] else [[(V x, V y)]] -- Devuelve la sustitución de identidad 
-- si las variables son iguales, o una sustitución que iguala las variables si son diferentes.
unifica (V x) t2    = if ((V x) `notElem` variables t2) then [[(V x,t2)]] else [] -- Aplica una sustitución si la variable no está en los términos de t2.
unifica  t1 (V x)   = if ((V x) `notElem` variables t1) then [[(V x,t1)]] else []   
unifica (T x l1) (T y l2) = [l | x == y, l <- unificaListas l1 l2] -- Unifica las listas de términos solo si los nombres de las funciones son iguales.

-- Función que regresa la lista formada por el unificador más general
-- de las listas de términos.
unificaListas :: [Termino] -> [Termino] -> [Sustitucion]
unificaListas [] [] = [epsilon] -- Devuelve la sustitución de identidad si ambas listas están vacías.
unificaListas l [] = [] -- No se puede unificar si la segunda lista está vacía, entonces devuelve una lista vacía.
unificaListas [] l = []
unificaListas (x:xs) (y:ys) = 
   [composicion l2 l1 | l1 <- unifica x y, -- Unifica las cabezas de ambas listas de términos.
                        l2 <- unificaListas [aplicaT l1 x | x <- xs] [aplicaT l1 y | y <- ys]]-- Compone las sustituciones de las listas de términos unificados, aplica las sustituciones a los términos restantes y unifica esas listas.
 
--FUNCIONES AUXILIARES

--Funcion que elimina las variables repetidas de una lista de variables
elim :: (Eq a) => [a] -> [a]
elim [] = []
elim (x:xs) = x : elim (filter (/= x) xs)

--Función que regresa una lista con la primer entrada de la pareja
dominioAux :: (Variable, Termino) -> [Variable]
dominioAux (x,t) = [x]

-- Funcion auxiliar que regresa una lista vacia si la primer entrada de la pareja es igual 
-- a la segunda.
reduceAux :: (Variable, Termino) -> Sustitucion
reduceAux (v,t) = if v == t then [] else [(v,t)]

---------------------------------------------------------------------------------
--------                             EJEMPLOS                            --------
---------------------------------------------------------------------------------

-- Ejemplos de variables.

x = V "x"
y = V "y"
z = V "z"
u = V "u"
w = V "w"

-- Ejemplos de constantes.

a = T "a" []
b = T "b" []
c = T "c" []

-- Ejemplos de simbolos de función.

f = T "f"
g = T "g"
h = T "h"
p = T "p"

-- Ejemplos de sustituciones
s1 = [(x, a), (z, f [x, y])]
s2 = [(x, z), (y, u)]
s3 = [(z, x), (x, b), (u, c)]
s4 = [(u, f [x]), (y, a)]
s5 = [(x, h [z]), (y, g [b])]

-- Ejemplos de las notas

-- ejemplo1 = unificaListas [w1] [w2]
w1 = f [w, f [x, h [z]]]
w2 = f [g [x], f [x, y]]

-- ejemplo2 = unificaListas [y1] [y2]
y1 = p [x, f [y]]
y2 = p [g [y, a], f [b]]

-- ejemplos funciones

esVariable1 = esVariable x
-- Regresa: True

esVariable2 = esVariable a
-- Regresa: False

variables1 = variables (g [f [x,y], z])
-- Regresa: [x,y,z]

variablesEnLista1 = variablesEnLista [f [x,y], g [f [x, y], z]]
-- Regresa: [x,y,z]

dominio1 = dominio s1
-- Regresa: [x,z]

aplicaVar1 = aplicaVar s1 x
-- Regresa: a

aplicaVar2 = aplicaVar s1 y
-- Regresa: y

aplicaVar3 = aplicaVar s1 z
-- Regresa: f [x, y]

aplicaT1 = aplicaT s1 (g [f [x, y], z])
-- Regresa: g [f [a, y], f [x, y]]

reduce1 = reduce [(x,a), (y,y), (z, f [x,y])]
-- Regresa: [(x,a), (z, f [x,y])]

composicion1 = composicion s2 s3
-- Retresa: [(y, c), (z, x), (u, c)]

composicion2 = composicion s4 s5
-- Retresa: [(u, f [h [z]]), (y, a), (x, h [z])]

unifica1 = unifica a a
-- Regresa: [[]]

unifica2 = unifica x a
-- Regresa: [[(x, a)]]

unifica3 = unifica x (f[y])
-- Regresa: [[(x, f[y])]]

unifica4 = unifica x (f[x])
-- Regresa: []

unifica5 = unifica (f[y]) x
-- Regresa: [[(x, f[y])]]

unifica6 = unifica (f[x]) x
-- Regresa: []

unificaListas1 = unificaListas [x, f[x], y] [a, y, z]
-- Regresa: [[(z, f[a]), (y, f[a]), (x, a)]]

unificaListas2 = unificaListas [x, f[x]] [y, y]
-- Regresa: []
