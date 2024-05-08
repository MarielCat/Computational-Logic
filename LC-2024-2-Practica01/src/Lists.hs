{-
- Lógica Computacional 2024-2
- Profesor: Francisco Hernández Quiroz
- Ayudante: Marco Vladimir Lemus Yáñez
- Ayudante: Naomi Itzel Reyes Granados
- Laboratorio: Emiliano Galeana Araujo
- Laboratorio: José Ricardo Desales Santos
- Practica 1: Recordando Haskell. Árboles
- Integrantes:
- Del Monte Ortega Maryam Michelle
- Monroy Romero Sahara Mariel
-}

module Lists where

data List a = Void | Cons a (List a) -- deriving (Show)

instance (Show a) => Show (List a) where
  show :: Show a => List a -> String
  show Void       = "[]"
  show (Cons a l) = "(" ++ show a ++ ":" ++ show l ++ ")"

--------------------------------------------------------------------------------
--------                            FUNCIONES                           --------
--------------------------------------------------------------------------------

-- | myHead. Función que regresa tal vez la cabeza de la lista.
myHead :: List a -> Maybe a
myHead Void = Nothing -- Si la lista es vacia, la cabeza no te da nada.
myHead (Cons x _) = Just x -- Si la lista no es vacia, la cabeza sera el primer elemento en la lista.

-- | myTail. Función que regresa tal vez la cola de la lista.
myTail :: List a -> Maybe (List a)
myTail Void = Nothing --Si la lista es vacia, no tiene cola.
myTail (Cons _ x) = Just x --Si la lista no es vacia, la cola sera el ultimo elemento.

-- | myLast. Función que regresa tal vez el último elemento de la
-- lista.
myLast :: List a -> Maybe a
myLast Void = Nothing --Si la lista es vacia, no tiene ultimo elemento.
myLast (Cons x Void) = Just x --Si la lista no es vacia, el ultimo elemento sera la cabeza
myLast (Cons x xs) = myLast xs --Llamado recursivo para encontrar el ultimo elemento.


-- | myLen. Función que regresa la longitud de la lista.
myLen :: List a -> Int
myLen Void = 0 --Cuando la lista es vacia, la longitud es 0.
myLen (Cons x xs) = myLen xs + 1 --La longitud de la lista sera 1 mas la longitud inicial.

-- | isElem. Función que nos dice si un elemento está en una lista.
isElem :: (Eq a) => List a -> a -> Bool
isElem Void a = False --Si la lista es vacia, el elemento no existe y regresa False
isElem (Cons x xs) a = (x == a) || isElem xs a --Si el elemento esta en la lista, regresa true, si no, llamado recursivo.

-- | myReverse. Función que regresa la reversa de una lista.
myReverse :: List a -> List a
myReverse Void = Void -- Si la lista es vacia, nos regresa una lista vacia
myReverse (Cons x xs) = myReverseHelper (Cons x xs) Void --Cualquier lista es ingresada a una funcion externa myHelper con una lista vacia como cola
  where
    -- | myReverseHelper Funcion que recibe dos listas, una lista que se ira invirtiendo, y una cola con los elementos ya invertidos, con lo que recursivamente nos dara toda la lista inversa.
    myReverseHelper :: List a -> List a -> List a
    myReverseHelper Void tail = tail -- Si la lo restante de la lista a invertir es vacio, regresamos la cola que ya es toda la lista invertida. 
    myReverseHelper (Cons h hs) tail =  myReverseHelper hs (Cons h tail) --Se va inviertiendo recursivamente toda lista excepto lo que ya se ha invertido guardado en una cola y se concatenan

-- | toHaskell. Función que pasa una de nuestras listas a las listas
-- de haskell.
toHaskell :: List a -> [a]
toHaskell Void = [] --Si la lista es vacia, la lista de haskell no tendrá ningun elemento.
toHaskell (Cons x xs) = x : toHaskell xs --Si la lista no es vacia, la lista de haskell sera el primer elemento de la lista mas el resto de la lista.


-- | fromHaskell. Función que pasa una lista de haskell a nuestras
-- listas.
fromHaskell :: [a] -> List a
fromHaskell [] = Void --Cuando recibe una list vacia, nos regresa otra lista vacia.
fromHaskell (x:xs) = Cons x (fromHaskell xs) -- Si la lista tiene al menos un elemento, creamos un nodo con ese elemento y llamamos recursivamente a fromHaskell con el resto de la lista

--------------------------------------------------------------------------------
--------                           AUXILIARES                           --------
--------------------------------------------------------------------------------

--------------------------------------------------------------------------------
--------                             PRUEBAS                            --------
--------------------------------------------------------------------------------

-- Lista que contiene a los primeros cinco elementos.
l1 = (Cons 1 (Cons 2 (Cons 3 (Cons 4 (Cons 5 Void)))))

-- Lista que contiene a los elementos del 6-10.
l2 = (Cons 6 (Cons 7 (Cons 8 (Cons 9 (Cons 10 Void)))))

head1 = myHead l1
-- Regresa: Just 1

head2 = myHead Void
-- Regresa: Nothing

tail1 = myTail l1
-- Regresa: Just (2:(3:(4:(5:[]))))

tail2 = myTail  Void
-- Regresa: Nothing

last1 = myLast l2
-- Regresa: 10

last2 = myLast Void
-- Regresa: Nothing

len1 = myLen l1
-- Regresa: 5

len2 = myLen l1
-- Regresa: 5

len3 = myLen Void
-- Regresa: 0

elem1 = isElem l1 9
-- Regresa: False

elem2 = isElem l2 9
-- Regresa: True

reverse1 = myReverse l1
-- Regresa: (5:(4:(3:(2:(1:[])))))

reverse2 = myReverse l2
-- Regresa: (10:(9:(8:(7:(6:[])))))

toHaskell1 = toHaskell l1
-- Regresa: [1,2,3,4,5]

toHaskell2 = toHaskell l2
-- Regresa: [6,7,8,9,10]

fromHaskell1 = fromHaskell [1,2,3]
-- Regresa: (1:(2:(3:[])))

fromHaskell2 = fromHaskell []
-- Regresa: Void
