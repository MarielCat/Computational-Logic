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

module Trees where

import Data.List

data BTree a = Void | Node a (BTree a) (BTree a) deriving (Show, Eq)

--------------------------------------------------------------------------------
--------                            FUNCIONES                           --------
--------------------------------------------------------------------------------
-- | Devuelve el número total de nodos en un árbol binario.
--
--
--   La función 'nNodes' toma un árbol binario y devuelve el número total de nodos en ese árbol.
--   Si el árbol está vacío, devuelve 0. Para un árbol no vacío, cuenta el nodo actual y
--   recursivamente suma el número de nodos en sus dos subárboles.
nNodes :: BTree a -> Int
nNodes Void = 0
nNodes (Node nd x1 x2) = 1 + nNodes x1 + nNodes x2

-- | Devuelve el número de hojas en un árbol binario.

--
--   La función 'nLeaves' toma un árbol binario y devuelve el número de hojas en ese árbol.
--   Si el árbol está vacío, devuelve 0. Si el nodo actual no tiene hijos, cuenta como una hoja.
--   Recursivamente suma el número de hojas en los subárboles izquierdo y derecho.
nLeaves :: BTree a -> Int
nLeaves Void = 0
nLeaves (Node nd Void Void) = 1
nLeaves (Node nd x1 x2) = nLeaves x1 + nLeaves x2

-- | Devuelve el número de nodos internos en un árbol binario.

--   La función 'nni' toma un árbol binario y devuelve el número de nodos internos en ese árbol.
--   Si el árbol está vacío o si el nodo actual es una hoja, devuelve 0. De lo contrario,
--   cuenta el nodo actual como un nodo interno y recursivamente suma el número de nodos internos
--   en los subárboles izquierdo y derecho.
nni :: BTree a -> Int
nni Void = 0
nni (Node nd Void Void) = 0
nni (Node nd x1 x2) = 1 + nni x1 + nni x2

-- | Verifica si un elemento está presente en un árbol binario ordenado.

--
--   La función 'contains' toma un elemento y un árbol binario y verifica si el elemento está presente
--   en el árbol. Si el árbol está vacío, devuelve False. Si el elemento coincide con el nodo actual,
--   devuelve True. De lo contrario, busca recursivamente en los subárboles izquierdo y derecho.
contains :: (Ord a, Eq a) => a -> BTree a -> Bool
contains a Void = False
contains a (Node nd x1 x2) = a == nd || contains a x1 || contains a x2

-- | Recorrido inorder de un árbol binario.

--   La función 'inorder' realiza un recorrido inorder en un árbol binario y devuelve una lista
--   con los elementos en orden ascendente. Si el árbol está vacío, devuelve una lista vacía.
--   Combina recursivamente los elementos de los subárboles izquierdo, el nodo actual y los
--   subárboles derechos en una lista.
inorder :: BTree a -> [a]
inorder Void = []
inorder (Node nd x1 x2) = inorder x1 ++ [nd] ++ inorder x2

-- | Recorrido preorder de un árbol binario.

--   La función 'preorder' realiza un recorrido preorder en un árbol binario y devuelve una lista
--   con los elementos en el orden de la raíz, el subárbol izquierdo y el subárbol derecho.
--   Si el árbol está vacío, devuelve una lista vacía.
preorder :: BTree a -> [a]
preorder Void = []
preorder (Node nd x1 x2) = [nd] ++ preorder x1 ++ preorder x2

-- | Recorrido postorder de un árbol binario.

--   La función 'postorder' realiza un recorrido postorder en un árbol binario y devuelve una lista
--   con los elementos en el orden del subárbol izquierdo, el subárbol derecho y la raíz.
--   Si el árbol está vacío, devuelve una lista vacía.
postorder :: BTree a -> [a]
postorder Void = []
postorder (Node nd x1 x2) = postorder x1 ++ postorder x2 ++ [nd]

-- | Agrega un elemento a un árbol binario manteniendo el orden.
--
--   La función 'add' toma un elemento y un árbol binario y agrega el elemento al árbol
--   manteniendo el orden. Si el árbol está vacío, el elemento se coloca como raíz.
--   Si el elemento es menor que el nodo actual, se agrega al subárbol izquierdo.
--   Si el elemento es mayor que el nodo actual, se agrega al subárbol derecho.
--   Si el elemento es igual al nodo actual, no se agrega nuevamente.
add :: (Ord a) => a -> BTree a -> BTree a
add a Void = Node a Void Void
add a (Node nd x1 x2) | a < nd =  Node nd (add a x1) x2 | a > nd = Node nd x1 (add a x2)
  | otherwise = Node nd x1 x2

-- | Convierte una lista en un árbol binario manteniendo el orden.
--
--   La función 'fromList' toma una lista de elementos y un árbol binario y convierte la lista
--   en un árbol binario manteniendo el orden. Para cada elemento en la lista, se utiliza la
--   función 'add' para agregar el elemento al árbol.
fromList :: (Ord a) => [a] -> BTree a -> BTree a
fromList [] t = t
fromList (x:xs) Void = fromList xs (add x Void)
fromList (x:xs) t = fromList xs (add x t) 



--------------------------------------------------------------------------------
--------                           AUXILIARES                           --------
--------------------------------------------------------------------------------

--------------------------------------------------------------------------------
--------                             PRUEBAS                            --------
--------------------------------------------------------------------------------

v :: BTree Int
v = Void

test :: BTree Int
test = (Node 10
         (Node 5
           (Node 2
             (Node 1 v v)
             (Node 3 v v))
           (Node 8
             (Node 6 v v)
             (Node 9 v v)))
         (Node 15
           (Node 12
             (Node 11 v v)
             (Node 13 v v))
           (Node 17
             (Node 16 v v)
             (Node 18 v v))))

nn1 = nNodes test
-- Regresa: 15

nLeaves1 = nLeaves test
-- Regresa: 8

nni1 = nni test
-- Regresa: 7

contains1 = contains 0 test
-- Regresa: False

contains2 = contains 13 test
-- Regresa: True

inorder1 = inorder test
-- Regresa: [1,2,3,5,6,8,9,10,11,12,13,15,16,17,18]

preorder1 = preorder test
-- Regresa: [10,5,2,1,3,8,6,9,15,12,11,13,17,16,18]

postorder1 = postorder test
-- Regresa: [1,3,2,6,9,8,5,11,13,12,16,18,17,15,10]

add1 = add 0 test
-- Regresa:
-- Node 10
--   (Node 5
--     (Node 2
--      (Node 1
--       (Node 0 Void Void)
--       Void)
--      (Node 3 Void Void))
--     (Node 8
--       (Node 6 Void Void)
--       (Node 9 Void Void)))
--   (Node 15
--     (Node 12
--      (Node 11 Void Void)
--      (Node 13 Void Void))
--     (Node 17
--       (Node 16 Void Void)
--       (Node 18 Void Void)))

add2 = add 20 test
-- Regresa:
-- Node 10
--   (Node 5
--     (Node 2
--       (Node 1 Void Void)
--       (Node 3 Void Void))
--     (Node 8
--       (Node 6 Void Void)
--       (Node 9 Void Void)))
--   (Node 15
--     (Node 12
--       (Node 11 Void Void)
--       (Node 13 Void Void))
--     (Node 17
--       (Node 16 Void Void)
--       (Node 18 Void
--         (Node 20 Void Void))))

add3 = add 7 v
-- Regresa: Node 7 Void Void

fromList1 = fromList [5,3,8,1,2,6,9,7,10,4] Void
-- Regresa:
-- Node 5
--   (Node 3
--     (Node 1 Void
--       (Node 2 Void Void))
--     (Node 4 Void Void))
--   (Node 8
--     (Node 6 Void
--       (Node 7 Void Void))
--     (Node 9 Void
--       (Node 10 Void Void)))