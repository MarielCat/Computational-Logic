module FormProp where

import Operadores
import Data.List

-- Representa variables p, q, r, s...
type Atom = String

-- Representa las variables que se evalúan a True.
type State = [Atom]

-- data Prop
data Prop = Var Atom
          | Neg Prop
          | Conj Prop Prop
          | Disy Prop Prop
          | Impl Prop Prop
          | Syss Prop Prop



instance Operadores Prop where
  (¬) p = Neg p
  (/\) p q = Conj p q
  (\/) p q = Disy p q
  (-->) p q = Impl p q
  (<-->) p q = Syss p q


---------------------------------------------------------------------------------
--------                             FUNCIONES                           --------
---------------------------------------------------------------------------------

-- Funcion que dada una formula, regresa el conjunto de todos los
-- símbolos que aparecen en ella.
vars :: Prop -> [Atom]
vars (Var p) = [p]
vars (Neg p) = vars p
vars (Conj p q) = nub $ vars p ++ vars q
vars (Disy p q) = nub $ vars p ++ vars q
vars (Impl p q) = nub $ vars p ++ vars q
vars (Syss p q) = nub $ vars p ++ vars q

-- Función que evalúa una proposición dada un estado.
interp :: State -> Prop -> Bool
interp s (Var p) = p `elem` s
interp s (Neg p) = not (interp s p)
interp s (Conj p q) = interp s p && interp s q
interp s (Disy p q) = interp s p || interp s q
interp s (Impl p q) = not (interp s p) || interp s q
interp s (Syss p q) = interp s p == interp s q

-- Función que elimina las equivalencias (<->).
elimEquiv :: Prop -> Prop
elimEquiv (Var p) = Var p
elimEquiv (Neg p) = Neg (elimEquiv p)
elimEquiv (Conj p q) = Conj (elimEquiv p) (elimEquiv q)
elimEquiv (Disy p q) = Disy (elimEquiv p) (elimEquiv q)
elimEquiv (Impl p q) = Disy (Neg (elimEquiv p)) (elimEquiv q)
elimEquiv (Syss p q) = Conj (Disy (Neg (elimEquiv p)) (elimEquiv q)) (Disy (Neg (elimEquiv q)) (elimEquiv p))

-- Función que elimina las implicaciones.
elimImpl :: Prop -> Prop
elimImpl (Var p) = Var p
elimImpl (Neg p) = Neg (elimImpl p)
elimImpl (Conj p q) = Conj (elimImpl p) (elimImpl q)
elimImpl (Disy p q) = Disy (elimImpl p) (elimImpl q)
elimImpl (Impl p q) = Disy (Neg (elimImpl p)) (elimImpl q)
elimImpl (Syss p q) = Syss (elimImpl p) (elimImpl q)

-- Función que da todas las posibles interpretaciones que podría tomar
-- una fórmula.
posiblesInterp :: Prop -> [State]
posiblesInterp prop = potencia (vars prop)

-- Función que nos dice si un estado es modelo de una proposición.
esModelo :: Prop -> State -> Bool
esModelo prop s = interp s prop

-- Función que nos da todos los modelos de una proposición.
todosModelos :: Prop -> [State]
todosModelos prop = filter (esModelo prop) (posiblesInterp prop)

-- Función que nos dice si una proposición es satisfacible.
esSatisfacible :: Prop -> Bool
esSatisfacible prop = not (null (todosModelos prop))

-- Función que nos dice si una proposición es insatisfacible.
esInsatisfacible :: Prop -> Bool
esInsatisfacible prop = null (todosModelos prop)

-- Función que nos dice si una proposición es una tautología.
esTautologia :: Prop -> Bool
esTautologia p = length (todosModelos p) == length (posiblesInterp p)

-- Función que nos dice si una proposición es una contradicción.
esContradiccion :: Prop -> Bool
esContradiccion p = null (todosModelos p)

-- Función auxiliar para calcular la potencia de un conjunto.
potencia :: [a] -> [[a]]
potencia [] = [[]]
potencia (x:xs) = [x:ys | ys <- xss] ++ xss
  where
    xss = potencia xs
