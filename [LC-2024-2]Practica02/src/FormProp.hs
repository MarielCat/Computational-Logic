module FormulasProposicionales where

import Operadores
import Data.List

-- Representa variables p, q, r, s...
type Atom = String

-- Representa las variables que se evalúan a True.
type State = [Atom]

-- data Prop
data Prop = Var Atom          -- Variable atómica
          | Neg Prop          -- Negación
          | Conj Prop Prop    -- Conjunción
          | Disy Prop Prop    -- Disyunción
          | Impl Prop Prop    -- Implicación
          | Syss Prop Prop    -- Equivalencia

-- Definición de la instancia Show para Prop
instance Show Prop where
  show (Var p) = p
  show (Neg p) = "¬" ++ show p
  show (Conj p q) = "(" ++ show p ++ " ^ " ++ show q ++ ")"
  show (Disy p q) = "(" ++ show p ++ " ∨ " ++ show q ++ ")"
  show (Impl p q) = "(" ++ show p ++ " → " ++ show q ++ ")"
  show (Syss p q) = "(" ++ show p ++ " ↔ " ++ show q ++ ")"

-- Definición de la instancia Operadores para Prop
instance Operadores Prop where
  (¬) :: Prop -> Prop
  (/\) :: Prop -> Prop -> Prop
  (\/) :: Prop -> Prop -> Prop
  (-->) :: Prop -> Prop -> Prop
  (<-->) :: Prop -> Prop -> Prop
  (¬) p = Neg p
  (/\) p q = Conj p q
  (\/) p q = Disy p q
  (-->) p q = Impl p q
  (<-->) p q = Syss p q

-- Función que dada una fórmula, regresa el conjunto de todas las
-- variables que aparecen en ella.
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

-- Ejemplo de uso de las funciones
i  = Conj (Var "p") (Var "q")
i' = (Var "p") /\ (Var "q")

p = Var "p"
q = Var "q"
r = Var "r"
varsrp = ["r", "p"]
form1 = ((p \/ q) /\ (((¬) q) \/ r))
interp1 = interp varsrp form1

taut1 = (p \/ (¬) p)
taut2 = ((p \/ q) \/ ((¬)p /\ (¬) q))

cont1 = ((p \/ q) /\ ((¬) p /\ (¬) q))
potencia1 = potencia [1,2,3]
