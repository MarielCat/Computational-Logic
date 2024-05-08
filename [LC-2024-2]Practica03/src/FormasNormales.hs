module FormasNormales where

import Operadores ( Operadores(..) )
import Data.List ()
import FormProp ( Prop(..), interp, elimEquiv, elimImpl )

fnn1 :: Prop
fnn1 = fnn (Disy (Neg (Conj p q)) r)

-- ejemplo 2 para forma normal negativa
fnn2 :: Prop
fnn2 = fnn (Conj (Neg q) (Neg (Disy r s)))

-- ejemplo 1 para forma normal conjuntiva
fnc1 :: Prop
fnc1 = fnc(Disy p (Conj q (Disy r s)))

fnc2 :: Prop
fnc2 = fnc (Disy (Conj p (Neg q)) r)

fnd1 :: Prop
fnd1 = fnd (Neg (Conj (Neg q) (Neg r)))
-- ejemplo 2 para forma normal disyuntiva
fnd2 :: Prop
fnd2 = fnd (Conj (Neg (Conj p q)) (Conj r s))

-- | Forma Normal Negativa
--
--   La función 'fnn' toma una fórmula y devuelve una fórmula equivalente
--   en su forma normal negativa.
fnn :: Prop -> Prop
fnn f = neg (elimImpl (elimEquiv f))
    where
      -- Aplica negaciones solo a los términos atómicos
      neg :: Prop -> Prop
      neg (Neg (Conj p q)) = Disy (neg (Neg p)) (neg (Neg q))
      neg (Neg (Disy p q)) = Conj (neg (Neg p)) (neg (Neg q))
      neg (Neg (Neg p)) = neg p
      -- Formulas que se quedan igual
      neg (Conj p q) = Conj (neg p) (neg q)
      neg (Disy p q) = Disy (neg p) (neg q)
      neg (Neg (Var p)) = Neg (Var p)
      neg (Var p) = Var p
      neg (Impl p q) = Disy (neg (Neg p)) (neg q)

-- | Forma Normal Conjuntiva
--
--   La función 'fnc' toma una fórmula y devuelve una fórmula equivalente
--   en su forma normal conjuntiva.
fnc :: Prop -> Prop
fnc f = formConj (fnn f)
    where
      -- Aplica los operadores para formar conjunciones de disyunciones
      formConj :: Prop -> Prop
      formConj (Disy (Conj p q) (Conj r s)) = Conj (formConj (Conj (Disy p r) (Disy p s))) (formConj(Conj (Disy q r) (Disy q s)))
      formConj (Disy (Conj p q) (Disy r s)) = Conj (formConj(Disy p (Disy r s))) (formConj(Disy q (Disy r s)))
      formConj (Disy (Disy p q) (Conj r s)) = Conj (formConj(Disy p (Disy q r))) (formConj(Disy p (Disy q s)))
      formConj (Disy p (Conj q r)) = Conj (formConj (Disy p q)) (formConj (Disy p r))
      formConj (Disy (Conj p q) r) = Conj (formConj (Disy p r)) (formConj (Disy q r))
      formConj (Disy p q) = Disy (formConj p) (formConj q)
      formConj (Conj p q) = Conj (formConj p) (formConj q)
      -- Mantiene las variables y sus negaciones 
      formConj (Neg (Var p)) = Neg (Var p)
      formConj (Var p) = Var p

-- | Forma Normal Disyuntiva
--
--   La función 'fnd' toma una fórmula y devuelve una fórmula equivalente
--   en su forma normal disyuntiva.
fnd :: Prop -> Prop
fnd f = formDisy (fnn f)
    where
      -- Aplica los operadores para formar disyunciones de conjunciones
      formDisy :: Prop -> Prop
      formDisy (Conj (Disy p q) (Disy r s)) = Disy (formDisy (Disy (Conj p r) (Conj p s))) (formDisy(Disy (Conj q r) (Conj q s)))
      formDisy (Conj (Disy p q) (Conj r s)) = Disy (formDisy(Conj p (Conj r s))) (formDisy(Conj q (Conj r s)))
      formDisy (Conj (Conj p q) (Disy r s)) = Disy (formDisy(Conj p (Conj q r))) (formDisy(Conj p (Conj q s)))
      formDisy (Conj p (Disy q r)) = Disy (formDisy (Conj p q)) (formDisy (Conj p r))
      formDisy (Conj (Disy p q) r) = Disy (formDisy (Conj p r)) (formDisy (Conj q r))
      formDisy (Conj p q) = Conj (formDisy p) (formDisy q)
      formDisy (Disy p q) = Disy (formDisy p) (formDisy q)
      -- Mantiene las variables y sus negaciones 
      formDisy (Neg (Var p)) = Neg (Var p)
      formDisy (Var p) = Var p

-- Variables de ejemplo
p :: Prop
p = Var "p"

q :: Prop
q = Var "q"

r :: Prop
r = Var "r"

s :: Prop
s = Var "s"

instance Show Prop where
  show (Var p) = show p
  show (Neg p) = "-" ++ show p
  show (Conj p q) = "(" ++ show p ++ " ^ " ++ show q ++ ")"
  show (Disy p q) =  "(" ++ show p ++ " || " ++ show q ++ ")"
  show (Impl p q) = "(" ++ show p ++ " -> " ++ show q ++ ")"
  show (Syss p q) = "("++ show p ++ " <--> " ++ show q ++ ")"
