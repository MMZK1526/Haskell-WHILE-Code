module SimpleExp where

-- | Simple Expression:
-- E ::= n | E + E | E * E
data SimpleExp 
  = Nmbr {num :: Integer} 
  | Plus {exp1 :: SimpleExp, exp2 :: SimpleExp}
  | Prod {exp1 :: SimpleExp, exp2 :: SimpleExp}
  deriving (Eq)

{-# INLINE isNmbr #-}
{-# INLINE isPlus #-}
{-# INLINE isProd #-}
isNmbr, isPlus, isProd :: SimpleExp -> Bool
isNmbr Nmbr {} = True
isNmbr _       = False
isPlus Plus {} = True
isPlus _       = False
isProd Prod {} = True
isProd _       = False

-- | Add a pair of round braces to the String.
{-# INLINE addBrace #-}
addBrace :: String -> String
addBrace = ('(' :) . (++ ")")

-- | Apply a function if the predicate is True.
{-# INLINE applyOn #-}
applyOn :: Bool -> (a -> a) -> a -> a
applyOn p f a = if p then f a else a

instance Show SimpleExp where
  show exp = "Exp: " ++ show' exp
    where
      show' (Nmbr n) = show n
      show' (Plus e e') 
        = show' e ++ 
          " + " ++ 
          applyOn (isPlus e') addBrace (show' e')
      show' (Prod e e')
        = applyOn (isPlus e) addBrace (show' e) ++ 
          " * " ++ 
          applyOn (not $ isNmbr e') addBrace (show' e')

instance Num SimpleExp where
  x + y = Plus x y
  x * y = Prod x y
  abs = undefined
  signum = undefined
  fromInteger = Nmbr
  negate = undefined

-- | Is normal (irreducible).
{-# INLINE isNormal #-}
isNormal :: SimpleExp -> Bool
isNormal = isNmbr

-- | Big-Step evaluation.
eval :: SimpleExp -> Integer
eval (Nmbr n)    = n                -- B-Num
eval (Plus e e') = eval e + eval e' -- B-Add
eval (Prod e e') = eval e * eval e' -- B-Mul

-- | Small-Step evaluation.
eval1 :: SimpleExp -> SimpleExp
eval1 (Plus (Nmbr n) (Nmbr n')) = Nmbr $ n + n'            -- S-Add-Num
eval1 (Plus (Nmbr n) e')        = Plus (Nmbr n) $ eval1 e' -- S-Add-Right
eval1 (Plus e        e')        = Plus (eval1 e) e'        -- S-Add-Left
eval1 (Prod (Nmbr n) (Nmbr n')) = Nmbr $ n * n'            -- S-Mul-Num
eval1 (Prod (Nmbr n) e')        = Prod (Nmbr n) $ eval1 e' -- S-Mul-Right
eval1 (Prod e        e')        = Prod (eval1 e) e'        -- S-Mul-Left
eval1 _                         = error "Cannnot reduce a normal form!"
