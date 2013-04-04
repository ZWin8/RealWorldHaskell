data Op = Plus | Minus | Mul | Div | Pow deriving (Eq, Show)
data SymbolicManip a = 
                   Number a
                 | Arith Op (SymbolicManip a) (SymbolicManip a)
                   deriving  (Eq, Show)

instance Num a => Num (SymbolicManip a) where
         a + b = Arith Plus a b
         a - b = Arith Minus a b
         a * b = Arith Mul a b
         negate = Arith Mul (Number (-1))
         abs _ = error "abs is not implemented"
         signum _ = error "signum is not implemented"
         fromInteger i = Number (fromInteger i)

prettyShow :: (Num a, Show a) => SymbolicManip a -> String
prettyShow (Number a) = " " ++ show a
prettyShow (Arith op l r) = case op of
                             Plus -> " + "  ++ remain
                             Minus -> " - "  ++ remain
                             Mul -> " * "  ++ remain
                             Div -> " / "  ++ remain
                             Pow -> " ^ "  ++ remain
                            where remain = prettyShow l ++ prettyShow r

-- let e = ((5 + 10) * 2 ) :: SymbolicManip Int
-- prettyShow e
-- reverse . Data.List.words $ prettyShow e