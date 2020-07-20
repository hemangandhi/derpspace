{-# LANGUAGE TemplateHaskell #-}
import Control.Monad
import Language.Haskell.TH


-- How should this work?
-- Roughly:
-- If `f x y = x + y`,
-- `withNamedParams f ["addend", "augend"]
-- gives a type `data fKwdArgsCoprod = addend a | augend a`
-- and a function `fKwds :: (Num a) => fKwdArgsCoprod a -> fKwdArgsCoprod a -> Maybe a`.
-- `fKwds (addend 1) (addend 2)` will produce `Nothing` though `fKwds (augend 3) (addend 2)`
-- should be `Just 5`.
withNamedParams :: Name -> [String] -> Q [Dec]
withNamedParams fn_name arg_names
   = do (PrimTyConI plz_b_arrow plz_b_2 