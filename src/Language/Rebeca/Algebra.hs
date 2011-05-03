module Language.Rebeca.Algebra where

import qualified Language.Rebeca.Absrebeca as R


data ModelAlgebra id tvd tp env kr sv rc msi ms stm mai mod = ModelAlgebra {
    model :: [env] -> [rc] -> mai -> mod
  , envVar :: tp -> env
  , reactiveClass :: id -> kr -> sv -> msi -> [ms] -> rc 
  , knownRebecs :: [tvd] -> kr
  , stateVars :: [tvd] -> sv
  , msgSrvInit :: [tp] -> [stm] -> msi
  , msgSrv :: id -> [tp] -> [stm] -> ms
  , main :: R.Main -> mai
}


data StmAlgebra id exp tvd aft dea cs eli el stm = StmAlgebra {
    ass :: id -> exp -> stm
  , local :: tvd -> stm
  , call :: id -> id -> [exp] -> aft -> dea -> stm
  , after :: Maybe exp -> aft
  , deadline :: Maybe exp -> dea
  , delay :: exp -> stm
  , sel :: exp -> cs -> [eli] -> el -> stm
  , compStm :: [stm] -> cs
}

data ValAlgebra id tvd tp = ValAlgebra {
    typedVarDecl :: R.TypedVarDecl -> tvd
  , typedParameter :: R.TypedParameter -> tp
  , ident :: String -> id
}

data ExpAlgebra id exp = ExpAlgebra {
    lor :: exp -> exp -> exp
  , land :: exp -> exp -> exp
  , bitor :: exp -> exp -> exp
  , bitexor :: exp -> exp -> exp
  , bitand :: exp -> exp -> exp
  , eq :: exp -> exp -> exp
  , neq :: exp -> exp -> exp
  , lthen :: exp -> exp -> exp
  , grthen :: exp -> exp -> exp
  , le :: exp -> exp -> exp
  , ge :: exp -> exp -> exp
  , left :: exp -> exp -> exp
  , right :: exp -> exp -> exp
  , plus :: exp -> exp -> exp
  , minus :: exp -> exp -> exp
  , times :: exp -> exp -> exp
  , div :: exp -> exp -> exp
  , mod :: exp -> exp -> exp
  , expcoercion :: exp -> exp
  , nondet :: [exp] -> exp
  , preop :: R.UnaryOperator -> exp -> exp
  , now :: exp
  , const :: R.Constant -> exp
  , var :: [id] -> exp
}

