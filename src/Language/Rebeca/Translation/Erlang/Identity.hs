module Language.Rebeca.Translation.Erlang.Identity where

import Language.Rebeca.Absrebeca
import Language.Rebeca.Algebra
import Language.Rebeca.Fold

identityAlgebra = RebecaAlgebra {
    identF = Ident

  , modelF = Model

  , envVarF = EnvVar

  , reactiveClassF = ReactiveClass

  , noKnownRebecsF = NoKnownRebecs
  , knownRebecsF = KnownRebecs

  , noStateVarsF = NoStateVars
  , stateVarsF = StateVars

  , msgSrvInitF = MsgSrvInit

  , msgSrvF = MsgSrv

  , vDeclAssignF = VDeclAssign
  , vDeclF = VDecl

  , typedVarDeclF = TypedVarDecl
  , typedVarDeclAssF = TypedVarDeclAss

  , typedParameterF = TypedParameter

  , basicTypeIntF = Tint
  , basicTypeTimeF = Ttime
  , basicTypeBooleanF = Tboolean

  , builtInF = BuiltIn
  , classTypeF = ClassType

  , assF = Ass
  , localF = Local
  , callF = Call
  , delayF = Delay
  , selF = Sel

  , singleCompStmF = SingleCompoundStm
  , multCompStmF = MultCompoundStm

  , noAfterF = NoAfter
  , withAfterF = WithAfter

  , noDeadlineF = NoDeadline
  , withDeadlineF = WithDeadline

  , elseifStmF = ElseifStm

  , emptyElseStmF = EmptyElseStm
  , elseStmF = ElseStm

  , lorF = Elor
  , landF = Eland
  , bitorF = Ebitor
  , bitexorF = Ebitexor
  , bitandF = Ebitand
  , eqF = Eeq
  , neqF = Eneq
  , lthenF = Elthen
  , grthenF = Egrthen
  , leF = Ele
  , geF = Ege
  , leftF = Eleft
  , rightF = Eright
  , plusF = Eplus
  , minusF = Eminus
  , timesF = Etimes
  , divF = Ediv
  , modF = Emod
  , expcoercionF = Eexpcoercion
  , nondetF = ENondet
  , preopF = Epreop
  , nowF = Enow
  , constF = Econst
  , varF = Evar

  , constantIntF = Eint
  , constantTrueF = Etrue
  , constantFalseF = Efalse

  , unaryPlusF = Plus
  , unaryNegativeF = Negative
  , unaryComplementF = Complement
  , unaryLogicalNegF = Logicalneg

  , opAssignF = Assign
  , opAssignMulF = AssignMul
  , opAssignDivF = AssignDiv
  , opAssignModF = AssignMod
  , opAssignAddF = AssignAdd
  , opAssignSubF = AssignSub

  , mainF = Main

  , instanceDeclF = InstanceDecl

  , nilEnv = []
  , consEnv = unEnvList
  , nilRcl = []
  , consRcl = unRcList
  , nilMs = []
  , consMs = unMsList
  , nilTvd = []
  , consTvd = unTvdList
  , nilTp = []
  , consTp = unTpList
  , nilStm = []
  , consStm = unStmList
  , nilExp = []
  , consExp = unExpList
  , nilEli = []
  , consEli = unEliList
  , nilId = []
  , consId = unIdList
  , nilIns = []
  , consIns = unInsList
  , nilVd = []
  , consVd = unVdList
}

-- this can be quick-check tested
translateIdentity :: Model -> Model
translateIdentity = fold identityAlgebra
