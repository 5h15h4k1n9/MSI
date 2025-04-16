{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
module Interpreter where
import Prelude
import Ast
import Control.Monad.Except
import qualified Data.List as List
import qualified Data.Map as Map
import Runtime
import Errors
import Control.Monad.State (runState)
import GHC.IO (unsafePerformIO)
import Debug.Trace
import Data.Map (empty)

-- Eval expr
evalAdd :: MSValue -> MSValue -> MSRuntime MSValue
evalAdd v1 v2 =
  case (v1, v2) of
    (VInt v1, VInt v2)       -> return $ VInt $ v1 + v2
    (VString v1, VString v2) -> return $ VString $ v1 ++ v2
    _                        -> throwUnsupportedBinOp Add v1 v2

evalEq :: MSValue -> MSValue -> MSRuntime MSValue
evalEq v1 v2 = return $ VBool $ v1 == v2

evalNeq :: MSValue -> MSValue -> MSRuntime MSValue
evalNeq v1 v2 = return $ VBool $ v1 /= v2

evalIndex :: MSValue -> MSValue -> MSRuntime MSValue
evalIndex v1 v2 =
  case (v2, v1) of
    (VArray _ vs, _)  -> arrayLookup v1 vs
    (VMapping _ _ m, _) -> mappingLookup v1 m
    _  -> throwUnsupportedBinOp Index v1 v2

evalIntOp :: MSBinOp -> (Int -> Int -> MSValue) -> MSValue -> MSValue -> MSRuntime MSValue
evalIntOp op f v1 v2 =
  case (op, v1, v2) of
    (Div, VInt _, VInt 0) -> throwMSInterpretationError ZeroDivision
    (_, VInt v1, VInt v2) -> return $ f v1 v2
    _                     -> throwUnsupportedBinOp op v1 v2

evalBoolOp :: MSBinOp -> (Bool -> Bool -> MSValue) -> MSValue -> MSValue -> MSRuntime MSValue
evalBoolOp op f v1 v2 =
  case (v1, v2) of
    (VBool v1, VBool v2) -> return $ f v1 v2
    _                    -> throwUnsupportedBinOp op v1 v2

wrap :: (b -> MSValue) -> (a -> a -> b) -> (a -> a -> MSValue)
wrap f op v1 v2 = f $ op v1 v2

evalBinOp :: MSBinOp -> MSValue -> MSValue -> MSRuntime MSValue
evalBinOp op = eval
  where
    eval =
      case op of
        Add   -> evalAdd
        Eq    -> evalEq
        Neq   -> evalNeq
        Minus -> evalIntOp Minus $ wrap VInt (-)
        Mul   -> evalIntOp Minus $ wrap VInt (*)
        Div   -> evalIntOp Div $ wrap VInt div
        Mod   -> evalIntOp Mod $ wrap VInt mod
        Gr    -> evalIntOp Gr $ wrap VBool (>)
        Le    -> evalIntOp Le $ wrap VBool (<)
        Gre   -> evalIntOp Gre $ wrap VBool (>=)
        Leq   -> evalIntOp Leq $ wrap VBool (<=)
        And   -> evalBoolOp And $ wrap VBool (&&)
        Or    -> evalBoolOp Or $ wrap VBool (||)
        Index -> evalIndex

evalUnOp :: MSUnOp -> MSValue -> MSRuntime MSValue
evalUnOp op v =
  case (op, v) of
    (Negate, VBool x) -> return $ VBool $ not x
    (UnMinus, VInt x) -> return $ VInt $ -x
    _                 -> throwMSInterpretationError $ UnsupportedUnOp op (typeOf v)

putArgs :: [MSValue] -> [(MSType, Id)] -> MSRuntime ()
putArgs vs args = do
  pushFrame
  pushScope
  helper vs args
  where
    helper (v:vs) ((t, name):args) = do
      putLocalVar name t v
      helper vs args
    helper [] [] = return ()
    helper vs args = throwMSInterpretationError MismatchedArgsCount

callFunction :: MSFunction -> [MSValue] -> MSRuntime (Maybe MSValue)
callFunction f args = do
  ctx <- getContext
  call f args ctx `catchError` catch ctx
  where
    onExit v ctx = do
      popScope
      popFrame
      setContext ctx
      return v
    call f args ctx = do
      putArgs args (argsSignature f)
      setContext Internal
      mapM_ evalStatement (body f)
      onExit Nothing ctx
    catch ctx e =
      case e of
        EInterpretation (Jump (Return v)) -> onExit (Just v) ctx
        e -> throwError e

evalExpr :: MSExpr -> MSRuntime MSValue
evalExpr (EValue v) = return v
evalExpr (EBinOp op e1 e2) = do
  v1 <- evalExpr e1
  v2 <- evalExpr e2
  evalBinOp op v1 v2
evalExpr (EUnOp op e) = do
  v <- evalExpr e
  evalUnOp op v
evalExpr (ECall name args) = do
  f <- getFunction name
  vs <- mapM evalExpr args
  res <- callFunction f vs
  case res of
    Just v -> return v
    Nothing -> return VVoid
evalExpr (EName name) = getValue name
evalExpr (EArrayInit t es) = do
  vs <- mapM evalExpr es
  return $ VArray t vs


-- Statement

evalBranch :: MSExpr -> MSRuntime a -> MSRuntime a -> MSRuntime a
evalBranch e trueBranch falseBranch = do
  v <- evalExpr e
  case v of
    VBool True  -> trueBranch
    VBool False -> falseBranch
    _           -> throwMSInterpretationError $ MismatchedType TBool (typeOf v)

evalBlock :: MSBlock -> MSRuntime ()
evalBlock b = do
  pushScope
  mapM_ evalStatement b
  popScope

evalLoop :: MSStatement -> MSExpr -> MSBlock -> MSRuntime ()
evalLoop delta cond body =
  catchError loop catch
  where
    trueBranch = do
      mapM_ evalStatement body
      evalStatement delta
      catchError loop catch
    falseBranch = do
      return ()
    loop = do
      evalBranch cond trueBranch falseBranch
    catch e =
      case e of
        EInterpretation (Jump Continue) -> loop
        EInterpretation (Jump Break)    -> return ()
        e                               -> throwError e

evalLocalVarDecl :: MSLocalVarDecl -> MSRuntime ()
evalLocalVarDecl (LocalVarDecl t i e) = do
  v <- evalExpr e
  putLocalVar i t v

evalStatement :: MSStatement -> MSRuntime ()
evalStatement (SExpr e) = do
  _ <- evalExpr e
  return ()
evalStatement (SFor decl cond delta block) = do
  pushScope
  evalLocalVarDecl decl
  evalLoop delta cond block
  popScope
evalStatement (SWhile cond block) = do
--  pushScope
  let delta = SExpr $ EValue VVoid
  evalLoop delta cond block
--  popScope
evalStatement (SLocalVarDecl decl) = evalLocalVarDecl decl
evalStatement (SAssign id e) = do
  v <- evalExpr e
  changeVar id v
evalStatement (SIndexAssign id is e) = do
  is <- mapM evalExpr is
  v <- evalExpr e
  changeByIndex id is v
evalStatement SBreak = throwMSInterpretationError $ Jump Break
evalStatement SContinue = throwMSInterpretationError $ Jump Continue
evalStatement (SRequire cond msg) = evalBranch cond trueBranch falseBranch
  where
    trueBranch = do
      return ()
    falseBranch = do
      msg <- evalExpr msg
      case msg of
        VString s -> throwMSInterpretationError $ RequireFailed s
        _         -> throwMSInterpretationError $ MismatchedType TString (typeOf msg)
evalStatement (SIf (ifCond, ifBlock) elifBrs elseBr) = evalBranch ifCond ifTrueBranch ifFalseBranch
  where
    ifTrueBranch = evalBlock ifBlock
    ifFalseBranch = evalOther elifBrs elseBr
    evalOther (elifBr:elifBrs) elseBr = do
      let (cond, block) = elifBr
      evalBranch cond (evalBlock block) (evalOther elifBrs elseBr)
    evalOther [] (Just elseBr) = evalBlock elseBr
    evalOther _ _ = return ()
evalStatement (SReturn e) = do
  v <- evalExpr e
  throwMSInterpretationError $ Jump $ Return v

evalTopLevelStatement :: MSTopLevelStatement -> MSRuntime ()
evalTopLevelStatement (STopLevelVarDecl t vis i e) = do
  v <-
    case e of
      Just e -> evalExpr e
      Nothing ->
        case t of
          TMapping tk tv -> return $ VMapping tk tv empty
          _              -> return $ VNull t
  putGlobalVar i vis t v
evalTopLevelStatement (SConstantDecl t vis i v) = putConst i vis t v
evalTopLevelStatement (SFunctionDecl i f) = putFunction i f

-- Program

loadContract :: MSProgram -> MSRuntime ()
loadContract p = mapM_ evalTopLevelStatement $ declarations p

evalRepl :: MSStatement -> MSRuntime MSValue
evalRepl s = do
  pushFrame
  setContext External
  case s of
    SExpr e -> do
      res <- evalExpr e
      popFrame
      return res
    SAssign _ _ -> do
      evalStatement s
      popFrame
      return VVoid
    _           -> undefined

-- type MSRuntime b = ExceptT MSError (State MSRuntimeData) b

run :: MSRuntime a -> MSRuntimeData -> (Either MSError a, MSRuntimeData)
run r = runState (runExceptT r)
