module Runtime where

import Ast
import Data.Map (Map, lookup, empty, insert, member, fromList, toList)
import Control.Monad.State.Lazy (gets, evalState, MonadState(get, put), State, lift)
import Prelude hiding (lookup, head)
import Errors
import Control.Monad.Except
import Debug.Trace


data Local = Local {
  localType  :: MSType,
  localValue :: MSValue
} deriving (Show)

data Global = Global {
  mutable     :: Bool,
  visibility  :: MSVisibility,
  globalType  :: MSType,
  globalValue :: MSValue
} deriving (Show)

type Scope a = Map Id a
type LocalScope = Scope Local
type GlobalScope = Scope Global

newtype Frame = Frame {
  scopes :: [LocalScope]
} deriving (Show)

emptyFrame :: Frame
emptyFrame = Frame {
    scopes = []
}

data MSExecutionContext =
    Internal
  | External
  deriving (Show)

data MSRuntimeData= MSRuntimeData {
    functions    :: Map Id MSFunction,
    globalScope  :: GlobalScope,
    frames       :: [Frame],
    context      :: MSExecutionContext
} deriving (Show)

emptyMSRuntime :: MSRuntimeData
emptyMSRuntime = MSRuntimeData {
    functions   = empty,
    globalScope = empty,
    frames      = [],
    context     = External
}

data MSRuntimeError =
    EmptyCallStack
  | EmptyScopeStack
  deriving (Show)

data MSError =
    ERuntime MSRuntimeError
  | EInterpretation MSInterpretationError
  deriving (Show)

type MSRuntime b = ExceptT MSError (State MSRuntimeData) b

-- helpers
throwMSRuntimeError :: MSRuntimeError -> MSRuntime t
throwMSRuntimeError r = throwError $ ERuntime r

throwMSInterpretationError :: MSInterpretationError -> MSRuntime t
throwMSInterpretationError r = throwError $ EInterpretation r

throwUnsupportedBinOp :: MSBinOp -> MSValue -> MSValue -> MSRuntime a
throwUnsupportedBinOp op v1 v2 = throwMSInterpretationError $ UnsupportedBinOp op (typeOf v1) (typeOf v2)

safeHead :: MSRuntimeError -> [t] -> MSRuntime t
safeHead err lst =
  case lst of
    x:xs -> return x
    []   -> throwMSRuntimeError err

safeTail :: MSRuntimeError -> [t] -> MSRuntime [t]
safeTail err lst =
  case lst of
    x:xs -> return xs
    []   -> throwMSRuntimeError err

getFrames :: MSRuntime [Frame]
getFrames = frames <$> get

setFrames :: [Frame] -> MSRuntime()
setFrames v = do
  msData <- get
  put $ msData { frames = v }

modifyFrames :: ([Frame] -> MSRuntime [Frame]) -> MSRuntime()
modifyFrames f = do
  x <- getFrames
  y <- f x
  setFrames y

getTopFrame :: MSRuntime Frame
getTopFrame = do
  frs <- getFrames
  safeHead EmptyCallStack frs

setTopFrame :: Frame -> MSRuntime ()
setTopFrame fr = do
  frs <- getFrames
  otherFrs <- safeTail EmptyCallStack frs
  setFrames $ fr:otherFrs

modifyTopFrame :: (Frame -> MSRuntime Frame) -> MSRuntime()
modifyTopFrame f = do
  x <- getTopFrame
  y <- f x
  setTopFrame y

modifyTopScope :: (LocalScope -> LocalScope) -> MSRuntime()
modifyTopScope f = modifyTopFrame g
  where
    g fr = do
      sc <- safeHead EmptyScopeStack $ scopes fr
      scs <- safeTail EmptyScopeStack $ scopes fr
      let newSc = f sc
      return $ fr { scopes = newSc:scs}

typeOf :: MSValue -> MSType
typeOf v =
  case v of
    VInt _           -> TInt
    VString _        -> TString
    VBool _          -> TBool
    VArray t _       -> TArray t
    VMapping tk tv _ -> TMapping tk tv
    VNull t          -> t
    VVoid            -> TVoid

assertType :: MSType -> MSValue -> MSRuntime ()
assertType t v = do
  let vt = typeOf v
  if t == vt
    then return ()
    else throwMSInterpretationError $ MismatchedType t vt

pickFrame :: MSRuntime Frame
pickFrame = do
  frs <- getFrames
  safeHead EmptyCallStack frs

popFrame :: MSRuntime ()
popFrame = modifyFrames $ safeTail EmptyCallStack

pushFrame :: MSRuntime ()
pushFrame = modifyFrames $ \frs -> return $ emptyFrame:frs

popScope :: MSRuntime ()
popScope = modifyTopFrame $ \fr -> do
  let scs = scopes fr
  newScs <- safeTail EmptyScopeStack scs
  return $ fr { scopes = newScs}

pushScope :: MSRuntime ()
pushScope = modifyTopFrame $ \fr -> do
  let scs = scopes fr
  return $ fr { scopes = empty:scs}

lookupGlobalScope :: Id -> MSRuntime (Maybe Global)
lookupGlobalScope i = do
  gsc <- globalScope <$> get
  return $ lookup i gsc

lookupLocalScope :: Id -> MSRuntime (Maybe Local)
lookupLocalScope id = do
  lscs <- scopes <$> pickFrame
  return $ lookupLocal id lscs
  where
    lookupLocal name (sc:scs) =
      case lookup name sc of
        Nothing -> lookupLocal name scs
        v       -> v
    lookupLocal _ _ = Nothing

lookupValue :: Id -> MSRuntime (Maybe MSValue)
lookupValue id = do
  ctx <- getContext
  lscValue <- lookupLocalScope id
  gscValue <- lookupGlobalScope id
  case ctx of
    Internal ->
      case (lscValue, gscValue) of
        (Just Local { localValue = v}, _)   -> return $ Just v
        (_, Just Global { globalValue = v}) -> return $ Just v
        _                                   -> return Nothing
    External ->
      case gscValue of
        Just Global { globalValue = v, visibility = Public } -> return $ Just v
        _                                                    -> return Nothing

getValue :: Id -> MSRuntime MSValue
getValue i = do
  v <- lookupValue i
  case v of
    Just v  -> return v
    Nothing -> throwMSInterpretationError $ UndefinedName i

putFunction :: Id -> MSFunction -> MSRuntime ()
putFunction i f = do
  msData <- get
  let fs = functions msData
  case lookup i fs of
    Just v  -> throwMSInterpretationError $ DuplicatedName i
    Nothing -> put $ msData { functions = insert i f fs}

getFunction :: Id -> MSRuntime MSFunction
getFunction i = do
  ctx <- getContext
  msData <- get
  case lookup i $ functions msData of
    Just v  ->
      case (ctx, funcVisibility v) of
        (Internal, _)      -> return v
        (External, Public) -> return v
        _                  -> throwMSInterpretationError $ UndefinedName i
    _ -> throwMSInterpretationError $ UndefinedName i

putConst :: Id -> MSVisibility -> MSType -> MSValue -> MSRuntime ()
putConst i vis t v = do
  const <- lookupGlobalScope i
  case const of
    Just _  -> throwMSInterpretationError $ DuplicatedName i
    _ -> do
      msData <- get
      let gscope = globalScope msData
      let cst = Global { mutable = False, visibility = vis, globalType = t, globalValue = v}
      put $ msData { globalScope = insert i cst gscope }

setGlobalVar :: Id -> MSVisibility -> MSType -> MSValue -> MSRuntime ()
setGlobalVar i vis t v = do
  msData <- get
  let gscope = globalScope msData
  let var = Global { mutable = True, visibility = vis, globalType = t, globalValue = v}
  put $ msData { globalScope = insert i var gscope }

putGlobalVar :: Id -> MSVisibility -> MSType -> MSValue -> MSRuntime ()
putGlobalVar i vis t v = do
  gvar <- lookupGlobalScope i
  case gvar of
    Just _  -> throwMSInterpretationError $ DuplicatedName i
    Nothing -> setGlobalVar i vis t v

modifyLocalVar :: Id -> MSType -> MSValue -> MSRuntime ()
modifyLocalVar i t v = do
  scs <- scopes <$> pickFrame
  newScs <- helper [] scs
  modifyTopFrame $ \fr -> return $ fr { scopes = newScs }
  where
    helper :: [LocalScope] -> [LocalScope] -> MSRuntime [LocalScope]
    helper _ [] = throwMSInterpretationError $ UndefinedName i
    helper l (sc:scs) = do
      case lookup i sc of
        Just _  -> do
          let var = Local { localType = t, localValue = v }
          let newSc = insert i var sc
          return $ reverse l ++ newSc:scs
        Nothing -> helper (sc:l) scs

setLocalVar :: Id -> MSType -> MSValue -> MSRuntime ()
setLocalVar i t v = do
    let var = Local { localType = t, localValue = v}
    modifyTopScope $ \sc -> insert i var sc

putLocalVar :: Id -> MSType -> MSValue -> MSRuntime ()
putLocalVar i t v = do
  lvar <- lookupLocalScope i
  case lvar of
    Just _  -> throwMSInterpretationError $ DuplicatedName i
    Nothing -> setLocalVar i t v

changeVar :: Id -> MSValue -> MSRuntime ()
changeVar i v = do
  lvar <- lookupLocalScope i
  gvar <- lookupGlobalScope i
  case (lvar, gvar) of
    (Just lvar, _) -> do
      let varType = localType lvar
      assertType varType v
      modifyLocalVar i varType v
    (_, Just gvar) -> do
      let varType = globalType gvar
      let vis = visibility gvar
      assertType varType v
      setGlobalVar i vis varType v
    _ ->
      throwMSInterpretationError $ UndefinedName i

mappingLookup :: MSValue -> Map MSValue MSValue -> MSRuntime MSValue
mappingLookup k m = do
  case lookup k m of
    Just v  -> return v
    Nothing -> throwMSInterpretationError $ KeyNotFound k

arrayLookup :: MSValue -> [MSValue] -> MSRuntime MSValue
arrayLookup i arr =
  case i of
    VInt n ->
      case safeIndex n arr of
        Just v  -> return v
        Nothing -> throwMSInterpretationError $ IndexOutOfBounds i
    _ -> throwMSInterpretationError $ MismatchedType (typeOf i) TInt
  where
    safeIndex 0 (x:_) = Just x
    safeIndex n (_:xs) = safeIndex (n - 1) xs
    safeIndex _ _      = Nothing

arraySet :: MSValue -> MSValue -> Id -> [MSValue] -> MSRuntime [MSValue]
arraySet i v name arr =
  case i of
    VInt n ->
      case safeIndexSet n [] arr v of
        Just v  -> return v
        Nothing -> throwMSInterpretationError $ IndexOutOfBounds i
    _ -> throwMSInterpretationError $ MismatchedType (typeOf i) TInt
  where
    safeIndexSet :: Int -> [MSValue] -> [MSValue] -> MSValue -> Maybe [MSValue]
    safeIndexSet 0 l (_:xs) v  = Just $ l ++ v:xs
    safeIndexSet n l (x:xs) v  = safeIndexSet (n - 1) (l ++ [x]) xs v
    safeIndexSet _ _ _ _  = Nothing

changeByIndex :: Id -> [MSValue] -> MSValue -> MSRuntime ()
changeByIndex name is v = do
  cur <- getValue name
  newV <- buildNewValue cur is v
  changeVar name newV
  where
    buildNewValue :: MSValue -> [MSValue] -> MSValue -> MSRuntime MSValue
    buildNewValue cur [i] v = do
      case cur of
        VMapping tk tv m -> do
          assertType tv v
          return $ VMapping tk tv $ insert i v m
        VArray t vs  -> do
          assertType t v
          arr <- arraySet i v name vs
          return $ VArray t arr
        _ -> throwUnsupportedBinOp Index cur i
    buildNewValue cur (i:is) v =
      case cur of
        VMapping tk tv m -> do
          oldInnerVal <- mappingLookup i m
          newInnerVal <- buildNewValue oldInnerVal is v
          return $ VMapping tk tv $ insert i newInnerVal m
        VArray t vs -> do
          oldInnerVar <- arrayLookup i vs
          newInnerVal <- buildNewValue oldInnerVar is v
          newArr <- arraySet i newInnerVal name vs
          return $ VArray t newArr
        _ -> throwUnsupportedBinOp Index cur i
    buildNewValue _ _ _ = undefined -- TODO: proper error handling

getContext :: MSRuntime MSExecutionContext
getContext = context <$> get

setContext :: MSExecutionContext -> MSRuntime ()
setContext ctx = do
  msData <- get
  put $ msData { context = ctx}
