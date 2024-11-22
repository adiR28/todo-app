{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}

module APIFlow.Fdep.Plugin (plugin) where

import Bag (bagToList)
import Control.Concurrent ( forkIO )
import Control.DeepSeq (force)
import Control.Exception (SomeException, evaluate, try)
import Control.Monad (void, when)
import Control.Monad.IO.Class (MonadIO (..))
import Control.Reference (biplateRef, (^?))
import Data.Aeson ( encode, Value(String, Object), ToJSON(toJSON) )
import qualified Data.Aeson as A
import Data.Aeson.Encode.Pretty (encodePretty)
import Data.Bool (bool)
import Data.ByteString.Lazy (toStrict, writeFile)
import qualified Data.ByteString.Lazy as BL
import Data.Data (toConstr)
import Data.Generics.Uniplate.Data ()
import qualified Data.HashMap.Strict as HM
import Data.List.Extra (splitOn)
import qualified Data.Map as Map
import Data.Maybe (fromJust, fromMaybe, isJust)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Encoding (decodeUtf8, encodeUtf8)
import Data.Time ( diffUTCTime, getCurrentTime )
import DynFlags ()
import Text.Read (readMaybe)
import APIFlow.Fdep.Types
    ( PFunction(PFunction),
      FunctionInfo(FunctionInfo), Func(Func),FunctionInfos(FunctionInfos))
import qualified APIFlow.Fdep.Types as FT
import GHC (
    GRHS (..),
    GRHSs (..),
    GhcTc,
    HsExpr (..),
    LGRHS,
    LHsExpr,
    LMatch,
    Match (m_grhss),
    MatchGroup (..),
    Module (moduleName),
    getName,
    hsmodDecls,
    moduleNameString, GhcPs,
 )
import GHC.Hs.Binds
    ( HsBindLR(PatBind, FunBind, AbsBinds, VarBind, PatSynBind,
               XHsBindsLR, fun_id, abs_binds, var_rhs),
      LHsBindLR,
      HsValBindsLR(XValBindsLR, ValBinds),
      HsLocalBindsLR(HsValBinds),
      NHsValBindsLR(NValBinds),
      PatSynBind(XPatSynBind, PSB, psb_def) )
import GHC.Hs.Decls
    ( HsDecl(SigD, TyClD, InstD, DerivD, ValD), LHsDecl )
import GHC.IO (unsafePerformIO)
import GhcPlugins (HsParsedModule, Hsc, Plugin (..), PluginRecompile (..), Var (..), getOccString, hpm_module, ppr, showSDocUnsafe, CoreToDo,CoreM,putMsgS)
import HscTypes (ModSummary (..),msHsFilePath)
import Name (nameStableString)
-- import Network.Socket (withSocketsDo)
import qualified Network.WebSockets as WS
import Outputable ()
import Plugins (CommandLineOption, defaultPlugin)
import SrcLoc ( GenLocated(L), getLoc, noLoc, unLoc,SrcSpan(RealSrcSpan),srcSpanStartLine,srcSpanEndLine)
import Streamly ( parallely )
import Streamly.Prelude (fromList, mapM, mapM_, toList)
import System.Directory ( createDirectoryIfMissing )
import System.Environment (lookupEnv)
import TcRnTypes (TcGblEnv (..), TcM)
import Prelude hiding (id, mapM, mapM_, writeFile)
import qualified Prelude as P
import qualified Data.List.Extra as Data.List
import StringBuffer
import qualified Data.Foldable as DF
import Data.List as DL
import qualified Data.Maybe as DM
import HscTypes (ModGuts(ModGuts))
import CoreMonad( CoreToDo( CoreDoPluginPass ) )

plugin :: Plugin
plugin =
    defaultPlugin{
        --    installCoreToDos = addFinalMessageAction
        , typeCheckResultAction = fDep
        , parsedResultAction = collectDecls
        }

-- Function to add a final action in the Core pipeline
-- addFinalMessageAction :: [CommandLineOption] -> [CoreToDo] -> CoreM [CoreToDo]
-- addFinalMessageAction _ todos = do
--     -- Define a final CoreToDo that runs at the end of the pipeline
--     let finalMessage = CoreDoPluginPass "FinalMessage" printFinalMessage
--     -- Append it at the end of the existing CoreToDos
--     return (todos ++ [finalMessage])

-- Function that prints the final message
-- printFinalMessage :: ModGuts -> CoreM ModGuts
-- printFinalMessage guts = do
--     -- Only print this message once after all modules are compiled
--     liftIO $ putStrLn "Compilation of all modules is complete."
--     return guts

-- purePlugin :: [CommandLineOption] -> IO PluginRecompile
-- purePlugin _ = return NoForceRecompile

install :: [CommandLineOption] -> [CoreToDo] -> CoreM [CoreToDo]
install _ todo = do
  putMsgS "Hello! Compiler"
  return todo

filterList :: [Text]
filterList =
    [ "show"
    , "showsPrec"
    , "from"
    , "to"
    , "showList"
    , "toConstr"
    , "toDomResAcc"
    , "toEncoding"
    , "toEncodingList"
    , "toEnum"
    , "toForm"
    , "toHaskellString"
    , "toInt"
    , "toJSON"
    , "toJSONList"
    , "toJSONWithOptions"
    , "encodeJSON"
    , "gfoldl"
    , "ghmParser"
    , "gmapM"
    , "gmapMo"
    , "gmapMp"
    , "gmapQ"
    , "gmapQi"
    , "gmapQl"
    , "gmapQr"
    , "gmapT"
    , "parseField"
    , "parseJSON"
    , "parseJSONList"
    , "parseJSONWithOptions"
    , "hasField"
    , "gunfold"
    , "getField"
    , "_mapObjectDeep'"
    , "_mapObjectDeep"
    , "_mapObjectDeepForSnakeCase"
    , "!!"
    , "/="
    , "<"
    , "<="
    , "<>"
    , "<$"
    , "=="
    , ">"
    , ">="
    , "readsPrec"
    , "readPrec"
    , "toDyn"
    , "fromDyn"
    , "fromDynamic"
    , "compare"
    , "readListPrec"
    , "toXml"
    , "fromXml"
    ]

collectDecls :: [CommandLineOption] -> ModSummary -> HsParsedModule -> Hsc HsParsedModule
collectDecls opts modSummary hsParsedModule = do
    _ <- liftIO $
        forkIO $ do
            -- let prefixPath = case opts of
                    -- [] -> "/tmp/fdep/"
                    -- local : _ -> local
            let modulePath = msHsFilePath modSummary
                path = (Data.List.intercalate "/" . reverse . tail . reverse . splitOn "/") modulePath
                declsList = hsmodDecls $ unLoc $ hpm_module hsParsedModule
            -- createDirectoryIfMissing True path
            functionsVsCodeString <- toList $ parallely $ mapM getDecls $ fromList declsList
            return ()
            -- writeFile (modulePath <> ".function_code.json") (encodePretty $ Map.fromList $ cat functionsVsCodeString)
    pure hsParsedModule

getDecls :: LHsDecl GhcPs -> IO [(Text, PFunction)]
getDecls x = do
    case x of
        (L _ (TyClD _ _)) -> pure mempty
        (L _ (InstD _ _)) -> pure mempty
        (L _ (DerivD _ _)) -> pure mempty
        (L _ (ValD _ bind)) -> pure $ getFunBind bind
        (L _ (SigD _ _)) -> pure mempty
        _ -> pure mempty
  where
    getFunBind f@FunBind{fun_id = funId} = [((T.pack $ showSDocUnsafe $ ppr $ unLoc funId) <> "**" <> (T.pack $ showSDocUnsafe $ ppr $ getLoc funId), PFunction ((T.pack $ showSDocUnsafe $ ppr $ unLoc funId) <> "**" <> (T.pack $ showSDocUnsafe $ ppr $ getLoc funId)) (T.pack $ showSDocUnsafe $ ppr f) (T.pack $ showSDocUnsafe $ ppr $ getLoc funId))]
    getFunBind _ = mempty

shouldForkPerFile :: Bool
shouldForkPerFile = readBool $ unsafePerformIO $ lookupEnv "SHOULD_FORK"
  where
    readBool :: (Maybe String) -> Bool
    readBool (Just "true") = True
    readBool (Just "True") = True
    readBool (Just "TRUE") = True
    readBool (Just "False") = False
    readBool (Just "false") = False
    readBool (Just "FALSE") = False
    readBool _ = True

shouldGenerateFdep :: Bool
shouldGenerateFdep = True--readBool $ unsafePerformIO $ lookupEnv "GENERATE_FDEP"
  where
    readBool :: (Maybe String) -> Bool
    readBool (Just "true") = True
    readBool (Just "True") = True
    readBool (Just "TRUE") = True
    readBool (Just "False") = False
    readBool (Just "false") = False
    readBool (Just "FALSE") = False
    readBool _ = True

shouldLog :: Bool
shouldLog = readBool $ unsafePerformIO $ lookupEnv "ENABLE_LOGS"
  where
    readBool :: (Maybe String) -> Bool
    readBool (Just "true") = True
    readBool (Just "True") = True
    readBool (Just "TRUE") = True
    readBool _ = False

websocketPort :: Int
websocketPort = maybe 8000 (fromMaybe 8000 . readMaybe) $ unsafePerformIO $ lookupEnv "SERVER_PORT"

websocketHost :: String
websocketHost = fromMaybe "localhost" $ unsafePerformIO $ lookupEnv "SERVER_HOST"

decodeBlacklistedFunctions :: IO [Text]
decodeBlacklistedFunctions = do
    mBlackListedFunctions <- lookupEnv "BLACKLIST_FUNCTIONS_FDEP"
    pure $ case mBlackListedFunctions of
        Just val' ->
            case A.decode $ BL.fromStrict $ encodeUtf8 (T.pack val') of
                Just val -> filterList <> val
                _ -> filterList
        _ -> filterList

fDep :: [CommandLineOption] -> ModSummary -> TcGblEnv -> TcM TcGblEnv
fDep opts modSummary tcEnv = do
    when (shouldGenerateFdep) $
        liftIO $
            bool P.id (void . forkIO) shouldForkPerFile $ do
                let prefixPath = case opts of
                        [] -> "/tmp/fdep/"
                        local : _ -> local
                    moduleName' = moduleNameString $ moduleName $ ms_mod modSummary
                    modulePath = prefixPath <> msHsFilePath modSummary
                let path = (Data.List.intercalate "/" . reverse . tail . reverse . splitOn "/") modulePath
                when shouldLog $ print ("generating dependancy for module: " <> moduleName' <> " at path: " <> path)
                let binds = bagToList $ tcg_binds tcEnv
                t1 <- getCurrentTime
                mapM_ (loopOverLHsBindLR (Just $ T.pack moduleName') (T.pack ("/" <> modulePath <> ".json"))) (fromList binds)
                t2 <- getCurrentTime
                when shouldLog $ print ("generated dependancy for module: " <> moduleName' <> " at path: " <> path <> " total-timetaken: " <> show (diffUTCTime t2 t1))
    return tcEnv

transformFromNameStableString :: (Maybe Text, Maybe Text, Maybe Text, [Text]) -> Maybe Func
transformFromNameStableString (Just str, Just loc, _type, args) =
    let parts = filter (\x -> x /= "") $ T.splitOn ("$") str
     in Just $ if length parts == 2 then Func (parts !! 0) (parts !! 1) else Func (parts !! 1) (parts !! 2)
transformFromNameStableString (Just str, Nothing, _type, args) =
    let parts = filter (\x -> x /= "") $ T.splitOn ("$") str
     in Just $ if length parts == 2 then Func (parts !! 0) (parts !! 1) else Func (parts !! 1) (parts !! 2)

sendTextData' ::  Text -> Text -> IO ()
sendTextData' path data_ = do
    return ()

blackListedModuleName :: [Text]
blackListedModuleName = ["_in","_lit","GHC.Base","base"]

loopOverLHsBindLR :: (Maybe Text) -> Text -> LHsBindLR GhcTc GhcTc -> IO ()
loopOverLHsBindLR mParentName path (L l x@(FunBind fun_ext id matches _ _)) = do
    let RealSrcSpan realSpan =  l
    funName <- evaluate $ force $ T.pack $ getOccString $ unLoc id
    fName <- evaluate $ force $ T.pack $ nameStableString $ getName id
    let matchList = mg_alts matches
    if funName `elem` (unsafePerformIO $ decodeBlacklistedFunctions) || ("$_in$$" `T.isPrefixOf` fName)
        then pure mempty
        else do
            -- when (shouldLog) $ print ("processing function: " <> fName)
            name <- evaluate $ force (fName <> "**" <> (T.pack $ showSDocUnsafe (ppr (getLoc id))))
            typeSignature <- evaluate $ force $ (T.pack $ showSDocUnsafe (ppr (varType (unLoc id))))
            nestedNameWithParent <- evaluate $ force $ (maybe (name) (\x -> x <> "::" <> name) mParentName)
            functionInfoList <- DF.foldl' (\acc' x -> do
                acc <- acc'
                functionInfo <- processMatch nestedNameWithParent path x
                return $ functionInfo ++ acc) (return []) (unLoc matchList)
            let functionInfoListUnique = DL.filter (\x@FT.Func{module_name} -> module_name `notElem` blackListedModuleName ) $ DM.catMaybes $ DL.nub functionInfoList
            -- putStrLn $ (show fName) <> "  Start Line: " ++ show (srcSpanStartLine realSpan)
            -- putStrLn $  (show fName) <> "  End Line: " ++ show (srcSpanEndLine realSpan)
            -- when (shouldLog) $ putStrLn ("processing function: " <> (show mParentName) <> " " <>(show fName))
            -- when (shouldLog) $ putStrLn ("functionInfo :")
            -- when (shouldLog) $ putStrLn $ show functionInfoListUnique
            let functionNameKey = (fromMaybe "Module" mParentName) <>"@"<> fName 
            let functionInfos = FunctionInfos functionInfoListUnique [] (srcSpanStartLine realSpan) (srcSpanEndLine realSpan)
            when (shouldLog) $ putStrLn ("processing function finished : " <> (show mParentName) <> " " <>(show fName))
            when (shouldLog) $ putStrLn ("functionInfos : ")
            when (shouldLog) $ putStrLn (show $ functionInfos)
            -- return $ (functionNameKey,functionInfos)
loopOverLHsBindLR mParentName path (L _ AbsBinds{abs_binds = binds}) =
    mapM_ (loopOverLHsBindLR  mParentName path) $ fromList $ bagToList binds
loopOverLHsBindLR _ _ (L _ VarBind{var_rhs = rhs}) = pure mempty
loopOverLHsBindLR _ _ (L _ (PatSynBind _ PSB{psb_def = def})) = pure mempty
loopOverLHsBindLR _ _ (L _ (PatSynBind _ (XPatSynBind _))) = pure mempty
loopOverLHsBindLR _ _ (L _ (XHsBindsLR _)) = pure mempty
loopOverLHsBindLR _ _ (L _ (PatBind _ _ pat_rhs _)) = pure mempty

processMatch ::  Text -> Text -> LMatch GhcTc (LHsExpr GhcTc) -> IO [Maybe Func]
processMatch keyFunction path (L _ match) = do
    whereClause <- (evaluate . force) =<< (processHsLocalBinds keyFunction path $ unLoc $ grhssLocalBinds (m_grhss match))
    DF.foldl' (\acc' x -> do
        acc <- acc'
        exprIter <- processGRHS keyFunction path x
        return $ exprIter ++ acc) (return []) $  grhssGRHSs (m_grhss match)
    -- pure mempty

processGRHS ::  Text -> Text -> LGRHS GhcTc (LHsExpr GhcTc) -> IO [Maybe Func]
processGRHS keyFunction path (L _ (GRHS _ _ body)) = processExpr keyFunction path body
processGRHS _ _ _ = pure mempty

processHsLocalBinds ::  Text -> Text -> HsLocalBindsLR GhcTc GhcTc -> IO ()
processHsLocalBinds keyFunction path (HsValBinds _ (ValBinds _ x y)) = do
    mapM_ (loopOverLHsBindLR (Just keyFunction) path) $ fromList $ bagToList $ x
processHsLocalBinds keyFunction path (HsValBinds _ (XValBindsLR (NValBinds x y))) = do
    mapM_ (\(recFlag, binds) -> mapM_ (loopOverLHsBindLR (Just keyFunction) path) $ fromList $ bagToList binds) (fromList x)
processHsLocalBinds _ _ _ = pure mempty

processExpr ::  Text -> Text -> LHsExpr GhcTc -> IO [Maybe Func]
processExpr keyFunction path x@(L _ (HsVar _ (L _ var))) = do
    let name = T.pack $ nameStableString $ varName var
        _type = T.pack $ showSDocUnsafe $ ppr $ varType var
    expr <- evaluate $ force $ transformFromNameStableString (Just name, Just $ T.pack $ showSDocUnsafe $ ppr $ getLoc $ x, Just _type, mempty)
    return [expr]
    -- sendTextData' path (decodeUtf8 $ toStrict $ Data.Aeson.encode $ Object $ HM.fromList [("key", String keyFunction), ("expr", toJSON expr)])
processExpr _ _ (L _ (HsUnboundVar _ _)) = pure mempty
processExpr keyFunction path (L _ (HsApp _ funl funr)) = do
    fl <- processExpr  keyFunction path funl
    fr <- processExpr  keyFunction path funr
    return $ fl ++ fr
processExpr  keyFunction path (L _ (OpApp _ funl funm funr)) = do
    fl <- processExpr  keyFunction path funl
    fnm <- processExpr  keyFunction path funm
    fr <- processExpr  keyFunction path funr
    return $ fl ++ fnm ++ fr
processExpr  keyFunction path (L _ (NegApp _ funl _)) =
    processExpr  keyFunction path funl
processExpr  keyFunction path (L _ (HsTick _ _ fun)) =
    processExpr  keyFunction path fun
processExpr  keyFunction path (L _ (HsStatic _ fun)) =
    processExpr  keyFunction path fun
processExpr  keyFunction path (L _ x@(HsWrap _ _ fun)) =
    processExpr  keyFunction path (noLoc fun)
processExpr  keyFunction path (L _ (HsBinTick _ _ _ fun)) =
    processExpr  keyFunction path fun
processExpr  keyFunction path (L _ (ExplicitList _ _ funList)) = do
    -- mapM_ (processExpr  keyFunction path) (fromList funList)
    DF.foldl' (\acc' x -> do
        acc <- acc'
        exprIter <- processExpr keyFunction path x
        return $ exprIter ++ acc  ) (return []) funList
processExpr  keyFunction path (L _ (HsTickPragma _ _ _ _ fun)) =
    processExpr  keyFunction path fun
processExpr  keyFunction path (L _ (HsSCC _ _ _ fun)) =
    processExpr  keyFunction path fun
processExpr  keyFunction path (L _ (HsCoreAnn _ _ _ fun)) =
    processExpr  keyFunction path fun
processExpr  keyFunction path (L _ (ExprWithTySig _ fun _)) =
    processExpr  keyFunction path fun
processExpr  keyFunction path (L _ (HsDo _ _ exprLStmt)) = do
    let stmts = exprLStmt ^? biplateRef :: [LHsExpr GhcTc]
    DF.foldl' (\acc' x -> do
        acc <- acc'
        expr <- processExpr  keyFunction path x
        return $ expr ++ acc) (return []) stmts
processExpr  keyFunction path (L _ (HsLet _ exprLStmt func)) = do
    let stmts = exprLStmt ^? biplateRef :: [LHsExpr GhcTc]
    DF.foldl' (\acc' x -> do
        acc <- acc'
        expr <- processExpr  keyFunction path x
        return $ expr ++ acc) (return []) $ [func] <> stmts
processExpr  keyFunction path (L _ (HsMultiIf _ exprLStmt)) = do
    let stmts = exprLStmt ^? biplateRef :: [LHsExpr GhcTc]
    DF.foldl' (\acc' x -> do
        acc <- acc'
        expr <- processExpr  keyFunction path x
        return $ expr ++ acc) (return []) stmts
processExpr  keyFunction path (L _ (HsIf _ exprLStmt funl funm funr)) = do
    let stmts = (exprLStmt ^? biplateRef :: [LHsExpr GhcTc])
    DF.foldl' (\acc' x -> do
        acc <- acc'
        expr <- processExpr  keyFunction path x
        return $ expr ++ acc) (return []) $ [funl, funm, funr] <> stmts
processExpr  keyFunction path (L _ (HsCase _ funl exprLStmt)) = do
    let stmts = (exprLStmt ^? biplateRef :: [LHsExpr GhcTc])
    DF.foldl' (\acc' x -> do
        acc <- acc'
        expr <- processExpr  keyFunction path x
        return $ expr ++ acc) (return []) $ [funl] <> stmts
processExpr  keyFunction path (L _ (ExplicitSum _ _ _ fun)) = processExpr  keyFunction path fun
processExpr  keyFunction path (L _ (SectionR _ funl funr)) = processExpr  keyFunction path funl <> processExpr  keyFunction path funr
processExpr  keyFunction path (L _ (ExplicitTuple _ exprLStmt _)) = do
    let stmts = (exprLStmt ^? biplateRef :: [LHsExpr GhcTc])
    DF.foldl' (\acc' x -> do
        acc <- acc'
        expr <- processExpr  keyFunction path x
        return $ expr ++ acc) (return []) stmts
processExpr  keyFunction path (L _ (HsPar _ fun)) =
    processExpr  keyFunction path fun
processExpr  keyFunction path (L _ (HsAppType _ fun _)) = processExpr  keyFunction path fun
processExpr  keyFunction path (L _ x@(HsLamCase _ exprLStmt)) = do
    let stmts = (exprLStmt ^? biplateRef :: [LHsExpr GhcTc])
    DF.foldl' (\acc' x -> do
        acc <- acc'
        expr <- processExpr  keyFunction path x
        return $ expr ++ acc) (return []) stmts
processExpr  keyFunction path (L _ x@(HsLam _ exprLStmt)) = do
    let stmts = (exprLStmt ^? biplateRef :: [LHsExpr GhcTc])
    DF.foldl' (\acc' x -> do
        acc <- acc'
        expr <- processExpr  keyFunction path x
        return $ expr ++ acc) (return []) stmts
processExpr  keyFunction path y@(L _ x@(HsLit _ hsLit)) = do
    expr <- evaluate $ force $ transformFromNameStableString (Just $ ("$_lit$" <> (T.pack $ showSDocUnsafe $ ppr hsLit)), (Just $ T.pack $ showSDocUnsafe $ ppr $ getLoc $ y), (Just $ T.pack $ show $ toConstr hsLit), mempty)
    -- sendTextData'  path (decodeUtf8 $ toStrict $ Data.Aeson.encode $ Object $ HM.fromList [("key", String keyFunction), ("expr", toJSON expr)])
    return [expr]
processExpr  keyFunction path y@(L _ x@(HsOverLit _ overLitVal)) = do
    expr <- evaluate $ force $ transformFromNameStableString (Just $ ("$_lit$" <> (T.pack $ showSDocUnsafe $ ppr overLitVal)), (Just $ T.pack $ showSDocUnsafe $ ppr $ getLoc $ y), (Just $ T.pack $ show $ toConstr overLitVal), mempty)
    -- sendTextData'  path (decodeUtf8 $ toStrict $ Data.Aeson.encode $ Object $ HM.fromList [("key", String keyFunction), ("expr", toJSON expr)])
    return [expr]
processExpr  keyFunction path (L _ (HsRecFld _ exprLStmt)) = do
    let stmts = (exprLStmt ^? biplateRef :: [LHsExpr GhcTc])
    DF.foldl' (\acc' x -> do
        acc <- acc'
        expr <- processExpr  keyFunction path x
        return $ expr ++ acc) (return []) stmts
processExpr  keyFunction path (L _ (HsSpliceE exprLStmtL exprLStmtR)) = do
    let stmtsL = (exprLStmtL ^? biplateRef :: [LHsExpr GhcTc])
        stmtsR = (exprLStmtR ^? biplateRef :: [LHsExpr GhcTc])
    DF.foldl' (\acc' x -> do
        acc <- acc'
        expr <- processExpr  keyFunction path x
        return $ expr ++ acc) (return []) $ stmtsL <> stmtsR
processExpr  keyFunction path (L _ (ArithSeq _ (Just exprLStmtL) exprLStmtR)) = do
    let stmtsL = (exprLStmtL ^? biplateRef :: [LHsExpr GhcTc])
        stmtsR = (exprLStmtR ^? biplateRef :: [LHsExpr GhcTc])
    DF.foldl' (\acc' x -> do
        acc <- acc'
        expr <- processExpr  keyFunction path x
        return $ expr ++ acc) (return []) $ stmtsL <> stmtsR
processExpr  keyFunction path (L _ (ArithSeq _ Nothing exprLStmtR)) = do
    let stmtsR = (exprLStmtR ^? biplateRef :: [LHsExpr GhcTc])
    DF.foldl' (\acc' x -> do
        acc <- acc'
        expr <- processExpr  keyFunction path x
        return $ expr ++ acc) (return []) stmtsR
processExpr  keyFunction path (L _ (HsRnBracketOut _ exprLStmtL exprLStmtR)) = do
    let stmtsL = (exprLStmtL ^? biplateRef :: [LHsExpr GhcTc])
        stmtsR = (exprLStmtR ^? biplateRef :: [LHsExpr GhcTc])
    DF.foldl' (\acc' x -> do
        acc <- acc'
        expr <- processExpr  keyFunction path x
        return $ expr ++ acc) (return []) $ stmtsL <> stmtsR
processExpr  keyFunction path (L _ (HsTcBracketOut _ exprLStmtL exprLStmtR)) = do
    let stmtsL = (exprLStmtL ^? biplateRef :: [LHsExpr GhcTc])
        stmtsR = (exprLStmtR ^? biplateRef :: [LHsExpr GhcTc])
    DF.foldl' (\acc' x -> do
        acc <- acc'
        expr <- processExpr  keyFunction path x
        return $ expr ++ acc) (return []) $ stmtsL <> stmtsR
processExpr  keyFunction path (L _ (RecordCon _ (L _ (iD)) r_flds)) = do
    let stmts = (r_flds ^? biplateRef :: [LHsExpr GhcTc])
    DF.foldl' (\acc' x -> do
        acc <- acc'
        expr <- processExpr  keyFunction path x
        return $ expr ++ acc) (return []) stmts
processExpr  keyFunction path (L _ (RecordUpd _ rupd_expr rupd_flds)) = do
    let stmts = (rupd_flds ^? biplateRef :: [LHsExpr GhcTc])
    DF.foldl' (\acc' x -> do
        acc <- acc'
        expr <- processExpr  keyFunction path x
        return $ expr ++ acc) (return []) stmts
processExpr _ _ _ = pure mempty