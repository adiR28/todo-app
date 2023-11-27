{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings, RecordWildCards,DuplicateRecordFields #-}
{-# OPTIONS_GHC -Wno-unused-local-binds #-}

module Utils.UtilsTH where

import Language.Haskell.TH
import Control.Monad.IO.Class
import Language.Haskell.TH.Datatype
import qualified Jsonifier as J
import Prelude
import qualified Utils.Jsonifier as UJ
import qualified Data.Aeson as Aeson


data Variants = RecordConstructorV | NormalConstructorV | Unsupported
  deriving(Show)

data InstanceInfo =
  InstanceInfo
    {
      dataName :: Name,
      consName :: Name,
      recordFields :: [Name],
      recordTypes :: [Type],
      variants :: Variants
    } deriving (Show)

-- creating function 
mkJsonifierInstanceTH :: Name -> Q[Dec]
mkJsonifierInstanceTH name = do
  _ <- liftIO $ putStrLn $ "generating Jsonifier instance for : " <> show (nameBase name)
  info <- reifyDatatype name
  _ <- liftIO $ putStrLn $ "data info : " <> show info
  _ <- liftIO $ putStrLn  "------------------¯"
  let DatatypeInfo ctx dname tyVars instTypes tyVariant tyCons = info
  -- functionSignature <- mkFunctionSignature (getfunctionName name) name
  instanceInfo <- getInstanceInfo name tyCons
  _ <- liftIO $ putStrLn $ "instanceInfo info : " <> show instanceInfo
  _ <- liftIO $ putStrLn  "------------------¯"
  _ <- liftIO $ putStrLn $ "datatypeVars null : " <> show (null tyVars)
  _ <- liftIO $ putStrLn  "------------------¯"
  resp <- mkJsonifierInstanceTHHelper dname instanceInfo (not $ null tyVars)
  return [resp]

mkJsonifierInstanceTHHelper :: Name -> [InstanceInfo] -> Bool -> Q Dec
mkJsonifierInstanceTHHelper dataName instanceInfo higherKindDataType = do
  matches <- mkMatchInstancesTH instanceInfo
  varLamb <- newName "value"
  let expr = LamE [VarP varLamb] $ CaseE (VarE varLamb ) matches
  let body = NormalB expr
  let fnClause = Clause [] body []
  let instanceBody = FunD ( mkName "toJsonifier") [fnClause]
  let ctx = []
  let instanceType = 
        if higherKindDataType
          then AppT (ConT ''UJ.Jsonifier) (ParensT $ AppT (ConT dataName) (ConT $ mkName "BM.Identity"))
          else AppT (ConT ''UJ.Jsonifier) (ParensT $ ConT dataName)
  let instanceDef = InstanceD Nothing ctx instanceType [instanceBody]
  return instanceDef

mkMatchInstancesTH :: [InstanceInfo] -> Q [Match]
mkMatchInstancesTH =
  foldr (\x acc -> do
      acc' <- acc
      let cons = consName x
      varMaybe <- newName "maybe"
      case variants x of
        RecordConstructorV -> do
          varName <- mkVariableName $ recordFields x
          let tupList = zip3 varName (recordFields x) (recordTypes x)
          tupListExpr <-
                foldr (\(varName,recordName,recordType) acc' -> do
                  acc <- acc'
                  func <- getJsonFunctionRecordConstructor recordType varName varMaybe
                  return $ TupE [Just $ LitE $ StringL $ nameBase recordName, Just $ func] :acc
                  ) (return []) tupList -- check for Bang types also
          let expr = AppE (VarE 'J.object) (ListE tupListExpr)
          let body = NormalB expr
          let pat = ConP cons $ map VarP varName
          let matchRecordConstructor = Match pat body []
          return $ matchRecordConstructor : acc'
        NormalConstructorV -> do
            varNewCons <- newName "normalCons"
            let consText = nameBase cons
            case recordTypes x of
              [] -> do
                let expr = AppE (VarE 'J.textString) (LitE $ StringL consText )
                let body = NormalB expr
                let pat = ConP cons []
                let matchNormalConstructor = Match pat body []
                return $ matchNormalConstructor : acc'
              recType ->  do
                varNew <- foldr (\_ acc' -> do
                      acc <- acc'
                      v <- newName "v"
                      return $ v:acc ) (return []) recType
                let tupList = zip recType varNew    
                let tag = TupE [Just $ LitE $ StringL "tag",Just $ AppE (VarE 'J.textString) (LitE $ StringL consText )]
                tempExpr <- 
                      foldr (\(ty,rt) acc' -> do
                        acc <- acc'
                        return $ (AppE (VarE $ getfunctionName rt) (VarE rt)) :acc ) (return []) tupList
                let content = TupE [Just $ LitE $ StringL "contents",Just $ AppE (VarE 'J.array) (ListE tempExpr)]
                let expr = AppE (VarE 'J.object) $ ListE [tag,content]
                let body = NormalB expr
                let pat = ConP cons $ map VarP varNew
                let matchNormalConstructorValue = Match pat body []
                return $ matchNormalConstructorValue : acc'
        _ -> fail "unsupported type "
  ) (return [])

mkVariableName :: [Name] -> Q [Name]
mkVariableName =
  foldr (\x acc -> do
    acc' <- acc
    var <- newName $ nameBase x
    return $ var:acc'
  ) (return [])

getInstanceInfo :: Name -> [ConstructorInfo] -> Q [InstanceInfo]
getInstanceInfo dname = foldl (\acc' x -> do
    acc <- acc'
    case constructorVariant x of
      NormalConstructor -> return $ getInstanceInfoNormalConstructor x dname : acc
      RecordConstructor _ -> return $ getInstanceInfoRecordConstructor x dname : acc
      _unsuported -> fail $ "unsupported type variant " <> show _unsuported
    ) (return [])


-- we can use Pattern Match also
getInstanceInfoNormalConstructor :: ConstructorInfo -> Name ->  InstanceInfo
getInstanceInfoNormalConstructor normalCons dname = do --(constructorName normalCons,[],NormalConstructorV)
  let recordFields = []
  let recordTypes = constructorFields normalCons
  let consName = constructorName normalCons
  mkInstanceInfo dname consName recordFields recordTypes NormalConstructorV


getInstanceInfoRecordConstructor :: ConstructorInfo -> Name -> InstanceInfo
getInstanceInfoRecordConstructor recordCons  dname= do
    let consName = constructorName recordCons
    let RecordConstructor consVariant = constructorVariant recordCons -- check for NormalConstructor also
    let recordTypes = constructorFields recordCons
    mkInstanceInfo dname consName consVariant recordTypes RecordConstructorV

mkFunctionSignature :: Name -> Name -> Q Dec
mkFunctionSignature fname name' =  do
  let ftype =   AppT (AppT ArrowT (ConT name')) (ConT ''J.Json) --SigT (ConT name') (ConT ''J.Json)
  let functionSig = SigD fname ftype
  return functionSig

mkFunctionSignature' :: Name -> Q Dec
mkFunctionSignature' fname = do
  name' <- newName  "a"
  let ftype =   AppT (AppT ArrowT (VarT name' )) (ConT ''J.Json) --SigT (ConT name') (ConT ''J.Json)
  let functionSig = SigD fname ftype
  return functionSig


-- For RecordConstructor 
getJsonFunctionRecordConstructor :: Type -> Name -> Name  ->  Q Exp
getJsonFunctionRecordConstructor ty varName varx = do
  let tempNothing = mkName "Nothing"
  case ty of
    -- [a] 
    AppT ListT (ConT ty) -> return $ mkArrFunction ty varName 

    -- / [Maybe a]
    AppT ListT (AppT (ConT  x) (ConT  y)) -> do  
              let fn = getfunctionName y
              return $ AppE (VarE 'J.array) $  UInfixE (LamE [VarP $ mkName "axc"] (mkMaybeFunction y (mkName "axc")  $ mkName "axc")) (VarE 'fmap) (VarE varName)

    -- Maybe a
    AppT (ConT _)(ConT ty) -> do  
      let fn = getfunctionName ty
      let m1 = Match (ConP 'Nothing []) (NormalB $ VarE 'J.null) []
      let m2 = Match (ConP 'Just [VarP varx ]) (NormalB $ AppE (VarE fn) (VarE varx)) []
      let matches = [m1,m2]
      return $ CaseE (VarE varName) matches

    -- Maybe [a]
    AppT (ConT _) (AppT ListT (ConT y)) -> do 
      let fn = getfunctionName y
      let m1 = Match (ConP 'Nothing []) (NormalB $ VarE 'J.null) []
      let m2 = Match (ConP 'Just [VarP varx ]) (NormalB $ mkArrFunction y varx) []
      let matches = [m1,m2]
      return $ CaseE (VarE varName) matches

    -- f a /f (Maybe a) /f [a] / f [Maybe a] -> f (Maybe [a])
    AppT (AppT (ConT _) (VarT variable)) exp -> do 
      return $ AppE (VarE $ getfunctionName varx ) (VarE varName)
    -- a
    ConT name -> return $ AppE (VarE $ getfunctionName name ) (VarE varName) 
    -- Todo add Either type, Add
    AppT (AppT (ConT e) (ConT lft)) (ConT rgt) -> return $ AppE (VarE $ getfunctionName varx ) (VarE varName) 
      --return $ AppE (VarE 'J.object) $ ListE [TupE [VarE recFeilds,AppE (VarE $ getfunctionName varx ) (VarE varName)]]

mkArrFunction :: Name -> Name -> Exp
mkArrFunction tyName varName = do
  let fn = getfunctionName tyName
  AppE (VarE 'J.array) $ UInfixE ( VarE fn) (VarE 'fmap) (VarE varName)


mkMaybeFunction :: Name -> Name -> Name ->  Exp
mkMaybeFunction ty varx varName =  do
      let fn = getfunctionName ty
      let m1 = Match (ConP 'Nothing []) (NormalB $ VarE 'J.null) []
      let m2 = Match (ConP 'Just [VarP varx ]) (NormalB $ AppE (VarE fn) (VarE varx)) []
      let matches = [m1,m2]
      CaseE (VarE varName) matches

getfunctionName :: Name -> Name
getfunctionName name = mkName "toJsonifier"

mkInstanceInfo :: Name -> Name -> [Name] -> [Type] -> Variants -> InstanceInfo
mkInstanceInfo dname consName rf ty var  =
  InstanceInfo
    {
      dataName =dname,
      consName = consName,
      recordFields = rf,
      recordTypes = ty,
      variants = var
    }
