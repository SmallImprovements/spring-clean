module Java.Autowired (
  Autowiring(AutowireAll, Autowiring),
  getAutowired
) where

import Language.Java.Syntax
import Java.Annotations (parseModifier)
import Data.Maybe
import Java.Helper (identAsString, getPackageDeclName)
import Data.Char (toUpper)
import qualified Data.Map.Strict as Map

data Autowiring = AutowireAll String | Autowiring String String
                deriving Show

getAutowired :: Map.Map String String -> CompilationUnit -> [Autowiring]
getAutowired imports (CompilationUnit package _ typeDecls) =
  map (identFirstToUpper.toQualified) (concatMap parseTypeDecl typeDecls)
  where
    toQualified = makeQualified packageName imports
    packageName = getPackageDeclName package

identFirstToUpper :: Autowiring -> Autowiring
identFirstToUpper (Autowiring type' (x:xs)) = Autowiring type' (toUpper x : xs)
identFirstToUpper x = x

makeQualified :: String -> Map.Map String String -> Autowiring -> Autowiring
makeQualified packageName imports (AutowireAll type') =
  AutowireAll $ getQualified packageName imports type'
makeQualified packageName imports (Autowiring type' ident) =
  Autowiring (getQualified packageName imports type') ident

getQualified :: String -> Map.Map String String -> String -> String
getQualified packageName imports ident =
  fromMaybe (packageName++"."++ident) (Map.lookup ident imports)

parseTypeDecl :: TypeDecl -> [Autowiring]
parseTypeDecl (ClassTypeDecl c) = parseClassDecl c
parseTypeDecl _ = []

parseClassDecl :: ClassDecl -> [Autowiring]
parseClassDecl (ClassDecl _ _ _ _ _ classBody) = parseClassBody classBody
parseClassDecl _ = []

parseClassBody :: ClassBody -> [Autowiring]
parseClassBody (ClassBody decls) = catMaybes getAllAutowiredFields
  where getAllAutowiredFields = map parseDecl decls

parseDecl :: Decl -> Maybe Autowiring
parseDecl (MemberDecl memberDecl) = parseMemberDecl memberDecl
parseDecl _ = Nothing

parseMemberDecl :: MemberDecl -> Maybe Autowiring
parseMemberDecl (FieldDecl modifiers type' varDecls) =
  if any isAutowired modifiers then Just (parseAutowiring type' varDecls)
                               else Nothing
parseMemberDecl _ = Nothing

parseAutowiring :: Type -> [VarDecl] -> Autowiring
parseAutowiring type' varDecls = if fst typeOfAutowire == "List"
                                   then AutowireAll (fromJust $ snd typeOfAutowire)
                                   else Autowiring (fst typeOfAutowire)
                                                   (head $ map parseVarDecl varDecls)
  where typeOfAutowire = parseType type'

parseType :: Type -> (String, Maybe String)
parseType (RefType refType) = parseRefType refType

parseRefType :: RefType -> (String, Maybe String)
parseRefType (ClassRefType classRefType) = parseClassType classRefType

parseClassType :: ClassType -> (String, Maybe String)
parseClassType (ClassType classType) = ((identAsString . getIdent) classType,
                                        getFirstTypeParam classType)
  where getIdent = fst . head
        getFirstTypeParam = fmap parseTypeArgument . listToMaybe . snd . head

parseTypeArgument :: TypeArgument -> String
parseTypeArgument (ActualType refType) = fst $ parseRefType refType

parseVarDecl :: VarDecl -> String
parseVarDecl (VarDecl varDeclId _) = parseVarDeclId varDeclId

parseVarDeclId :: VarDeclId -> String
parseVarDeclId (VarId ident) = identAsString ident
parseVarDeclId (VarDeclArray varDeclId) = parseVarDeclId varDeclId

isAutowired :: Modifier -> Bool
isAutowired m = parseModifier m == "Autowired"
