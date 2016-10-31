module Java.References (
  getReferences
) where

import Language.Java.Syntax
import Java.Helper (getPackageDeclName, parseRefType, parseType, identAsString, concatIdent)
import Java.SuperClass (getImplementsFromTypeDecls)
import qualified Data.Map.Strict as Map

getReferences :: Map.Map String String -> CompilationUnit -> [String]
getReferences imports (CompilationUnit package _ decl) =
  ( map (mappend $ getPackageDeclName package ++ ".")
  . filter (`Map.notMember` imports)
  . filter (/="String")
  . filter (/="Object")
  . filter (/="Iterable")
  . filter (/="")
  . concatMap parseTypeDecl) decl

parseTypeDecl :: TypeDecl -> [String]
parseTypeDecl decl@(ClassTypeDecl (ClassDecl _ _ tp r rt body)) =
  parseBody body ++
  getImplementsFromTypeDecls decl ++
  concatMap parseTypeParams tp ++
  concatMap parseRefType rt ++
  maybe [] parseRefType r
parseTypeDecl _ = []

parseBody :: ClassBody -> [String]
parseBody (ClassBody decl) = concatMap parseDecl decl

parseDecl :: Decl -> [String]
parseDecl (MemberDecl (FieldDecl _ t varDecls)) = parseType t ++ concatMap parseVarDecl varDecls
parseDecl (MemberDecl (MethodDecl _ tp r _ params _ body)) =
  maybe [] parseType r ++
  concatMap parseTypeParams tp ++
  concatMap parseFormalParam params ++
  parseMethodBody body
parseDecl (MemberDecl (ConstructorDecl _ tp _ params _ body)) =
  concatMap parseTypeParams tp ++
  parseConstructorBody body ++
  concatMap parseFormalParam params
parseDecl (MemberDecl (MemberClassDecl classDecl)) = parseTypeDecl (ClassTypeDecl classDecl)
parseDecl _ = []

parseFormalParam :: FormalParam -> [String]
parseFormalParam (FormalParam _ type' _ _) = parseType type'

parseTypeParams :: TypeParam -> [String]
parseTypeParams (TypeParam ident rt) = identAsString ident : concatMap parseRefType rt

parseMethodBody :: MethodBody -> [String]
parseMethodBody (MethodBody (Just b)) = parseBlock b
parseMethodBody (MethodBody Nothing) = []

parseConstructorBody :: ConstructorBody -> [String]
parseConstructorBody (ConstructorBody s b) = parseExplConstrInv s ++ parseBlock (Block b)

parseExplConstrInv :: Maybe ExplConstrInv -> [String]
parseExplConstrInv (Just (ThisInvoke r a)) = concat $ map parseRefType r ++ map parseExp a
parseExplConstrInv (Just (SuperInvoke r a)) = concat $ map parseRefType r ++ map parseExp a
parseExplConstrInv (Just (PrimarySuperInvoke e r a)) =
  concat $ [parseExp e] ++ map parseRefType r ++ map parseExp a
parseExplConstrInv Nothing = []

parseBlock :: Block -> [String]
parseBlock (Block bs) = concatMap parseBlockStmt bs

parseBlockStmt :: BlockStmt -> [String]
parseBlockStmt (LocalVars _ t v) = concat $ parseType t : map parseVarDecl v
parseBlockStmt (LocalClass c) = parseTypeDecl (ClassTypeDecl c)
parseBlockStmt (BlockStmt s) = parseStmt s

parseVarDecl :: VarDecl -> [String]
parseVarDecl (VarDecl _ i) = maybe [] parseVarInit i

parseVarInit :: VarInit -> [String]
parseVarInit (InitExp e) = parseExp e
parseVarInit _ = []

parseStmt :: Stmt -> [String]
parseStmt (IfThenElse expr s1 s2) = parseStmt s1 ++ parseStmt s2 ++ parseExp expr
parseStmt (IfThen expr s1) = parseStmt s1 ++ parseExp expr
parseStmt (Throw e) = parseExp e
parseStmt (Return (Just e)) = parseExp e
parseStmt (StmtBlock b) = parseBlock b
parseStmt (Try b catch _) =
  parseBlock b ++
  concatMap parseCatch catch
parseStmt (ExpStmt e) = parseExp e
parseStmt (Switch expr switchBlocks) = parseExp expr ++ concatMap parseSwitchBlock switchBlocks
parseStmt _ = []

parseCatch :: Catch -> [String]
parseCatch (Catch formalParam block) = parseFormalParam formalParam ++
                                       parseBlock block

parseSwitchBlock :: SwitchBlock -> [String]
parseSwitchBlock (SwitchBlock _ blockStmt) = concatMap parseBlockStmt blockStmt

parseExp :: Exp -> [String]
parseExp (InstanceCreation _ ct args cb) = parseClassType ct ++
                                           concatMap parseExp args ++
                                           maybe [] parseBody cb
parseExp (ClassLit (Just t)) = parseType t
parseExp (ClassLit Nothing) = []
parseExp (MethodInv mi) = parseMethodInvocation mi
parseExp (Assign _ _ exp) = parseExp exp
parseExp (ExpName name) = parseName name
parseExp (FieldAccess fieldAccess) = parseFieldAccess fieldAccess
parseExp (Cond exp1 exp2 exp3) = parseExp exp1 ++ parseExp exp2 ++ parseExp exp3
parseExp _ = []

parseFieldAccess :: FieldAccess -> [String]
parseFieldAccess (PrimaryFieldAccess e ident) = parseExp e ++ [identAsString ident]
parseFieldAccess (SuperFieldAccess ident) = [identAsString ident]
parseFieldAccess (ClassFieldAccess name ident) = parseName name ++ [identAsString ident]

parseName :: Name -> [String]
parseName (Name idents) = map identAsString idents

parseMethodInvocation :: MethodInvocation -> [String]
parseMethodInvocation (ClassMethodCall n refTypes _ a) =
  concatIdent n : concatMap parseExp a ++
  concatMap parseRefType refTypes
parseMethodInvocation (SuperMethodCall refTypes n a) =
  identAsString n : concatMap parseExp a ++
  concatMap parseRefType refTypes
parseMethodInvocation (PrimaryMethodCall e refTypes n a) =
  parseExp e ++
  [identAsString n] ++
  concatMap parseExp a ++
  concatMap parseRefType refTypes
parseMethodInvocation (TypeMethodCall n refTypes _ a) =
  concatIdent n : concatMap parseExp a ++
  concatMap parseRefType refTypes
parseMethodInvocation (MethodCall name a) = parseName name ++ concatMap parseExp a

parseClassType :: ClassType -> [String]
parseClassType (ClassType ct) = map (identAsString.fst) ct
