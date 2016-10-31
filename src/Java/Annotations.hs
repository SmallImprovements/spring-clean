module Java.Annotations (
  getAnnotations,
  parseModifier
) where

import Language.Java.Syntax
import Java.Helper (concatIdent)

getAnnotations :: CompilationUnit -> [String]
getAnnotations (CompilationUnit _ _ typeDecls) =
  concatMap fromTypeDecls typeDecls ++
  concatMap methods typeDecls

fromTypeDecls :: TypeDecl -> [String]
fromTypeDecls (ClassTypeDecl (ClassDecl an _ _ _ _ _)) =
  (filter (/="") . map parseModifier) an
fromTypeDecls _ = []

parseModifier :: Modifier -> String
parseModifier (Annotation (MarkerAnnotation name)) = concatIdent name
parseModifier (Annotation (NormalAnnotation name _)) = concatIdent name
parseModifier (Annotation (SingleElementAnnotation name _)) = concatIdent name
parseModifier _ = []

methods :: TypeDecl -> [String]
methods (ClassTypeDecl (ClassDecl _ _ _ _ _ cb)) =
  classBody cb
methods _ = []

classBody :: ClassBody -> [String]
classBody (ClassBody decl) = concatMap fromDecl decl

fromDecl :: Decl -> [String]
fromDecl (MemberDecl mb) = methodDecl mb
fromDecl _ = []

methodDecl :: MemberDecl -> [String]
methodDecl (MethodDecl modifier _ _ _ _ _ _) = map parseModifier modifier
methodDecl (MemberClassDecl classDecl) =
  fromTypeDecls (ClassTypeDecl classDecl)
methodDecl _ = []
