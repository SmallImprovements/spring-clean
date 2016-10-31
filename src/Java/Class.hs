module Java.Class (
  getQualifyingClassName
) where

import Language.Java.Syntax
import Java.Helper (concatIdent, getPackageDeclName)
import Data.Maybe (listToMaybe)

getQualifyingClassName :: CompilationUnit -> String
getQualifyingClassName (CompilationUnit package _ typeDecls) =
  getPackageDeclName package ++ maybe "" getClassName (listToMaybe typeDecls)

getClassName :: TypeDecl -> String
getClassName (ClassTypeDecl (ClassDecl _ name _ _ _ _)) =
  "." ++ concatIdent (Name [name])
getClassName (ClassTypeDecl (EnumDecl _ name _ _)) =
  "." ++ concatIdent (Name [name])
getClassName (InterfaceTypeDecl (InterfaceDecl _ name _ _ _)) =
  "." ++ concatIdent (Name [name])
