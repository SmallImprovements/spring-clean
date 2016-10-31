module Java.Imports (
  getImports,
  getImportsNonQualified
) where

import Language.Java.Syntax
import Data.List (isPrefixOf)
import Java.Helper (concatIdent)

getImports :: CompilationUnit -> [String]
getImports (CompilationUnit _ imports _) = getImportsFromAst imports
  where getImportsFromAst = map getImportDeclName

getImportsNonQualified :: CompilationUnit -> [String]
getImportsNonQualified (CompilationUnit _ imports _) = getImportsFromAst imports
  where getImportsFromAst = map getImportNonQualifyingDeclName

getImportDeclName :: ImportDecl -> String
getImportDeclName (ImportDecl False name _) = concatIdent name
getImportDeclName (ImportDecl True (Name ids) _) = concatIdent $ Name (init ids)

getImportNonQualifyingDeclName :: ImportDecl -> String
getImportNonQualifyingDeclName (ImportDecl False (Name ids) _) = concatIdent $ Name [last ids]
getImportNonQualifyingDeclName (ImportDecl True (Name ids) _) = concatIdent $ Name [(last.init) ids]
