module Java.SuperClass (
  getSuperClasses,
  getImplementsFromTypeDecls
) where

import qualified Data.Map.Strict as Map
import Language.Java.Syntax
import Java.Helper (getPackageDeclName, parseRefType)
import Data.Maybe (fromMaybe)

getSuperClasses :: Map.Map String String -> CompilationUnit -> [String]
getSuperClasses imports (CompilationUnit p _ typeDecls) =
  map makeFullyQualified $ concatMap getImplementsFromTypeDecls
                                        typeDecls
  where package = getPackageDeclName p
        makeFullyQualified ref = fromMaybe (package ++ "." ++ ref) (Map.lookup ref imports)

getImplementsFromTypeDecls :: TypeDecl -> [String]
getImplementsFromTypeDecls (ClassTypeDecl (ClassDecl _ _ _ ext interfaces _)) =
  filter (/="") (concatMap parseRefType interfaces) ++
  filter (/="") (maybe [] parseRefType ext)
getImplementsFromTypeDecls (InterfaceTypeDecl (InterfaceDecl _ _ _ interfaces _)) =
  filter (/="") (concatMap parseRefType interfaces)
getImplementsFromTypeDecls _ = []
