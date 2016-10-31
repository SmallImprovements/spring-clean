module Java.Helper (
  concatIdent,
  identAsString,
  getPackageDeclName,
  parseRefType,
  parseType,
) where

import Language.Java.Syntax

concatIdent :: Name -> String
concatIdent (Name []) = ""
concatIdent (Name [x]) = identAsString x
concatIdent (Name (x:xs)) = identAsString x ++ "." ++ concatIdent (Name xs)

identAsString :: Ident -> String
identAsString (Ident cs) = cs

getPackageDeclName :: Maybe PackageDecl -> String
getPackageDeclName (Just (PackageDecl name)) = concatIdent name
getPackageDeclName Nothing = ""

parseType :: Type -> [String]
parseType (RefType r) = parseRefType r
parseType _ = []

parseRefType :: RefType -> [String]
parseRefType (ClassRefType (ClassType val)) =
  (identAsString.fst.head) val : (concat . concat) (map (map parseTypeArgument .snd) val)
parseRefType _ = []

parseTypeArgument :: TypeArgument -> [String]
parseTypeArgument (ActualType r) = parseRefType r
parseTypeArgument _ = []
