module Java (
  toAst,
  parseAst,
  importsToResult,
  Result(..),
) where

import Language.Java.Parser
import Language.Java.Syntax
import Text.Parsec.Error
import qualified Data.Map.Strict as Map
import Java.Annotations (getAnnotations)
import Java.SuperClass (getSuperClasses)
import Java.References (getReferences)
import Java.Class (getQualifyingClassName)
import Java.Imports (getImports, getImportsNonQualified)
import Java.Autowired (Autowiring, getAutowired)

data Result = Result { fileName :: String
                     , imports :: [String]
                     , references :: [String]
                     , topLevelAnnotations :: [String]
                     , methodAnnotations :: [String]
                     , implements :: [String]
                     , autowired :: [Autowiring]
                     } deriving (Show)

toAst :: (String, String) -> Either (String, ParseError) CompilationUnit
toAst (path, file) = case parser compilationUnit file of
  (Left x) -> Left (path, x)
  (Right x) -> Right x

parseAst :: CompilationUnit -> Result
parseAst c = Result { fileName = getQualifyingClassName c
                    , imports = imports'
                    , references = getReferences nonQualifyingToQualifying c
                    , topLevelAnnotations = getAnnotations c
                    , methodAnnotations = getAnnotations c
                    , implements = getSuperClasses nonQualifyingToQualifying c
                    , autowired = getAutowired nonQualifyingToQualifying c
                    }
  where imports' = getImports c
        nonQualifyingImports = getImportsNonQualified c
        nonQualifyingToQualifying = Map.fromList $ zip nonQualifyingImports imports'

importsToResult :: [String] -> Result
importsToResult e = Result { fileName = ""
                           , imports = e
                           , references = []
                           , topLevelAnnotations = []
                           , methodAnnotations = []
                           , implements = []
                           , autowired = [] }
