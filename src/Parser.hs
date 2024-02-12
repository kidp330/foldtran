module Parser
  ( FortranParser(..),
    preprocess,
    charP,
    commentP,
    ltrP,
    idP,
    FValue(..),
    typeSpecP,
    valueToExpr,
    valueP,
    AssignmentStmt(..),
        assignmentStmtP,
  )
where

import Control.Applicative
import Data.Char
  ( isLetter,
    isDigit,
    isAlphaNum,
    isSpace,
    toUpper,
  )
import Data.Functor (($>))
import Data.Maybe (fromMaybe)
import Data.Bifunctor(second)

type Source = String
-- simplify source by removing indentation and empty lines
preprocess :: Source -> Source
preprocess = map toUpper . unlines . filter (/= "\n") . map (dropWhile isSpace) . lines

newtype FortranParser a = FortranParser {runParser :: String -> Maybe (String, a)}

instance Functor FortranParser where
  fmap f p = FortranParser $ \input -> do
    (input', a) <- runParser p input
    return (input', f a)

instance Applicative FortranParser where
  pure a = FortranParser $ \input -> Just (input, a)
  (<*>) f a = FortranParser $ \input -> do
    (input', ff) <- runParser f input
    (input'', aa) <- runParser a input'
    return (input'', ff aa)


instance Alternative FortranParser where
  empty = FortranParser $ const empty
  (<|>) (FortranParser fp) (FortranParser fq) =
    FortranParser $ \input ->
      fp input <|> fq input

ifP :: (Char -> Bool) -> FortranParser Char
ifP pred' = FortranParser tryParse
  where tryParse (c:cs)
          | pred' c = return (cs, c)
        tryParse _ = empty

spanP :: (Char -> Bool) -> FortranParser String
spanP pred' = FortranParser $ \input ->
  let (consumed, rest) = span pred' input
   in return (rest, consumed)

spanNemptyP :: (Char -> Bool) -> FortranParser String
spanNemptyP pred' = firstCombinableP <*> spanP pred'
  where
    firstP = ifP pred'
    firstCombinableP = fmap (:) firstP

type Identifier = String
idP :: FortranParser Identifier
idP = firstCombinableP <*> spanP (\c -> isAlphaNum c || c == '_')
  where firstP = ifP isLetter
        firstCombinableP = (:) <$> firstP
-- shame about the duplication, but we do need 2 different predicates in idP

charP :: Char -> FortranParser Char
charP c = ifP (== c)

ltrP :: FortranParser Char
ltrP = ifP isLetter

commentP :: FortranParser ()
commentP = charP '!' *> spanP (/= '\n') *> eosP $> ()

stringP :: String -> FortranParser String
stringP = traverse charP

eosP :: FortranParser ()
eosP = charP '\n' $> ()

wsP :: FortranParser ()
wsP = spanP isSpace $> ()

-- IMPLICIT NONE if any
implicitStmtP :: FortranParser ()
implicitStmtP = stringP "IMPLICIT" *> wsP *> stringP "NONE" $> () <* wsP

-- data Parameter = Param {
--   paramName :: String,
--   paramType :: FValue
-- }
-- type ParameterStmt = Parameter
-- parameterStmtP :: FortranParser ParameterStmt
-- parameterStmtP = stringP "PARAMETER" *> wsP *> charP
data FValue
  = FInteger Int
  -- | FReal Float
  | FLogical Bool
  | FCharacter Char
  deriving (Show, Eq)
  -- | FComplex (FValue, FValue)
  -- | FList [FValue] -- make sure this is homogenous

typeSpecP :: FortranParser FValue
typeSpecP = fmap stringToType $ foldl1 (<|>) $ map stringP ["INTEGER", "LOGICAL", "CHARACTER"]
  where
    stringToType "INTEGER" = FInteger 0
    -- stringToType "REAL" = FReal 0
    stringToType "LOGICAL" = FLogical False
    stringToType "CHARACTER" = FCharacter ' '

-- data ATypeDecl = TypeDecl FValue [String]
-- typeDeclP :: FortranParser ATypeDecl
-- typeDeclP = _a

-- Position of IMPLICIT NONE: If you try to put IMPLICIT NONE at the bottom of your program, after the executable statements, it will result in a compilation error. The Fortran standards specify that IMPLICIT NONE should appear before any type declarations and executable statements.

-- Executable statements before declarations: If you place any executable statement before a type declaration, the compiler will raise an error. In Fortran, type declarations must precede executable statements in the same program scope.

-- Order of initialization inside type declarations: The order of initialization in type declarations does not matter, as assignments to variables during declaration are not considered as executable statements. Therefore, the initializations are independent of each other.

-- Using value of one initialized variable when initializing another: Variables cannot be used in the initialization of other variables in type declarations. Every declaration statement is an individual entity and they can't reference each other during the initialization. However, you can assign the value of an already-declared variable to another variable in an executable statement, which comes after all type declarations.

type DeclarationConstruct = [(Handle, FValue)]
  -- | SpecificationStmt -- not supported

integerP :: FortranParser FValue
integerP = FInteger . read <$> strIntegerP
  where
    strIntegerP = (:) <$> ifP (`elem` ['1'..'9']) <*> spanP isDigit
-- this can be simpler

-- idP = firstCombinableP <*> spanP isAlphaNum
--   where firstP = ifP isLetter
--         firstCombinableP = (:) <$> firstP

logicalP :: FortranParser FValue
logicalP = fmap (FLogical . strToBool) $ stringP ".TRUE." <|> stringP ".FALSE."
  where strToBool ".TRUE." = True
        strToBool ".FALSE." = False
        strToBool _ = error "Parser did not fail on invalid input"

-- characterP :: FortranParser FValue
-- characterP = FCharacter <$> (charP '\'' *> ifP (const True) <* charP '\'')
-- bugged due to how we're preprocessing the program now :)

valueP :: FortranParser FValue
valueP = integerP <|> logicalP -- <|> characterP

valueToExpr :: FValue -> Expr
valueToExpr = l4expr
  where
    l4expr v = L4Expr (l3expr v) []
    l3expr v = L3Expr (l2expr v) []
    l2expr v = L2Expr Nothing (addoperand v) []
    addoperand v = AddOperand (multoperand v) []
    multoperand v = MultOperand (l1expr v) Nothing
    l1expr = AConstant

assignmentStmtP' :: FortranParser (Handle, FValue)
assignmentStmtP' = (,) <$> (idP <* wsP <* charP '=' <* wsP) <*> valueP <* wsP <* eosP

assignmentStmtP :: FortranParser AssignmentStmt
assignmentStmtP = (\(handle, value) -> (:=) handle (valueToExpr value)) <$> assignmentStmtP'

initializationP :: FortranParser (Handle, Maybe FValue)
initializationP = (Data.Bifunctor.second . Just <$> assignmentStmtP') <|> (idP <*> pure Nothing)

declarationConstructP :: FortranParser DeclarationConstruct
declarationConstructP = map . Data.Bifunctor.second . fromMaybe
  <$> (wsP *> typeSpecP <* wsP <* stringP "::" <* wsP)
  <*> sepBy1 initializationP (charP ',')

sepBy1 :: FortranParser a -> FortranParser b -> FortranParser [a]
sepBy1 p sepP = sequenceA ((:) <$> p <*> starSepThenPatternP)
  where starSepThenPatternP = many (sepP *> p)

type Handle = String

data Level1Expr
  = AConstant FValue
  | Parenthesized Expr
  | Handle
  deriving (Show, Eq)


newtype PowerUpMultOperand = (:**:) MultOperand
  deriving (Show, Eq)

-- grammar implies exponentiation is non-associative
data MultOperand = MultOperand Level1Expr (Maybe PowerUpMultOperand)
  deriving (Show, Eq)

data MultOp = (:*:) | (:/:)
  deriving (Show, Eq)

data MultOpMultOperand = MultOpMultOperand MultOp MultOperand
  deriving (Show, Eq)

data AddOp = (:+:) | (:-:)
  deriving (Show, Eq)

newtype Sign = Sign AddOp
  deriving (Show, Eq)

data AddOperand = AddOperand MultOperand [MultOpMultOperand]
  deriving (Show, Eq)

data AddOpAddOperand = AddOpAddOperand AddOp AddOperand
  deriving (Show, Eq)

data Level2Expr = L2Expr {
  l2exprHasMinus         :: Maybe Sign,
  l2exprAddOperand       :: AddOperand,
  l2exprOptionalOperands :: [AddOpAddOperand]
  }
  deriving (Show, Eq)

data ConcatOp = (://:)
  deriving (Show, Eq)

data ConcatOpL2Expr = ConcatOpL2Expr ConcatOp Level2Expr
  deriving (Show, Eq)

data Level3Expr = L3Expr {
  l3exprL2Expr           :: Level2Expr,
  l3exprOptionalOperands :: [ConcatOpL2Expr]
  }
  deriving (Show, Eq)

data RelOp
  = (:==:)
	| (:/=:)
	| (:<:)
	| (:<=:)
	| (:>:)
	| (:>=:)
  deriving (Show, Eq)
 
data RelOpL3Expr = RelOpL3Expr RelOp Level3Expr
  deriving (Show, Eq)

data Level4Expr = L4Expr {
  l4exprL3Expr :: Level3Expr,
  l4exprOptionalOperands :: [RelOpL3Expr]
  }
  deriving (Show, Eq)


type Expr = Level4Expr
data AssignmentStmt = (:=) Handle Expr
  deriving (Show, Eq)

data IfStmt = IfStmt Expr ActionStmt
newtype PrintStmt = PrintStmt [Expr]
newtype ReadStmt = ReadStmt [Handle]

data ActionStmt
  = ActionAssignmentStmt AssignmentStmt
  | ActionIfStmt IfStmt
  | ActionPrintStmt PrintStmt
  | ActionReadStmt ReadStmt
-- | ReturnStmt
  | StopStmt

data DoConstruct
  = DoRange {
    doRangeHandle :: Handle,
    doRangeStart :: Int,
    doRangeEnd :: Int,
    doRangeStep :: Maybe Int,
    doRangeBody :: BodyExecutable
    }
  | DoWhile {
    doWhileExpr :: Expr,
    doWhileBody :: BodyExecutable
    }

data IfConstruct = IfConstruct {
  ifConstructExpr :: Expr,
  ifConstructBody :: BodyExecutable,
  ifConstructElseIf :: [IfConstruct],
  ifConstructElseBody :: Maybe BodyExecutable
  }

data ExecutableConstruct
  = ExecActionStmt
  | ExecDoConstruct
  | ExecIfConstruct

type BodyExecutable = [ExecutableConstruct]
type BodySpecification = [DeclarationConstruct]

-- \| Declare
-- \| (:=) Var Expr
-- \| While Expr Stmt
-- \| Seq [Stmt]
-- \| Comment String


-- statementP :: FortranParser Stmt
-- statementP =

-- programMainP :: FortranParser ProgramUnit
-- programMainP = many commentP *> fp  <* commentP
-- where fp

-- data Program = Program {
--   programName  :: String,
--   programStmts :: [Stmt]
-- }




-- data Expr
--   = Constant Const
--   | Variable Var
--   | BinOp

data Declare = Int

-- data AST = AST {
--   mainProgram :: [Stmt],
--   dummy :: ()
-- }

type Token = String

-- parse :: String -> AST
-- parse = undefined
