{-# LANGUAGE GADTs #-}
module Main where
import Text.ParserCombinators.Parsec.Char
import Text.ParserCombinators.Parsec.Combinator
import Text.ParserCombinators.Parsec
import Data.Char
import qualified Data.Map.Strict as Map
import qualified Control.Applicative as Apply
import Data.List
import Data.Maybe(fromMaybe)
filt = filter (not.isSpace)

data AExpr a where
    AENum :: Double -> AExpr Double
    AEVar :: String -> AExpr Double
    AESqrt :: AExpr Double -> AExpr Double
    AESum :: AExpr Double -> AExpr Double -> AExpr Double
    AESub :: AExpr Double -> AExpr Double -> AExpr Double
    AEProd :: AExpr Double -> AExpr Double -> AExpr Double
    AEDiv :: AExpr Double -> AExpr Double -> AExpr Double
    AEEq :: AExpr Double -> AExpr Double -> AExpr Bool
    AEExp :: AExpr Double -> AExpr Double -> AExpr Double
    AEFrac :: AExpr Double -> AExpr Double -> AExpr Double
    Minus :: AExpr Double -> AExpr Double

instance Show a => Show (AExpr a) where
    show (AENum num) = show num
    show (AEVar var) = var
    show (AESum e1 e2) = "(" ++ show e1 ++ "+" ++ show e2 ++ ")"
    show (AESub e1 e2) = "(" ++ show e1 ++ "-" ++ show e2 ++ ")"
    show (AEProd e1 e2) = "(" ++ show e1 ++ "*" ++ show e2 ++ ")"
    show (AEDiv e1 e2) = "(" ++ show e1 ++ "/" ++ show e2 ++ ")"
    show (AEEq e1 e2) = "(" ++ show e1 ++ "=" ++ show e2 ++ ")"
    show (AEExp e1 e2) = "(" ++ show e1 ++ "^" ++ show e2 ++ ")"
    show (AEFrac e1 e2) = "(" ++ show e1 ++ "/" ++ show e2 ++ ")"
    show (AESqrt e) = "sqrt(" ++ show e ++ ")" 
    show (Minus e) = "(-" ++ show e ++ ")" 
    
parseXml :: String -> Parser a -> Parser a
parseXml tag parser = string ("<" ++ tag ++ ">") *> parser <* string ("</" ++ tag ++ ">")

exp' :: Double
exp' = 0.00001
interpret :: AExpr a -> Map.Map String Double -> Maybe a
interpret (AENum e) _ = Just e
interpret (AEVar e) ma = Map.lookup e ma
interpret (AESqrt e) ma = sqrt <$> interpret e ma
interpret (AESum e1 e2) ma = Apply.liftA2 (+) (interpret e1 ma) (interpret e2 ma)
interpret (AESub e1 e2) ma= Apply.liftA2 (-)(interpret e1 ma) (interpret e2 ma)
interpret (AEProd e1 e2) ma= Apply.liftA2 (*)(interpret e1 ma) (interpret e2 ma)
interpret (AEDiv e1 e2) ma= Apply.liftA2 (/) (interpret e1 ma) (interpret e2 ma)
interpret (AEEq e1 e2) ma= fmap (\s -> abs(s) < exp') $ Apply.liftA2 (-) (interpret e1 ma) (interpret e2 ma) 
interpret (AEExp e1 e2) ma= Apply.liftA2 (**) (interpret e1 ma) (interpret e2 ma)
interpret (AEFrac e1 e2) ma= Apply.liftA2 (/) (interpret e1 ma) (interpret e2 ma)
interpret (Minus e1) ma = (*(-1)) <$> interpret e1 ma

heading = "<mathxmlns=\'http://www.w3.org/1998/Math/MathML\'>"


parseNum :: Parser (AExpr Double)
parseNum = ((.) AENum read) <$> parseXml "mn" parser
              where parser = (++) <$> many1 digit <*> (((++) <$> string "." <*> many1 digit) <|> pure "")

parseVar :: Parser (AExpr Double)
parseVar =  AEVar <$> parseXml "mi" ((:[]) <$> letter)

parseSign = \s -> parseXml "mo" $ ((:[]) <$> char s)

parseLeftAssociative :: Parser (AExpr Double) -> Parser (AExpr Double) -> [Parser String] -> Parser (AExpr Double)
parseLeftAssociative parseAtom parseAtom' parseOp = 
            do
                atom <- parseAtom
                ((try $ parseTail parseAtom' atom parseOp) <|> pure atom)
           where 
            parseTail :: Parser (AExpr Double) -> (AExpr Double) -> [Parser String] -> Parser (AExpr Double)
            parseTail parseAtom atom1 listP = 
                do
                    c <- choice listP
                    atom2 <- parseAtom                   
                    let op = case c of
                                "+" -> AESum
                                "-" -> AESub
                                "*" -> AEProd
                                "/" -> AEDiv
                    ((try $ parseTail parseAtom (op atom1 atom2) listP) <|> pure (op atom1 atom2))
parseSum :: Parser (AExpr Double)                    
parseSum = parseLeftAssociative parseProduct parseProduct [try $ parseSign '+', parseSign '-']
parseProduct :: Parser (AExpr Double)  
parseProduct = parseLeftAssociative parseAtom parseAtom' [try $ string "&#x00D7;<!--multiplicationsign-->" >> pure "*", try $ parseSign '/', (lookAhead parseAtom >> pure "*")]

parseExpon = do
                string "<msup>"
                string "<mrow>"
                exp1 <- parseSum
                string "</mrow>"
                string "<mrow>"
                exp2 <- parseSum
                string "</mrow>"
                string "</msup>"
                return $ AEExp exp1 exp2
                
parseSqrt = AESqrt <$> parseXml "msqrt" parseSum
parseBrackets = parseXml "msqrt" $ parseXml "mfenced" parseSum
parseFrac = do
                string "<mfrac>"
                string "<mrow>"
                exp1 <- parseSum
                string "</mrow>"
                string "<mrow>"
                exp2 <- parseSum
                string "</mrow>"
                string "</mfrac>"
                return $ AEFrac exp1 exp2
parseMinus = Minus <$> (parseSign '-' >> parseAtom')
parseAtom = (try $ parseSqrt) <|> (try $ parseBrackets) <|> (try $ parseFrac) <|> (try $ parseExpon) <|> (try $ parseVar) <|> (try parseMinus) <|> (parseNum)    
parseAtom' = (try $ parseSqrt) <|> (try $ parseBrackets) <|> (try $ parseFrac) <|> (try $ parseExpon) <|> (try $ parseVar) <|> (parseNum)
parseExpr :: Parser (AExpr Bool)
parseExpr = do
                expr1 <- parseSum
                parseSign '='
                expr2 <- parseSum
                return $ AEEq expr1 expr2
parseExprs :: Parser ([AExpr Bool])                
parseExprs = do 
                string "<mtablecolumnalign=\'left\'>"
                mas <- parseExprs' [] 
                string "</mtable>"
                return mas
            where 
                parseExprs' :: [AExpr Bool] -> Parser ([AExpr Bool])
                parseExprs' mas = do
                            string "<mtr>"
                            string "<mtd>"
                            exp <- parseExpr
                            string "</mtd>"
                            string "</mtr>"
                            (try $ parseExprs' (exp:mas)) <|> pure (exp:mas)
                                

parseInput = do
                string heading
                exprs <- ((try parseExprs) <|> (:[]) <$> parseExpr)
                string "</math>"
                return exprs

check :: Either ParseError [AExpr Bool] -> String
check eith = either (\_ -> "ParseError") (check') eith
             where
                pred (AEEq (AEVar _) (AENum _)) = True
                pred (AEEq (AEVar _) (Minus (AENum _))) = True
                pred _                         = False
                func :: AExpr Bool -> (String, Double)
                func (AEEq (AEVar a) (AENum b)) = (a, b)
                func (AEEq (AEVar a) (Minus (AENum b))) = (a, -1 * b)
                
                check' :: [AExpr Bool] -> String
                check' list = let
                               (ans, expr) = partition pred list
                               ma = Map.fromList $ map (func) ans 
                               res = sequenceA (map (\e -> interpret e ma) expr) >>= \mas -> if (all id mas) then Just "Rigth answer" else Just $ show (ans, expr)
                              in Data.Maybe.fromMaybe "missing answer for variable" res
                
main :: IO ()
main = do
    exp <- getLine
    putStrLn $ check $ parse parseInput "" (filt exp)
    
