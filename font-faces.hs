import Parse.Parser
import Util.Sort
import Control.Applicative
import Control.Monad
import System.Directory
import Text.Printf
import System.IO.Unsafe
import Data.Maybe
import Control.Monad.Trans.Maybe

data FontSrc = FontSrc [(String, String)]
data FontFace = FontFace
    { family :: String
    , style :: String
    , weight :: String
    , src :: FontSrc
    }

orderSrc (_, "woff2") = 0
orderSrc (_, "woff") = 1
orderSrc (_, "ttf") = 2
orderSrc (_, "otf") = 3

riffle val ls = foldl1 (\a b -> a ++ val ++ b) ls
instance Show FontSrc where
    show (FontSrc sources) = riffle ",\n" (print <$> sortOn orderSrc sources) where
        print (url, format) = printf "         url(%s) format(%s)" url format

instance Show FontFace where
    show (FontFace family style weight src) = printf template family style weight (show src) where
        template = unlines [
            "@font-face {",
            "    font-family: %s;",
            "    font-style: %s;",
            "    font-weight: %s;",
            "    font-display: swap;",
            "    src: local(''),\n%s;",
            "}\n"]

printRet x = do
    print x
    return x
debugParser x = do
    return $ unsafePerformIO (printRet x)

parseFontFace :: String -> Parser Maybe FontFace
parseFontFace urlPrefix = do
    -- escapeIf (matchC '.' :: Parser Maybe Char)
    filename <- copyRemainder
    debugParser filename
    family <- riffle " " <$> name
    -- This will likely be latin-
    dashAfter token
    weight <- digits <|> return "normal"
    style <- prefix "italic" <|> (prefix "regular" >> return "normal")
    matchC '.'
    format <- token
    let source = FontSrc [(urlPrefix ++ filename, format)]
    return $ FontFace family style weight source
    where
        maybeDash = maybeParse $ matchC '-'
        dashAfter p = do
            item <- p
            maybeDash
            return item
        version = matchC 'v' >> digits >> return Ignore
        name = takeUntil version (dashAfter token)
getFontFaces :: [String] -> [FontFace]
getFontFaces files = [] where
    faces :: [FontFace]
    faces = catMaybes $ (unwrap $ parseFontFace "fonts/") <$> files
main = do
    files <- getDirectoryContents "static/fonts"
    print (length files)
    let faces = show <$> getFontFaces files
    print (length faces)
    -- putStrLn faces