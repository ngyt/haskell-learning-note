-- Data.Char
import Data.Char

encode2 :: Int -> String -> String
encode2 shift msg = map chr $ map (+shift) $ map ord msg