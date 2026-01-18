import Data.Text.IO qualified as T
import Lima.Converter

main :: IO ()
main = T.readFile "README.hs" >>= T.writeFile "README.md" . (Hs `convertTo` Md) def
