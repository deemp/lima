import Lima.Converter
import Data.Text.IO qualified as T

main :: IO ()
main = T.readFile "README.md" >>= T.writeFile "README.hs" . (Md `convertTo` Hs) def
