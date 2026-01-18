import Data.Text.IO qualified as T
import Lima.Converter

main :: IO ()
main = T.readFile "README.md" >>= T.writeFile "README.hs" . (Md `convertTo` Hs) def
