module Lima.Converter.Internal where

import Data.Data (Data (toConstr), showConstr)
import Data.List (dropWhileEnd)
import Data.Text qualified as T

-- | A wrapper for prettyprinting strings
newtype Pretty a = Pretty String

instance Show a => Show (Pretty a) where
  show :: Pretty a -> String
  show (Pretty s) = s

-- | A class for prettyprinting data on multiple lines in haddocks.
--
-- It's not meant to be used outside of this library.
class Show a => PrettyPrint a where
  -- | A printing function
  --
  -- It's not meant to be used outside of this library.
  pp :: a -> Pretty String

instance PrettyPrint String where
  pp :: String -> Pretty String
  pp = Pretty . dropWhileEnd (== '\n')

instance PrettyPrint T.Text where
  pp :: T.Text -> Pretty String
  pp = pp . T.unpack

-- | Escaped hash character
escapedHash :: T.Text
escapedHash = "\\#"

-- | Hash character
hash :: T.Text
hash = "#"

-- | Drop a prefix of a line with length of a given line
dropLen :: T.Text -> T.Text -> T.Text
dropLen x = T.drop (T.length x)

-- | Check if a list starts with a given list
startsWith :: T.Text -> T.Text -> Bool
startsWith = flip T.isPrefixOf

-- | Check if a list ends with a given list
endsWith :: T.Text -> T.Text -> Bool
endsWith = flip T.isSuffixOf

-- | Drop leading spaces and drop at each end of a 'T.Text' the number of characters as in the supplied prefix and suffix.
stripEnds :: T.Text -> T.Text -> T.Text -> T.Text
stripEnds prefix suffix x = T.dropEnd (T.length suffix) (dropLen prefix (stripSpaces x))

-- | Drop spaces at the start and the end of a 'T.Text'.
stripSpaces :: T.Text -> T.Text
stripSpaces = T.strip

-- | Strip the given value from the beginning and the end of a list.
stripList :: Eq a => a -> [a] -> [a]
stripList x = dropWhileEnd (== x) . dropWhile (== x)

-- | Pad a 'T.Text' with a given number of spaces
indentN :: Int -> T.Text -> T.Text
indentN x s = T.concat (replicate x " ") <> s

-- | Show the name of a constructor.
constructorName :: Data a => a -> String
constructorName x = showConstr (toConstr x)

-- | Remove empty lines from the beginning and the end of a list.
stripEmpties :: [T.Text] -> [T.Text]
stripEmpties = stripList T.empty

-- | Drop leading empty strings
dropEmpties :: [T.Text] -> [T.Text]
dropEmpties = dropWhile (== T.empty)

-- | Check if a line without leading spaces is surrounded by the given 'T.Text's.
isEnclosedWith :: T.Text -> T.Text -> T.Text -> Bool
isEnclosedWith start end (stripSpaces -> x) = x `startsWith` start && x `endsWith` end

-- | Count leading spaces in a 'T.Text'.
countSpaces :: T.Text -> Int
countSpaces x = T.length $ T.takeWhile (== ' ') x

-- | Show error with line number for a token.
-- errorEmptyCommentAt :: Int -> String
errorEmptyCommentAt :: Show a1 => a1 -> a2
errorEmptyCommentAt lineNumber =
  error $
    ("Expected a 'Comment' at line " <> show lineNumber <> ".\n\n")
      <> "However, there are no characters after '{- '.\n\n"
      <> "Please, write there something after '{- '."


------
-- TeX

-- | Prepend start of a @TeX@ comment (@'% '@) to a 'T.Text'.
prependTexComment :: T.Text -> T.Text
prependTexComment l
  | l == T.empty = l
  | otherwise = texCommentSpace <> l

-- | Drop start of a @TeX@ comment from a 'T.Text'.
dropTexComment :: Show a => T.Text -> a -> T.Text
dropTexComment l lineNumber
  | l `startsWith` texCommentSpace = dropLen texCommentSpace l
  | l == T.empty = l
  | otherwise =
      error $
        "Lines in a 'Disabled' block must either be empty or start with '% '\n\n"
          <> "Note that each 'Disabled' block must have at least one line starting with '% ' and having nonempty text after '% ' "
          <> ("The line " <> show lineNumber <> " must either be empty or start with '% '")

-- | Start a @TeX@ comment.
texComment :: T.Text
texComment = "%"

-- | Start a @TeX@ comment plus a space.
texCommentSpace :: T.Text
texCommentSpace = texComment <> " "

-------------------
-- Literate Haskell

-- | Start a @Literate Haskell@ comment.
lhsComment :: T.Text
lhsComment = "%"

-- | Start a @Literate Haskell@ comment plus a space.
lhsCommentSpace :: T.Text
lhsCommentSpace = lhsComment <> " "

-- | Start a @Literate Haskell@ line of @Haskell@ code.
lhsHsCode :: T.Text
lhsHsCode = ">"

-- | Start a @Literate Haskell@ line of @Haskell@ code plus a space.
lhsHsCodeSpace :: T.Text
lhsHsCodeSpace = lhsHsCode <> " "

-- | Prepend start of a @TeX@ comment (@'% '@) to a 'T.Text'.
prependLhsComment :: T.Text -> T.Text
prependLhsComment l
  | l == T.empty = l
  | otherwise = texCommentSpace <> l

-- | Drop start of a @TeX@ comment from a 'T.Text'.
dropLhsComment :: Show a => T.Text -> a -> T.Text
dropLhsComment l lineNumber
  | l `startsWith` lhsCommentSpace = dropLen lhsCommentSpace l
  | l == T.empty = l
  | otherwise = error $ "The line " <> show lineNumber <> " must either be empty or start with '% '"

-- | Replace "\\#" with "#" in a 'T.Text' prefix.
lhsUnescapeHash :: T.Text -> T.Text
lhsUnescapeHash x = if x `startsWith` escapedHash then hash <> dropLen escapedHash x else x

-- | Replace "#" with "\\#" in a 'T.Text' prefix.
lhsEscapeHash :: T.Text -> T.Text
lhsEscapeHash x = if x `startsWith` hash then escapedHash <> dropLen hash x else x

-----------
-- Markdown

-- | Open a @Markdown@ comment.
mdCommentOpen :: T.Text
mdCommentOpen = "<!--"

-- | Close a @Markdown@ comment.
mdCommentClose :: T.Text
mdCommentClose = "-->"

-- | Open a @Markdown@ comment plus a space.
mdCommentOpenSpace :: T.Text
mdCommentOpenSpace = mdCommentOpen <> " "

-- | A space plus close a @Markdown@ comment.
mdCommentCloseSpace :: T.Text
mdCommentCloseSpace = " " <> mdCommentClose

-- | Strip comment markers from a 'T.Text'.
stripMdComment :: T.Text -> T.Text
stripMdComment = stripEnds mdCommentOpenSpace mdCommentCloseSpace

-- | Check if a line is a @Markdown@ comment.
isMdComment :: T.Text -> Bool
isMdComment = isEnclosedWith mdCommentOpenSpace mdCommentCloseSpace

-----------
-- Haskell

-- | Open a @Haskell@ multi-line comment.
hsCommentOpen :: T.Text
hsCommentOpen = "{-"

-- | Open a @Haskell@ multi-line comment plus a space.
hsCommentOpenSpace :: T.Text
hsCommentOpenSpace = hsCommentOpen <> " "

-- | Close a @Haskell@ multi-line comment.
hsCommentClose :: T.Text
hsCommentClose = "-}"

-- | A space plus close a @Haskell@ multi-line comment.
hsCommentCloseSpace :: T.Text
hsCommentCloseSpace = " " <> hsCommentClose

-- | Drop leading spaces and drop at each end of a 'T.Text' the number of characters as in the supplied prefix and suffix.
stripHsComment :: T.Text -> T.Text
stripHsComment = stripEnds hsCommentOpenSpace hsCommentCloseSpace

-- | Check if a line without leading zeros is a multi-line @Haskell@ comment
isHsComment :: T.Text -> Bool
isHsComment = isEnclosedWith hsCommentOpenSpace hsCommentCloseSpace