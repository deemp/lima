-- |
-- == Terms #terms#
--
-- * @format@ - specific encoding of some information. See 'Format'.
-- * @document@ - 'T.Text' in a specific format, e.g., @Haskell@ (@.hs@) file.
-- * @document block@ - consecutive lines of a document.
-- * 'Token' - a representation of a document block as a @Haskell@ type.
-- * 'Tokens' - a list of 'Token's.
-- * @parser@ - a function that reads a document line by line and converts it to 'Tokens'. Example: 'hsToTokens'.
-- * @printer@ - a function that converts 'Tokens' to a document. Example: 'hsFromTokens'.
-- * @tag@ - a marker that affects how 'Tokens' are parsed.
--
--     * Each parser recognizes tags of a specific form.
--     * Tags can be represented as a wrapper and a name.
--
--         E.g., in @'% LIMA_DISABLE some text'@, a @TeX@ tag, the wrapper is @'% '@ and the name is @'LIMA_DISABLE some text'@.
--
--     * Parsers recognize the tag names that /start with/ tag names specified in a 'Config'.
--
--         E.g., in the example above, a parser will recognize the [_disable](#v:_disable) tag and will become disabled.
--
--     * When a parser is disabled, it copies lines verbatim into a 'Disabled' 'Token' and doesn't recognize any tags until it finds an [_enable](#v:_enable) tag.
--
-- == Assumptions #assumptions#
--
-- The following assumptions must hold for outputs of parsers and inputs of printers:
--
--     - 'Tokens' are in the same order as the corresponding blocks of document.
--     - Lines inside 'Tokens' are reversed compared to the document. Example:
--
--         - @Literate Haskell@ document:
--
--             @
--             line 1
--             line 2
--
--             % line 3
--
--             % line 4
--             @
--
--         - Corresponding 'Tokens':
--
--             @
--             [
--               Text {manyLines = ["line2","line 1"]},
--               Comment {someLines = "line 4" :| ["", "line 3"]}
--             ]
--             @
--
--      - There are no leading or trailing empty lines inside of 'Tokens'.
--
-- There are several forms of @Haskell@ code blocks in @Literate Haskell@ recognized by @GHC@.
--
-- 1. Code between @'\\begin{code}'@ and @'\\end{code}'@ tags.
--
--     @
--     \begin{code}
--        a = 42
--     \end{code}
--     \begin{code}
--        b = a
--     \end{code}
--     @
--
--     - The line starting with @'\\begin{code}'@ cannot have other non-space characters after @'\\begin{code}'@.
--     - The indentation of all expressions in code blocks must be the same.
--
-- 1. Code lines starting with @'> '@.
--
--     @
--     \begin{mycode}
--
--     >    a = 42
--
--     \end{mycode}
--     \begin{mycode}
--
--     >    b = a
--
--     \end{mycode}
--     @
--
--     - There must be at least a single empty line before and after each @Haskell@ code block.
--     - Any text may surround @Haskell@ code blocks.
--     - The indentation of all expressions in code blocks must be the same.
--
-- This library supports only the second form as this form is more versatile.
--
-- Moreover, this form does not require writing @Markdown@ tags like @\'```haskell\'@.
--
-- Such tags will automatically be printed when converting @Literate Haskell@ to @Markdown@.
module Lima.Converter (
  -- * Config
  Mode,
  User,
  Internal,
  Config (..),
  def,
  toConfigInternal,

  -- ** Lenses
  disable,
  enable,
  indent,
  dedent,
  mdHaskellCodeStart,
  mdHaskellCodeEnd,
  texHaskellCodeStart,
  texHaskellCodeEnd,
  texSingleLineCommentStart,
  lhsSingleLineCommentStart,

  -- * microlens
  (&),
  (?~),

  -- * Format
  Format (..),
  convertTo,
  showFormatExtension,
  showFormatName,

  -- * Tokens
  Token (..),
  Tokens,
  selectFromTokens,
  selectToTokens,
  mergeTokens,
  stripTokens,
  normalizeTokens,

  -- * Printers
  hsFromTokens,
  hsFromTokens',
  lhsFromTokens,
  lhsFromTokens',
  mdFromTokens,
  mdFromTokens',
  texFromTokens,
  texFromTokens',

  -- * Parsers
  hsToTokens,
  lhsToTokens,
  mdToTokens,
  texToTokens,

  -- * Helpers
  mkFromTokens,
  mkToTokens,
  parseLineToToken,
  errorExpectedToken,
  errorNotEnoughTokens,
  pp,

  -- * Examples
  exampleNonTexTokens',
  exampleNonTexTokens,
  exampleTexTokens,
) where

import Data.Char (isAlpha)
import Data.Data (Data)
import Data.Default (Default (def))
import Data.List (intersperse)
import Data.List.NonEmpty as NonEmpty (NonEmpty ((:|)), fromList, init, last, toList, (<|))
import Data.Text qualified as T
import GHC.Generics (Generic)
import Lens.Micro (non, to, (&), (?~), (^.))
import Lens.Micro.TH (makeLenses)
import Lima.Converter.Internal
import Text.Read (readMaybe)
import PyF

-- | A kind of data markers.
data Mode'
  = Internal
  | User

-- | Marks data for internal usage.
type Internal = 'Internal

-- | Marks data supplied by a user.
type User = 'User

-- | Calculates the mode for data.
type family Mode a where
  Mode User = Maybe String
  Mode Internal = T.Text

-- | Configuration of tag names.
--
-- The default values of @Config User@ are all 'Nothing's.
--
-- Inside the library functions, @Config User@ is converted to @Config Internal@.
--
-- The below examples show the names from @Config Internal@.
--
-- >>> pp (def :: Config User)
-- Config {
--   _disable = "LIMA_DISABLE",
--   _enable = "LIMA_ENABLE",
--   _indent = "LIMA_INDENT",
--   _dedent = "LIMA_DEDENT",
--   _mdHaskellCodeStart = "```haskell",
--   _mdHaskellCodeEnd = "```",
--   _texHaskellCodeStart = "\\begin{mycode}",
--   _texHaskellCodeEnd = "\\end{mycode}",
--   _texSingleLineCommentStart = "SINGLE_LINE ",
--   _lhsSingleLineCommentStart = "SINGLE_LINE "
-- }
--
-- It's possible to override these names.
--
-- >>> pp ((def :: Config User) & disable ?~ "off" & enable ?~ "on" & indent ?~ "indent" & dedent ?~ "dedent")
-- Config {
--   _disable = "off",
--   _enable = "on",
--   _indent = "indent",
--   _dedent = "dedent",
--   _mdHaskellCodeStart = "```haskell",
--   _mdHaskellCodeEnd = "```",
--   _texHaskellCodeStart = "\\begin{mycode}",
--   _texHaskellCodeEnd = "\\end{mycode}",
--   _texSingleLineCommentStart = "SINGLE_LINE ",
--   _lhsSingleLineCommentStart = "SINGLE_LINE "
-- }
data Config (a :: Mode') = Config
  { _disable :: Mode a
  -- ^
  -- Make parser ignore tags and just copy the following lines verbatim.
  --
  -- Set indentation to @0@.
  , _enable :: Mode a
  -- ^ Stop parser from ignoring tags.
  , _indent :: Mode a
  -- ^ Set code indentation to a given 'Int'.
  , _dedent :: Mode a
  -- ^ Set code indentation to @0@.
  , _mdHaskellCodeStart :: Mode a
  -- ^ Mark the start of a @Haskell@ code block in @Markdown@.
  , _mdHaskellCodeEnd :: Mode a
  -- ^ Mark the end of a @Haskell@ code block in @Markdown@.
  , _texHaskellCodeStart :: Mode a
  -- ^ Mark the start of a @Haskell@ code block in @TeX@.
  , _texHaskellCodeEnd :: Mode a
  -- ^ Mark the end of a @Haskell@ code block in @TeX@.
  , _texSingleLineCommentStart :: Mode a
  -- ^ Mark start of a comment that must be single-line in @TeX@.
  , _lhsSingleLineCommentStart :: Mode a
  -- ^ Mark start of a comment that must be single-line in @Literate Haskell@.
  }
  deriving (Generic)

makeLenses ''Config

deriving instance Show (Config User)
deriving instance Eq (Config User)
deriving instance Show (Config Internal)

instance PrettyPrint (Config User) where
  pp :: Config User -> Pretty String
  pp (toConfigInternal -> config) =
    pp $
      concatMap
        ( \(a, b) ->
            if
              | [a, b] == " _" -> "\n  "
              | [a, b] == "{_" -> "{\n  "
              | otherwise -> [a]
        )
        (zip (show config) (drop 1 $ show config))
        <> "\n}"

instance Default (Config Internal) where
  def :: Config Internal
  def =
    Config
      { _disable = "LIMA_DISABLE"
      , _enable = "LIMA_ENABLE"
      , _indent = "LIMA_INDENT"
      , _dedent = "LIMA_DEDENT"
      , _mdHaskellCodeStart = "```haskell"
      , _mdHaskellCodeEnd = "```"
      , _texHaskellCodeStart = "\\begin{mycode}"
      , _texHaskellCodeEnd = "\\end{mycode}"
      , _texSingleLineCommentStart = "SINGLE_LINE"
      , _lhsSingleLineCommentStart = "SINGLE_LINE"
      }

deriving instance Default (Config User)

-- | Convert a user 'Config' to an internal 'Config' with user-supplied values.
--
-- It's important to do this conversion at a single entrypoint.
--
-- Otherwise, repeated conversions will accumulate changes such as appended spaces.
toConfigInternal :: Config User -> Config Internal
toConfigInternal conf =
  Config
    { _disable = l disable _disable
    , _enable = l enable _enable
    , _indent = l indent _indent
    , _dedent = l dedent _dedent
    , _mdHaskellCodeStart = l mdHaskellCodeStart _mdHaskellCodeStart
    , _mdHaskellCodeEnd = l mdHaskellCodeEnd _mdHaskellCodeEnd
    , _texHaskellCodeStart = l texHaskellCodeStart _texHaskellCodeStart
    , _texHaskellCodeEnd = l texHaskellCodeEnd _texHaskellCodeEnd
    , _texSingleLineCommentStart = (l texSingleLineCommentStart _texSingleLineCommentStart) <> " "
    , _lhsSingleLineCommentStart = (l lhsSingleLineCommentStart _lhsSingleLineCommentStart) <> " "
    }
 where
  l a b = conf ^. a . to (T.pack <$>) . non b
  Config
    { _disable
    , _enable
    , _indent
    , _dedent
    , _mdHaskellCodeStart
    , _mdHaskellCodeEnd
    , _texHaskellCodeStart
    , _texHaskellCodeEnd
    , _texSingleLineCommentStart
    , _lhsSingleLineCommentStart
    } = def @(Config Internal)

-- | A format of a document.
data Format
  = -- | @Haskell@
    Hs
  | -- | @Literate Haskell@
    Lhs
  | -- | @Markdown@
    Md
  | -- | @TeX@
    TeX
  deriving (Eq)

-- | Show a 'Format' as a file extension.
--
-- >>>showFormatExtension Lhs
-- "lhs"
showFormatExtension :: Format -> String
showFormatExtension = \case
  Hs -> "hs"
  Md -> "md"
  Lhs -> "lhs"
  TeX -> "tex"

-- | Show a 'Format' as a full name.
--
-- >>>showFormatName Lhs
-- "Literate Haskell"
showFormatName :: Format -> String
showFormatName = \case
  Hs -> "Haskell"
  Md -> "Markdown"
  Lhs -> "Literate Haskell"
  TeX -> "TeX"

-- | Internal representation of a document.
--
-- A printer processes 'Tokens' one by one.
--
-- A 'Token' can have:
--
-- - Action - how this 'Token' affects the subsequent 'Tokens'.
-- - Target - a type of 'Tokens' that are affected by this 'Token'.
-- - Range - the nearest 'Token' until which this 'Token' affects the subsequent 'Tokens'.
data Token
  = -- |
    -- - Action: set indentation to @n@.
    --
    -- - Target: 'HaskellCode'.
    --
    -- - Range: 'Indent', 'Dedent', or 'Disabled'.
    Indent {n :: Int}
  | -- |
    -- - Action: set indentation to @0@.
    --
    -- - Target: 'HaskellCode'.
    --
    -- - Range: 'Indent', 'Dedent', or 'Disabled'.
    Dedent
  | -- | A block that should be invisible when rendered outside of @.hs@.
    --
    -- - Action: set indentation to @0@.
    --
    -- - Target: 'HaskellCode'.
    --
    -- - Range: 'Indent', 'Dedent', or 'Disabled'.
    Disabled {manyLines :: [T.Text]}
  | -- | Lines copied verbatim while a parser was in a @Haskell@ code block.
    HaskellCode {manyLines :: [T.Text]}
  | -- | Lines copied verbatim while a parser was in a text block.
    Text {someLines :: NonEmpty T.Text}
  | -- | Lines copied verbatim while a parser was in a comment block.
    Comment {someLines :: NonEmpty T.Text}
  | -- | A line of a comment that must be kept on a single-line.
    --
    -- E.g., {\- FOURMOLU_DISABLE -\} from a @.hs@.
    CommentSingleLine {someLine :: T.Text}
  deriving (Show, Data, Eq)

-- | A list of 'Token's.
type Tokens = [Token]

instance PrettyPrint Tokens where
  pp :: Tokens -> Pretty String
  pp ts =
    Pretty $
      concatMap
        ( \(a, b) ->
            if
              | a == ',' && isAlpha b -> ",\n  "
              | a == '[' && isAlpha b -> "[\n  "
              | otherwise -> [a]
        )
        (zip (show ts) (drop 1 $ show ts))
        <> "\n]"

-- | Select a printer function based on a given format.
selectFromTokens :: Config User -> Format -> Tokens -> T.Text
selectFromTokens config format =
  ( case format of
      Hs -> hsFromTokens
      Lhs -> lhsFromTokens
      Md -> mdFromTokens
      TeX -> texFromTokens
  )
    config

-- | Select a parser function based on a given format.
selectToTokens :: Config User -> Format -> T.Text -> Tokens
selectToTokens config format =
  ( case format of
      Hs -> hsToTokens
      Lhs -> lhsToTokens
      Md -> mdToTokens
      TeX -> texToTokens
  )
    config

-- | Example non-@TeX@ 'Tokens'. See 'exampleTexTokens'.
--
-- When printed to a @TeX@ document, these 'Tokens' can't be correctly parsed.
-- This is because they don't have necessary tags surrounding @Haskell@ code blocks.
--
-- >>> pp $ exampleNonTexTokens'
-- [
--   Indent {n = 3},
--   Disabled {manyLines = ["-- What's the answer?"]},
--   Indent {n = 1},
--   Indent {n = 2},
--   Text {someLines = "- Intermediate results" :| []},
--   HaskellCode {manyLines = ["   b = a 4","   a = const 3"]},
--   Dedent,
--   HaskellCode {manyLines = ["answer = b * 14"]},
--   Comment {someLines = "Hello from comments," :| []},
--   Comment {someLines = "world!" :| []},
--   CommentSingleLine {someLine = "Comment on a single line."},
--   Text {someLines = "Hello from text," :| []},
--   Text {someLines = "world!" :| []}
-- ]
exampleNonTexTokens' :: Tokens
exampleNonTexTokens' =
  [ Indent{n = 3}
  , Disabled{manyLines = ["-- What's the answer?"]}
  , Indent{n = 1}
  , Indent{n = 2}
  , Text{someLines = ("- Intermediate results" :| [])}
  , HaskellCode{manyLines = ["   b = a 4", "   a = const 3"]}
  , Dedent
  , HaskellCode{manyLines = ["answer = b * 14"]}
  , Comment{someLines = ("Hello from comments," :| [])}
  , Comment{someLines = ("world!" :| [])}
  , CommentSingleLine{someLine = "Comment on a single line."}
  , Text{someLines = ("Hello from text," :| [])}
  , Text{someLines = ("world!" :| [])}
  ]

-- | Merge specific consecutive 'Tokens'.
--
-- >>> pp exampleNonTexTokens'
-- [
--   Indent {n = 3},
--   Disabled {manyLines = ["-- What's the answer?"]},
--   Indent {n = 1},
--   Indent {n = 2},
--   Text {someLines = "- Intermediate results" :| []},
--   HaskellCode {manyLines = ["   b = a 4","   a = const 3"]},
--   Dedent,
--   HaskellCode {manyLines = ["answer = b * 14"]},
--   Comment {someLines = "Hello from comments," :| []},
--   Comment {someLines = "world!" :| []},
--   CommentSingleLine {someLine = "Comment on a single line."},
--   Text {someLines = "Hello from text," :| []},
--   Text {someLines = "world!" :| []}
-- ]
--
-- >>> pp $ mergeTokens exampleNonTexTokens'
-- [
--   Indent {n = 3},
--   Disabled {manyLines = ["-- What's the answer?"]},
--   Indent {n = 1},
--   Indent {n = 2},
--   Text {someLines = "- Intermediate results" :| []},
--   HaskellCode {manyLines = ["   b = a 4","   a = const 3"]},
--   Dedent,
--   HaskellCode {manyLines = ["answer = b * 14"]},
--   Comment {someLines = "world!" :| ["","Hello from comments,"]},
--   CommentSingleLine {someLine = "Comment on a single line."},
--   Text {someLines = "world!" :| ["","Hello from text,"]}
-- ]
mergeTokens :: Tokens -> Tokens
mergeTokens (t1@Text{} : t2@Text{} : ts) = mergeTokens $ Text{someLines = someLines t2 <> (T.empty <| someLines t1)} : ts
mergeTokens (Comment{someLines = ls1} : Comment{someLines = ls2} : ts) =
  mergeTokens $ Comment{someLines = ls2 <> (T.empty <| ls1)} : ts
mergeTokens (t : ts) = t : mergeTokens ts
mergeTokens ts = ts

-- | Strip empty lines and leading spaces in 'Tokens'.
--
-- - Remove empty lines in 'Tokens'.
-- - Shift lines in 'HaskellCode' to the left by the minimal number of leading spaces in nonempty lines.
--
-- >>> pp exampleNonTexTokens'
-- [
--   Indent {n = 3},
--   Disabled {manyLines = ["-- What's the answer?"]},
--   Indent {n = 1},
--   Indent {n = 2},
--   Text {someLines = "- Intermediate results" :| []},
--   HaskellCode {manyLines = ["   b = a 4","   a = const 3"]},
--   Dedent,
--   HaskellCode {manyLines = ["answer = b * 14"]},
--   Comment {someLines = "Hello from comments," :| []},
--   Comment {someLines = "world!" :| []},
--   CommentSingleLine {someLine = "Comment on a single line."},
--   Text {someLines = "Hello from text," :| []},
--   Text {someLines = "world!" :| []}
-- ]
--
-- >>> pp $ stripTokens exampleNonTexTokens'
-- [
--   Indent {n = 3},
--   Disabled {manyLines = ["-- What's the answer?"]},
--   Indent {n = 1},
--   Indent {n = 2},
--   Text {someLines = "- Intermediate results" :| []},
--   HaskellCode {manyLines = ["b = a 4","a = const 3"]},
--   Dedent,
--   HaskellCode {manyLines = ["answer = b * 14"]},
--   Comment {someLines = "Hello from comments," :| []},
--   Comment {someLines = "world!" :| []},
--   CommentSingleLine {someLine = "Comment on a single line."},
--   Text {someLines = "Hello from text," :| []},
--   Text {someLines = "world!" :| []}
-- ]
stripTokens :: Tokens -> Tokens
stripTokens xs =
  ( \case
      Disabled{manyLines} -> Disabled{manyLines = stripEmpties manyLines}
      HaskellCode{manyLines} ->
        let ls = stripEmpties manyLines
         in HaskellCode{manyLines = T.drop (minimum (countSpaces <$> filter (not . T.null) ls)) <$> ls}
      Text{someLines} -> Text{someLines = fromList $ stripEmpties (toList someLines)}
      Comment{someLines} -> Comment{someLines = fromList $ stripEmpties (toList someLines)}
      x -> x
  )
    <$> xs

-- | 'mergeTokens' and 'stripTokens'.
--
-- >>>pp $ normalizeTokens exampleNonTexTokens
-- [
--   Indent {n = 3},
--   Disabled {manyLines = ["-- What's the answer?"]},
--   Indent {n = 1},
--   Indent {n = 2},
--   Text {someLines = "- Intermediate results" :| []},
--   HaskellCode {manyLines = ["b = a 4","a = const 3"]},
--   Dedent,
--   HaskellCode {manyLines = ["answer = b * 14"]},
--   Comment {someLines = "world!" :| ["","Hello from comments,"]},
--   CommentSingleLine {someLine = "Comment on a single line."},
--   Text {someLines = "world!" :| ["","Hello from text,"]}
-- ]
normalizeTokens :: Tokens -> Tokens
normalizeTokens tokens = stripTokens $ mergeTokens tokens

-- | Normalized 'exampleNonTexTokens''.
--
-- >>>pp $ exampleNonTexTokens
-- [
--   Indent {n = 3},
--   Disabled {manyLines = ["-- What's the answer?"]},
--   Indent {n = 1},
--   Indent {n = 2},
--   Text {someLines = "- Intermediate results" :| []},
--   HaskellCode {manyLines = ["b = a 4","a = const 3"]},
--   Dedent,
--   HaskellCode {manyLines = ["answer = b * 14"]},
--   Comment {someLines = "world!" :| ["","Hello from comments,"]},
--   CommentSingleLine {someLine = "Comment on a single line."},
--   Text {someLines = "world!" :| ["","Hello from text,"]}
-- ]
exampleNonTexTokens :: Tokens
exampleNonTexTokens = normalizeTokens exampleNonTexTokens'

-- | same as 'exampleNonTexTokens', but with @TeX@-specific tags that make @Haskell@ code blocks correctly parsable.
--
-- >>> pp $ exampleTexTokens
-- [
--   Indent {n = 3},
--   Disabled {manyLines = ["-- What's the answer?"]},
--   Indent {n = 1},
--   Indent {n = 0},
--   Text {someLines = "\\begin{mycode}" :| ["","Intermediate results"]},
--   HaskellCode {manyLines = ["b = a 4","a = const 3"]},
--   Text {someLines = "\\end{mycode}" :| []},
--   Dedent,
--   Text {someLines = "\\begin{mycode}" :| []},
--   HaskellCode {manyLines = ["answer = b * 14"]},
--   Text {someLines = "\\end{mycode}" :| []},
--   Comment {someLines = "world!" :| ["","Hello from comments,"]},
--   CommentSingleLine {someLine = "Comment on a single line."}
-- ]
exampleTexTokens :: Tokens
exampleTexTokens =
  normalizeTokens
    [ Indent 3
    , Disabled{manyLines = ["-- What's the answer?"]}
    , Indent 1
    , Indent 0
    , Text{someLines = "Intermediate results" :| []}
    , codeStart
    , HaskellCode ["   b = a 4", "   a = const 3"]
    , codeEnd
    , Dedent
    , codeStart
    , HaskellCode ["answer = b * 14"]
    , codeEnd
    , Comment ("Hello from comments," :| [])
    , Comment ("world!" :| [])
    , CommentSingleLine "Comment on a single line."
    ]
 where
  codeStart = Text{someLines = def @(Config Internal) ^. texHaskellCodeStart :| []}
  codeEnd = Text{someLines = def @(Config Internal) ^. texHaskellCodeEnd :| []}

-- | Compose a function that converts a document in one 'Format' to a document in another 'Format'.
convertTo :: Format -> Format -> Config User -> T.Text -> T.Text
convertTo a b config src = selectFromTokens config b $ selectToTokens config a src

-- | State of a parser.
--
-- Only one flag can be enabled when processing a line.
--
-- Flags signify in what document block a converter is at the moment.
data State = State
  { inText :: Bool
  , inHaskellCode :: Bool
  , inDisabled :: Bool
  , inComment :: Bool
  }
  deriving (Generic)

instance Default State where
  def :: State
  def =
    State
      { inText = False
      , inHaskellCode = False
      , inDisabled = False
      , inComment = False
      }

-- | Compose a function from 'Tokens' to a 'T.Text'.
mkFromTokens :: (Config User -> Tokens -> [T.Text]) -> Config User -> Tokens -> T.Text
mkFromTokens f' config = (<> "\n") . T.intercalate "\n" . f' config

-- | Compose a function from a 'T.Text' to 'Tokens'.
mkToTokens :: (State -> [(Int, T.Text)] -> [Token] -> [Token]) -> T.Text -> Tokens
mkToTokens toTokens xs = normalizeTokens (drop 1 $ reverse $ toTokens def (zip [1 ..] (T.lines xs)) [Dedent])

-- | Parse a single line to a token.
--
-- - Merge comments
parseLineToToken :: Config Internal -> Format -> Token -> T.Text -> Int -> Tokens
parseLineToToken Config{_indent, _dedent, _texSingleLineCommentStart, _lhsSingleLineCommentStart} format prev l lineNumber
  | l `startsWith` _indent =
      maybe
        (error $ "Expected a number after " <> T.unpack _indent <> " at line: " <> show lineNumber)
        (\x -> [Indent (max 0 x), prev])
        (readMaybe @Int (T.unpack (dropLen _indent l)))
  | l == _dedent = [Dedent, prev]
  | format `elem` [Md, Hs] = [CommentSingleLine{someLine = l}, prev]
  | format == TeX && l `startsWith` _texSingleLineCommentStart =
      [CommentSingleLine{someLine = dropLen _texSingleLineCommentStart l}, prev]
  | format == Lhs && l `startsWith` _lhsSingleLineCommentStart =
      [CommentSingleLine{someLine = dropLen _lhsSingleLineCommentStart l}, prev]
  | otherwise =
      case prev of
        Comment{someLines} -> [Comment{someLines = l <| someLines}]
        _ -> [Comment{someLines = (l :| [])}, prev]

-- | Show error with line number for a token.
errorExpectedToken :: Int -> Token -> Token -> a
errorExpectedToken lineNumber lastToken expectedToken =
  error
    [fmt|
      Wrong state at line: {lineNumber}.
      
      Expected last token: {constructorName expectedToken}.
      
      Got last token: {show lastToken}.
      
      Please create an issue in the package repository.
    |]

errorNotEnoughTokens :: Format -> a
errorNotEnoughTokens format = error $ "Got not enough tokens when converting 'Tokens' to " <> showFormatName format

-- | Convert 'Tokens' to @TeX@ code.
--
-- __Rules__
--
-- - Certain [assumptions]("Converter#assumptions") must hold for inputs.
-- - These are the relations between tokens and document blocks when the default 'Config' values are used.
--
--     - 'Indent' ~ @'% LIMA_INDENT N'@ (@N@ is an 'Int').
--     - 'Dedent' ~ @'% LIMA_DEDENT'@.
--     - 'Disabled' ~ @'% LIMA_DISABLE'@ and @'% LIMA_ENABLE'@ and lines between them.
--     - 'CommentSingleLine' ~ a line starting with @'% SINGLE_LINE '@.
--
--         @
--         % SINGLE_LINE line
--         @
--
--     - 'Comment' ~ consecutive lines, either empty or starting with @'% '@.
--
--         @
--         % Hello,
--         % world!
--
--         % Hello,
--         % user!
--         @
--
--         - At least one line must have nonempty text after @'% '@.
--
--     - 'HaskellCode' ~ lines between possibly indented tags @'\\begin{code}'@ and @'\\end{code}'@.
--
--         - Inside a 'Token', code will be shifted to the left. See 'normalizeTokens'.
--         - When printing the 'Tokens', code will be indented according to previous 'Tokens'.
--
--     - 'Text' ~ other lines.
--
-- === __Example__
--
-- >>> pp $ texFromTokens def exampleTexTokens
-- % LIMA_INDENT 3
-- <BLANKLINE>
-- % LIMA_DISABLE
-- <BLANKLINE>
-- % -- What's the answer?
-- <BLANKLINE>
-- % LIMA_ENABLE
-- <BLANKLINE>
-- % LIMA_INDENT 1
-- <BLANKLINE>
-- % LIMA_INDENT 0
-- <BLANKLINE>
-- Intermediate results
-- <BLANKLINE>
-- \begin{mycode}
-- a = const 3
-- b = a 4
-- \end{mycode}
-- <BLANKLINE>
-- % LIMA_DEDENT
-- <BLANKLINE>
-- \begin{mycode}
-- answer = b * 14
-- \end{mycode}
-- <BLANKLINE>
-- % Hello from comments,
-- <BLANKLINE>
-- % world!
-- <BLANKLINE>
-- % SINGLE_LINE Comment on a single line.
texFromTokens :: Config User -> Tokens -> T.Text
texFromTokens = mkFromTokens texFromTokens'

-- | Convert 'Tokens' to @TeX@ code.
--
-- Each 'Token' becomes a 'T.Text' in a list.
--
-- These 'T.Text's are concatenated in 'texFromTokens'.
texFromTokens' :: Config User -> Tokens -> [T.Text]
texFromTokens' (toConfigInternal -> Config{_disable, _enable, _indent, _dedent, _texSingleLineCommentStart}) tokens =
  dropEmpties $ reverse (T.intercalate "\n" . reverse <$> fromTokens (Dedent : tokens) (0, []))
 where
  fromTokens :: Tokens -> (Int, [[T.Text]]) -> [[T.Text]]
  fromTokens bs'@(_ : cur : bs) (curIndent, rs) =
    fromTokens (cur : bs) (translate curIndent bs' rs)
  fromTokens [_] (_, rs) = rs
  fromTokens _ _ = errorNotEnoughTokens TeX
  translate curIndent (prev : cur : _) rs =
    case cur of
      Indent{n} -> (n,) $ [texCommentSpace <> _indent <> " " <> T.show n] : [] : rs
      Dedent -> (0,) $ [texCommentSpace <> _dedent] : [] : rs
      Disabled{manyLines} -> (0,) $ [[texCommentSpace <> _enable], [], prependTexComment <$> manyLines, [], [texCommentSpace <> _disable], []] <> rs
      HaskellCode{manyLines} ->
        (curIndent,) $
          (indentN curIndent <$> manyLines)
            : ( case prev of
                  Text{} -> rs
                  _ -> [] : rs
              )
      Text{someLines} ->
        (curIndent,) $
          toList someLines
            : ( case prev of
                  HaskellCode{} -> rs
                  _ -> [] : rs
              )
      Comment{someLines = t :| ts} -> (curIndent, (prependTexComment <$> (t : ts)) : [] : rs)
      CommentSingleLine{someLine} -> (curIndent, [prependTexComment $ _texSingleLineCommentStart <> someLine] : [] : rs)
  translate _ _ _ = errorNotEnoughTokens TeX

-- | Convert 'Tokens' to @TeX@ code.
--
-- Inverse of 'texFromTokens'.
--
-- >>> (texToTokens def $ texFromTokens def exampleTexTokens) == exampleTexTokens
-- True
texToTokens :: Config User -> T.Text -> Tokens
texToTokens (toConfigInternal -> conf@Config{_disable, _enable, _texHaskellCodeStart, _texHaskellCodeEnd}) xs = tokens
 where
  tokens = mkToTokens toTokens xs
  toTokens :: State -> [(Int, T.Text)] -> Tokens -> Tokens
  toTokens State{inText, inHaskellCode, inDisabled, inComment} ((lineNumber, l) : ls) result@(r : rs)
    | inDisabled =
        if l `startsWith` (texCommentSpace <> _enable)
          then toTokens def ls result
          else -- copy lines
            toTokens def{inDisabled} ls $
              case r of
                Disabled{manyLines} -> r{manyLines = dropTexComment l lineNumber : manyLines} : rs
                _ -> errorExpected Disabled{}
    | inHaskellCode =
        if -- end of a snippet
        stripSpaces l `startsWith` _texHaskellCodeEnd
          then toTokens def{inText = True} ls (Text{someLines = l :| []} : result)
          else toTokens def{inHaskellCode} ls $
            case r of
              HaskellCode{manyLines} -> r{manyLines = l : manyLines} : rs
              _ -> errorExpected HaskellCode{}
    | stripSpaces l `startsWith` _texHaskellCodeStart =
        toTokens def{inHaskellCode = True} ls $
          HaskellCode{manyLines = []}
            : case r of
              Text{someLines} -> Text{someLines = l <| someLines} : rs
              _ -> Text{someLines = l :| []} : result
    | -- Comment on a single line.
      l `startsWith` texCommentSpace =
        let l' = dropLen texCommentSpace l
         in if -- disable
            l' `startsWith` _disable
              then toTokens def{inDisabled = True} ls (Disabled [] : result)
              else
                toTokens def ls $
                  parseLineToToken conf TeX r l' lineNumber <> rs
    | inText =
        toTokens def{inText} ls $
          case r of
            Text{someLines} -> Text{someLines = l <| someLines} : rs
            _ -> errorExpected Text{}
    | -- a blank line
      T.null l =
        case r of
          Comment{someLines} -> toTokens def{inComment} ls (Comment{someLines = l <| someLines} : rs)
          _ -> toTokens def ls result
    | -- start of a text
      otherwise =
        toTokens def{inText = True} ls $ Text{someLines = l :| []} : result
   where
    errorExpected = errorExpectedToken lineNumber r
  toTokens _ _ res = res

-- | Convert 'Tokens' to @Literate Haskell@ code.
--
-- __Rules__
--
-- - Certain [assumptions]("Converter#assumptions") must hold for inputs.
--
-- - These are the relations between document blocks and tokens when the default 'Config' values are used.
--
--     - 'Indent' ~ @'% LIMA_INDENT N'@ (@N@ is an 'Int').
--     - 'Dedent' ~ @'% LIMA_DEDENT'@.
--     - 'Disabled' ~ Lines between and including @'% LIMA_DISABLE'@ and @'% LIMA_ENABLE'@.
--
--         - There must be at least one nonempty line between these tags.
--
--     - 'CommentSingleLine' ~ a line starting with @'% SINGLE_LINE '@.
--
--         @
--         % SINGLE_LINE line
--         @
--
--     - 'Comment' ~ consecutive lines, either empty or starting with @'% '@.
--
--         @
--         % Hello,
--         % world!
--
--         % Hello,
--         % user!
--         @
--
--         - At least one line must have nonempty text after @'% '@
--
--     - 'HaskellCode' ~ consecutive lines starting with @'> '@.
--
--         @
--         > a4 = 4
--         > a2 = 2
--         @
--
--         - Inside a 'Token', code is shifted to the left. See 'normalizeTokens'.
--         - During printing, code is indented according to previous 'Tokens'.
--
--     - 'Text' ~ other lines.
--
-- === __Example__
--
-- >>> pp $ lhsFromTokens def exampleNonTexTokens
-- % LIMA_INDENT 3
-- <BLANKLINE>
-- % LIMA_DISABLE
-- <BLANKLINE>
-- % -- What's the answer?
-- <BLANKLINE>
-- % LIMA_ENABLE
-- <BLANKLINE>
-- % LIMA_INDENT 1
-- <BLANKLINE>
-- % LIMA_INDENT 2
-- <BLANKLINE>
-- - Intermediate results
-- <BLANKLINE>
-- >   a = const 3
-- >   b = a 4
-- <BLANKLINE>
-- % LIMA_DEDENT
-- <BLANKLINE>
-- > answer = b * 14
-- <BLANKLINE>
-- % Hello from comments,
-- <BLANKLINE>
-- % world!
-- <BLANKLINE>
-- % SINGLE_LINE Comment on a single line.
-- <BLANKLINE>
-- Hello from text,
-- <BLANKLINE>
-- world!
lhsFromTokens :: Config User -> Tokens -> T.Text
lhsFromTokens = mkFromTokens lhsFromTokens'

-- | Convert 'Tokens' to @Literate Haskell@ code.
--
-- Each 'Token' becomes a 'T.Text' in a list.
--
-- These 'T.Text's are concatenated in 'lhsFromTokens'.
lhsFromTokens' :: Config User -> Tokens -> [T.Text]
lhsFromTokens' (toConfigInternal -> Config{_disable, _enable, _indent, _dedent, _lhsSingleLineCommentStart}) blocks =
  dropEmpties $ reverse (T.intercalate "\n" . reverse <$> fromTokens blocks (0, []))
 where
  fromTokens :: Tokens -> (Int, [[T.Text]]) -> [[T.Text]]
  fromTokens (cur : bs) (curIndent, rs) =
    fromTokens bs (translate curIndent (cur : bs) rs)
  fromTokens [] (_, rs) = rs
  translate curIndent (cur : _) rs =
    case cur of
      Indent{n} -> (n, [lhsCommentSpace <> _indent <> " " <> T.pack (show n)] : [] : rs)
      Dedent -> (0, [lhsCommentSpace <> _dedent] : [] : rs)
      Disabled{manyLines} -> (0, [lhsCommentSpace <> _enable] : [] : (prependLhsComment <$> manyLines) : [] : [lhsCommentSpace <> _disable] : [] : rs)
      HaskellCode{manyLines} -> (curIndent, ((lhsHsCodeSpace <>) . indentN curIndent <$> manyLines) : [] : rs)
      Text{someLines} -> (curIndent, toList (lhsEscapeHash <$> someLines) : [] : rs)
      Comment{someLines = t :| ts} -> (curIndent, (prependLhsComment <$> t : ts) : [] : rs)
      CommentSingleLine{someLine} -> (curIndent, [prependLhsComment $ _lhsSingleLineCommentStart <> someLine] : [] : rs)
  translate _ _ _ = errorNotEnoughTokens Lhs

-- | Convert 'Tokens' to @Markdown@ code.
--
-- Inverse of 'lhsFromTokens'.
--
-- >>> (lhsToTokens def $ lhsFromTokens def exampleNonTexTokens) == exampleNonTexTokens
-- True
lhsToTokens :: Config User -> T.Text -> Tokens
lhsToTokens (toConfigInternal -> conf@Config{_disable, _enable}) xs = tokens
 where
  tokens = mkToTokens toTokens xs
  toTokens :: State -> [(Int, T.Text)] -> Tokens -> Tokens
  toTokens State{inText, inDisabled, inComment} ((lineNumber, lhsUnescapeHash -> l) : ls) result@(r : rs)
    | inDisabled =
        if -- enable
        l `startsWith` (lhsCommentSpace <> _enable)
          then toTokens def ls result
          else -- copy lines
            toTokens def{inDisabled} ls $
              case r of
                Disabled{manyLines} -> r{manyLines = dropLhsComment l lineNumber : manyLines} : rs
                _ -> errorExpected Disabled{}
    | -- Comment on a single line.
      l `startsWith` lhsCommentSpace =
        let l' = dropLen lhsCommentSpace l
         in if -- disable
            l' `startsWith` _disable
              then toTokens def{inDisabled = True} ls (Disabled{manyLines = []} : result)
              else
                toTokens def ls $
                  parseLineToToken conf Lhs r l' lineNumber <> rs
    | -- start of a snippet
      l `startsWith` lhsHsCodeSpace =
        toTokens def{inHaskellCode = True} ls $
          let l' = dropLen lhsHsCodeSpace l
           in case r of
                HaskellCode{manyLines} -> r{manyLines = l' : manyLines} : rs
                _ -> HaskellCode{manyLines = [l']} : result
    | inText =
        toTokens def{inText} ls $
          case r of
            Text{someLines} -> Text{someLines = l <| someLines} : rs
            _ -> errorExpected Text{}
    | -- a blank line
      T.null l =
        case r of
          Comment{someLines} -> toTokens def{inComment} ls (Comment{someLines = l <| someLines} : rs)
          _ -> toTokens def ls result
    | -- start of a text
      otherwise =
        toTokens def{inText = True} ls $ Text{someLines = l :| []} : result
   where
    errorExpected = error . errorExpectedToken lineNumber r
  toTokens _ _ res = res

-- | Convert 'Tokens' to @Markdown@ code.
--
-- __Rules__
--
-- - Certain [assumptions]("Converter#assumptions") must hold for inputs.
--
-- - These are the relations between document blocks and tokens when the default 'Config' values are used.
--
--     - 'Indent' ~ @'<!-- LIMA_INDENT N --\>'@, where @N@ is an 'Int'.
--     - 'Dedent' ~ @'<!-- LIMA_DEDENT --\>'@.
--     - 'Disabled' ~ a multiline comment
--       starting with @'<!-- LIMA_DISABLE\\n'@
--       and ending with @'\\nLIMA_ENABLE --\>'@.
--
--         @
--         <!-- LIMA_DISABLE
--         a4 = 4
--         a2 = 2
--         LIMA_ENABLE --\>
--         @
--
--     - 'CommentSingleLine' ~ a line starting with @'<!-- '@ and ending with @' -->'@.
--
--         @
--         <!-- line -->
--         @
--
--     - 'Comment' ~ a multiline comment starting with @'<!-- {text}'@, where @{text}@ is nonempty text.
--
--         @
--         <!-- line 1
--         line 2
--         --\>
--         @
--
--         - Consecutive 'Comment's are merged into a single 'Comment'.
--
--     - 'HaskellCode' ~ possibly indented block starting with @\'```haskell\'@ and ending with @'```'@.
--
--         @
--           ```haskell
--             a4 = 2
--           ```
--         @
--
--     - 'Text' ~ other lines.
--
-- === __Example__
--
-- >>> pp $ mdFromTokens def exampleNonTexTokens
--    <!-- LIMA_INDENT 3 -->
-- <BLANKLINE>
-- <!-- LIMA_DISABLE
-- <BLANKLINE>
-- -- What's the answer?
-- <BLANKLINE>
-- LIMA_ENABLE -->
-- <BLANKLINE>
--  <!-- LIMA_INDENT 1 -->
-- <BLANKLINE>
--   <!-- LIMA_INDENT 2 -->
-- <BLANKLINE>
-- - Intermediate results
-- <BLANKLINE>
--   ```haskell
--   a = const 3
--   b = a 4
--   ```
-- <BLANKLINE>
-- <!-- LIMA_DEDENT -->
-- <BLANKLINE>
-- ```haskell
-- answer = b * 14
-- ```
-- <BLANKLINE>
-- <!-- Hello from comments,
-- <BLANKLINE>
-- world!
-- -->
-- <BLANKLINE>
-- <!-- Comment on a single line. -->
-- <BLANKLINE>
-- Hello from text,
-- <BLANKLINE>
-- world!
mdFromTokens :: Config User -> Tokens -> T.Text
mdFromTokens = mkFromTokens mdFromTokens'

-- | Convert 'Tokens' to @Haskell@ code.
--
-- Each 'Token' becomes a 'T.Text' in a list.
--
-- These 'T.Text's are concatenated in 'mdFromTokens'.
mdFromTokens' :: Config User -> Tokens -> [T.Text]
mdFromTokens' (toConfigInternal -> Config{_disable, _enable, _indent, _dedent, _mdHaskellCodeStart, _mdHaskellCodeEnd}) blocks =
  intersperse T.empty . reverse $ T.intercalate "\n" . reverse <$> fromTokens 0 blocks []
 where
  fromTokens :: Int -> Tokens -> [[T.Text]] -> [[T.Text]]
  fromTokens _ [] res = res
  fromTokens curIndent (b : bs) res =
    case b of
      Indent{n} -> fromTokens n bs ([indentN n $ mdCommentOpenSpace <> _indent <> " " <> T.pack (show n) <> mdCommentCloseSpace] : res)
      Dedent -> fromTokens 0 bs ([mdCommentOpenSpace <> _dedent <> mdCommentCloseSpace] : res)
      Disabled{manyLines} -> fromTokens 0 bs ([[_enable <> mdCommentCloseSpace]] <> [manyLines] <> [[mdCommentOpenSpace <> _disable]] <> res)
      HaskellCode{manyLines} -> fromTokens curIndent bs ((indentN curIndent <$> ([_mdHaskellCodeEnd] <> manyLines <> [_mdHaskellCodeStart])) : res)
      Text{someLines} -> fromTokens curIndent bs (toList someLines : res)
      Comment{someLines} ->
        fromTokens curIndent bs $
          [mdCommentClose] <> NonEmpty.init someLines <> [mdCommentOpenSpace <> NonEmpty.last someLines] : res
      CommentSingleLine{someLine} ->
        fromTokens curIndent bs $
          [mdCommentOpenSpace <> someLine <> mdCommentCloseSpace] : res

-- | Convert 'Tokens' to @Markdown@ code.
--
-- Inverse of 'mdFromTokens'.
--
-- >>> (mdToTokens def $ mdFromTokens def exampleNonTexTokens) == exampleNonTexTokens
-- True
mdToTokens :: Config User -> T.Text -> Tokens
mdToTokens (toConfigInternal -> conf@Config{_disable, _enable, _mdHaskellCodeStart, _mdHaskellCodeEnd}) xs = tokens
 where
  tokens = mkToTokens toTokens xs
  toTokens :: State -> [(Int, T.Text)] -> Tokens -> Tokens
  toTokens State{inText, inHaskellCode, inDisabled, inComment} ((lineNumber, l) : ls) res@(r : rs)
    | inDisabled =
        -- enable
        if l `startsWith` (_enable <> mdCommentCloseSpace)
          then toTokens def ls res
          else -- copy lines
            toTokens def{inDisabled} ls $
              case r of
                Disabled{manyLines} -> r{manyLines = l : manyLines} : rs
                _ -> errorExpected Disabled{}
    | inComment =
        if l `startsWith` mdCommentClose
          then -- finish comment
            toTokens def ls res
          else -- copy lines
            toTokens def{inComment} ls $
              case r of
                Comment{someLines} -> r{someLines = l <| someLines} : rs
                _ -> errorExpected Comment{}
    | inHaskellCode =
        if stripSpaces l `startsWith` _mdHaskellCodeEnd
          then -- finish snippet
            toTokens def ls res
          else -- copy lines
            toTokens def{inHaskellCode} ls $
              case r of
                HaskellCode{manyLines} -> r{manyLines = l : manyLines} : rs
                _ -> errorExpected HaskellCode{}
    -- Doesn't matter if in text

    | -- Comment on a single line.
      isMdComment l =
        toTokens def ls $ parseLineToToken conf Md r (stripMdComment l) lineNumber <> rs
    | -- start of a comment on multiple lines
      l `startsWith` mdCommentOpenSpace =
        let l' = dropLen mdCommentOpenSpace l
         in if
              | l' == _disable ->
                  toTokens def{inDisabled = True} ls (Disabled{manyLines = []} : res)
              | T.null l' -> error $ errorEmptyCommentAt lineNumber
              | otherwise ->
                  toTokens def{inComment = True} ls $
                    Comment{someLines = (l' :| [])} : res
    | -- start of a haskell snippet
      stripSpaces l `startsWith` _mdHaskellCodeStart =
        toTokens def{inHaskellCode = True} ls (HaskellCode{manyLines = []} : res)
    -- Again matters if in a text
    | inText =
        toTokens def{inText} ls $
          case r of
            Text{someLines} -> Text{someLines = l <| someLines} : rs
            _ -> errorExpected Text{}
    | otherwise =
        if T.null l
          then -- skip
            toTokens def ls res
          else -- start a text
            toTokens def{inText = True} ls $ Text{someLines = l :| []} : res
   where
    errorExpected = error . errorExpectedToken lineNumber r
  toTokens _ _ res = res

-- | Convert 'Tokens' to @Haskell@ code.
--
-- __Rules__
--
-- - Certain [assumptions]("Converter#assumptions") must hold for inputs.
--
-- - These are the relations between 'Tokens' and document blocks when the default 'Config' values are used.
--
--     - 'Indent' ~ @'{- LIMA_INDENT N -}'@ where @N@ is an 'Int'.
--     - 'Dedent' ~ @'{- LIMA_DEDENT -}'@.
--     - 'Disabled' ~ @'{- LIMA_DISABLE -}'@ and @'{- LIMA_ENABLE -}'@ and lines between them.
--
--         @
--         {- LIMA_DISABLE -}
--
--         disabled
--
--         {- LIMA_ENABLE -}
--         @
--
--     - 'Text' ~ a multiline comment starting with @'{-\\n'@ and ending with @'\\n-}'@.
--
--         @
--         {-
--         line 1
--         -}
--         @
--
--         - Consecutive 'Text's are merged into a single 'Text'.
--         - There must be at list one nonempty line inside this comment.
--
--     - 'CommentSingleLine' ~ a multiline comment on a single line.
--
--         @
--         {- line -}
--         @
--
--     - 'Comment' ~ a multiline comment starting with @'{- TEXT'@, where @TEXT@ is nonempty text, and ending with @\\n-}@
--
--         @
--         {- line 1
--         line 2
--         -}
--         @
--
--         - Consecutive 'Comment's are merged into a single 'Comment'.
--
--     - 'HaskellCode' ~ other lines.
--
-- === __Example__
--
-- >>> pp $ hsFromTokens def exampleNonTexTokens
-- {- LIMA_INDENT 3 -}
-- <BLANKLINE>
-- {- LIMA_DISABLE -}
-- <BLANKLINE>
-- -- What's the answer?
-- <BLANKLINE>
-- {- LIMA_ENABLE -}
-- <BLANKLINE>
-- {- LIMA_INDENT 1 -}
-- <BLANKLINE>
-- {- LIMA_INDENT 2 -}
-- <BLANKLINE>
-- {-
-- - Intermediate results
-- -}
-- <BLANKLINE>
-- a = const 3
-- b = a 4
-- <BLANKLINE>
-- {- LIMA_DEDENT -}
-- <BLANKLINE>
-- answer = b * 14
-- <BLANKLINE>
-- {- Hello from comments,
-- <BLANKLINE>
-- world!
-- -}
-- <BLANKLINE>
-- {- Comment on a single line. -}
-- <BLANKLINE>
-- {-
-- Hello from text,
-- <BLANKLINE>
-- world!
-- -}
hsFromTokens :: Config User -> Tokens -> T.Text
hsFromTokens = mkFromTokens hsFromTokens'

-- | Convert 'Tokens' to @Haskell@ code.
--
-- Each 'Token' becomes a 'T.Text' in a list.
--
-- These 'T.Text's are concatenated in 'hsFromTokens'.
hsFromTokens' :: Config User -> Tokens -> [T.Text]
hsFromTokens' (toConfigInternal -> Config{_disable, _enable, _indent, _dedent}) blocks =
  intersperse T.empty . reverse $ T.intercalate "\n" . reverse <$> toHs blocks []
 where
  toHs :: Tokens -> [[T.Text]] -> [[T.Text]]
  toHs [] res = res
  toHs (b : bs) res =
    toHs bs $
      case b of
        Indent{n} -> [hsCommentOpenSpace <> _indent <> " " <> T.pack (show n) <> hsCommentCloseSpace] : res
        Dedent -> [hsCommentOpenSpace <> _dedent <> hsCommentCloseSpace] : res
        Disabled{manyLines} ->
          [[hsCommentOpenSpace <> _enable <> hsCommentCloseSpace]]
            <> [manyLines]
            <> [[hsCommentOpenSpace <> _disable <> hsCommentCloseSpace]]
            <> res
        HaskellCode{manyLines} -> manyLines : res
        Text{someLines} -> [hsCommentClose] <> toList someLines <> [hsCommentOpen] : res
        Comment{someLines} ->
          [hsCommentClose] <> NonEmpty.init someLines <> [hsCommentOpenSpace <> NonEmpty.last someLines] : res
        CommentSingleLine{someLine} ->
          [hsCommentOpenSpace <> someLine <> hsCommentCloseSpace] : res

-- | Convert 'Tokens' to @Haskell@ code.
--
-- Inverse of 'hsFromTokens'.
--
-- >>> (hsToTokens def $ hsFromTokens def exampleNonTexTokens) == exampleNonTexTokens
-- True
hsToTokens :: Config User -> T.Text -> Tokens
hsToTokens (toConfigInternal -> conf@Config{_disable, _enable}) xs = tokens
 where
  tokens = mkToTokens toTokens xs
  toTokens :: State -> [(Int, T.Text)] -> Tokens -> Tokens
  toTokens State{inText, inHaskellCode, inDisabled, inComment} ((lineNumber, l) : ls) res@(r : rs)
    | inText =
        if l `startsWith` hsCommentClose
          then case r of
            Text{someLines}
              | null (stripEmpties (toList someLines)) ->
                  error $
                    ("No text in a 'Text' token ending at line " <> show lineNumber <> ".\n\n")
                      <> "Please, write some text between '{-\\n' and '\\n-}'."
              | otherwise -> toTokens def ls res
            _ -> errorExpected Text{}
          else -- copy lines
            toTokens (def{inText}) ls $
              case r of
                Text{someLines} -> r{someLines = l <| someLines} : rs
                _ -> errorExpected Text{}
    | inDisabled =
        if isHsComment l && stripHsComment l `startsWith` _enable
          then -- enable
            toTokens def ls res
          else -- copy lines
            toTokens def{inDisabled} ls $
              case r of
                Disabled{manyLines} -> r{manyLines = l : manyLines} : rs
                _ -> errorExpected Disabled{}
    | inComment =
        if -- finish comment
        l `startsWith` hsCommentClose
          then case r of
            Comment{someLines}
              | null (stripEmpties (toList someLines)) ->
                  error $
                    ("No text in a 'Comment' token ending at line " <> show lineNumber <> ".\n\n")
                      <> "Please, write some text between '{- ' and '\\n-}'."
              | otherwise -> toTokens def ls res
            _ -> errorExpected Comment{}
          else -- copy lines
            toTokens def{inComment} ls $
              case r of
                Comment{someLines} -> r{someLines = l <| someLines} : rs
                _ -> errorExpected Comment{}
    -- Doesn't matter if in a snippet

    | -- start of text
      l == hsCommentOpen =
        toTokens def{inText = True} ls (Text{someLines = T.empty :| []} : res)
    | -- Comment on a single line.
      isHsComment l =
        let l' = stripHsComment l
         in if l' `startsWith` _disable
              then toTokens def{inDisabled = True} ls (Disabled{manyLines = []} : res)
              else toTokens def ls $ parseLineToToken conf Hs r l' lineNumber <> rs
    | -- start of a comment on multiple lines
      l `startsWith` hsCommentOpenSpace =
        let l' = dropLen hsCommentOpenSpace l
         in if T.null l'
              then errorEmptyCommentAt lineNumber
              else
                toTokens def{inComment = True} ls $
                  Comment{someLines = (l' :| [])} : res
    -- Again matters if in a snippet
    | inHaskellCode =
        toTokens def{inHaskellCode} ls $
          case r of
            HaskellCode{manyLines} -> HaskellCode{manyLines = l : manyLines} : rs
            _ -> errorExpected HaskellCode{}
    | -- a blank line
      T.null l =
        toTokens def ls res
    | -- start of a snippet
      otherwise =
        toTokens def{inHaskellCode = True} ls (HaskellCode{manyLines = [l]} : res)
   where
    errorExpected = error . errorExpectedToken lineNumber r
  toTokens _ _ res = res
