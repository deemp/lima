[ Indent
    { n = 3 }
, Disabled
    { manyLines = [ "-- What's the answer?" ] }
, Indent
    { n = 1 }
, Indent
    { n = 2 }
, Text
    { someLines = "- Intermediate results" :| [] }
, HaskellCode
    { manyLines =
        [ "b = a 4"
        , "a = const 3"
        ]
    }
, Dedent
, HaskellCode
    { manyLines = [ "answer = b * 14" ] }
, Comment
    { someLines = "world!" :|
        [ ""
        , "Hello from comments,"
        ]
    }
, CommentSingleLine
    { someLine = "Comment on a single line." }
, Text
    { someLines = "world!" :|
        [ ""
        , "Hello from text,"
        ]
    }
]
