[ Indent
    { n = 3 }
, Disabled
    { manyLines = [ "-- What's the answer?" ] }
, Indent
    { n = 1 }
, Indent
    { n = 0 }
, Text
    { someLines = "\\begin{mycode}" :|
        [ ""
        , "Intermediate results"
        ]
    }
, HaskellCode
    { manyLines =
        [ "b = a 4"
        , "a = const 3"
        ]
    }
, Text
    { someLines = "\\end{mycode}" :| [] }
, Dedent
, Text
    { someLines = "\\begin{mycode}" :| [] }
, HaskellCode
    { manyLines = [ "answer = b * 14" ] }
, Text
    { someLines = "\\end{mycode}" :| [] }
, Comment
    { someLines = "world!" :|
        [ ""
        , "Hello from comments,"
        ]
    }
, CommentSingleLine
    { someLine = "Comment on a single line." }
]
