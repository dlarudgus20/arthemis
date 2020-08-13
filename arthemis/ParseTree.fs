namespace Arthemis

open FParsec

type ParseTree =
    | Identifier of string
    | LiteralString of string
    | LiteralNumber of float
    | LiteralBoolean of bool
    | LiteralNull
    | UnaryPlus of ParseTree
    | UnaryMinus of ParseTree
    | LogicalNOT of ParseTree
    | AssignExpr of ParseTree * ParseTree list
    | Multiply of ParseTree * ParseTree list
    | Division of ParseTree * ParseTree list
    | Modular of ParseTree * ParseTree list
    | BitwiseOR of ParseTree * ParseTree list
    | BitwiseAND of ParseTree * ParseTree list
    | BitwiseXOR of ParseTree * ParseTree list
    | Addition of ParseTree * ParseTree list
    | Subtraction of ParseTree * ParseTree list
    | LessThan of ParseTree * ParseTree list
    | LessOrEq of ParseTree * ParseTree list
    | GreaterThan of ParseTree * ParseTree list
    | GreaterOrEq of ParseTree * ParseTree list
    | EqualTo of ParseTree * ParseTree list
    | NotEqualTo of ParseTree * ParseTree list
    | LogicalAND of ParseTree * ParseTree list
    | LogicalOR of ParseTree * ParseTree list
    | CallExpr of ParseTree * ParseTree list
    | MemberAccess of ParseTree * string * string list
    | Subscription of ParseTree * ParseTree list
    | ArrayExpr of ParseTree list
    | Variable of string * ParseTree
    | Block of ParseTree list
    | IfThen of ParseTree * ParseTree list * (ParseTree * ParseTree list) list * ParseTree list option
    | WhileDo of ParseTree * ParseTree list
    | ForIn of string * ParseTree * ParseTree list
    | Function of string list * ParseTree list
    | BreakStatement
    | ContinueStatement

module ParseTree =
    let keywords = Set.ofList [
       "var";
       "fun";
       "if";
       "while";
       "do";
       "end";
       "and";
       "or";
       "break";
       "continue";
       "else";
       "elif";
       "false";
       "true";
       "for";
       "null";
       "not";
       "then";
    ]

    let id_first c = isLetter c || c = '_'
    let id_remain c = isLetter c || isDigit c || c = '_'
    let identifier =
        many1Satisfy2 id_first id_remain >>= (fun str ->
            if Set.contains str keywords then fail "keyword cannot be identifier" else (preturn str))

    let ws = spaces
    let ws1 = spaces1
    let newline = skipSepEndBy1 (anyOf "\r\n") ws
    let token str = attempt (ws >>. skipString str >>. ws)

    let lit_string =
        let normal = satisfy (fun c -> c <> '\\' && c <> '"')
        let escaped = skipChar '\\' >>. (anyOf "\"\\bfnrt" |>> function
            | 'b' -> '\b' | 'f' -> '\u000c' | 'n' -> '\n' | 'r' -> '\r' | 't' -> '\t'
            | c -> c)
        between (skipChar '"') (skipChar '"') (manyChars (normal <|> escaped))

    let lit_number = pfloat
    let lit_boolean = (skipString "true" >>% true) <|> (skipString "false" >>% false)
    let lit_null = skipString "null"

    let private binaryParser p sym f = (p .>> token sym) .>>. (sepBy1 p (token sym)) |>> f |> attempt
    let private binaryParser1 p sym f = (p .>> token sym .>> ws1) .>>. (sepBy1 p (token sym .>> ws1)) |>> f |> attempt

    let rec expression x = op11 x
    and op11 x = x |> (op_assign <|> op10)
    and op_assign x = x |> binaryParser op10 "<-" AssignExpr

    and op10 x = x |> (op_not <|> op9)
    and op_not x = let p = skipString "not" >>. ws1 >>. op10 in (p |>> LogicalNOT) x

    and op9 x = x |> (op_lor <|> op8)
    and op_lor x = x |> binaryParser1 op8 "or" LogicalOR

    and op8 x = x |> (op_land <|> op7)
    and op_land x = x |> binaryParser1 op7 "and" LogicalAND

    and op7 x = x |> (op_lt <|> op_le <|> op_gt <|> op_ge <|> op_eq <|> op_ne <|> op6)
    and op_lt x = x |> binaryParser op6 "<" LessThan
    and op_le x = x |> binaryParser op6 "<=" LessOrEq
    and op_gt x = x |> binaryParser op6 ">" GreaterThan
    and op_ge x = x |> binaryParser op6 ">=" GreaterOrEq
    and op_eq x = x |> binaryParser op6 "=" EqualTo
    and op_ne x = x |> binaryParser op6 "<>" NotEqualTo

    and op6 x = x |> (op_add <|> op_sub <|> op5)
    and op_add x = x |> binaryParser op5 "+" Addition
    and op_sub x = x |> binaryParser op5 "-" Subtraction

    and op5 x = x |> (op_bor <|> op_band <|> op_bxor <|> op4)
    and op_bor x = x |> binaryParser op4 "|" BitwiseOR
    and op_band x = x |> binaryParser op4 "&" BitwiseAND
    and op_bxor x = x |> binaryParser op4 "^" BitwiseXOR

    and op4 x = x |> (op_mul <|> op_div <|> op_mod <|> op3)
    and op_mul x = x |> binaryParser op3 "*" Multiply
    and op_div x = x |> binaryParser op3 "/" Division
    and op_mod x = x |> binaryParser op3 "%" Modular

    and op3 x = x |> ((attempt op_fcall) <|> op2)
    and op_fcall x =
        let args = sepBy op2 (token ",")
        let p = op2 .>>. (between (token "(") (token ")") args)
        (p |>> CallExpr) x

    and op2 x = x |> (op_plus <|> op_minus <|> op1)
    and op_plus x = let p = skipString "+" >>. ws >>. op1 in (p |>> UnaryPlus) x
    and op_minus x = let p = skipString "-" >>. ws >>. op1 in (p |>> UnaryMinus) x

    and op1 x = x |> ((attempt op_member) <|> (attempt op_subscript) <|> op0)
    and op_member x =
        let names = binaryParser identifier "." id
        ((op0 .>>. ((token ".") >>. names)) |>> (fun (obj, (id, lst)) -> MemberAccess (obj, id, lst))) x
    and op_subscript x =
        let indices = sepBy1 expression (token "]" >>. token "[")
        ((op0 .>>. between (token "[") (token "]") indices) |>> Subscription) x

    and op0 x = ((attempt op_parenthesis) <|> terminal) x
    and op_parenthesis = between (token "(") (token ")") expression
    and terminal x =
        ((lit_null >>% LiteralNull)
        <|> (lit_boolean |>> LiteralBoolean)
        <|> (lit_string |>> LiteralString)
        <|> (lit_number |>> LiteralNumber)
        <|> (identifier |>> Identifier)
        <|> (arrayExpr |>> ArrayExpr)) x
    and arrayExpr x =
        x |> between (token "{") (token "}") (sepBy expression (token ","))

    let variable =
        (skipString "var" >>. ws1 >>. identifier .>>. (token "=" >>. expression))

    let stt_break = token "break"
    let stt_continue = token "continue"

    let rec statement x =
        ((variable |>> Variable)
        <|> (stt_break >>% BreakStatement)
        <|> (stt_continue >>% ContinueStatement)
        <|> (block |>> Block)
        <|> (ifThen |>> IfThen)
        <|> (whileDo |>> WhileDo)
        <|> (forIn |>> ForIn)
        <|> (funDef |>> Function)
        <|> expression) x

    and private blockContent = (sepEndBy (attempt statement) (newline <|> token ";"))
    and block = between (skipString "begin" .>> ws1) (token "end") blockContent

    and ifThen =
        let first = (token "if" >>. expression) .>>. (token "then" >>. blockContent)
        let middle = (token "elif" >>. expression) .>>. (token "then" >>. blockContent)
        let last = token "else" >>. blockContent
        let p = (first .>>. many middle) .>>. opt last .>> token "end"
        p |>> (fun (((a, b), xs), y) -> (a, b, xs, y))

    and whileDo =
        (token "while" >>. expression) .>>. (token "do" >>. blockContent .>> token "end")

    and forIn =
        let first = (token "for" >>. identifier) .>>. (token "in" >>. expression)
        let p = first .>>. (token "do" >>. blockContent .>> token "end")
        p |>> (fun ((index, obj), block) -> (index, obj, block))

    and funDef =
        let param = sepBy identifier (token ",")
        let first = token "fun" >>. between (token "(") (token ")") param
        first .>>. (token "begin" >>. blockContent .>> token "end")

    let parseScript str =
        run (statement .>> eof) str
