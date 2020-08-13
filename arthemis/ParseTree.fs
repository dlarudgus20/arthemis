namespace Arthemis

open FParsec

type Expression =
    | Identifier of string
    | LiteralString of string
    | LiteralNumber of float
    | LiteralBoolean of bool
    | LiteralNull
    | UnaryPlus of Expression
    | UnaryMinus of Expression
    | LogicalNOT of Expression
    | Assignment of Expression * Expression
    | Multiply of Expression * Expression
    | Division of Expression * Expression
    | Modular of Expression * Expression
    | BitwiseOR of Expression * Expression
    | BitwiseAND of Expression * Expression
    | BitwiseXOR of Expression * Expression
    | Addition of Expression * Expression
    | Subtraction of Expression * Expression
    | LessThan of Expression * Expression
    | LessOrEq of Expression * Expression
    | GreaterThan of Expression * Expression
    | GreaterOrEq of Expression * Expression
    | EqualTo of Expression * Expression
    | NotEqualTo of Expression * Expression
    | LogicalAND of Expression * Expression
    | LogicalOR of Expression * Expression
    | CallExpr of Expression * Expression list
    | MemberAccess of Expression * string
    | Subscription of Expression * Expression
    | ArrayExpr of Expression list
    | TableExpr of (string * Expression) list

type Statement =
    | Expression of Expression
    | Variable of string * Expression
    | Block of Statement list
    | IfThen of Expression * Statement list * (Expression * Statement list) list * Statement list option
    | WhileDo of Expression * Statement list
    | ForIn of string * Expression * Statement list
    | Function of string * string list * Statement list
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
    let tokenr1 str = attempt (ws >>. skipString str >>. ws1)

    let lit_string =
        let normal = satisfy (fun c -> c <> '\\' && c <> '"')
        let escaped = skipChar '\\' >>. (anyOf "\"\\bfnrt" |>> function
            | 'b' -> '\b' | 'f' -> '\u000c' | 'n' -> '\n' | 'r' -> '\r' | 't' -> '\t'
            | c -> c)
        between (skipChar '"') (skipChar '"') (manyChars (normal <|> escaped))

    let lit_number = pfloat
    let lit_boolean = (skipString "true" >>% true) <|> (skipString "false" >>% false)
    let lit_null = skipString "null"

    let private binop str f = token str >>. preturn (fun x y -> f (x, y))
    let private binopr1 str f = tokenr1 str >>. preturn (fun x y -> f (x, y))

    let rec expression x = op11 x
    and op11 x =
        let op_assign = binop "=" Assignment
        chainr1 op10 op_assign x

    and op10 x =
        let op_not = (tokenr1 "not" >>. op10) |>> LogicalNOT
        (op_not <|> op9) x

    and op9 x =
        let op_lor = binopr1 "or" LogicalOR
        chainl1 op8 op_lor x

    and op8 x =
        let op_land = binopr1 "and" LogicalAND
        chainl1 op7 op_land x

    and op7 x =
        let op_lt = binop "<" LessThan
        let op_le = binop "<=" LessOrEq
        let op_gt = binop ">" GreaterThan
        let op_ge = binop ">=" GreaterOrEq
        let op_eq = binop "==" EqualTo
        let op_ne = binop "!=" NotEqualTo
        chainl1 op6 (op_lt <|> op_le <|> op_gt <|> op_ge <|> op_eq <|> op_ne) x

    and op6 x =
        let op_add = binop "+" Addition
        let op_sub = binop "-" Subtraction
        chainl1 op5 (op_add <|> op_sub) x

    and op5 x =
        let op_bor = binop "|" BitwiseOR
        let op_band = binop "&" BitwiseAND
        let op_bxor = binop "^" BitwiseXOR
        chainl1 op4 (op_bor <|> op_band <|> op_bxor) x

    and op4 x =
        let op_mul = binop "*" Multiply
        let op_div = binop "/" Division
        let op_mod = binop "%" Modular
        chainl1 op3 (op_mul <|> op_div <|> op_mod) x

    and op3 x =
        let op_fcall =
            let args = sepBy op2 (token ",")
            (op2 .>>. (between (token "(") (token ")") args)) |>> CallExpr
        ((attempt op_fcall) <|> op2) x

    and op2 x =
        let op_plus = (token "+" >>. op2) |>> UnaryPlus
        let op_minus = (token "-"  >>. op2) |>> UnaryMinus
        (op_plus <|> op_minus <|> op1) x

    and op1 x =
        let rec pack f stt = function
        | x :: xs -> pack f (f (stt, x)) xs
        | [] -> stt
        let op_member =
            let names = sepBy1 identifier (token ".")
            (op0 .>>. names) |>> (fun (expr, names) -> pack MemberAccess expr names)
        let op_subscript =
            let indices = sepBy1 expression (token "]" >>. token "[")
            (op0 .>>. between (token "[") (token "]") indices) |>> (fun (expr, indices) -> pack Subscription expr indices)
        ((attempt op_member) <|> (attempt op_subscript) <|> op0) x

    and op0 x =
        let op_parenthesis = between (token "(") (token ")") expression
        let expr_array = between (token "{") (token "}") (sepBy expression (token ","))
        let terminal =
            (lit_null >>% LiteralNull)
            <|> (lit_boolean |>> LiteralBoolean)
            <|> (lit_string |>> LiteralString)
            <|> (lit_number |>> LiteralNumber)
            <|> (identifier |>> Identifier)
            <|> (expr_array |>> ArrayExpr)
        ((attempt op_parenthesis) <|> terminal) x

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
        <|> (expression |>> Expression)) x

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
        let name = token "fun" >>. identifier
        let param = between (token "(") (token ")") (sepBy identifier (token ","))
        (name .>>. param) .>>. (token "begin" >>. blockContent .>> token "end")
        |>> (fun ((a, b), c) -> a, b, c)

    let parseScript str =
        run (statement .>> eof) str
