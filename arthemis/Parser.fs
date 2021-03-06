﻿namespace Arthemis

open FParsec

type Expression =
    | VariableRef of int
    | LiteralNull
    | LiteralString of string
    | LiteralNumber of float
    | LiteralBoolean of bool
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
    | Function of param: int list * name: int option * closure: int list * body: Statement list
and Statement =
    | Expression of Expression
    | VariableDecl of int * Expression
    | Block of Statement list
    | IfThen of Expression * Statement list * (Expression * Statement list) list * Statement list
    | WhileDo of Expression * LoopStatement list
    | ForIn of int * Expression * LoopStatement list
and LoopStatement =
    | Ordinary of Statement
    | BreakStatement
    | ContinueStatement

type ParserState = {
    Vars: (* count *) int * (* scopces *) Map<string, int> list
    Closures: (int * Set<int>) list
    VarDesc: string list
    ScopeDesc: (string * int) list
    TagCount: int
}

module ParserState =
    let empty = {
        Vars = 0, [ Map.empty ]
        Closures = []
        VarDesc = []
        ScopeDesc = []
        TagCount = 0
    }

    let createTag desc =
        getUserState >>= (fun s ->
            match s.ScopeDesc with
            | (d, t) :: ds -> setUserState { s with ScopeDesc = (d, t + 1) :: ds } >>% (t + 1)
            | [] -> let t = s.TagCount + 1 in setUserState { s with TagCount = t } >>% t)
        |>> sprintf "#%s_%d" desc

    let pushScope desc = updateUserState (fun s ->
        let i, lst = s.Vars
        { s with Vars = i, (Map.ofList []) :: lst; ScopeDesc = (desc, 0) :: s.ScopeDesc })

    let popScope = updateUserState (fun s ->
        match s.Vars, s.ScopeDesc with
        | (i, _ :: ms), (_ :: ds) -> { s with Vars = i, ms; ScopeDesc = ds }
        | _ -> failwith "invalid popScope")

    let pushClosure desc = pushScope desc >>. updateUserState (fun s ->
        let _, scopes = s.Vars
        { s with Closures = (List.length scopes, Set.empty) :: s.Closures })

    let popClosure = getUserState >>= (fun s ->
        match s.Closures with
        | (scope, set) :: sx -> setUserState { s with Closures = sx } >>% List.ofSeq set
        | _ -> failwith "invalid popClosure") .>> popScope

    let addVariable var =
        let getDesc scope =
            var :: (List.map fst scope) |> List.rev |> String.concat ":"
        getUserState >>= (fun s ->
        match s.Vars with
        | i, m :: ms ->
            if Map.containsKey var m then
                failFatally (sprintf "variable '%s' already exists in this scope" var)
            else
                let vm = i + 1, Map.add var (i + 1) m :: ms
                let desc = getDesc s.ScopeDesc :: s.VarDesc
                setUserState { s with Vars = vm; VarDesc = desc } >>% (i + 1)
        | i, [] ->
            let vm = i + 1, [ Map.ofList [var, i + 1] ]
            setUserState { s with Vars = vm } >>% (i + 1))

    let referenceVariable var =
        let rec tryFind scope key = function
        | i, m :: ms ->
            match Map.tryFind key m with
            | Some x -> Some (scope, x)
            | None -> tryFind (scope - 1) key (i, ms)
        | _, [] -> None
        getUserState >>= (fun s ->
            let topmost = List.length (snd s.Vars)
            match tryFind topmost var s.Vars with
            | Some (varScope, varIndex) ->
                match s.Closures with
                | (scope, scopeSet) :: sx when varScope < scope ->
                    let e = scope, Set.add varIndex scopeSet
                    setUserState { s with Closures = e :: sx } >>% varIndex
                | _ -> preturn varIndex
            | None -> fail (sprintf "variable '%s' is not found" var))

module Parser =
    let keywords = Set.ofList [
       "var";
       "fun";
       "if";
       "while";
       "do";
       "end";
       "break";
       "continue";
       "else";
       "elif";
       "false";
       "true";
       "for";
       "null";
       "then";
    ]

    let ws = spaces
    let ws1 = spaces1 <|> eof
    let token str = skipString str >>. ws
    let token1 str = skipString str >>. ws1

    let id_first c = isLetter c || c = '_'
    let id_remain c = isLetter c || isDigit c || c = '_'
    let identifier =
        many1Satisfy2L id_first id_remain "identifier" >>=? (fun str ->
            if Set.contains str keywords then fail "keyword cannot be identifier" else (preturn str))
            .>> ws

    let variableRef = identifier >>= ParserState.referenceVariable

    let lit_null = token "null"

    let lit_string =
        let normal = satisfy (fun c -> c <> '\\' && c <> '"')
        let escaped = skipChar '\\' >>. (anyOf "\"\\bfnrt" |>> function
            | 'b' -> '\b' | 'f' -> '\u000c' | 'n' -> '\n' | 'r' -> '\r' | 't' -> '\t'
            | c -> c)
        between (skipChar '"') (skipChar '"') (manyChars (normal <|> escaped))

    let lit_number = pfloat .>> ws
    let lit_boolean = (token "true" >>% true) <|> (token "false" >>% false)

    let private binop str f = ws >>. token str >>. preturn (fun x y -> f (x, y))

    let rec expression x = op11 x
    and op11 x =
        let op_assign = binop "=" Assignment
        (chainr1 op10 op_assign .>> ws) x

    and op10 x =
        let op_not = (token "!" >>. op10) |>> LogicalNOT
        (op_not <|> op9) x

    and op9 x =
        let op_lor = binop "||" LogicalOR
        (chainl1 op8 op_lor .>> ws) x

    and op8 x =
        let op_land = binop "&&" LogicalAND
        (chainl1 op7 op_land .>> ws) x

    and op7 x =
        let op_lt = binop "<" LessThan
        let op_le = binop "<=" LessOrEq
        let op_gt = binop ">" GreaterThan
        let op_ge = binop ">=" GreaterOrEq
        let op_eq = binop "==" EqualTo
        let op_ne = binop "!=" NotEqualTo
        (chainl1 op6 (op_lt <|> op_le <|> op_gt <|> op_ge <|> op_eq <|> op_ne) .>> ws) x

    and op6 x =
        let op_add = binop "+" Addition
        let op_sub = binop "-" Subtraction
        (chainl1 op5 (op_add <|> op_sub) .>> ws) x

    and op5 x =
        let op_bor = binop "|" BitwiseOR
        let op_band = binop "&" BitwiseAND
        let op_bxor = binop "^" BitwiseXOR
        (chainl1 op4 (op_bor <|> op_band <|> op_bxor) .>> ws) x

    and op4 x =
        let op_mul = binop "*" Multiply
        let op_div = binop "/" Division
        let op_mod = binop "%" Modular
        (chainl1 op3 (op_mul <|> op_div <|> op_mod) .>> ws) x

    and op3 x =
        let op_fcall =
            let args = sepBy op2 (token ",") .>> ws
            (op2 .>>. (between (token "(") (token ")") args)) |>> CallExpr
        (attempt op_fcall <|> op2) x

    and op2 x =
        let op_plus = (token "+" >>. op2) |>> UnaryPlus
        let op_minus = (token "-"  >>. op2) |>> UnaryMinus
        (op_plus <|> op_minus <|> op1) x

    and op1 x =
        let rec pack f stt = function
        | x :: xs -> pack f (f (stt, x)) xs
        | [] -> stt
        let op_member =
            let names = between (token ".") ws (sepBy1 identifier (token "."))
            (op0 .>>. names)
            |>> (fun (expr, names) -> pack MemberAccess expr names)
        let op_subscript =
            let indices = sepBy1 expression (token "]" >>. token "[") .>> ws
            (op0 .>>. between (token "[") (token "]") indices)
            |>> (fun (expr, indices) -> pack Subscription expr indices)
        (attempt op_member <|> attempt op_subscript <|> op0) x

    and op0 x =
        let op_parenthesis = between (token "(") (token ")") expression
        (op_parenthesis <|> terminal) x

    and terminal x =
        let expr_array =
            between (token "{") (token "}") (sepBy expression (token ","))
        let functionDef =
            let param = identifier >>= ParserState.addVariable
            let name = opt identifier >>= function
                | Some x -> ParserState.addVariable x .>> ParserState.pushClosure x |>> Some
                | None -> ParserState.createTag "fun" >>= ParserState.pushClosure >>% None
            let ps = between (token "(") (token ")") (sepBy param (token ","))
            token "fun" >>. (name .>>. ps .>>. block .>>. ParserState.popClosure)
            |>> (fun (((n, par), body), closure) -> par, n, closure, body)
        ((functionDef |>> Function)
        <|> (lit_null >>% LiteralNull)
        <|> (lit_boolean |>> LiteralBoolean)
        <|> (lit_string |>> LiteralString)
        <|> (lit_number |>> LiteralNumber)
        <|> (variableRef |>> VariableRef)
        <|> (expr_array |>> ArrayExpr)) x

    and statement x =
        ((variableDecl |>> VariableDecl)
        <|> (block |>> Block)
        <|> (ifThen |>> IfThen)
        <|> (whileDo |>> WhileDo)
        <|> (forIn |>> ForIn)
        <|> (expression |>> Expression)) x

    and variableDecl =
        let decl = identifier >>= ParserState.addVariable
        (skipString "var" >>. ws1 >>. decl) .>>. (token "=" >>. expression)

    and private blockContent p = sepEndBy (p .>> ws) ((skipAnyOf "\n;") .>> ws)
    and block = between (token1 "begin") (token "end") (blockContent statement)

    and ifThen x =
        let scope desc = ParserState.createTag desc >>= ParserState.pushScope
        let body desc = scope desc >>. blockContent statement .>> ParserState.popScope
        let first = (token "if" >>. expression) .>>. (token "then" >>. body "if")
        let middle = (token "elif" >>. expression) .>>. (token "then" >>. body "elif")
        let last = token "else" >>. body "else"
        let p = (first .>>. many middle) .>>. (last <|>% []) .>> token "end"
        (p |>> (fun (((a, b), xs), y) -> a, b, xs, y)) x

    and private loop_stt =
        (token "break" >>% BreakStatement)
        <|> (token "continue" >>% ContinueStatement)
        <|> (statement |>> Ordinary)

    and whileDo =
        let scope = ParserState.createTag "while" >>= ParserState.pushScope
        let body = scope >>. blockContent loop_stt .>> ParserState.popScope
        (token "while" >>. expression) .>>. (token "do" >>. body .>> token "end")

    and forIn =
        let scope = ParserState.createTag "for" >>= ParserState.pushScope
        let first = (token "for" >>. scope >>. identifier >>= ParserState.addVariable) .>>. (token "in" >>. expression)
        let body = between (token "do") (token "end") (blockContent loop_stt)
        (first .>>. body) .>> ParserState.popScope
        |>> (fun ((index, obj), block) -> (index, obj, block))

    let script = (blockContent statement .>> eof)
