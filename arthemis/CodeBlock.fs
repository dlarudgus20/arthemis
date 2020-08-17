namespace Arthemis

type CodeRepr =
    | NopCode
    | LoadNull of int
    | LoadString of int * string
    | LoadNumber of int * float
    | LoadBoolean of int * bool
    | LoadType of int * int
    | Assign of int * int

type CodeBlockEnd =
    | UnconditionalJump of CodeBlock
    | ConditionalJump of int * CodeBlock * CodeBlock
and CodeBlock = {
    phi: (int * int list) list
    content: CodeRepr list
    last: CodeBlockEnd
}

type VariableMap = {
    size: int
    desc: string list
}

module VariableMap =
    let ofParserState (state: ParserState) =
        { size = fst state.Vars; desc = state.VarDesc }

module CodeBlock =
    let rec stuck = { phi = []; content = []; last = UnconditionalJump stuck }

    let rec compileStatement (stt: Statement) =
        match stt with
        | Expression expr -> {
            phi = []
            content = [ compileExpression None expr ]
            last = UnconditionalJump stuck
            }
        | VariableDecl (var, expr) -> {
            phi = []
            content = [ compileExpression (Some var) expr ]
            last = UnconditionalJump stuck
            }
        | Block body -> stuck
        | IfThen (cond, body, middle, es) -> stuck
        | WhileDo (cond, body) -> stuck
        | ForIn (var, obj, body) -> stuck

    and compileExpression (target: int option) (expr: Expression) =
        match expr with
        | VariableRef var ->
            match target with
            | Some t -> Assign (t, var)
            | None -> NopCode
        | LiteralNull ->
            match target with
            | Some t -> LoadNull t
            | None -> NopCode
        | LiteralString str ->
            match target with
            | Some t -> LoadString (t, str)
            | None -> NopCode
        | LiteralNumber n ->
            match target with
            | Some t -> LoadNumber (t, n)
            | None -> NopCode
        | LiteralBoolean b ->
            match target with
            | Some t -> LoadBoolean (t, b)
            | None -> NopCode
        | UnaryPlus arg -> NopCode
        | UnaryMinus arg -> NopCode
        | LogicalNOT arg -> NopCode
        | Assignment (arg1, arg2) -> NopCode
        | Multiply (arg1, arg2) -> NopCode
        | Division (arg1, arg2) -> NopCode
        | Modular (arg1, arg2) -> NopCode
        | BitwiseOR (arg1, arg2) -> NopCode
        | BitwiseAND (arg1, arg2) -> NopCode
        | BitwiseXOR (arg1, arg2) -> NopCode
        | Addition (arg1, arg2) -> NopCode
        | Subtraction (arg1, arg2) -> NopCode
        | LessThan (arg1, arg2) -> NopCode
        | LessOrEq (arg1, arg2) -> NopCode
        | GreaterThan (arg1, arg2) -> NopCode
        | GreaterOrEq (arg1, arg2) -> NopCode
        | EqualTo (arg1, arg2) -> NopCode
        | NotEqualTo (arg1, arg2) -> NopCode
        | LogicalAND (arg1, arg2) -> NopCode
        | LogicalOR (arg1, arg2) -> NopCode
        | CallExpr (fn, args) -> NopCode
        | MemberAccess (obj, name) -> NopCode
        | Subscription (obj, index) -> NopCode
        | ArrayExpr lst -> NopCode
        | TableExpr lst -> NopCode
        | Function (param, name, closure, body) -> NopCode
