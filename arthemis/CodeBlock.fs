namespace Arthemis

open StateBuilder

type CodeRepr =
    | MultipleCode of CodeRepr list
    | NopCode
    | LoadNull of int
    | LoadString of int * string
    | LoadNumber of int * float
    | LoadBoolean of int * bool
    | LoadType of int * int
    | Assign of int * int
    | NumPlus of int * int
    | NumMinus of int * int
    | BoolNot of int * int

type CodeBlockEnd =
    | UnconditionalJump of CodeBlock
    | ConditionalJump of int * CodeBlock * CodeBlock
and CodeBlock = {
    phi: (int * int list) list
    content: CodeRepr
    last: CodeBlockEnd
}

type CompileState = {
    VarCount: int
    VarDesc: Map<int, string * int>
}

module CompileState =
    let ofParserState (state: ParserState) =
        let desc = state.VarDesc |> List.rev |> List.mapi (fun i x -> i + 1, (x, 0)) |> Map.ofList
        { VarCount = fst state.Vars; VarDesc = desc }

    let addTempVar (origin: int option) = State (fun s ->
        let next = s.VarCount + 1
        match origin with
        | Some x ->
            let desc, tc = Map.find x s.VarDesc
            let desc' = sprintf "%s%%%d" desc tc
            let vd = s.VarDesc |> Map.add x (desc, tc + 1) |> Map.add next (desc', 0)
            next, { s with VarCount = next; VarDesc = vd }
        | None ->
            let desc = sprintf "#temp_%d" next
            let vd = Map.add next (desc, 0) s.VarDesc
            next, { s with VarCount = next; VarDesc = vd })

module CodeBlock =
    let rec stuck = { phi = []; content = NopCode; last = UnconditionalJump stuck }

    let rec compileStatement (stt: Statement) = state {
        match stt with
        | Expression expr ->
            let! content = compileExpression None expr
            return {
                phi = []
                content = content
                last = UnconditionalJump stuck
            }
        | VariableDecl (var, expr) ->
            let! content = compileExpression (Some var) expr
            return {
                phi = []
                content = content
                last = UnconditionalJump stuck
            }
        | Block body -> return stuck
        | IfThen (cond, body, middle, es) -> return stuck
        | WhileDo (cond, body) -> return stuck
        | ForIn (var, obj, body) -> return stuck
        }

    and compileExpression (target: int option) (expr: Expression) = state {
        let unary f arg = state {
            let! temp = CompileState.addTempVar target
            let! code = compileExpression (Some temp) arg
            let v = match target with Some t -> t | None -> temp
            return MultipleCode [ code; f (v, temp) ]
        }
        let binary f arg1 arg2 = state {
            let! t1 = CompileState.addTempVar target
            let! t2 = CompileState.addTempVar target
            let! c1 = compileExpression (Some t1) arg1
            let! c2 = compileExpression (Some t2) arg2
            let v = match target with Some t -> t | None -> t1
            return MultipleCode [ c1; c2; f (v, t1, t2) ]
        }
        match expr with
        | VariableRef var ->
            match target with
            | Some t -> return Assign (t, var)
            | None -> return NopCode
        | LiteralNull ->
            match target with
            | Some t -> return LoadNull t
            | None -> return NopCode
        | LiteralString str ->
            match target with
            | Some t -> return LoadString (t, str)
            | None -> return NopCode
        | LiteralNumber n ->
            match target with
            | Some t -> return LoadNumber (t, n)
            | None -> return NopCode
        | LiteralBoolean b ->
            match target with
            | Some t -> return LoadBoolean (t, b)
            | None -> return NopCode
        | UnaryPlus arg -> return! unary NumPlus arg
        | UnaryMinus arg -> return! unary NumMinus arg
        | LogicalNOT arg -> return! unary BoolNot arg
        | Assignment (arg1, arg2) -> return NopCode
        | Multiply (arg1, arg2) -> return NopCode
        | Division (arg1, arg2) -> return NopCode
        | Modular (arg1, arg2) -> return NopCode
        | BitwiseOR (arg1, arg2) -> return NopCode
        | BitwiseAND (arg1, arg2) -> return NopCode
        | BitwiseXOR (arg1, arg2) -> return NopCode
        | Addition (arg1, arg2) -> return NopCode
        | Subtraction (arg1, arg2) -> return NopCode
        | LessThan (arg1, arg2) -> return NopCode
        | LessOrEq (arg1, arg2) -> return NopCode
        | GreaterThan (arg1, arg2) -> return NopCode
        | GreaterOrEq (arg1, arg2) -> return NopCode
        | EqualTo (arg1, arg2) -> return NopCode
        | NotEqualTo (arg1, arg2) -> return NopCode
        | LogicalAND (arg1, arg2) -> return NopCode
        | LogicalOR (arg1, arg2) -> return NopCode
        | CallExpr (fn, args) -> return NopCode
        | MemberAccess (obj, name) -> return NopCode
        | Subscription (obj, index) -> return NopCode
        | ArrayExpr lst -> return NopCode
        | TableExpr lst -> return NopCode
        | Function (param, name, closure, body) -> return NopCode
        }
