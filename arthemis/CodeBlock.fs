namespace Arthemis

open StateBuilder

type CodeTree =
    | LoadNull
    | LoadString of string
    | LoadNumber of float
    | LoadBoolean of bool
    | LoadVariable of int
    | LoadType of CodeTree
    | StoreVariable of int * CodeTree
    | LoadElement of CodeTree * CodeTree
    | StoreElement of CodeTree * CodeTree * CodeTree
    | NewTable
    | NumPlus of CodeTree
    | NumMinus of CodeTree
    | BoolNot of CodeTree
    | NumMul of CodeTree * CodeTree
    | NumDiv of CodeTree * CodeTree
    | NumMod of CodeTree * CodeTree
    | NumBitOr of CodeTree * CodeTree
    | NumBitAnd of CodeTree * CodeTree
    | NumBitXor of CodeTree * CodeTree
    | NumAdd of CodeTree * CodeTree
    | NumSub of CodeTree * CodeTree
    | CompLt of CodeTree * CodeTree
    | CompLeq of CodeTree * CodeTree
    | CompGt of CodeTree * CodeTree
    | CompGeq of CodeTree * CodeTree
    | CompEq of CodeTree * CodeTree
    | CompNeq of CodeTree * CodeTree
    | Select of CodeTree * CodeTree * CodeTree
    | Invoke of CodeTree * CodeTree list

type CodeBlockEnd =
    | UnconditionalJump of CodeBlock
    | ConditionalJump of int * CodeBlock * CodeBlock
and CodeBlock = {
    phi: (int * int list) list
    content: CodeTree
    last: CodeBlockEnd
}

type CompileState = {
    VarCount: int
    VarDesc: Map<int, string * int>
    Functions: Map<int, CodeTree>
}

module CompileState =
    let ofParserState (state: ParserState) =
        let desc = state.VarDesc |> List.rev |> List.mapi (fun i x -> i + 1, (x, 0)) |> Map.ofList
        { VarCount = fst state.Vars; VarDesc = desc; Functions = Map.empty }

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
    let rec stuck = { phi = []; content = LoadNull; last = UnconditionalJump stuck }

    let rec compileBlock (body: Statement list) =
        LoadNull

    and compileExpr (expr: Expression): State<CompileState, CodeTree> =
        let unary f arg = compileExpr arg |>> f
        let binary f arg1 arg2 = (compileExpr arg1 .>>. compileExpr arg2) |>> f
        match expr with
        | VariableRef var -> State.ofValue (LoadVariable var)
        | LiteralNull -> State.ofValue LoadNull
        | LiteralString str -> State.ofValue (LoadString str)
        | LiteralNumber n -> State.ofValue (LoadNumber n)
        | LiteralBoolean b -> State.ofValue (LoadBoolean b)
        | UnaryPlus arg -> unary NumPlus arg
        | UnaryMinus arg -> unary NumPlus arg
        | LogicalNOT arg -> unary NumPlus arg
        | Assignment (arg1, arg2) ->
            match arg1 with
            | VariableRef x ->
                compileExpr arg2 |>> fun a -> StoreVariable (x, a)
            | MemberAccess (obj, name) ->
                compileExpr obj .>>. compileExpr arg2
                |>> fun (a, b) -> StoreElement (a, LoadString name, b)
            | Subscription (obj, index) ->
                compileExpr obj .>>. compileExpr index .>>. compileExpr arg2
                |>> fun ((a, b), c) -> StoreElement (a, b, c)
            | _ -> failwith "only variable, member, or subscription can be assigned"
        | Multiply (arg1, arg2) -> binary NumMul arg1 arg2
        | Division (arg1, arg2) -> binary NumDiv arg1 arg2
        | Modular (arg1, arg2) -> binary NumMod arg1 arg2
        | BitwiseOR (arg1, arg2) -> binary NumBitOr arg1 arg2
        | BitwiseAND (arg1, arg2) -> binary NumBitAnd arg1 arg2
        | BitwiseXOR (arg1, arg2) -> binary NumBitXor arg1 arg2
        | Addition (arg1, arg2) -> binary NumAdd arg1 arg2
        | Subtraction (arg1, arg2) -> binary NumSub arg1 arg2
        | LessThan (arg1, arg2) -> binary CompLt arg1 arg2
        | LessOrEq (arg1, arg2) -> binary CompLeq arg1 arg2
        | GreaterThan (arg1, arg2) -> binary CompGt arg1 arg2
        | GreaterOrEq (arg1, arg2) -> binary CompGeq arg1 arg2
        | EqualTo (arg1, arg2) -> binary CompEq arg1 arg2
        | NotEqualTo (arg1, arg2) -> binary CompNeq arg1 arg2
        | LogicalAND (arg1, arg2) ->
            compileExpr arg1 .>>. compileExpr arg2
            |>> fun (a, b) -> Select (a, LoadBoolean true, b)
        | LogicalOR (arg1, arg2) ->
            compileExpr arg1 .>>. compileExpr arg2
            |>> fun (a, b) -> Select (a, b, LoadBoolean false)
        | MemberAccess (obj, name) ->
            compileExpr obj |>> fun a -> LoadElement (a, LoadString name)
        | Subscription (obj, index) ->
            compileExpr obj .>>. compileExpr index
            |>> fun (a, b) -> LoadElement (a, b)
        | CallExpr (fn, args) ->
            compileExpr fn .>>. State.mapM compileExpr args
            |>> fun (a, b) -> Invoke (a, b)
        | ArrayExpr lst ->
            let folder p value =
                p .>>. compileExpr value
                |>> fun ((obj, index), x) -> StoreElement (obj, LoadNumber index, x), index + 1.0
            let initial = State.ofValue (NewTable, 0.0)
            lst |> List.fold folder initial |>> fst
        | TableExpr lst ->
            let folder p (index, value) =
                p .>>. compileExpr value
                |>> fun (obj, x) -> StoreElement (obj, LoadString index, x)
            let initial = State.ofValue NewTable
            lst |> List.fold folder initial
        | Function (param, name, closure, body) -> state {
            let! closure' =
                State.mapM (fun v ->
                    CompileState.addTempVar (Some v) |>> (fun v' -> v, v')) closure
            let closureMap = Map.ofList closure'
            return LoadNull
        }
