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
    let rec stuck = { phi = []; content = LoadNull; last = UnconditionalJump stuck }

    let rec compileStatement (stt: Statement) =
        let compileBlock (body: Statement list) = LoadNull
        match stt with
        | Expression expr -> compileExpr expr
        | VariableDecl (var, expr) -> StoreVariable (var, compileExpr expr)
        | Block body -> compileBlock body
        | IfThen (cond, body, middle, es) ->
            List.foldBack (fun (cond', body') es' ->
                Select (compileExpr cond', compileBlock body', es'))
                ((cond, body) :: middle)
                (compileBlock es)
        | WhileDo (cond, body) -> LoadNull
        | ForIn (var, obj, body) -> LoadNull

    and compileExpr (expr: Expression) =
        let unary f arg = f (compileExpr arg)
        let binary f arg1 arg2 = f (compileExpr arg1, compileExpr arg2)
        match expr with
        | VariableRef var -> LoadVariable var
        | LiteralNull -> LoadNull
        | LiteralString str -> LoadString str
        | LiteralNumber n -> LoadNumber n
        | LiteralBoolean b -> LoadBoolean b
        | UnaryPlus arg -> unary NumPlus arg
        | UnaryMinus arg -> unary NumPlus arg
        | LogicalNOT arg -> unary NumPlus arg
        | Assignment (arg1, arg2) ->
            match arg1 with
            | VariableRef x ->
                StoreVariable (x, compileExpr arg2)
            | MemberAccess (obj, name) ->
                StoreElement (compileExpr obj, LoadString name, compileExpr arg2)
            | Subscription (obj, index) ->
                StoreElement (compileExpr obj, compileExpr index, compileExpr arg2)
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
        | LogicalAND (arg1, arg2) -> Select (compileExpr arg1, LoadBoolean true, compileExpr arg2)
        | LogicalOR (arg1, arg2) -> Select (compileExpr arg1, compileExpr arg2, LoadBoolean false)
        | MemberAccess (obj, name) -> LoadElement (compileExpr obj, LoadString name)
        | Subscription (obj, index) -> LoadElement (compileExpr obj, compileExpr index)
        | CallExpr (fn, args) -> Invoke (compileExpr fn, List.map compileExpr args)
        | ArrayExpr lst ->
            lst |> List.fold (fun (obj, index) value ->
                StoreElement (obj, LoadNumber index, compileExpr value), index + 1.0) (NewTable, 0.0) |> fst
        | TableExpr lst ->
            lst |> List.fold (fun obj (index, value) ->
                StoreElement (obj, LoadString index, compileExpr value)) NewTable
        | Function (param, name, closure, body) -> LoadNull
