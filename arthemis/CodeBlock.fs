namespace Arthemis

type CodeVariable = {
    id: int
    gen: int
}

type CodeUnaryOp = BoolNotOp | IntNotOp

type CodeBinaryOp = AddOp

type CodeRepr =
    | AssignCode of CodeVariable * CodeVariable
    | UnaryOpCode of CodeUnaryOp * CodeVariable * CodeVariable
    | BinaryOpCode of CodeBinaryOp * CodeVariable * CodeVariable * CodeVariable

type CodeBlockEnd =
    | UnconditionalJump of CodeBlock
    | ConditionalJump of CodeVariable * CodeBlock
and CodeBlock = {
    phi: CodeVariable list list
    content: CodeRepr list
    last: CodeBlockEnd
}

module CodeBlock =
    let rec stuck = { phi = []; content = []; last = UnconditionalJump stuck }
