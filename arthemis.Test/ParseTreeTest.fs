namespace Arthemis.Test

open NUnit.Framework
open FParsec

open Arthemis
open Arthemis.ParseTree

[<TestFixture>]
type ParseTreeTest() =
    let passTest parser (tests: (string * bool) list) =
        tests |> List.iter (fun (str, exp) ->
            match run (parser .>> eof) str with
            | Success _ -> Assert.True (exp, str)
            | Failure (msg, _, _) -> Assert.False (exp, sprintf "'%s': %s" str msg))

    let exprTest parser (tests: (string * Expression option) list) =
        tests |> List.iter (fun (str, exp) ->
            match run (parser .>> eof) str with
            | Success (result, _, _) ->
                match exp with
                | Some expected -> Assert.AreEqual(expected, result, str)
                | None -> Assert.Fail(sprintf "'%s': failure expected" str)
            | Failure (msg, _, _)  -> Assert.True (exp.IsNone, sprintf "'%s': %s" str msg))

    [<Test>]
    member this.IdentifierTest () =
        passTest identifier [
            "anhdbhjsSDGdfl", true;
            "a;akgbjd;abha", false;
            "127984y2ufh", false;
            "_bnadf_", true;
            "_18034eur2oifjcs", true;
            "@VFJSKADVJ4f'", false;
        ]

    [<Test>]
    member this.LiteralStringTest () =
        passTest lit_string [
            "\"\"", true;
            "\"h hqeuafhnwj kngawe;lkmfj 1j4r82tf0892 ujmqa3\"", true;
            "\" \\\" \"", true;
            "\" egnqa;fj wa\\n \\\" bajn;fw\"", true;
        ]

    [<Test>]
    member this.LiteralBooleanTest () =
        passTest lit_boolean [
            "true", true;
            "false", true;
            "hnbag", false;
        ]

    [<Test>]
    member this.ParenthesisTest () =
        exprTest expression [
            "a", Some (Identifier "a")
        ]
