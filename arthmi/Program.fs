open System
open Arthemis
open FParsec

type ReplState = {
    ParserState: ParserState
    Verbose: bool
}

let rec mainLoop state =
    printf "> "
    let line = Console.ReadLine ()
    if line = ".exit" then
        printfn "bye"
        ()
    elif line = ".reset" then
        printfn "reset parser state"
        mainLoop { state with ParserState = ParserState.empty }
    elif line = ".verbose" then
        printfn "verbose mode %s" (if state.Verbose then "off" else "on")
        mainLoop { state with Verbose = not state.Verbose }
    else
        match runParserOnString Parser.script state.ParserState "" line with
        | Success (cst, ps, _) ->
            printfn "parse result: %A" cst
            if state.Verbose then printfn "Parser State: %A" ps
            mainLoop { state with ParserState = ps }
        | Failure (msg, _, _) ->
            printfn "parse error: %s" msg
            mainLoop state

mainLoop {
    ParserState = ParserState.empty
    Verbose = true
}
