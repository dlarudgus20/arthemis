open System
open Arthemis
open FParsec

while true do
    Console.Write "> "
    let line = Console.ReadLine ()
    match ParseTree.parseScript line with
    | Success (result, _, _) -> printfn "%A" result
    | Failure (msg, _, _) -> printfn "%s" msg
