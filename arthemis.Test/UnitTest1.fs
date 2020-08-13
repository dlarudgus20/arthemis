namespace Arthemis.Test

open NUnit.Framework
open Arthemis

[<TestFixture>]
type SayTest() =
    [<Test>]
    member this.HelloTest () =
        Assert.AreEqual (Say.hello "ikh", "Hello ikh")
