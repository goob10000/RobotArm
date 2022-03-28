module Client.Tests

open Fable.Mocha

open Index
open Shared
open Calc

let client = testList "Client" [
    testCase "Square" <| fun _ ->
        let x = sq 4.0

        Expect.equal 16.0 x "4*4 should = 16"
]

let all =
    testList "All"
        [
#if FABLE_COMPILER // This preprocessor directive makes editor happy
            Shared.Tests.shared
#endif
            client
        ]

[<EntryPoint>]
let main _ = Mocha.runTests all