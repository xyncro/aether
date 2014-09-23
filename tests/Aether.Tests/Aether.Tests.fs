namespace Aether.Tests

open System
open Aether
open Aether.Operators
open NUnit.Framework
open Swensen.Unquote


[<AutoOpen>]
module Data =

    type First =
        { A: string
          B: string option }

    let firstALens : Lens<First, string> =
        ((fun x -> x.A),
         (fun a x -> { x with A = a }))

    let firstBPLens : PLens<First, string> =
        ((fun x -> x.B),
         (fun b x -> { x with B = Some b }))

    let f1 =
        { A = "a"
          B = Some "b" }

    let f2 =
        { A = "a"
          B = None }


module Functions =

    [<Test>]
    let ``getL returns correct values`` () =
        getL firstALens f1 =? "a"

    [<Test>]
    let ``getPL returns correct values`` () =
        getPL firstBPLens f1 =? Some "b"

    [<Test>]
    let ``setL sets value correctly`` () =
        setL firstALens "aa" f1 |> fun x -> x.A =? "aa"

    [<Test>]
    let ``setPL sets value correctly`` () =
        setPL firstBPLens "bb" f1 |> fun x -> x.B =? Some "bb"

    [<Test>]
    let ``modL modifies values correctly`` () =
        modL firstALens (fun x -> x + x) f1 |> fun x -> x.A =? "aa"

    [<Test>]
    let ``modPL modifies values correctly`` () =
        modPL firstBPLens (fun x -> x + x) f1 |> fun x -> x.B =? Some "bb"

module Isomorphisms =

    let lens = 
        firstBPLens 
        <?-> ((fun x -> x.ToCharArray ()), (fun x -> String (x)))

    [<Test>]
    let ``isomorphism over partial return value when Some`` () =
        getPL lens f1 =? Some [| 'b' |]

    [<Test>]
    let ``isomorphism over partial return None when None`` () =
        getPL lens f2 =? None

    [<Test>]
    let ``isomorphism over partial sets value when Some`` () =
        setPL lens [| 'b'; 'b' |] f1 |> fun x -> x.B =? Some "bb"

    [<Test>]
    let ``isomorphism over partial sets value when None`` () =
        setPL lens [| 'b'; 'b' |] f2 |> fun x -> x.B =? Some "bb"