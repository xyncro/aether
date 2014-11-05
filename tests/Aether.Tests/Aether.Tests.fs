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
          B: string option
          C: int list }

    let firstALens : Lens<First, string> =
        ((fun x -> x.A),
         (fun a x -> { x with A = a }))

    let firstBPLens : PLens<First, string> =
        ((fun x -> x.B),
         (fun b x -> { x with B = Some b }))

    let firstCLens : Lens<First, int list> =
        ((fun x -> x.C),
         (fun c x -> { x with C = c }))

    let f1 =
        { A = "ab"
          B = Some "ba"
          C = List.empty }

    let f2 =
        { A = "ab"
          B = None
          C = [ 1; 2; 3 ] }


module Functions =

    [<Test>]
    let ``getL returns correct values`` () =
        getL firstALens f1 =? "ab"

    [<Test>]
    let ``getPL returns correct values`` () =
        getPL firstBPLens f1 =? Some "ba"

    [<Test>]
    let ``setL sets value correctly`` () =
        setL firstALens "ba" f1 |> fun x -> x.A =? "ba"

    [<Test>]
    let ``setPL sets value correctly`` () =
        setPL firstBPLens "ab" f1 |> fun x -> x.B =? Some "ab"

    [<Test>]
    let ``modL modifies values correctly`` () =
        modL firstALens (fun x -> x + x) f1 |> fun x -> x.A =? "abab"

    [<Test>]
    let ``modPL modifies values correctly`` () =
        modPL firstBPLens (fun x -> x + x) f1 |> fun x -> x.B =? Some "baba"

    [<Test>]
    let ``mapL modifies values correctly`` () =
        mapL firstCLens ((*) 2) f2 |> fun x -> x.C =? [ 2; 4; 6 ]


module Composition =

    let private chars : Iso<string, char[]> =
        (fun x -> x.ToCharArray ()), (fun x -> String (x))

    let private rev : Iso<char[], char[]> =
        (fun x -> Array.rev x), (fun x -> Array.rev x)

    [<Test>]
    let ``isomorphism over partial return value when Some`` () =
        getPL (firstBPLens <?-> chars) f1 =? Some [| 'b'; 'a' |]

    [<Test>]
    let ``isomorphism over partial return None when None`` () =
        getPL (firstBPLens <?-> chars) f2 =? None

    [<Test>]
    let ``isomorphism over partial sets value when Some`` () =
        setPL (firstBPLens <?-> chars) [| 'a'; 'b' |] f1 |> fun x -> x.B =? Some "ab"

    [<Test>]
    let ``isomorphism over partial sets value when None`` () =
        setPL (firstBPLens <?-> chars) [| 'a'; 'b' |] f2 |> fun x -> x.B =? Some "ab"

    [<Test>]
    let ``multiple isomorphisms over partial return value when Some`` () =
        getPL (firstBPLens <?-> chars <?-> rev) f1 =? Some [| 'a'; 'b' |]

    [<Test>]
    let ``multiple isomorphisms over partial return None when None`` () =
        getPL (firstBPLens <?-> chars <?-> rev) f2 =? None

    [<Test>]
    let ``multiple isomorphisms over partial sets value when Some`` () =
        setPL (firstBPLens <?-> chars <?-> rev) [| 'b'; 'a' |] f1 |> fun x -> x.B =? Some "ab"

    [<Test>]
    let ``multiple isomorphisms over partial sets value when None`` () =
        setPL (firstBPLens <?-> chars <?-> rev) [| 'b'; 'a' |] f2 |> fun x -> x.B =? Some "ab"
