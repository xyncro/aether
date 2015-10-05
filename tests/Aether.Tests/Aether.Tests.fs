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
    let ``Lens.get returns correct values`` () =
        Lens.get firstALens f1 =! "ab"

    [<Test>]
    let ``Lens.getPartial returns correct values`` () =
        Lens.getPartial firstBPLens f1 =! Some "ba"

    [<Test>]
    let ``Lens.set sets value correctly`` () =
        Lens.set firstALens "ba" f1 |> fun x -> x.A =! "ba"

    [<Test>]
    let ``Lens.setPartial sets value correctly`` () =
        Lens.setPartial firstBPLens "ab" f1 |> fun x -> x.B =! Some "ab"

    [<Test>]
    let ``Lens.map modifies values correctly`` () =
        Lens.map firstALens (fun x -> x + x) f1 |> fun x -> x.A =! "abab"

    [<Test>]
    let ``Lens.mapPartial modifies values correctly`` () =
        Lens.mapPartial firstBPLens (fun x -> x + x) f1 |> fun x -> x.B =! Some "baba"


module Composition =

    let private chars : Iso<string, char[]> =
        (fun x -> x.ToCharArray ()), (fun x -> String (x))

    let private rev : Iso<char[], char[]> =
        (fun x -> Array.rev x), (fun x -> Array.rev x)

    [<Test>]
    let ``isomorphism over partial return value when Some`` () =
        Lens.getPartial (firstBPLens <?-> chars) f1 =! Some [| 'b'; 'a' |]

    [<Test>]
    let ``isomorphism over partial return None when None`` () =
        Lens.getPartial (firstBPLens <?-> chars) f2 =! None

    [<Test>]
    let ``isomorphism over partial sets value when Some`` () =
        Lens.setPartial (firstBPLens <?-> chars) [| 'a'; 'b' |] f1 |> fun x -> x.B =! Some "ab"

    [<Test>]
    let ``isomorphism over partial sets value when None`` () =
        Lens.setPartial (firstBPLens <?-> chars) [| 'a'; 'b' |] f2 |> fun x -> x.B =! Some "ab"

    [<Test>]
    let ``multiple isomorphisms over partial return value when Some`` () =
        Lens.getPartial (firstBPLens <?-> chars <?-> rev) f1 =! Some [| 'a'; 'b' |]

    [<Test>]
    let ``multiple isomorphisms over partial return None when None`` () =
        Lens.getPartial (firstBPLens <?-> chars <?-> rev) f2 =! None

    [<Test>]
    let ``multiple isomorphisms over partial sets value when Some`` () =
        Lens.setPartial (firstBPLens <?-> chars <?-> rev) [| 'b'; 'a' |] f1 |> fun x -> x.B =! Some "ab"

    [<Test>]
    let ``multiple isomorphisms over partial sets value when None`` () =
        Lens.setPartial (firstBPLens <?-> chars <?-> rev) [| 'b'; 'a' |] f2 |> fun x -> x.B =! Some "ab"
