module Aether.Tests

open System
open Aether
open Aether.Operators
open Fuchu
open FsCheck
open Swensen.Unquote

[<AutoOpen>]
module Data =
  let choice1Of2_ : PLens<Choice<_,_>, _> =
    ((fun x -> match x with | Choice1Of2 v -> Some v | _ -> None),
      (fun v x -> match x with | Choice1Of2 _ -> Choice1Of2 v | _ -> x))

  let choice2Of2_ : PLens<Choice<_,_>, _> =
    ((fun x -> match x with | Choice2Of2 v -> Some v | _ -> None),
      (fun v x -> match x with | Choice2Of2 _ -> Choice2Of2 v | _ -> x))

module Lens =
  let inline getSetIdentityWith lensName lensGet lensSet =
    testProperty (sprintf "%s lens demonstrates Get-Set identity" lensName) <| fun outer ->
      test <@ lensSet (lensGet outer) outer = outer @>

  let inline getSetIdentity lensName lens =
    getSetIdentityWith lensName (Lens.get lens) (Lens.set lens)

  let inline setGetSymmetryWith lensName lensGet lensSet =
    testProperty (sprintf "%s lens demonstrates Set-Get symmetry" lensName) <| fun outer inner ->
      test <@ lensGet (lensSet inner outer) = inner @>

  let inline setGetSymmetry lensName lens =
    setGetSymmetryWith lensName (Lens.get lens) (Lens.set lens)

  let inline setSetOrderDependenceWith lensName lensSet =
    testProperty (sprintf "%s lens demonstrates Set-Set order dependence" lensName) <| fun outer inner dummy ->
      lazy test <@ lensSet inner (lensSet dummy outer) = lensSet inner outer @>
      |> Prop.trivial (inner = dummy)

  let inline setSetOrderDependence lensName lens =
    setSetOrderDependenceWith lensName (Lens.set lens)

  let inline getSetMapCorrespondenceWith lensName lensGet lensSet lensMap =
    testProperty (sprintf "%s lens demonstrates Get-Set to Map correspondence" lensName) <| fun f outer ->
      test <@ lensSet (lensGet outer |> f) outer = lensMap f outer @>
  
  let inline getSetMapCorrespondence lensName lens =
    getSetMapCorrespondenceWith lensName (Lens.get lens) (Lens.set lens) (Lens.map lens)

  let inline followsLensLawsWithName lensName lens =
    testList (sprintf "%s follows the Lens Laws" lensName) [
      getSetIdentity lensName lens
      setGetSymmetry lensName lens
      setSetOrderDependence lensName lens
      getSetMapCorrespondence lensName lens
    ]

  let followsLensLaws (lensQ : Quotations.Expr<Lens<_,_>>) =
    let lensName = lensQ.Decompile()
    let lens = lensQ.Eval()
    followsLensLawsWithName lensName lens

module Prism =
  let classifyTarget prismGet outer =
    let innerP = prismGet outer 
    Prop.classify (Option.isSome innerP) "has inner"
    >> Prop.classify (Option.isNone innerP) "no inner"

  let inline getSetIdentityWith prismName prismGet prismSet =
    testProperty (sprintf "%s prism demonstrates Get-Set identity" prismName) <| fun outer dummy ->
      lazy test <@ prismSet (defaultArg (prismGet outer) dummy) outer = outer @>
      |> classifyTarget prismGet outer

  let inline getSetIdentity prismName prism =
    getSetIdentityWith prismName (Lens.getPartial prism) (Lens.setPartial prism)

  let inline setGetSymmetryWith prismName prismGet prismSet =
    testProperty (sprintf "%s prism demonstrates Set-Get symmetry" prismName) <| fun outer inner ->
      lazy
        match prismGet outer with
        | Some _ -> "inner should be changed" @| (test <@ prismSet inner outer |> prismGet = Some inner @>)
        | None -> "outer should remain unchanged" @| (test <@ prismSet inner outer = outer @>)
      |> classifyTarget prismGet outer

  let inline setGetSymmetry prismName prism =
    setGetSymmetryWith prismName (Lens.getPartial prism) (Lens.setPartial prism)

  let inline setSetOrderDependenceWith prismName prismGet prismSet =
    testProperty (sprintf "%s prism demonstrates Set-Set order dependence" prismName) <| fun outer inner dummy ->
      lazy test <@ prismSet inner outer = (prismSet inner (prismSet dummy outer)) @>
      |> classifyTarget prismGet outer
      |> Prop.trivial (inner = dummy)
    
  let inline setSetOrderDependence prismName prism =
    setSetOrderDependenceWith prismName (Lens.getPartial prism) (Lens.setPartial prism)

  let inline getSetMapCorrespondenceWith prismName prismGet prismSet prismMap =
    testProperty (sprintf "%s prism demonstrates Get-Set to Map correspondence" prismName) <| fun f outer ->
      lazy test <@ prismMap f outer = (prismGet outer |> function | Some i -> prismSet (f i) outer | None -> outer) @>
      |> classifyTarget prismGet outer

  let inline getSetMapCorrespondence prismName prism =
    getSetMapCorrespondenceWith prismName (Lens.getPartial prism) (Lens.setPartial prism) (Lens.mapPartial prism)

  let inline followsPrismLawsWithName prismName prism =
    testList (sprintf "%s prism follows the Prism Laws" prismName) [
      getSetIdentity prismName prism
      setGetSymmetry prismName prism
      setSetOrderDependence prismName prism
      getSetMapCorrespondence prismName prism
    ]

  let followsPrismLaws (prismQ : Quotations.Expr<PLens<_,_>>) =
    let prismName = prismQ.Decompile()
    let prism = prismQ.Eval()
    followsPrismLawsWithName prismName prism

module Iso =
  let roundtripEquality isoName iso =
    testProperty (sprintf "%s isomorphism demonstrates roundtrip equality" isoName) <| fun v ->
      test <@ fst iso v |> snd iso = v @>

  let inline followsIsoLawsWithName isoName iso =
    let isoAsLens = id_ <--> iso
    let isoAsLensName = (sprintf "%s isomorphism as a" isoName)
    testList (sprintf "%s isomorphism follows the Isomorphism Laws" isoName) [
      Lens.getSetIdentity isoAsLensName isoAsLens
      Lens.setSetOrderDependence isoAsLensName isoAsLens
      Lens.getSetMapCorrespondence isoAsLensName isoAsLens
      roundtripEquality isoName iso
    ]

  let followsLensLaws (isoQ : Quotations.Expr<Iso<_,_>>) =
    let isoName = isoQ.Decompile()
    let iso = isoQ.Eval()
    followsIsoLawsWithName isoName iso

[<Tests>]
let lensLawTests =
  testList "Provided Lenses" [
    Lens.followsLensLaws <@ id_ @>
    Lens.followsLensLaws <@ fst_ @>
    Lens.followsLensLaws <@ snd_ @>
    Prism.followsPrismLaws <@ choice1Of2_ @>
    Prism.followsPrismLaws <@ choice2Of2_ @>
    Prism.followsPrismLaws <@ head_ @>
    Prism.followsPrismLaws <@ tail_ @>
    Prism.followsPrismLaws <@ pos_ 0 @>
    Prism.followsPrismLaws <@ pos_ 1 @>
    Prism.followsPrismLaws <@ pos_ 2 @>
    Prism.followsPrismLaws <@ key_ "" @>
  ]

type MapExample =
  { MyMap : Map<string,string> }
  with
    static member myMap_ =
      (fun x -> x.MyMap),
      (fun v x -> { x with MyMap = v })

[<Tests>]
let exampleTests =
  testList "Example Tests" [
    testCase "Upserting into a Map" <| fun _ ->
      let example = { MyMap = Map.ofList ["TestKey","TestValue"]}
      let newValue = Lens.map MapExample.myMap_ (Map.add "TestKey2" "OtherValue") example
      test <@ newValue.MyMap.["TestKey"] = "TestValue" @>
      test <@ newValue.MyMap.["TestKey2"] = "OtherValue" @>
    testCase "Trying to update a value not contained in a Map" <| fun _ ->
      let example = { MyMap = Map.ofList ["TestKey","TestValue"]}
      let newValue = Lens.setPartial (MapExample.myMap_ >-?> key_ "TestKey2") "OtherValue" example
      test <@ newValue.MyMap.TryFind "TestKey2" = None @>
    testCase "Trying to update a value in a Map" <| fun _ ->
      let example = { MyMap = Map.ofList ["TestKey","TestValue"]}
      let newValue = Lens.setPartial (MapExample.myMap_ >-?> key_ "TestKey") "OtherValue" example
      test <@ newValue.MyMap.["TestKey"] = "OtherValue" @>
    testCase "Uses default for a value missing from a Map" <| fun _ ->
      test <@ Lens.getPartialOrElse (key_ "TestKey") "Default" Map.empty = "Default" @>
    testCase "Gets value from a Map" <| fun _ ->
      test <@ Lens.getPartialOrElse (key_ "TestKey") "Default" (Map.ofList ["TestKey","Hit"]) = "Hit" @>
    testCase "Prepending to a List" <| fun _ ->
      test <@ Lens.map id_ (fun l -> "Head" :: l) ["Tail"] = ["Head"; "Tail"] @>
    testCase "Setting the head on an empty list" <| fun _ ->
      test <@ Lens.setPartial head_ "Bad" [] = [] @>
    testCase "Setting the head on a list with one element replaces that element" <| fun _ ->
      test <@ Lens.setPartial head_ "Good" ["Bad"] = ["Good"] @>
    testCase "Appending to a List" <| fun _ ->
      test <@ Lens.map id_ (fun l -> l @ ["Tail"]) ["Head"] = ["Head"; "Tail"] @>
    testCase "Swapping the tail on an empty list" <| fun _ ->
      test <@ Lens.setPartial tail_ ["Bad"] [] = [] @>
    testCase "Swapping the tail on a list replaces the tail" <| fun _ ->
      test <@ Lens.setPartial tail_ ["Tail"] ["Head"; "Bad"; "Value"] = ["Head"; "Tail"] @>
    testCase "Swapping the tail on a list with one element appends the tail" <| fun _ ->
      test <@ Lens.setPartial tail_ ["Long"; "Tail"] ["Head"] = ["Head"; "Long"; "Tail"] @>
    testList "Map <--> List Isomorphism" [Iso.followsIsoLawsWithName "Map-to-List" (Map.toList<int,string>, Map.ofList<int,string>)]
  ]

[<Tests>]
let functionTests =
  testList "Basic Lens functions" [
    testCase "Lens.get returns correct values" <| fun _ ->
      Lens.get fst_ ("Good","Bad") =! "Good"

    testCase "Lens.getPartial returns correct values for existing values" <| fun _ ->
      Lens.getPartial choice1Of2_ (Choice1Of2 "Good") =! Some "Good"

    testCase "Lens.getPartial returns correct value for missing values" <| fun _ ->
      Lens.getPartial choice2Of2_ (Choice1Of2 "Bad") =! None

    testCase "Lens.getPartialOrElse returns correct value for existing values" <| fun _ ->
      Lens.getPartialOrElse choice1Of2_ "Bad" (Choice1Of2 "Good") =! "Good"

    testCase "Lens.getPartialOrElse returns correct value for missing values" <| fun _ ->
      Lens.getPartialOrElse choice2Of2_ "Good" (Choice1Of2 "Bad") =! "Good"

    testCase "Lens.set sets value correctly" <| fun _ ->
      Lens.set fst_ "Good" ("Bad",()) =! ("Good",())

    testCase "Lens.setPartial returns correct values for existing values" <| fun _ ->
      Lens.setPartial choice1Of2_ "Good" (Choice1Of2 "Bad") =! Choice1Of2 "Good"

    testCase "Lens.setPartial returns correct value for missing values" <| fun _ ->
      Lens.setPartial choice2Of2_ "Bad" (Choice1Of2 "Good") =! Choice1Of2 "Good"

    testCase "Lens.map modifies values correctly" <| fun _ ->
      Lens.map fst_ (fun x -> x + x) ("Good",()) =! ("GoodGood",())

    testCase "Lens.map modifies values correctly for existing values" <| fun _ ->
      Lens.mapPartial choice1Of2_ (fun x -> x + x) (Choice1Of2 "Good") =! Choice1Of2 "GoodGood"

    testCase "Lens.map modifies values correctly for missing values" <| fun _ ->
      Lens.mapPartial choice2Of2_ (fun x -> x + x) (Choice1Of2 "Good") =! Choice1Of2 "Good"
  ]

[<Tests>]
let compositionTests =
  let chars : Iso<string, char[]> =
    (fun x -> x.ToCharArray ()), (fun x -> String (x))

  let rev : Iso<char[], char[]> =
    Array.rev, Array.rev

  testList "Composition Tests" [
    testCase "isomorphism over return value" <| fun _ ->
      Lens.get (fst_ <--> chars) ("Good",()) =! [| 'G'; 'o'; 'o'; 'd' |]

    testCase "isomorphism over partial return value when Some" <| fun _ ->
      Lens.getPartial (choice1Of2_ <?-> chars) (Choice1Of2 "Good") =! Some [| 'G'; 'o'; 'o'; 'd' |]

    testCase "isomorphism over partial return None when None" <| fun _ ->
      Lens.getPartial (choice2Of2_ <?-> chars) (Choice1Of2 "Bad") =! None

    testCase "isomorphism over sets value" <| fun _ ->
      Lens.set (fst_ <--> chars) [| 'G'; 'o'; 'o'; 'd' |] ("Bad",()) =! ("Good",())

    testCase "isomorphism over partial sets value when Some" <| fun _ ->
      Lens.setPartial (choice1Of2_ <?-> chars) [| 'G'; 'o'; 'o'; 'd' |] (Choice1Of2 "Bad") =! Choice1Of2 "Good"

    testCase "isomorphism over partial sets value when None" <| fun _ ->
      Lens.setPartial (choice2Of2_ <?-> chars) [| 'B'; 'a'; 'd' |] (Choice1Of2 "Good")  =! Choice1Of2 "Good"

    testCase "multiple isomorphisms over return value" <| fun _ ->
      Lens.get (fst_ <--> chars <--> rev) ("dooG",()) =! [| 'G'; 'o'; 'o'; 'd' |]

    testCase "multiple isomorphisms over partial return value when Some" <| fun _ ->
      Lens.getPartial (choice1Of2_ <?-> chars <?-> rev) (Choice1Of2 "dooG") =! Some [| 'G'; 'o'; 'o'; 'd' |]

    testCase "multiple isomorphisms over partial return None when None" <| fun _ ->
      Lens.getPartial (choice2Of2_ <?-> chars <?-> rev) (Choice1Of2 "daB") =! None

    testCase "multiple isomorphisms over sets value" <| fun _ ->
      Lens.set (fst_ <--> chars <--> rev) [| 'd'; 'o'; 'o'; 'G' |] ("Bad",()) =! ("Good",())

    testCase "multiple isomorphisms over partial sets value when Some" <| fun _ ->
      Lens.setPartial (choice1Of2_ <?-> chars <?-> rev) [| 'd'; 'o'; 'o'; 'G' |] (Choice1Of2 "Bad") =! Choice1Of2 "Good"

    testCase "multiple isomorphisms over partial sets value when None" <| fun _ ->
      Lens.setPartial (choice2Of2_ <?-> chars <?-> rev) [| 'd'; 'a'; 'B' |] (Choice1Of2 "Good") =! Choice1Of2 "Good"
  ]

[<EntryPoint>]
let main argv = 
  defaultMainThisAssembly argv