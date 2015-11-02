namespace Aether.Tests

open System
open Aether
open Aether.Operators
open FsCheck
open FsCheck.Xunit
open global.Xunit
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
  let getSetIdentityPropertyWith lensGet lensSet outer =
    "Get-Set Identity" @| lazy
      test <@ lensSet (lensGet outer) outer = outer @>

  let setGetSymmetryPropertyWith lensGet lensSet outer inner =
    "Set-Get Symmetry" @| lazy
      test <@ lensGet (lensSet inner outer) = inner @>

  let setSetOrderDependencePropertyWith lensSet outer inner dummy =
    "Set-Set Order Dependence" @| lazy
      test <@ lensSet inner (lensSet dummy outer) = lensSet inner outer @>
    |> Prop.trivial (inner = dummy)
      
  let getSetMapCorrespondencePropertyWith lensGet lensSet lensMap f outer =
    "Get-Set to Map Correspondence" @| lazy
      test <@ lensSet (lensGet outer |> f) outer = lensMap f outer @>

  let inline getSetIdentityProperty lens =
    getSetIdentityPropertyWith (Lens.get lens) (Lens.set lens)

  let inline setGetSymmetryProperty lens =
    setGetSymmetryPropertyWith (Lens.get lens) (Lens.set lens)

  let inline setSetOrderDependenceProperty lens =
    setSetOrderDependencePropertyWith (Lens.set lens)

  let inline getSetMapCorrespondenceProperty lens =
    getSetMapCorrespondencePropertyWith (Lens.get lens) (Lens.set lens) (Lens.map lens)

  let followsLensLawsProperty lens outer inner dummy f =
    getSetIdentityProperty lens outer .&.
    setGetSymmetryProperty lens outer inner .&.
    setSetOrderDependenceProperty lens outer inner dummy .&.
    getSetMapCorrespondenceProperty lens f outer
    |> Prop.trivial (inner = dummy)

module Prism =
  let classifyInner innerP =
    Prop.classify (Option.isSome innerP) "has inner"
    >> Prop.classify (Option.isNone innerP) "no inner"

  let inline classifyInnerWith prismGet outer =
    classifyInner (prismGet outer)

  let inline getSetIdentityPropertyWith prismGet prismSet outer dummy =
    "Get-Set Identity" @| lazy
      test <@ prismSet (defaultArg (prismGet outer) dummy) outer = outer @>
    |> classifyInnerWith prismGet outer

  let inline setGetSymmetryPropertyWith prismGet prismSet outer inner =
    "Set-Get Symmetry" @| lazy
      match prismGet outer with
      | Some _ -> "inner should be changed" @| (test <@ prismSet inner outer |> prismGet = Some inner @>)
      | None -> "outer should remain unchanged" @| (test <@ prismSet inner outer = outer @>)
    |> classifyInnerWith prismGet outer

  let inline setSetOrderDependencePropertyWith prismGet prismSet outer inner dummy =
    "Set-Set Order Dependence" @| lazy
      test <@ prismSet inner outer = (prismSet inner (prismSet dummy outer)) @>
    |> classifyInnerWith prismGet outer
    |> Prop.trivial (inner = dummy)

  let inline getSetMapCorrespondencePropertyWith prismGet prismSet prismMap f outer =
    "Get-Set to Map Correspondence" @| lazy
      test <@ prismMap f outer = (prismGet outer |> function | Some i -> prismSet (f i) outer | None -> outer) @>
    |> classifyInnerWith prismGet outer

  let inline unwrapPrism f prism =
    f (Lens.getPartial prism) (Lens.setPartial prism)

  let inline getSetIdentityProperty prism =
    unwrapPrism getSetIdentityPropertyWith prism

  let inline setGetSymmetryProperty prism =
    unwrapPrism setGetSymmetryPropertyWith prism

  let inline setSetOrderDependenceProperty prism =
    unwrapPrism setSetOrderDependencePropertyWith prism

  let inline getSetMapCorrespondenceProperty prism =
    unwrapPrism getSetMapCorrespondencePropertyWith prism (Lens.mapPartial prism)

  let followsPrismLawsProperty prism outer inner dummy f =
    getSetIdentityProperty prism outer dummy .&.
    setGetSymmetryProperty prism outer inner .&.
    setSetOrderDependenceProperty prism outer inner dummy .&.
    getSetMapCorrespondenceProperty prism f outer
    |> Prop.trivial (inner = dummy)

module Iso =
  let roundtripEqualityProperty iso v =
    "Roundtrip Equality" @| lazy
      test <@ fst iso v |> snd iso = v @>

  let inline followsIsoLawsProperty iso v =
    Lens.followsLensLawsProperty (id_ <--> iso) .&.
    roundtripEqualityProperty iso v

module ``Built-in Lenses`` =
  [<Property>]
  let ``id_ follows the Lens Laws`` (outer : obj, inner, dummy, f) =
    Lens.followsLensLawsProperty id_ outer inner dummy f

  [<Property>]
  let ``fst_ follows the Lens Laws`` (outer : obj * obj, inner, dummy, f) =
    Lens.followsLensLawsProperty fst_ outer inner dummy f

  [<Property>]
  let ``snd_ follows the Lens Laws`` (outer : obj * obj, inner, dummy, f) =
    Lens.followsLensLawsProperty snd_ outer inner dummy f

module ``Built-in Prisms`` =
  [<Property>]
  let ``choice1Of2_ follows the Prism Laws`` (outer : Choice<obj,obj>, inner, dummy, f) =
    Prism.followsPrismLawsProperty choice1Of2_ outer inner dummy f

  [<Property>]
  let ``choice2Of2_ follows the Prism Laws`` (outer : Choice<obj,obj>, inner, dummy, f) =
    Prism.followsPrismLawsProperty choice2Of2_ outer inner dummy f

  [<Property>]
  let ``head_ follows the Prism Laws`` (outer : obj list, inner, dummy, f) =
    Prism.followsPrismLawsProperty head_ outer inner dummy f

  [<Property>]
  let ``tail_ follows the Prism Laws`` (outer : obj list, inner, dummy, f) =
    Prism.followsPrismLawsProperty tail_ outer inner dummy f

  [<Property>]
  let ``pos_ follows the Prism Laws`` (idx : NonNegativeInt, outer : obj list, inner, dummy, f) =
    Prism.followsPrismLawsProperty (pos_ idx.Get) outer inner dummy f

  [<Property>]
  let ``key_ follows the Prism Laws`` (key, outer : Map<string,obj>, inner, dummy, f) =
    Prism.followsPrismLawsProperty (key_ key) outer inner dummy f

module ``Built-in Isomorphisms`` =
  [<Property>]
  let ``Map(toList/ofList) follows the Isomorphism Laws`` v =
    Iso.followsIsoLawsProperty (Map.toList,Map.ofList) v

type MapExample =
  { MyMap : Map<string,string> }
  with
    static member myMap_ =
      (fun x -> x.MyMap),
      (fun v x -> { x with MyMap = v })

module ``Examplar Usage Tests`` =
  [<Fact>]
  let ``Upserting into a Map using a Lens`` () =
    let example = { MyMap = Map.ofList ["TestKey","TestValue"]}
    let newValue = Lens.map MapExample.myMap_ (Map.add "TestKey2" "OtherValue") example
    test <@ newValue.MyMap.["TestKey"] = "TestValue" @>
    test <@ newValue.MyMap.["TestKey2"] = "OtherValue" @>

  [<Fact>]
  let ``Updating a value not contained in a Map using a Prism`` () =
    let example = { MyMap = Map.ofList ["TestKey","TestValue"]}
    let newValue = Lens.setPartial (MapExample.myMap_ >-?> key_ "TestKey2") "OtherValue" example
    test <@ newValue.MyMap.TryFind "TestKey2" = None @>

  [<Fact>]
  let ``Updating a value contained in a Map using a Prism`` () =
    let example = { MyMap = Map.ofList ["TestKey","TestValue"]}
    let newValue = Lens.setPartial (MapExample.myMap_ >-?> key_ "TestKey") "OtherValue" example
    test <@ newValue.MyMap.["TestKey"] = "OtherValue" @>

  [<Fact>]
  let ``Trying to retreive a value not contained in a Map with a default using a Prism`` () =
    test <@ Lens.getPartialOrElse (key_ "TestKey") "Default" Map.empty = "Default" @>

  [<Fact>]
  let ``Trying to retrieve a value contained in a Map using a Prism`` () =
    test <@ Lens.getPartialOrElse (key_ "TestKey") "Default" (Map.ofList ["TestKey","Hit"]) = "Hit" @>

  [<Fact>]
  let ``Prepending an element onto a List using a Lens`` () =
    test <@ Lens.map id_ (fun l -> "Head" :: l) ["Tail"] = ["Head"; "Tail"] @>

  [<Fact>]
  let ``Appending a List onto aanother List using a Lens`` () =
    test <@ Lens.map id_ (fun l -> l @ ["Tail"]) ["Head"] = ["Head"; "Tail"] @>

  [<Fact>]
  let ``Setting the head on an empty List using a Prism`` () =
    test <@ Lens.setPartial head_ "Bad" [] = [] @>

  [<Fact>]
  let ``Setting the head on a non-empty List using a Prism`` () =
    test <@ Lens.setPartial head_ "Good" ["Bad"] = ["Good"] @>

  [<Fact>]
  let ``Setting the tail on an empty List using a Prism`` () =
    test <@ Lens.setPartial tail_ ["Bad"] [] = [] @>

  [<Fact>]
  let ``Setting the tail on a non-empty List using a Prism`` () =
    test <@ Lens.setPartial tail_ ["Tail"] ["Head"; "Bad"; "Value"] = ["Head"; "Tail"] @>

  [<Fact>]
  let ``Setting the tail on a single-element List using a Prism`` () =
    test <@ Lens.setPartial tail_ ["Long"; "Tail"] ["Head"] = ["Head"; "Long"; "Tail"] @>

module ``Basic Lens functions`` =
  [<Fact>]
  let ``Lens.get returns correct values`` () =
    Lens.get fst_ ("Good","Bad") =! "Good"

  [<Fact>]
  let ``Lens.set sets value correctly`` () =
    Lens.set fst_ "Good" ("Bad",()) =! ("Good",())

  [<Fact>]
  let ``Lens.map modifies values correctly`` () =
    Lens.map fst_ (fun x -> x + x) ("Good",()) =! ("GoodGood",())

module ``Basic Prism functions`` =
  [<Fact>]
  let ``Lens.getPartial returns correct values for existing values`` () =
    Lens.getPartial choice1Of2_ (Choice1Of2 "Good") =! Some "Good"

  [<Fact>]
  let ``Lens.getPartial returns correct value for missing values`` () =
    Lens.getPartial choice2Of2_ (Choice1Of2 "Bad") =! None

  [<Fact>]
  let ``Lens.getPartialOrElse returns correct value for existing values`` () =
    Lens.getPartialOrElse choice1Of2_ "Bad" (Choice1Of2 "Good") =! "Good"

  [<Fact>]
  let ``Lens.getPartialOrElse returns correct value for missing values`` () =
    Lens.getPartialOrElse choice2Of2_ "Good" (Choice1Of2 "Bad") =! "Good"

  [<Fact>]
  let ``Lens.setPartial returns correct values for existing values`` () =
    Lens.setPartial choice1Of2_ "Good" (Choice1Of2 "Bad") =! Choice1Of2 "Good"

  [<Fact>]
  let ``Lens.setPartial returns correct value for missing values`` () =
    Lens.setPartial choice2Of2_ "Bad" (Choice1Of2 "Good") =! Choice1Of2 "Good"

  [<Fact>]
  let ``Lens.mapPartial modifies values correctly for existing values`` () =
    Lens.mapPartial choice1Of2_ (fun x -> x + x) (Choice1Of2 "Good") =! Choice1Of2 "GoodGood"

  [<Fact>]
  let ``Lens.mapPartial modifies values correctly for missing values`` () =
    Lens.mapPartial choice2Of2_ (fun x -> x + x) (Choice1Of2 "Good") =! Choice1Of2 "Good"

module ``Isomorphism composition`` =
  let chars : Iso<string, char[]> =
    (fun x -> x.ToCharArray ()), (fun x -> String (x))

  let rev : Iso<char[], char[]> =
    Array.rev, Array.rev

  module ``over a Lens`` =
    [<Fact>]
    let ``gets value`` () =
      Lens.get (fst_ <--> chars) ("Good",()) =! [| 'G'; 'o'; 'o'; 'd' |]

    [<Fact>]
    let ``sets value`` () =
      Lens.set (fst_ <--> chars) [| 'G'; 'o'; 'o'; 'd' |] ("Bad",()) =! ("Good",())

    [<Fact>]
    let ``gets value over multiple isomorphisms`` () =
      Lens.get (fst_ <--> chars <--> rev) ("dooG",()) =! [| 'G'; 'o'; 'o'; 'd' |]

    [<Fact>]
    let ``sets value over multiple isomorphisms`` () =
      Lens.set (fst_ <--> chars <--> rev) [| 'd'; 'o'; 'o'; 'G' |] ("Bad",()) =! ("Good",())

  module ``over a Prism`` =
    [<Fact>]
    let ``gets value when inner exists`` () =
      Lens.getPartial (choice1Of2_ <?-> chars) (Choice1Of2 "Good") =! Some [| 'G'; 'o'; 'o'; 'd' |]

    [<Fact>]
    let ``gets nothing when inner does not exist`` () =
      Lens.getPartial (choice2Of2_ <?-> chars) (Choice1Of2 "Bad") =! None

    [<Fact>]
    let ``sets value when inner exists`` () =
      Lens.setPartial (choice1Of2_ <?-> chars) [| 'G'; 'o'; 'o'; 'd' |] (Choice1Of2 "Bad") =! Choice1Of2 "Good"

    [<Fact>]
    let ``sets nothing when inner does not exist`` () =
      Lens.setPartial (choice2Of2_ <?-> chars) [| 'B'; 'a'; 'd' |] (Choice1Of2 "Good")  =! Choice1Of2 "Good"

    [<Fact>]
    let ``gets value when inner exists over multiple isomorphisms`` () =
      Lens.getPartial (choice1Of2_ <?-> chars <?-> rev) (Choice1Of2 "dooG") =! Some [| 'G'; 'o'; 'o'; 'd' |]

    [<Fact>]
    let ``gets nothing when inner does not exist over multiple isomorphisms`` () =
      Lens.getPartial (choice2Of2_ <?-> chars <?-> rev) (Choice1Of2 "daB") =! None

    [<Fact>]
    let ``sets value when inner exists over multiple isomorphisms`` () =
      Lens.setPartial (choice1Of2_ <?-> chars <?-> rev) [| 'd'; 'o'; 'o'; 'G' |] (Choice1Of2 "Bad") =! Choice1Of2 "Good"

    [<Fact>]
    let ``sets nothing when inner does not exist over multiple isomorphisms`` () =
      Lens.setPartial (choice2Of2_ <?-> chars <?-> rev) [| 'd'; 'a'; 'B' |] (Choice1Of2 "Good") =! Choice1Of2 "Good"
