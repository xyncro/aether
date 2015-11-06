namespace Aether.Tests

open System
open Aether
open Aether.Operators
open Aether.Testing.Properties
open FsCheck
open FsCheck.Xunit
open global.Xunit
open Swensen.Unquote

[<AutoOpen>]
module Data =
    let chars : Iso<string, char[]> =
        (fun x -> x.ToCharArray ()), (fun x -> String (x))

    let rev : Iso<char[], char[]> =
        Array.rev, Array.rev

module ``Built-in Lenses`` =
    [<Property>]
    let ``id_ follows the Lens Laws`` (outer : obj) inner dummy f =
        Lens.followsLensLaws id_ outer inner dummy f

    [<Property>]
    let ``fst_ follows the Lens Laws`` (outer : obj * obj) inner dummy f =
        Lens.followsLensLaws fst_ outer inner dummy f

    [<Property>]
    let ``snd_ follows the Lens Laws`` (outer : obj * obj) inner dummy f =
        Lens.followsLensLaws snd_ outer inner dummy f

module ``Built-in Prisms`` =
    [<Property>]
    let ``choice1Of2_ follows the Prism Laws`` (outer : Choice<obj,obj>) inner dummy f =
        Prism.followsPrismLaws Choice.choice1Of2_ outer inner dummy f

    [<Property>]
    let ``choice2Of2_ follows the Prism Laws`` (outer : Choice<obj,obj>) inner dummy f =
        Prism.followsPrismLaws Choice.choice2Of2_ outer inner dummy f

    [<Property>]
    let ``head_ follows the Prism Laws`` (outer : obj list) inner dummy f =
        Prism.followsPrismLaws List.head_ outer inner dummy f

    [<Property>]
    let ``tail_ follows the Prism Laws`` (outer : obj list) inner dummy f =
        Prism.followsPrismLaws List.tail_ outer inner dummy f

    [<Property>]
    let ``pos_ follows the Prism Laws`` (idx : NonNegativeInt) (outer : obj list) inner dummy f =
        Prism.followsPrismLaws (List.pos_ idx.Get) outer inner dummy f

    [<Property>]
    let ``key_ follows the Prism Laws`` key (outer : Map<string,obj>) inner dummy f =
        Prism.followsPrismLaws (Map.key_ key) outer inner dummy f

module ``Built-in Isomorphisms`` =
    [<Property>]
    let ``Map.list_ follows the Weak Isomorphism Laws`` (outer : Map<string,obj>) inner dummy =
        Iso.followsWeakIsoLaws Map.list_ outer inner dummy

    [<Property>]
    let ``Map.array_ follows the Weak Isomorphism Laws`` (outer : Map<string,obj>) inner dummy =
        Iso.followsWeakIsoLaws Map.array_ outer inner dummy

    [<Property>]
    let ``Array.list_ follows the Isomorphism Laws`` (outer : obj []) inner dummy f =
        Iso.followsIsoLaws Array.list_ outer inner dummy f

    [<Property>]
    let ``List.array_ follows the Isomorphism Laws`` (outer : obj list) inner dummy f =
        Iso.followsIsoLaws List.array_ outer inner dummy f

    [<Property>]
    let ``choice1Of2_ mapped through Map(toList/ofList) as a partial isomorphism follows the Weak Partial Isomorphism Laws`` (outer : Choice<Map<string,obj>,obj>) inner dummy =
        PIso.followsWeakPIsoLaws ((fst Choice.choice1Of2_ >> Option.map Map.toList),(Map.ofList >> Choice1Of2)) outer inner dummy

    [<Property>]
    let ``choice1Of2_ as a partial isomorphism follows the Partial Isomorphism Laws`` (outer : Choice<obj,obj>) inner dummy =
        PIso.followsPIsoLaws (fst Choice.choice1Of2_,Choice1Of2) outer inner dummy

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
        let newValue = Prism.set (MapExample.myMap_ >-?> Map.key_ "TestKey2") "OtherValue" example
        test <@ newValue.MyMap.TryFind "TestKey2" = None @>

    [<Fact>]
    let ``Updating a value contained in a Map using a Prism`` () =
        let example = { MyMap = Map.ofList ["TestKey","TestValue"]}
        let newValue = Prism.set (MapExample.myMap_ >-?> Map.key_ "TestKey") "OtherValue" example
        test <@ newValue.MyMap.["TestKey"] = "OtherValue" @>

    [<Fact>]
    let ``Trying to retreive a value not contained in a Map with a default using a Prism`` () =
        test <@ Prism.getOrElse (Map.key_ "TestKey") "Default" Map.empty = "Default" @>

    [<Fact>]
    let ``Trying to retrieve a value contained in a Map using a Prism`` () =
        test <@ Prism.getOrElse (Map.key_ "TestKey") "Default" (Map.ofList ["TestKey","Hit"]) = "Hit" @>

    [<Fact>]
    let ``Prepending an element onto a List using a Lens`` () =
        test <@ Lens.map id_ (fun l -> "Head" :: l) ["Tail"] = ["Head"; "Tail"] @>

    [<Fact>]
    let ``Appending a List onto aanother List using a Lens`` () =
        test <@ Lens.map id_ (fun l -> l @ ["Tail"]) ["Head"] = ["Head"; "Tail"] @>

    [<Fact>]
    let ``Setting the head on an empty List using a Prism`` () =
        test <@ Prism.set List.head_ "Bad" [] = [] @>

    [<Fact>]
    let ``Setting the head on a non-empty List using a Prism`` () =
        test <@ Prism.set List.head_ "Good" ["Bad"] = ["Good"] @>

    [<Fact>]
    let ``Setting the tail on an empty List using a Prism`` () =
        test <@ Prism.set List.tail_ ["Bad"] [] = [] @>

    [<Fact>]
    let ``Setting the tail on a non-empty List using a Prism`` () =
        test <@ Prism.set List.tail_ ["Tail"] ["Head"; "Bad"; "Value"] = ["Head"; "Tail"] @>

    [<Fact>]
    let ``Setting the tail on a single-element List using a Prism`` () =
        test <@ Prism.set List.tail_ ["Long"; "Tail"] ["Head"] = ["Head"; "Long"; "Tail"] @>

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
    let ``Prism.get returns correct values for existing values`` () =
        Prism.get Choice.choice1Of2_ (Choice1Of2 "Good") =! Some "Good"

    [<Fact>]
    let ``Prism.get returns correct value for missing values`` () =
        Prism.get Choice.choice2Of2_ (Choice1Of2 "Bad") =! None

    [<Fact>]
    let ``Prism.getOrElse returns correct value for existing values`` () =
        Prism.getOrElse Choice.choice1Of2_ "Bad" (Choice1Of2 "Good") =! "Good"

    [<Fact>]
    let ``Prism.getOrElse returns correct value for missing values`` () =
        Prism.getOrElse Choice.choice2Of2_ "Good" (Choice1Of2 "Bad") =! "Good"

    [<Fact>]
    let ``Prism.set returns correct values for existing values`` () =
        Prism.set Choice.choice1Of2_ "Good" (Choice1Of2 "Bad") =! Choice1Of2 "Good"

    [<Fact>]
    let ``Prism.set returns correct value for missing values`` () =
        Prism.set Choice.choice2Of2_ "Bad" (Choice1Of2 "Good") =! Choice1Of2 "Good"

    [<Fact>]
    let ``Prism.map modifies values correctly for existing values`` () =
        Prism.map Choice.choice1Of2_ (fun x -> x + x) (Choice1Of2 "Good") =! Choice1Of2 "GoodGood"

    [<Fact>]
    let ``Prism.map modifies values correctly for missing values`` () =
        Prism.map Choice.choice2Of2_ (fun x -> x + x) (Choice1Of2 "Good") =! Choice1Of2 "Good"

module ``Isomorphism composition`` =
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
            Prism.get (Choice.choice1Of2_ <?-> chars) (Choice1Of2 "Good") =! Some [| 'G'; 'o'; 'o'; 'd' |]

        [<Fact>]
        let ``gets nothing when inner does not exist`` () =
            Prism.get (Choice.choice2Of2_ <?-> chars) (Choice1Of2 "Bad") =! None

        [<Fact>]
        let ``sets value when inner exists`` () =
            Prism.set (Choice.choice1Of2_ <?-> chars) [| 'G'; 'o'; 'o'; 'd' |] (Choice1Of2 "Bad") =! Choice1Of2 "Good"

        [<Fact>]
        let ``sets nothing when inner does not exist`` () =
            Prism.set (Choice.choice2Of2_ <?-> chars) [| 'B'; 'a'; 'd' |] (Choice1Of2 "Good") =! Choice1Of2 "Good"

        [<Fact>]
        let ``gets value when inner exists over multiple isomorphisms`` () =
            Prism.get (Choice.choice1Of2_ <?-> chars <?-> rev) (Choice1Of2 "dooG") =! Some [| 'G'; 'o'; 'o'; 'd' |]

        [<Fact>]
        let ``gets nothing when inner does not exist over multiple isomorphisms`` () =
            Prism.get (Choice.choice2Of2_ <?-> chars <?-> rev) (Choice1Of2 "daB") =! None

        [<Fact>]
        let ``sets value when inner exists over multiple isomorphisms`` () =
            Prism.set (Choice.choice1Of2_ <?-> chars <?-> rev) [| 'd'; 'o'; 'o'; 'G' |] (Choice1Of2 "Bad") =! Choice1Of2 "Good"

        [<Fact>]
        let ``sets nothing when inner does not exist over multiple isomorphisms`` () =
            Prism.set (Choice.choice2Of2_ <?-> chars <?-> rev) [| 'd'; 'a'; 'B' |] (Choice1Of2 "Good") =! Choice1Of2 "Good"
