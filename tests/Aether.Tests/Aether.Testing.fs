module Aether.Testing

open Aether
open Aether.Operators
open FsCheck
open Swensen.Unquote

module Properties =
  [<RequireQualifiedAccess;CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
  module Lens =
    let getSetIdentityWith lensGet lensSet outer =
      "Get-Set Identity" @| lazy
        test <@ lensSet (lensGet outer) outer = outer @>

    let setGetSymmetryWith lensGet lensSet outer inner =
      "Set-Get Symmetry" @| lazy
        test <@ lensGet (lensSet inner outer) = inner @>

    let setSetOrderDependenceWith lensSet outer inner dummy =
      "Set-Set Order Dependence" @| lazy
        test <@ lensSet inner (lensSet dummy outer) = lensSet inner outer @>
      |> Prop.trivial (inner = dummy)
      
    let getSetMapCorrespondenceWith lensGet lensSet lensMap f outer =
      "Get-Set to Map Correspondence" @| lazy
        test <@ lensSet (lensGet outer |> f) outer = lensMap f outer @>

    let inline unwrapLens f lens =
      f (Lens.get lens) (Lens.set lens)

    let inline getSetIdentity lens =
      unwrapLens getSetIdentityWith lens

    let inline setGetSymmetry lens =
      unwrapLens setGetSymmetryWith lens

    let inline setSetOrderDependence lens =
      setSetOrderDependenceWith (Lens.set lens)

    let inline getSetMapCorrespondence lens =
      unwrapLens getSetMapCorrespondenceWith lens (Lens.map lens)

    let followsLensLaws lens outer inner dummy f =
      getSetIdentity lens outer .&.
      setGetSymmetry lens outer inner .&.
      setSetOrderDependence lens outer inner dummy .&.
      getSetMapCorrespondence lens f outer
      |> Prop.trivial (inner = dummy)

  [<RequireQualifiedAccess;CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
  module Prism =
    let classifyInner innerP =
      Prop.classify (Option.isSome innerP) "has inner"
      >> Prop.classify (Option.isNone innerP) "no inner"

    let inline classifyInnerWith prismGet outer =
      classifyInner (prismGet outer)

    let inline getSetIdentityWith prismGet prismSet outer dummy =
      "Get-Set Identity" @| lazy
        test <@ prismSet (defaultArg (prismGet outer) dummy) outer = outer @>
      |> classifyInnerWith prismGet outer

    let inline setGetSymmetryWith prismGet prismSet outer inner =
      "Set-Get Symmetry" @| lazy
        match prismGet outer with
        | Some _ -> "inner should be changed" @| (test <@ prismSet inner outer |> prismGet = Some inner @>)
        | None -> "outer should remain unchanged" @| (test <@ prismSet inner outer = outer @>)
      |> classifyInnerWith prismGet outer

    let inline setSetOrderDependenceWith prismGet prismSet outer inner dummy =
      "Set-Set Order Dependence" @| lazy
        test <@ prismSet inner outer = (prismSet inner (prismSet dummy outer)) @>
      |> classifyInnerWith prismGet outer
      |> Prop.trivial (inner = dummy)

    let inline getSetMapCorrespondenceWith prismGet prismSet prismMap f outer =
      "Get-Set to Map Correspondence" @| lazy
        test <@ prismMap f outer = (prismGet outer |> function | Some i -> prismSet (f i) outer | None -> outer) @>
      |> classifyInnerWith prismGet outer

    let inline unwrapPrism f prism =
      f (Lens.getPartial prism) (Lens.setPartial prism)

    let inline getSetIdentity prism =
      unwrapPrism getSetIdentityWith prism

    let inline setGetSymmetry prism =
      unwrapPrism setGetSymmetryWith prism

    let inline setSetOrderDependence prism =
      unwrapPrism setSetOrderDependenceWith prism

    let inline getSetMapCorrespondence prism =
      unwrapPrism getSetMapCorrespondenceWith prism (Lens.mapPartial prism)

    let followsPrismLaws prism outer inner dummy f =
      getSetIdentity prism outer dummy .&.
      setGetSymmetry prism outer inner .&.
      setSetOrderDependence prism outer inner dummy .&.
      getSetMapCorrespondence prism f outer
      |> Prop.trivial (inner = dummy)

  [<RequireQualifiedAccess;CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
  module Iso =
    let roundtripEquality iso outer =
      "Roundtrip Equality" @| lazy
        test <@ fst iso outer |> snd iso = outer @>

    let converseRoundtripEquality iso inner =
      "Converse Roundtrip Equality" @| lazy
        test <@ snd iso inner |> fst iso = inner @>

    let inline followsWeakIsoLaws iso outer inner dummy =
      let isoAsLens = id_ <--> iso
      Lens.getSetIdentity isoAsLens outer .&.
      Lens.setSetOrderDependence isoAsLens outer inner dummy .&.
      roundtripEquality iso outer

    let inline followsIsoLaws iso outer inner dummy f =
      Lens.followsLensLaws (id_ <--> iso) outer inner dummy f .&.
      roundtripEquality iso outer .&.
      converseRoundtripEquality iso inner

  [<RequireQualifiedAccess;CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
  module PIso =
    let roundtripEquality isoP outer =
      fst isoP outer |> Option.isSome ==>
        "Roundtrip Equality" @| lazy
          test <@ fst isoP outer |> Option.map (snd isoP) = Some outer @>

    let converseRoundtripEquality isoP inner =
        "Converse Roundtrip Equality" @| lazy
          test <@ snd isoP inner |> fst isoP = Some inner @>

    let inline followsWeakPIsoLaws isoP outer inner dummy =
      let isoPAsLens = id_ <-?> isoP
      Prism.setSetOrderDependence isoPAsLens outer inner dummy .&.
      roundtripEquality isoP outer

    let inline followsPIsoLaws isoP outer inner dummy =
      let isoPAsLens = id_ <-?> isoP
      Prism.setSetOrderDependence isoPAsLens outer inner dummy .&.
      roundtripEquality isoP outer .&.
      converseRoundtripEquality isoP inner
