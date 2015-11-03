module Aether.Testing

open Aether
open Aether.Operators
open FsCheck
open Swensen.Unquote

[<RequireQualifiedAccess;CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
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

  let inline unwrapLens f lens =
    f (Lens.get lens) (Lens.set lens)

  let inline getSetIdentityProperty lens =
    unwrapLens getSetIdentityPropertyWith lens

  let inline setGetSymmetryProperty lens =
    unwrapLens setGetSymmetryPropertyWith lens

  let inline setSetOrderDependenceProperty lens =
    setSetOrderDependencePropertyWith (Lens.set lens)

  let inline getSetMapCorrespondenceProperty lens =
    unwrapLens getSetMapCorrespondencePropertyWith lens (Lens.map lens)

  let followsLensLawsProperty lens outer inner dummy f =
    getSetIdentityProperty lens outer .&.
    setGetSymmetryProperty lens outer inner .&.
    setSetOrderDependenceProperty lens outer inner dummy .&.
    getSetMapCorrespondenceProperty lens f outer
    |> Prop.trivial (inner = dummy)

[<RequireQualifiedAccess;CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
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

[<RequireQualifiedAccess;CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module Iso =
  let roundtripEqualityProperty iso v =
    "Roundtrip Equality" @| lazy
      test <@ fst iso v |> snd iso = v @>

  let inline followsIsoLawsProperty iso v =
    Lens.followsLensLawsProperty (id_ <--> iso) .&.
    roundtripEqualityProperty iso v

[<RequireQualifiedAccess;CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module PIso =
  let roundtripEqualityProperty iso v =
    fst iso v |> Option.isSome ==>
      "Roundtrip Equality" @| lazy
        test <@ fst iso v |> Option.map (snd iso) = Some v @>

  let inline followsPIsoLawsProperty iso v =
    Prism.followsPrismLawsProperty (id_ <-?> iso) .&.
    roundtripEqualityProperty iso v