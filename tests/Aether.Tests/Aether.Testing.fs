module Aether.Testing

open Aether
open Aether.Operators
open FsCheck
open Swensen.Unquote

/// Properties that can be used to evaluate the conformance of Lens, Prism, and Isos
/// to certain invariants.
module Properties =
  /// Lens properties
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

    /// The Get-Set Identity requires that modifying an entity through a lens by setting a value
    /// to exactly what it was before then nothing happens.
    let inline getSetIdentity lens =
      unwrapLens getSetIdentityWith lens

    /// The Set-Get Symmetry requires that modifying an entity through a lens by setting a value
    /// and then viewing that same value returns the value that was just set through it.
    let inline setGetSymmetry lens =
      unwrapLens setGetSymmetryWith lens

    /// The Set-Set Order Dependence requires that modifying an entity through a lens by setting
    /// a value to `a` and then setting the same value to `b` behaves the same as having modified
    /// the original entity by only setting the value to `b`.
    let inline setSetOrderDependence lens =
      setSetOrderDependenceWith (Lens.set lens)

    /// The Get-Set to Map Corresponsence requires that modifying an entity through a lens
    /// by getting a value, applying some function `f` to that value and then setting the value to the result
    /// behaves the same as having mapped that function through the lens.
    let inline getSetMapCorrespondence lens =
      unwrapLens getSetMapCorrespondenceWith lens (Lens.map lens)

    /// Requires that a lens follows the Lens Laws and is well-behaved:
    /// * Get-Set Identity,
    /// * Set-Get Symmetry, and
    /// * Set-Set Order Dependence.
    /// Is "Trivial" when `inner = dummy`.
    let followsLensLaws lens outer inner dummy f =
      getSetIdentity lens outer .&.
      setGetSymmetry lens outer inner .&.
      setSetOrderDependence lens outer inner dummy .&.
      getSetMapCorrespondence lens f outer
      |> Prop.trivial (inner = dummy)

  /// Prism properties
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

    /// The Get-Set Identity requires that modifying an entity through a prism by setting a value
    /// to exactly what it was before then nothing happens.
    /// Classifies evaluation by whether the prism resolves to `Some` on the tested entity.
    let inline getSetIdentity prism =
      unwrapPrism getSetIdentityWith prism

    /// The Set-Get Symmetry requires that modifying an entity through a prism by setting a value
    /// and then viewing that same value returns either the value that was just set through it. If
    /// the prism doesn't resolve to `Some`, then the entity should not be modified.
    /// Classifies evaluation by whether the prism resolves to `Some` on the tested entity.
    let inline setGetSymmetry prism =
      unwrapPrism setGetSymmetryWith prism

    /// The Set-Set Order Dependence requires that modifying an entity through a prism by setting
    /// a value to `a` and then setting the same value to `b` behaves the same as having modified
    /// the original entity by only setting the value to `b`.
    /// Classifies evaluation by whether the prism resolves to `Some` on the tested entity.
    /// Is "Trivial" when `inner = dummy`.
    let inline setSetOrderDependence prism =
      unwrapPrism setSetOrderDependenceWith prism

    /// The Get-Set to Map Corresponsence requires that modifying an entity through a prism
    /// by getting a value, applying some function `f` to that value and then setting the value to the result
    /// behaves the same as having mapped that function through the prism.
    /// Classifies evaluation by whether the prism resolves to `Some` on the tested entity.
    let inline getSetMapCorrespondence prism =
      unwrapPrism getSetMapCorrespondenceWith prism (Lens.mapPartial prism)

    /// Requires that a prism follows the Prism Laws and is well-behaved:
    /// * Get-Set Identity,
    /// * Set-Get Symmetry, and
    /// * Set-Set Order Dependence.
    /// Classifies evaluation by whether the prism resolves to `Some` on the tested entity.
    /// Is "Trivial" when `inner = dummy`.
    let followsPrismLaws prism outer inner dummy f =
      getSetIdentity prism outer dummy .&.
      setGetSymmetry prism outer inner .&.
      setSetOrderDependence prism outer inner dummy .&.
      getSetMapCorrespondence prism f outer
      |> Prop.trivial (inner = dummy)

  /// Isomorphism properties
  [<RequireQualifiedAccess;CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
  module Iso =
    /// Requires that mapping a value through an isomorphism and then mapping it
    /// back returns a value equivalent to the original.
    let roundtripEquality iso outer =
      "Roundtrip Equality" @| lazy
        test <@ fst iso outer |> snd iso = outer @>

    /// Requires that conversely mapping a value through an isomorphism and then
    /// mapping it back returns a value equivalent to the original.
    let converseRoundtripEquality iso inner =
      "Converse Roundtrip Equality" @| lazy
        test <@ snd iso inner |> fst iso = inner @>

    /// Requires that an isomorphism demonstrates certain properties to ensure
    /// unidirectional isomorphic operations are sane.
    let inline followsWeakIsoLaws iso outer inner dummy =
      let isoAsLens = id_ <--> iso
      Lens.getSetIdentity isoAsLens outer .&.
      Lens.setSetOrderDependence isoAsLens outer inner dummy .&.
      roundtripEquality iso outer

    /// Requires that an isomorphism demonstrates certain properties to ensure
    /// isomorphic operations in either direction are sane.
    let inline followsIsoLaws iso outer inner dummy f =
      Lens.followsLensLaws (id_ <--> iso) outer inner dummy f .&.
      roundtripEquality iso outer .&.
      converseRoundtripEquality iso inner

  /// Partial Isomorphism properties
  [<RequireQualifiedAccess;CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
  module PIso =
    /// Requires that, if mapping a value through a partial isomorphism results in a value,
    /// mapping that value back returns a value equivalent to the original.
    let roundtripEquality isoP outer =
      fst isoP outer |> Option.isSome ==>
        "Roundtrip Equality" @| lazy
          test <@ fst isoP outer |> Option.map (snd isoP) = Some outer @>

    /// Requires that conversely mapping a value through a partial isomorphism and then
    /// mapping it back returns a value equivalent to the original.
    let converseRoundtripEquality isoP inner =
        "Converse Roundtrip Equality" @| lazy
          test <@ snd isoP inner |> fst isoP = Some inner @>

    /// Requires that a partial isomorphism demonstrates minimal properties to ensure
    /// sane operations in one direction.
    let inline followsWeakPIsoLaws isoP outer inner dummy =
      let isoPAsLens = id_ <-?> isoP
      Prism.setSetOrderDependence isoPAsLens outer inner dummy .&.
      roundtripEquality isoP outer

    /// Requires that a partial isomorphism demonstrates minimal properties to ensure
    /// sane operations in both directions.
    let inline followsPIsoLaws isoP outer inner dummy =
      let isoPAsLens = id_ <-?> isoP
      Prism.setSetOrderDependence isoPAsLens outer inner dummy .&.
      roundtripEquality isoP outer .&.
      converseRoundtripEquality isoP inner
