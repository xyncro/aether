namespace Aether


/// Partial lens ('a -> 'b option) * ('b -> 'a -> 'a)
type LensP<'a,'b> = ('a -> 'b option) * ('b -> 'a -> 'a)

/// Total lens ('a -> 'b) * ('b -> 'a -> 'a)
type LensT<'a,'b> = ('a -> 'b) * ('b -> 'a -> 'a)


[<AutoOpen>]
module Functions =

    let internal getL (g, _) = g
    let internal setL (_, s) = s

    /// Get a value option using a Partial lens
    let getP (l: LensP<'a,'b>) = getL l

    /// Set a value using a Partial lens
    let setP (l: LensP<'a,'b>) = setL l

    /// Update a value using a Partial lens
    let updP (l: LensP<'a,'b>) = fun f a -> Option.map f (getL l a) |> function | Some b -> setL l b a | _ -> a

    /// Get a value using a Total lens
    let getT (l: LensT<'a,'b>) = getL l

    /// Set a value using a Total lens
    let setT (l: LensT<'a,'b>) = setL l

    /// Update a value using a Total lens
    let updT (l: LensT<'a,'b>) = fun f a -> setL l (f (getL l a)) a


[<AutoOpen>]
module internal Composition =

    // Partial

    let private partialTotalGet l1 l2 =
        fun a -> Option.map (getL l2) (getL l1 a )

    let private partialPartialGet l1 l2 =
        fun a -> Option.bind (getL l2) (getL l1 a)

    let private partialSet l1 l2 =
        fun c a -> Option.map (setL l2 c) (getL l1 a) |> function | Some b -> setL l1 b a | _ -> a

    let partialTotal l1 l2 =
        partialTotalGet l1 l2, partialSet l1 l2

    let partialPartial l1 l2 =
        partialPartialGet l1 l2, partialSet l1 l2

    // Total

    let private totalGet l1 l2 =
        fun a -> getL l2 (getL l1 a)

    let private totalSet l1 l2 =
        fun c a -> setL l1 (setL l2 c (getL l1 a)) a

    let totalStar l1 l2  =
        totalGet l1 l2, totalSet l1 l2


[<AutoOpen>]
module Operators =

    /// Compose two Total lenses, giving a Total lens
    let (>-->) (t1: LensT<'a,'b>) (t2: LensT<'b,'c>) : LensT<'a,'c> = totalStar t1 t2

    /// Compose a Total lens and a Partial lens, giving a Partial lens
    let (>-?>) (t1: LensT<'a,'b>) (p1: LensP<'b,'c>) : LensP<'a,'c> = totalStar t1 p1

    /// Compose a Total lens and a Partial lens, giving a Partial lens
    let (>?->) (p1: LensP<'a,'b>) (t1: LensT<'b,'c>) : LensP<'a,'c> = partialTotal p1 t1

    /// Compose two Partial lenses, giving a Partial lens
    let (>??>) (p1: LensP<'a,'b>) (p2: LensP<'b,'c>) : LensP<'a,'c> = partialPartial p1 p2


[<AutoOpen>]
module Lenses =

    // Isomorphisms

    /// Total Isomorphism of 'a -> 'b giving a Total lens
    let isoT (ab: 'a -> 'b) (ba: 'b -> 'a) : LensT<'a,'b> =
        (fun a -> ab a), (fun b _ -> ba b)

    /// Partial Isomorphism of 'a -> 'b giving a Partial lens
    let isoP (ab: 'a -> 'b option) (ba: 'b -> 'a) : LensP<'a, 'b> =
        (fun a -> ab a), (fun b _ -> ba b)
