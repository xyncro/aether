namespace Aether


/// Lens ('a -> 'b) * ('b -> 'a -> 'a)
type Lens<'a,'b> = ('a -> 'b) * ('b -> 'a -> 'a)

/// Partial lens ('a -> 'b option) * ('b -> 'a -> 'a)
type PLens<'a,'b> = ('a -> 'b option) * ('b -> 'a -> 'a)


[<AutoOpen>]
module Functions =

    let internal get (g, _) = g
    let internal set (_, s) = s

    // Lens
    
    /// Get a value using a lens
    let getL ((g, _): Lens<'a,'b>) = g

    /// Set a value using a lens
    let setL ((_, s): Lens<'a,'b>) = s

    /// Modify a value using a lens
    let modL ((g, s): Lens<'a,'b>) = fun f a -> s (f (g a)) a

    // Partial Lens

    /// Get a value option using a partial lens
    let getPL ((g, _): PLens<'a,'b>) = g

    /// Set a value using a partial lens
    let setPL ((_, s): PLens<'a,'b>) = s

    /// Modify a value using a partial lens
    let modPL ((g, s): PLens<'a,'b>) = fun f a -> Option.map f (g a) |> function | Some b -> s b a | _ -> a


[<AutoOpen>]
module internal Composition =

    let composePartialTotalGet l1 l2 =
        fun a -> Option.map (get l2) (get l1 a)

    let composePartialPartialGet l1 l2 =
        fun a -> Option.bind (get l2) (get l1 a)

    let composePartialSet l1 l2 =
        fun c a -> Option.map (set l2 c) (get l1 a) |> function | Some b -> set l1 b a | _ -> a

    let composeGet l1 l2 =
        fun a -> get l2 (get l1 a)

    let composeSet l1 l2 =
        fun c a -> set l1 (set l2 c (get l1 a)) a


[<AutoOpen>]
module Operators =

    // Composition

    /// Compose two lenses
    let (>-->) (l1: Lens<'a,'b>) (l2: Lens<'b,'c>) : Lens<'a,'c> =
        composeGet l1 l2, composeSet l1 l2

    /// Compose a lens and a partial lens, giving a partial lens
    let (>-?>) (l1: Lens<'a,'b>) (l2: PLens<'b,'c>) : PLens<'a,'c> =
        composeGet l1 l2, composeSet l1 l2

    /// Compose a lens and a partial lens, giving a partial lens
    let (>?->) (l1: PLens<'a,'b>) (l2: Lens<'b,'c>) : PLens<'a,'c> =
        composePartialTotalGet l1 l2, composePartialSet l1 l2

    /// Compose two partial lenses, giving partial lens
    let (>??>) (l1: PLens<'a,'b>) (l2: PLens<'b,'c>) : PLens<'a,'c> =
        composePartialPartialGet l1 l2, composePartialSet l1 l2

    // Functions

    /// Get a value using a lens
    let (^.) (l: Lens<'a,'b>) (a: 'a) : 'b =
        getL l a

    /// Set a value using a lens
    let (^=) (l: Lens<'a,'b>) (b: 'b) : 'a -> 'a =
        setL l b

    /// Modify a value using a lens
    let (^%=) (l: Lens<'a,'b>) (f: 'b -> 'b) : 'a -> 'a =
        modL l f


[<AutoOpen>]
module Lenses =

    // Isomorphisms

    /// Total isomorphism giving a lens
    let isoL (ab: 'a -> 'b) (ba: 'b -> 'a) : Lens<'a,'b> =
        ab, (fun b _ -> ba b)

    /// Partial isomorphism giving a partial lens
    let isoPL (ab: 'a -> 'b option) (ba: 'b -> 'a) : PLens<'a, 'b> =
        ab, (fun b _ -> ba b)

    // Lists

    /// Position of a list giving a partial lens
    let listPL (i: int) : PLens<'v list, 'v> =
        (function | l when List.length l > i -> Some (List.nth l i) | _ -> None), 
        (fun v l -> List.mapi (fun i' x -> if i = i' then v else x) l)

    // Maps

    /// Key of a map giving a partial lens
    let mapPL (k: 'k) : PLens<Map<'k,'v>,'v> =
        Map.tryFind k, Map.add k

    // Tuples
    
    /// First item of a tuple giving a lens
    let fstL (t: 'a * 'b) : Lens<('a * 'b),'a> =
        fst, (fun a t -> a, snd t)
        
    /// Second item of a tuple giving a lens
    let sndL (t: 'a * 'b) : Lens<('a * 'b),'b> =
        snd, (fun b t -> fst t, b)
