namespace Aether


/// Lens of get/set functions which will always succeed
type Lens<'a,'b> = ('a -> 'b) * ('b -> 'a -> 'a)

/// Partial lens of get/set functions which may not succeed
type PLens<'a,'b> = ('a -> 'b option) * ('b -> 'a -> 'a)


[<AutoOpen>]
module Functions =

    let internal get (g, _) = g
    let internal set (_, s) = s
    
    /// Get a value using a lens
    let getL ((g, _): Lens<'a,'b>) = g

    /// Get a value option using a partial lens
    let getPL ((g, _): PLens<'a,'b>) = g

    /// Get a value or a default using a partial lens
    let getOrPL ((g, _): PLens<'a,'b>) (b: 'b) = fun a -> g a |> function | Some b -> b | _ -> b

    /// Set a value using a lens
    let setL ((_, s): Lens<'a,'b>) = s

    /// Set a value using a partial lens
    let setPL ((_, s): PLens<'a,'b>) = s

    /// Modify a value using a lens
    let modL ((g, s): Lens<'a,'b>) = fun f a -> s (f (g a)) a

    /// Modify a value using a partial lens
    let modPL ((g, s): PLens<'a,'b>) = fun f a -> Option.map f (g a) |> function | Some b -> s b a | _ -> a


[<AutoOpen>]
module Compositors =

    let internal composeGet l1 l2 =
        fun a -> get l2 (get l1 a)

    let internal composePartialTotalGet l1 l2 =
        fun a -> Option.map (get l2) (get l1 a)

    let internal composePartialPartialGet l1 l2 =
        fun a -> Option.bind (get l2) (get l1 a)

    let internal composeSet l1 l2 =
        fun c a -> set l1 (set l2 c (get l1 a)) a

    let internal composePartialSet l1 l2 =
        fun c a -> Option.map (set l2 c) (get l1 a) |> function | Some b -> set l1 b a | _ -> a

    /// Compose two lenses
    let composeTT (l1: Lens<'a,'b>) (l2: Lens<'b,'c>) : Lens<'a,'c> =
        composeGet l1 l2, composeSet l1 l2

    /// Compose a lens and a partial lens, giving a partial lens
    let composeTP (l1: Lens<'a,'b>) (l2: PLens<'b,'c>) : PLens<'a,'c> =
        composeGet l1 l2, composeSet l1 l2

    /// Compose a partial lens and a lens, giving a partial lens
    let composePT (l1: PLens<'a,'b>) (l2: Lens<'b,'c>) : PLens<'a,'c> =
        composePartialTotalGet l1 l2, composePartialSet l1 l2

    /// Compose two partial lenses, giving a partial lens
    let composePP (l1: PLens<'a,'b>) (l2: PLens<'b,'c>) : PLens<'a,'c> =
        composePartialPartialGet l1 l2, composePartialSet l1 l2


[<AutoOpen>]
module Lenses =

    /// Isomorphism giving a lens
    let isoLens (ab: 'a -> 'b) (ba: 'b -> 'a) : Lens<'a,'b> =
        ab, (fun b _ -> ba b)

    /// Partial isomorphism giving a partial lens
    let isoPLens (ab: 'a -> 'b option) (ba: 'b -> 'a) : PLens<'a, 'b> =
        ab, (fun b _ -> ba b)

    /// First item of a tuple giving a lens
    let fstLens (t: 'a * 'b) : Lens<('a * 'b),'a> =
        fst, (fun a t -> a, snd t)
        
    /// Second item of a tuple giving a lens
    let sndLens (t: 'a * 'b) : Lens<('a * 'b),'b> =
        snd, (fun b t -> fst t, b)

    /// Head of a list giving a partial lens
    let headPLens : PLens<'v list, 'v> =
        (function | h :: t -> Some h | _ -> None),
        (fun v -> function | h :: t -> v :: t | l -> l)

    /// Position of a list giving a partial lens
    let listPLens (i: int) : PLens<'v list, 'v> =
        (function | l when List.length l > i -> Some (List.nth l i) | _ -> None), 
        (fun v l -> List.mapi (fun i' x -> if i = i' then v else x) l)

    /// Tail of a list giving a partial lens
    let tailPLens : PLens<'v list, 'v list> =
        (function | h :: t -> Some t | _ -> None),
        (fun t -> function | h :: _ -> h :: t | l -> l)

    /// Key of a map giving a partial lens
    let mapPLens (k: 'k) : PLens<Map<'k,'v>,'v> =
        Map.tryFind k, Map.add k


module Operators =

    /// Compose two lenses
    let (>-->) l1 l2 =
        composeTT l1 l2

    /// Compose a lens and a partial lens, giving a partial lens
    let (>-?>) l1 l2 =
        composeTP l1 l2

    /// Compose a partial lens and a lens, giving a partial lens
    let (>?->) l1 l2 =
        composePT l1 l2

    /// Compose two partial lenses, giving a partial lens
    let (>??>) l1 l2 =
        composePP l1 l2


    module Infix =

        /// Get a value using a lens
        let (^.) (l: Lens<'a,'b>) (a: 'a) : 'b =
            getL l a

        /// Get a value using a partial lens
        let (^?.) (l: PLens<'a,'b>) (a: 'a) : 'b option =
            getPL l a

        /// Set a value using a lens
        let (^=) (l: Lens<'a,'b>) (b: 'b) : 'a -> 'a =
            setL l b

        /// Set a value using a partial lens
        let (^?=) (l: PLens<'a,'b>) (b: 'b) : 'a -> 'a =
            setPL l b

        /// Modify a value using a lens
        let (^%=) (l: Lens<'a,'b>) (f: 'b -> 'b) : 'a -> 'a =
            modL l f

        /// Modify a value using a partial lens
        let (^?%=) (l: PLens<'a,'b>) (f: 'b -> 'b) : 'a -> 'a =
            modPL l f
