namespace Aether

// Lenses

/// Lens of get/set functions which will always succeed
type Lens<'a,'b> = ('a -> 'b) * ('b -> 'a -> 'a)

/// Partial lens of get/set functions which may not succeed
type PLens<'a,'b> = ('a -> 'b option) * ('b -> 'a -> 'a)

// Isomorphisms

/// Isomorphism of a <> b which will always succeed
type Iso<'a,'b> = ('a -> 'b) * ('b -> 'a)

/// Isomorphism of a <> b which may not succeed
type PIso<'a,'b> = ('a -> 'b option) * ('b -> 'a)


[<AutoOpen>]
module Functions =

    /// Get a value using a lens
    let getL ((g, _): Lens<'a,'b>) = 
        g

    /// Get a value option using a partial lens
    let getPL ((g, _): PLens<'a,'b>) = 
        g

    /// Get a value or a default using a partial lens
    let getOrPL ((g, _): PLens<'a,'b>) = 
        fun b a -> g a |> function | Some b -> b | _ -> b

    /// Set a value using a lens
    let setL ((_, s): Lens<'a,'b>) =
        s

    /// Set a value using a partial lens
    let setPL ((_, s): PLens<'a,'b>) =
        s

    /// Modify a value using a lens
    let modL ((g, s): Lens<'a,'b>) = 
        fun f a -> s (f (g a)) a

    /// Modify a value using a partial lens
    let modPL ((g, s): PLens<'a,'b>) = 
        fun f a -> Option.map f (g a) |> function | Some b -> s b a | _ -> a


[<AutoOpen>]
module Composition =

    // Lenses with Lenses

    /// Compose a total lens and a total lens, giving a total lens
    let composeLL ((g1, s1): Lens<'a,'b>) ((g2, s2): Lens<'b,'c>) : Lens<'a,'c> =
        ((fun a -> g2 (g1 a)),
         (fun c a -> s1 (s2 c (g1 a)) a))

    /// Compose a lens and a partial lens, giving a partial lens
    let composeLPL ((g1, s1): Lens<'a,'b>) ((g2, s2): PLens<'b,'c>) : PLens<'a,'c> =
        ((fun a -> g2 (g1 a)),
         (fun c a -> s1 (s2 c (g1 a)) a))

    /// Compose a partial lens and a lens, giving a partial lens
    let composePLL ((g1, s1): PLens<'a,'b>) ((g2, s2): Lens<'b,'c>) : PLens<'a,'c> =
        ((fun a -> Option.map g2 (g1 a)),
         (fun c a -> Option.map (s2 c) (g1 a) |> function | Some b -> s1 b a | _ -> a))

    /// Compose two partial lenses, giving a partial lens
    let composePLPL ((g1, s1): PLens<'a,'b>) ((g2, s2): PLens<'b,'c>) : PLens<'a,'c> =
        ((fun a -> Option.bind g2 (g1 a)),
         (fun c a -> Option.map (s2 c) (g1 a) |> function | Some b -> s1 b a | _ -> a))

    // Lenses with Isomorphisms

    /// Compose a total lens with a total isomorphism, giving a total lens
    let composeLI ((g, s): Lens<'a,'b>) ((f, t): Iso<'b,'c>) : Lens<'a,'c> =
        ((fun a -> f (g a)),
         (fun c a -> s (t c) a))

    /// Compose a partial lens with a total isomorphism, giving a partial lens
    let composePLI ((g, s): PLens<'a,'b>) ((f, t): Iso<'b, 'c>) : PLens<'a,'c> =
        ((fun a -> Option.map f (g a)),
         (fun c a -> s (t c) a))


module Operators =

    /// Compose a total lens and a total lens, giving a total lens
    let (>-->) l1 l2 =
        composeLL l1 l2

    /// Compose a total lens and a partial lens, giving a partial lens
    let (>-?>) l1 l2 =
        composeLPL l1 l2

    /// Compose a partial lens and a total lens, giving a partial lens
    let (>?->) l1 l2 =
        composePLL l1 l2

    /// Compose two partial lenses, giving a partial lens
    let (>??>) l1 l2 =
        composePLPL l1 l2

    /// Compose a total lens with a total isomorphism, giving a total lens
    let (<-->) l i =
        composeLI l i

    /// Compose a partial lens with a total isomorphism, giving a partial lens
    let (<?->) l i =
        composePLI l i


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


[<AutoOpen>]
module Lenses =

    /// First item of a tuple giving a lens
    let fstLens (_: 'a * 'b) : Lens<('a * 'b),'a> =
        fst, (fun a t -> a, snd t)
        
    /// Second item of a tuple giving a lens
    let sndLens (_: 'a * 'b) : Lens<('a * 'b),'b> =
        snd, (fun b t -> fst t, b)

    /// Head of a list giving a partial lens
    let headPLens : PLens<'v list, 'v> =
        (function | h :: _ -> Some h | _ -> None),
        (fun v -> function | _ :: t -> v :: t | l -> l)

    /// Position of a list giving a partial lens
    let listPLens (i: int) : PLens<'v list, 'v> =
        (function | l when List.length l > i -> Some (List.nth l i) | _ -> None), 
        (fun v l -> List.mapi (fun i' x -> if i = i' then v else x) l)

    /// Tail of a list giving a partial lens
    let tailPLens : PLens<'v list, 'v list> =
        (function | _ :: t -> Some t | _ -> None),
        (fun t -> function | h :: _ -> h :: t | l -> l)

    /// Key of a map giving a partial lens
    let mapPLens (k: 'k) : PLens<Map<'k,'v>,'v> =
        Map.tryFind k, Map.add k