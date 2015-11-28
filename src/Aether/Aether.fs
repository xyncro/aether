module Aether

(* Types

   Types defining lenses, prisms, and isomorphisms
   as standard pairs. These can be implemented implicitly,
   so an assembly *providing* lenses without also consuming them
   requires no dependency on Aether, just an implicit structuring. *)

/// Lens from a -> b
type Lens<'a,'b> =
    ('a -> 'b) * ('b -> 'a -> 'a)

/// Prism from a -> b
type Prism<'a,'b> =
    ('a -> 'b option) * ('b -> 'a -> 'a)

// Isomorphisms

/// Isomorphism of a <> b
type Isomorphism<'a,'b> =
    ('a -> 'b) * ('b -> 'a)

/// Epimorphism of a <> b
type Epimorphism<'a,'b> =
    ('a -> 'b option) * ('b -> 'a)

/// Functions for using lenses to get, set, and modify values on
/// product-types, such as tuples and records.
[<RequireQualifiedAccess>]
module Lens =

    /// Converts an isomorphism into a lens
    let ofIsomorphism ((f, t): Isomorphism<'a,'b>) : Lens<'a,'b> =
        f, (fun b _ -> t b)

/// Functions for using prisms to get, set, and modify values on
/// sum-types, such as discriminated unions.
[<RequireQualifiedAccess>]
module Prism =

    /// Converts an epimorphism into a prism
    let ofEpimorphism ((f, t): Epimorphism<'a,'b>) : Prism<'a,'b> =
        f, (fun b _ -> t b)

[<RequireQualifiedAccess>]
module Optic =

    type Get =
        | Get with

        static member (^.) (Get, (g, _): Lens<'a,'b>) =
            fun (a: 'a) ->
                g a : 'b

        static member (^.) (Get, (g, _): Prism<'a,'b>) =
            fun (a: 'a) ->
                g a : 'b option

    /// Get a value using an optic
    let inline get ab a =
        (Get ^. ab) a

    type Set =
        | Set with

        static member (^=) (Set, (_, s): Lens<'a,'b>) =
            fun (b: 'b) ->
                s b : 'a -> 'a

        static member (^=) (Set, (_, s): Prism<'a,'b>) =
            fun (b: 'b) ->
                s b : 'a -> 'a

    /// Set a value using an optic
    let inline set ab b =
        (Set ^= ab) b

    type Map =
        | Map with

        static member (^%) (Map, (g, s): Lens<'a,'b>) =
            fun (f: 'b -> 'b) ->
                (fun a -> s (f (g a)) a) : 'a -> 'a

        static member (^%) (Map, (g, s): Prism<'a,'b>) =
            fun (f: 'b -> 'b) ->
                (fun a -> Option.map f (g a) |> function | Some b -> s b a 
                                                         | _ -> a) : 'a -> 'a

    /// Modify a value using an optic
    let inline map ab f =
        (Map ^% ab) f

/// Functions for composing lenses, prisms, and isomorphisms, each of which
/// returns a new lens or prism based on the lenses, prisms,
/// or isomorphisms composed.
///
/// Open `Aether.Operators` to use the infix operator forms of these compositions,
/// which is significantly less verbose.
[<RequireQualifiedAccess>]
module Compose =

    type Lens =
        | Lens with

        static member (>-) (Lens, (g2, s2): Lens<'b,'c>) =
            fun ((g1, s1): Lens<'a,'b>) ->
                (fun a -> g2 (g1 a)),
                (fun c a -> s1 (s2 c (g1 a)) a) : Lens<'a,'c>
        
        static member (>-) (Lens, (g2, s2): Prism<'b,'c>) =
            fun ((g1, s1): Lens<'a,'b>) ->
                (fun a -> g2 (g1 a)),
                (fun c a -> s1 (s2 c (g1 a)) a) : Prism<'a,'c>

        static member (>-) (Lens, (f, t): Isomorphism<'b,'c>) =
            fun ((g, s): Lens<'a,'b>) ->
                (fun a -> f (g a)),
                (fun c a -> s (t c) a) : Lens<'a,'c>

        static member (>-) (Lens, (f, t): Epimorphism<'b,'c>) =
            fun ((g, s): Lens<'a,'b>) ->
                (fun a -> f (g a)),
                (fun c a -> s (t c) a) : Prism<'a,'c>

    /// Compose a lens with another optic
    let inline lens l o =
        (Lens >- o) l

    type Prism = 
        | Prism with

        static member (>?) (Prism, (g2, s2): Lens<'b,'c>) =
            fun ((g1, s1): Prism<'a,'b>) ->
                (fun a -> Option.map g2 (g1 a)),
                (fun c a -> Option.map (s2 c) (g1 a) |> function | Some b -> s1 b a
                                                                 | _ -> a) : Prism<'a,'c>
        
        static member (>?) (Prism, (g2, s2): Prism<'b,'c>) =
            fun ((g1, s1): Prism<'a,'b>) ->
                (fun a -> Option.bind g2 (g1 a)),
                (fun c a -> Option.map (s2 c) (g1 a) |> function | Some b -> s1 b a
                                                                 | _ -> a) : Prism<'a,'c>

        static member (>?) (Prism, (f, t): Isomorphism<'b,'c>) =
            fun ((g, s): Prism<'a,'b>) ->
                (fun a -> Option.map f (g a)),
                (fun c a -> s (t c) a) : Prism<'a,'c>

        static member (>?) (Prism, (f, t): Epimorphism<'b,'c>) =
            fun ((g, s): Prism<'a,'b>) ->
                (fun a -> Option.bind f (g a)),
                (fun c a -> s (t c) a) : Prism<'a,'c>

    /// Compose a prism with another optic
    let inline prism p o =
        (Prism >? o) p

/// Various optics implemented for common types such as tuples,
/// lists and maps, along with an id_ lens.
[<AutoOpen>]
module Optics =

    let id_ : Lens<'a,'a> =
        (fun x -> x),
        (fun x _ -> x)

    /// Lens to the first item of a tuple
    let fst_ : Lens<('a * 'b),'a> =
        fst,
        (fun a t -> a, snd t)

    /// Lens to the second item of a tuple
    let snd_ : Lens<('a * 'b),'b> =
        snd,
        (fun b t -> fst t, b)

    [<RequireQualifiedAccess>]
    module Array =

        /// Isomorphism to an list
        let list_ : Isomorphism<'v[], 'v list> =
            Array.toList,
            Array.ofList

    [<RequireQualifiedAccess>]
    module Choice =

        /// Prism to Choice1Of2
        let choice1Of2_ : Prism<Choice<_,_>, _> =
            (fun x ->
                match x with
                | Choice1Of2 v -> Some v 
                | _ -> None),
            (fun v x ->
                match x with
                | Choice1Of2 _ -> Choice1Of2 v
                | _ -> x)

        /// Prism to Choice2Of2
        let choice2Of2_ : Prism<Choice<_,_>, _> =
            (fun x ->
                match x with
                | Choice2Of2 v -> Some v
                | _ -> None),
            (fun v x ->
                match x with
                | Choice2Of2 _ -> Choice2Of2 v
                | _ -> x)

    [<RequireQualifiedAccess>]
    module List =

        /// Prism to the head of a list
        let head_ : Prism<'v list, 'v> =
            (function | h :: _ -> Some h
                      | _ -> None),
            (fun v ->
                function | _ :: t -> v :: t 
                         | l -> l)

        /// Prism to an indexed element in a list
        let pos_ (i: int) : Prism<'v list, 'v> =
            (function | l when List.length l > i -> Some (List.nth l i)
                      | _ -> None),
            (fun v l ->
                List.mapi (fun i' x -> if i = i' then v else x) l)

        /// Prism to the tail of a list
        let tail_ : Prism<'v list, 'v list> =
            (function | _ :: t -> Some t
                      | _ -> None),
            (fun t ->
                function | h :: _ -> h :: t
                         | [] -> [])

        /// Isomorphism to an array
        let array_ : Isomorphism<'v list, 'v[]> =
            List.toArray,
            List.ofArray

    [<RequireQualifiedAccess>]
    module Map =

        /// Prism to a value associated with a key in a map
        let key_ (k: 'k) : Prism<Map<'k,'v>,'v> =
            Map.tryFind k,
            (fun v x ->
                if Map.containsKey k x then Map.add k v x else x)

        /// Lens to a value option associated with a key in a map
        let value_ (k: 'k) : Lens<Map<'k,'v>, 'v option> =
            Map.tryFind k,
            (fun v x ->
                match v with
                | Some v -> Map.add k v x
                | _ -> Map.remove k x)

        /// Weak Isomorphism to an array of key-value pairs
        let array_ : Isomorphism<Map<'k,'v>, ('k * 'v)[]> =
            Map.toArray,
            Map.ofArray

        /// Weak Isomorphism to a list of key-value pairs
        let list_ : Isomorphism<Map<'k,'v>, ('k * 'v) list> =
            Map.toList,
            Map.ofList

    [<RequireQualifiedAccess>]
    module Option =

        /// Prism to the value in an Option
        let value_ : Prism<'v option, 'v> =
            id,
            (fun v ->
                function | Some _ -> Some v
                         | None -> None)

/// Optional custom operators for composing optics. Provided as syntactic
/// alternatives to more verbose composition functions in `Aether.Compose`.
module Operators =

    (* Compositions *)

    /// Compose a lens with another optic
    let inline (>-) l o =
        Compose.lens l o

    /// Compose a prism with another optic
    let inline (>?) p o =
        Compose.prism p o

    (* Operations *)

    /// Get a value using an optic
    let inline (^.) a ab =
        Optic.get ab a

    /// Set a value using an optic
    let inline (^=) b ab =
        Optic.set ab b

    /// Modify a value using a lens
    let inline (^%) f ab =
        Optic.map ab f