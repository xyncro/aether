module Aether.Operators

(* Composition Operators

   Operators as syntactical alternatives to more verbose composition
   functions given. These are expected to be much more commonly used
   and syntactially provide more clues as to their function. *)

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

/// Compose a total lens with a partial isomorphism, giving a partial lens
let (<-?>) l i =
    composeLPI l i

/// Compose a partial lens with a total isomorphism, giving a partial lens
let (<?->) l i =
    composePLI l i

/// Compose a partial lens with a partial isomorphism, giving a partial lens
let (<??>) l i =
    composePLPI l i

(* Function Operators

   Operators as infix alternatives to some of the standard get, set,
   modify functions (getL, setL, etc.) Should likely be used rather 
   sparingly and in specific controlled areas unless you're aiming for 
   symbol soup. *)

/// Get a value using a total lens
let (^.) (a: 'a) (l: Lens<'a,'b>) : 'b =
    getL l a

/// Get a value using a partial lens
let (^?.) (a: 'a) (l: PLens<'a,'b>) : 'b option =
    getPL l a

/// Set a value using a total lens
let (^=) (b: 'b) (l: Lens<'a,'b>) : 'a -> 'a =
    setL l b

/// Set a value using a partial lens
let (^?=) (b: 'b) (l: PLens<'a,'b>) : 'a -> 'a =
    setPL l b

/// Modify a value using a total lens
let (^%=) (f: 'b -> 'b) (l: Lens<'a,'b>) : 'a -> 'a =
    modL l f

/// Modify a value using a partial lens
let (^?%=) (f: 'b -> 'b) (l: PLens<'a,'b>) : 'a -> 'a =
    modPL l f