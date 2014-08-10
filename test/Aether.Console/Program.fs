open System
open System.Text
open Aether

// Types (with Lenses)

type A =
    { One: B 
      Two: B option }

    static member one =
        (fun x -> x.One), (fun o x -> { x with A.One = o })

    static member twoP =
        (fun x -> x.Two), (fun t x -> { x with A.Two = Some t })

    static member two =
        (fun x -> x.Two), (fun t x -> { x with A.Two = t })

and B =
    { One: string
      Two: string option }

    static member one =
        (fun x -> x.One), (fun o x -> { x with B.One = o })

    static member twoP =
        (fun x -> x.Two), (fun t x -> { x with B.Two = Some t })

    static member two =
        (fun x -> x.Two), (fun t x -> { x with B.Two = t })

// Example

let a : A =
    { One = 
        { One = "Hello"
          Two = Some "World" }
      Two =
        Some {
          One = "Goodbye"
          Two = Some "Universe" } }

// Helpers 

let rev (s: string) = String (Array.rev (s.ToCharArray ()))

// Main

[<EntryPoint>]
let main _ =

    let a1b1 = A.one >--> B.one
    let a1b2 = A.one >--> B.two

    let a2b1P = A.twoP >?-> B.one

    let a1b2P = A.one >-?> B.twoP
    let a2b2P = A.twoP >??> B.twoP
    let a2b2P = A.twoP >?-> B.two

    let x = a1b1 ^. a
    let aset = a1b1 ^= "Hi" <| a
    let amod = a1b1 ^%= rev <| a

    0
