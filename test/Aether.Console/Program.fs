open Aether

// Types (with Lenses)

type A =
    { One: B 
      Two: B option }

    static member oneT =
        (fun x -> x.One), (fun o x -> { x with A.One = o })

    static member twoP =
        (fun x -> x.Two), (fun t x -> { x with A.Two = Some t })

    static member twoT =
        (fun x -> x.Two), (fun t x -> { x with A.Two = t })

and B =
    { One: string
      Two: string option }

    static member oneT =
        (fun x -> x.One), (fun o x -> { x with B.One = o })

    static member twoP =
        (fun x -> x.Two), (fun t x -> { x with B.Two = Some t })

    static member twoT =
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

// Main

[<EntryPoint>]
let main _ =

    let a1b1TT = A.oneT >--> B.oneT
    let a1b2TT = A.oneT >--> B.twoT

    let a2b1PT = A.twoP >?-> B.oneT

    let a1b2TP = A.oneT >-?> B.twoP
    let a2b2PP = A.twoP >??> B.twoP
    let a2b2PT = A.twoP >?-> B.twoT

    let x = getP a2b2PT a


    0
