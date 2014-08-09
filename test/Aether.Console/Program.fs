open Aether

// Types (with Lenses)

type Details =
    { Name: string
      Timing: Timing
      Prefix: Prefix option }

    static member nameT =
        (fun x -> x.Name), (fun n x -> { x with Name = n })

    static member timingT =
        (fun x -> x.Timing), (fun t x -> { x with Timing = t })

    static member prefixP =
        (fun x -> x.Prefix), (fun p x -> { x with Prefix = Some p })

and Timing =
    { Age: int
      Other: string option }

    static member ageT =
        (fun x -> x.Age), (fun a x -> { x with Age = a })

    static member otherP =
        (fun x -> x.Other), (fun o x -> { x with Other = Some o })

    static member otherT =
        (fun x -> x.Other), (fun o x -> { x with Other = o })

and Prefix =
    { Title: string option }

    static member titleP =
        (fun x -> x.Title), (fun t x -> { x with Title = Some t })

// Main

[<EntryPoint>]
let main _ =

    let example =
        { Name = "Andrew"
          Timing =
            { Age = 33
              Other = Some "foo" }
          Prefix =
            Some {
              Title = Some "Mr" } }
    
    let ageT = Details.timingT >--> Timing.ageT
    let titleP = Details.prefixP >??> Prefix.titleP
    let otherP = Details.timingT >-?> Timing.otherP

    let age = getT ageT example
    let ex1 = setT ageT 34 example
    let ex2 = updT ageT ((*) 2) example

    let title = getP titleP example
    let ex3 = setP titleP "Dr" example
    let ex4 = updP titleP (String.replicate 3) example

    let other = getP otherP example
    let ex5 = setP otherP "bar" example

    0
