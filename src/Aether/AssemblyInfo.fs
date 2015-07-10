namespace System
open System.Reflection

[<assembly: AssemblyTitleAttribute("Aether")>]
[<assembly: AssemblyProductAttribute("Aether")>]
[<assembly: AssemblyDescriptionAttribute("Total/Partial Lenses for F#")>]
[<assembly: AssemblyVersionAttribute("6.1.0")>]
[<assembly: AssemblyFileVersionAttribute("6.1.0")>]
do ()

module internal AssemblyVersionInformation =
    let [<Literal>] Version = "6.1.0"
