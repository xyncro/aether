# Aether

[![Build status](https://ci.appveyor.com/api/projects/status/uch7r4f3ivnb5bxe)](https://ci.appveyor.com/project/kolektiv/aether)

__NOTE: Aether is very new. It almost certainly has horrible and foolish bugs that I haven't found/understood yet. If you wish to try it, feel free to do so, but be aware of that! Pull requests to fix my shortage of talent welcome.__

Aether is a lens library for F# roughly based on the Haskell Data.Lens package. It draws a distinction between Total and Partial lenses, providing separate operators and functions for composing and working with lenses that are either total or partial, along with a few common lenses for F# types.

I've written a couple of blog posts as a quick intro to Aether which might help in addition to the source. They're [here][aether-intro] and [here][aether-guide].

Thanks and acknowledgements must go to the authors of the Haskell lens libraries, and to Mauricio Scheffer for his work on lenses as part of FSharpx.

## Installation

Aether can be installed from [NuGet](https://www.nuget.org/packages/Aether "Aether on NuGet"). Using the Package Manager Console:

```posh
PM> Install-Package Aether
```

[aether-intro]: http://kolektiv.github.io/fsharp/aether/2014/08/10/aether/
[aether-guide]: http://kolektiv.github.io/fsharp/aether/2014/08/13/aether-guide/
