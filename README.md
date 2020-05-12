# Aether

[![Chat](https://badges.gitter.im/Join%20Chat.svg)](https://gitter.im/xyncro/aether?utm_source=badge&utm_medium=badge&utm_campaign=pr-badge&utm_content=badge)
[![Build](https://ci.appveyor.com/api/projects/status/3jl24iaac9ka3ov2?svg=true)](https://ci.appveyor.com/project/xyncro/aether)


## What is Aether?

[Aether][aether] is an Optics library for F#, similar to the Haskell Data.Lens package. See the dedicated [Aether Site][aether] for more information, including guides, reference material, updates, etc.

## Installation

Aether can be installed from [NuGet](https://www.nuget.org/packages/aether "Aether on NuGet"). Using the Package Manager Console:

```batch
PM> Install-Package Aether
```

You can also install directly from GitHub using [Paket](https://fsprojects.github.io/Paket/). This will work with [Fable](https://fable.io/). 

Add this line to your `paket.dependencies`:

```
github xyncro/aether:8.3.1 src/Aether/Aether.fs
```

And add this to your `paket.references`:

```
File: Aether.fs
```

## License

Aether is under the MIT license.

[aether]: https://xyncro.tech/aether
