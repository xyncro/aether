# Aether

[![Build status](https://ci.appveyor.com/api/projects/status/uch7r4f3ivnb5bxe)](https://ci.appveyor.com/project/kolektiv/aether)

## What is Aether?

Aether is a lens library for F# roughly based on the Haskell Data.Lens package.

## What is a lens?

Lenses are functional properties; they're pairs of get and 'set' functions that can be composed.

[See this blog post for a brief introduction](https://kolektiv.github.io/fsharp/aether/2014/08/13/aether-guide/)

## Why should I use a lens?

Because you want to work with records in a consistent and simple way.

The default approach:
```fsharp
let fooWithNewBaz = { foo with bar = { foo.bar with baz = newBaz }}}
```
The lens approach:
```fsharp
let fooToBazLens = Foo.barLens >--> Bar.bazLens
let fooWithNewBaz = Lens.set fooToBazLens newBaz
```

 Lenses can greatly reduce boilerplate for complex type hierarchies.

## Usage

### How do I define a lens?

A lens is a pair of functions:

```fsharp
// ('a -> 'b) * ('b -> 'a -> 'a)
let barLens =
	let get = fun foo -> foo.bar
	let set = fun newBar foo -> {foo with bar = newBar}
	(get, set)
```

### Can Aether generate lenses?

There is presently no way to generate lenses. [You can vote for lenses as a language feature here](https://fslang.uservoice.com/forums/245727-f-language/suggestions/6906132-implement-first-class-lensing-lenses-in-f)

Lenses are commonly defined using shorthand, and you are encouraged to do so:

```fsharp
let barLens = (fun foo -> fo.bar), (fun newBar foo -> {foo with bar = newBar})
```

### How do I compose lenses?

```fsharp
// Lens from Foo to Bar
let barLens = (fun foo -> fo.bar), (fun newBar foo -> {foo with bar = newBar})
// Lens from Foo to Baz
let bazLens = (fun bar -> bar.baz), (fun newBaz bar -> {bar with baz = newBaz})

// Combined lens from Foo to Baz
let fooToBaz = barLens >--> bazLens
```

### How do I work with the composed lenses?

```fsharp
let fooToBaz = barLens >--> bazLens

// retrieves the baz from someFoo.
let baz = Lens.get fooToBaz someFoo
// creates a new foo with the baz updated.
let newFoo = Lens.set fooToBaz newBaz someFoo

```

## Other types of lenses:

The above lenses are all total lenses for simplicity. Aether also provides *Partial lenses* and *Isomorphisms*. [See this blog post for precise information](https://kolektiv.github.io/fsharp/aether/2014/08/10/aether/)

## About

__NOTE: Aether is very new. It almost certainly has horrible and foolish bugs that I haven't found/understood yet. If you wish to try it, feel free to do so, but be aware of that! Pull requests to fix my shortage of talent welcome.__

Aether is a lens library for F# roughly based on the Haskell Data.Lens package. It draws a distinction between Total and Partial lenses, providing separate operators and functions for composing and working with lenses that are either total or partial, along with a few common lenses for F# types.

I've written a couple of blog posts as a quick intro to Aether which might help in addition to the source. They're [here][aether-intro] and [here][aether-guide].

Thanks and acknowledgements must go to the authors of the Haskell lens libraries, and to Mauricio Scheffer for his work on lenses as part of FSharpx.

## Installation

Aether can be installed from [NuGet](https://www.nuget.org/packages/Aether "Aether on NuGet"). Using the Package Manager Console:


```posh
PM> Install-Package Aether
```


## License

Aether is under the MIT license.

[aether-intro]: http://kolektiv.github.io/fsharp/aether/2014/08/10/aether/
[aether-guide]: http://kolektiv.github.io/fsharp/aether/2014/08/13/aether-guide/
