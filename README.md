# Aether

[![Build status](https://ci.appveyor.com/api/projects/status/uch7r4f3ivnb5bxe)](https://ci.appveyor.com/project/kolektiv/aether)

## What is Aether?

Aether is a lens library for F# roughly based on the Haskell Data.Lens package.

## What is a lens?

Lenses are functional properties; they're pairs of 'get' and 'set' functions that can be composed, allowing for useful operations to be built up.

[See this blog post for a brief introduction](https://kolektiv.github.io/fsharp/aether/2014/08/13/aether-guide/)

## Why should I use a lens?

Because you want to work with records and other immutable datatypes (generally data structures that have some level of complexity) in a consistent and simple way.

The default approach:
```fsharp
let fooWithNewBaz = { foo with bar = { foo.bar with baz = newBaz } }
```
The lens approach:
```fsharp
let fooToBaz_ = Foo.bar_ >--> Bar.baz_
let fooWithNewBaz = Lens.set fooToBaz_ newBaz
```
Lenses can greatly reduce boilerplate for complex type hierarchies.

## Usage

### How do I define a lens?

A lens is a pair of functions:

```fsharp
// ('a -> 'b) * ('b -> 'a -> 'a)
let bar_ =
	let get = fun foo -> foo.bar
	let set = fun newBar foo -> { foo with bar = newBar }
	(get, set)
```

### Can Aether generate lenses?

There is presently no way to generate lenses (as there is using Template Haskell for Data.Lens). [You can vote for lenses as a language feature here](https://fslang.uservoice.com/forums/245727-f-language/suggestions/6906132-implement-first-class-lensing-lenses-in-f)

Lenses are commonly defined using shorthand, and you are encouraged to do so:

```fsharp
let bar_ = (fun foo -> foo.bar), (fun newBar foo -> { foo with bar = newBar })
```

### How do I compose lenses?

```fsharp
// Lens from Foo to Bar
let bar_ = (fun foo -> foo.bar), (fun newBar foo -> { foo with bar = newBar })
// Lens from Foo to Baz
let baz_ = (fun bar -> bar.baz), (fun newBaz bar -> { bar with baz = newBaz })

// Combined lens from Foo to Baz
let fooToBaz_ = bar_ >--> baz_
```

### How do I work with the composed lenses?

```fsharp
let fooToBaz_ = bar_ >--> baz_

// retrieves the baz from someFoo.
let baz = Lens.get fooToBaz_ someFoo
// creates a new foo with the baz updated.
let newFoo = Lens.set fooToBaz_ newBaz someFoo

```

## Other types of lenses:

The above lenses are all total lenses for simplicity. Aether also provides *partial lenses* and *isomorphisms*. [See this blog post for precise information](https://kolektiv.github.io/fsharp/aether/2014/08/10/aether/)

## Conventions

It's handy to have a naming convention for things which are lenses (or like lenses). After discussion, the convention has been that a lens is generally named after the property, with an additional underscore. So if you had a record, you might define a property of that record and the associated lens like so:

```fsharp
type Foo =
    { Bar: string }

    static member Bar_ =
        (fun x -> x.Bar), (fun b x -> { x with Bar = b })
```

You may also wish to define lenses in modules, where the same convention applies. It is generally expected that the lens name, if it is public, will be capitalized. The alternative approach to providing a lens for the `Bar` type above using a module might look like so:

```fsharp
[<RequireQualifiedAccess>]
module Foo =

    let Bar_ =
        (fun x -> x.Bar), (fun b x -> { x with Bar = b }) 
```

The convention holds whether the lens is total or partial, and also applies to isomorphisms.

## About

I've written a couple of blog posts as a quick intro to Aether which might help in addition to this README and the source. They're [here][aether-intro] and [here][aether-guide].

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
