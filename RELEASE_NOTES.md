# Release Notes

## 8.0.0 - 2015.12.13

Hello to the version 8 release! The big changes in 8 are about simplification. This release also marks the release of the new dedicated [Aether Site][aether] with guides, reference, etc.

### Composition

The old multitude of functions for composition are now obsolete:

```fsharp
// Old
Compose.lensWithLens l1 l2
Compose.lensWithPrism l p
...

// New
Compose.lens l x
Compose.prism p x
```
Operators for composition have also been dramatically simplified, as they match the functions:

```fsharp
// Old
>--> // Compose.lensWithLens
>-?> // Compose.lensWithPrism
<--> // Compose.lensWithIsomorphism
... 

// New
>->  // Compose.lens
>?>  // Compose.prism
```

### Operations

The old distinct sets of functions for operations, with different methods for lensed and prisms (previously called partial lenses) have been unified:

```fsharp
// Old
Lens.get
Lens.set
Lens.map

Prism.get
Prism.set
Prism.map

// (Even) Older
Lens.get
Lens.getPartial
...

// New
Optic.get
Optic.set
Optic.map
```

As with the composition functions, the operators have also been simplified:

```fsharp
// New
^. // Optic.get
^= // Optic.set
^% // Optic.map
```

### Miscellaneous

Some provided lenses and prisms, etc. have been moved and corrected. The [Reference][reference] on the Aether site has more on what's available within Aether.

[aether]: https://xyncro.tech/aether
[reference]: https://xyncro.tech/aether/reference
