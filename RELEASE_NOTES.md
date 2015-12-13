# Release Notes

## 8.0.0

Hello to the version 8 release! The big changes in 8 are about simplification.

### Composition

The old multitude of methods for composition are now obsolete:

```fsharp
// Old
Compose.lensWithLens l1 l2
Compose.lensWithPrism l p
Compose.<etc> 

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
