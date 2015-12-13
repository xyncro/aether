# Release Notes

## 8.0.0

Hello to the version 8 release! The big changes in 8 are about simplification.

### Composition

The old multitude of methods for composition are now obsolete:

```fsharp
\\ Old
Compose.lensWithLens ...
Compose.lensWithPrism ...
...

\\ New
Compose.lens
Compose.prism
```
