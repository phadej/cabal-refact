# cabal-refact

A tool for editing `.cabal` files.

## Synopsis

```
$ grep aeson example.cabal

$ cabal-refact edit-bound aeson '<1.1' '< 1.2'

$ grep aeson example.cabal
```

*NOTE:* not yet implemented!

## Installing

Currently `cabal-refact` isn't released on Hackage. To install, clone this repository
and `cabal new-build && cp dist-newstyle/**/cabal-refact/cabal-refact ~/.local/bin`
or `stack install`

## Example refactorings

See haddock documentation for the rest of the refactorings.

### Identity

This is refactoring is important for testing that parser and pretty-printer
are able to do formatting-preserving roundtrip.

```
$ cabal-refact identity --dry fixtures/increase-revision/focus.cabal
no changes
```

### Increase revision

Increase `x-revision` counter, or add `x-revision: 1` after the `name` field, if
`x-revision` field doesn't exist.

```
$ cabal-refact increase-revision --dry fixtures/increase-revision/focus.cabal
========================================================================
  name:
    focus
+ x-revision:
+   1
  version:
    0.1.5
  synopsis:
```

### Populate extra source files

Populate `extra-source-files` field using comment pragma.

```
$ cabal-refact populate-extra-source-files --dry cabal-refact.cabal
========================================================================
      CHANGELOG.md
      README.md
      -- cabal-refact-populate: fixtures/**/*.*
+     fixtures/identity/Agda.cabal
+     fixtures/identity/JuicyPixels.cabal
+     fixtures/identity/QuickCheck.cabal
...
```
