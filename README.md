psa
===

A pretty, flexible error/warning reporting frontend for the PureScript compiler
(`psc`).

* Colors!
* Original source spans in errors
* Fine-grained warning filtering
* Warning persistence

Install
-------

```
npm install -g purescript-psa
```

Sample Usage
------------

Censor all warnings:
```
psa --censor-warnings <psc-options>
```

Censor library warnings:
```
psa --censor-lib <psc-options>
```

Censor source warnings:
```
psa --censor-src <psc-options>
```

Censor specific warning codes:
```
psa --censor-codes=ShadowedName,ImplicitImport,MissingTypeDeclaration <psc-options>
```

Only show specific warning codes:
```
psa --filter-codes=DeprecatedOperatorDecl,DeprecatedClassExport <psc-options>
```

Turn source warnings into errors:
```
psa --strict <psc-options>
```

**Note:** It's assumed `psc` is in your path. If you'd like to use a custom
binary location you can set the `--psc=/foo/bar/psc` flag.

Persisting Warnings
-------------------

`psc` does not persist warnings between compilations, but `psa` can do it with
the `--stash` flag. This serializes the set of warnings to disk and merges it
with the new set on each compilation.

```
psa --stash <psc-options>
```

If you are compiling multiple projects from the same root, you can specify
which stash file should be used:

```
psa --stash=.foo-stash <psc-options>
```

Usage with pulp
---------------

[Pulp](https://github.com/bodil/pulp) supports building with `psa`: it will be used by default if it is installed, and options will be passed through, eg:

```
pulp build -- --stash --censor-lib <psc-options>
```
