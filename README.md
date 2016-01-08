psa
===

A pretty, flexible error/warning reporting frontend for the PureScript compiler
(`psc`).

* Colors!
* Original source spans in errors
* Fine-grained warning filtering

**Note:** This requires the latest version (`0.8.0.0`) of `psc` which has not
been released yet. It's recommended to use this with `master` until then.

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

**Note:** It's assumed `psc` is in your path. If you'd like to use a custom
binary location you can set the `--psc=/foo/bar/psc` flag.
