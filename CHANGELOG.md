### v0.8.2 (2021-01-25)

* Don't swallow "Compiling..." messages (@hdgarrood)

### v0.8.1 (2021-01-23)

* Fix ignored `--purs=` command-line argument (@joneshf)

### v0.8.0 (2020-10-01)

* Add .spago to lib dirs by default (@d0liver)
* Support both 0.13 and 0.14 compiler versions (@hdgarrood)

### v0.7.3 (2018-08-23)

* Fix file not found exception when processing errors that have no file information

### v0.7.2 (2018-07-20)

* Fix lib detection when purs emits an absolute path

### v0.7.1 (2018-07-18)

* Removed stray log statement

### v0.7.0 (2018-07-13)

* Account for changes in 0.12 for relative paths in errors

### v0.6.0 (2018-01-06)

* Added `--censor-stats` option (@Rufflewind)

### v0.5.1 (2017-04-08)

* Fix parsing of `--purs` options (@bfly2000)

### v0.5.0 (2017-03-28)

* Updated to call `purs build`.

### v0.4.0 (2016-11-19)

* Filtering/censoring warnings is applied before --strict

### v0.3.9 (2016-6-4)

* Support for 0.9.x suggestions

### v0.3.8 (2016-5-8)

* Always show errors, regardless of filter/censor configuration

### v0.3.7 (2016-4-18)

* Apply filters and stash to `--json-errors`

### v0.3.6 (2016-4-15)

* Fix edge-case position trimming
* Bump dependencies

### v0.3.5 (2016-3-5)

* Retry with `.cmd` on Windows
* Proxy psc output when called with `--json-errors` (for tooling)

### v0.3.4 (2016-2-20)

* Only read the stash file when `--stash` is set

### v0.3.3 (2016-2-20)

* Purge stale `--stash` warnings based on file modification time

### v0.3.2 (2016-1-22)

* Better handling of `--is-lib` as a real directory

### v0.3.0 (2016-1-20)

* Add `--strict` flag
* Change `--lib-dir` to `--is-lib` for clarity

### v0.2.1 (2016-1-18)

* Always print to stderr

### v0.2.0 (2016-1-12)

* Fix stale stash bug
* Add `--verbose-stats` flag

### v0.1.3 (2016-1-10)

* Add `--stash` options for persisting warnings

### v0.1.2 (2016-1-8)

* Fix stdout piping

### v0.1.1 (2016-1-8)

* Pipe psc stdout

### v0.1.0 (2016-1-8)

* Initial release
