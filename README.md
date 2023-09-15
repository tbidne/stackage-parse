<div align="center">

# stackage-parse

[![ci](http://img.shields.io/github/actions/workflow/status/tbidne/stackage-parse/ci.yaml?branch=main)](https://github.com/tbidne/stackage-parse/actions/workflows/ci.yaml)
[![MIT](https://img.shields.io/github/license/tbidne/stackage-parse?color=blue)](https://opensource.org/licenses/MIT)

</div>

---

# Introduction

`stackage-parse` is a CLI utility for retrieving `stackage` snapshot data from [stackage.org](www.stackage.org). The primary motivation is testing with [clc-stackage](https://github.com/Bodigrim/clc-stackage): that is, easily updating a cabal file with every package from the latest stackage snapshot. See the [Pkgs](#pkgs) command for this specific workflow.

**Usage:**

```
stackage-parse: A tool for parsing stackage snapshot data.

Usage: stackage-parse [--lts (latest|LTS_STR)] [--nightly (latest|DATE_STR)]
                      [-e|--exclude PATH] [-i|--include PATH] COMMAND
                      [-v|--version]

Available options:
  --lts (latest|LTS_STR)   LTS snapshot e.g. 20.14 or the string 'latest'.
                           Overridden by --nightly.

  --nightly (latest|DATE_STR)
                           Nightly snapshot e.g. 2023-03-14 or the string
                           'latest'. Overrides --lts.

  -e,--exclude PATH        Path to file with a list of packages to exclude from
                           the package list. Each package should be listed on a
                           separate line, without version numbers e.g. text.

  -i,--include PATH        Path to file with a list of packages to include from
                           the package list. Each package should be listed on a
                           separate line, without version numbers e.g. text.

  -h,--help                Show this help text

Available commands:
  full                     Prints full package list and snapshot metadata
                           formatted as json.

  pkgs                     Lists all packages in a given snapshot.

  snapshot                 Prints snapshot metadata formatted as json.


Version: 0.1
```

# Commands

## Pkgs

**Description:** Lists all packages in a given snapshot. The package list is extremely long, so the examples here are truncated.

**Usage:**

```
Usage: stackage-parse pkgs [-f|--format (short|cabal)]
                           [-c|--comma (append|prepend)]

  Lists all packages in a given snapshot.


Available options:
  -f,--format (short|cabal)
                           Short corresponds to <pkg-name>-<vers> e.g.
                           'text-2.0.1'.Cabal corresponds to format suitable to
                           be pasted into a cabal file's 'build-depends' e.g.
                           'text ==2.0.1'. Defaults to short.

  -c,--comma (append|prepend)
                           If given, prepends/appends a comma before/after each
                           entry.

  -h,--help                Show this help text
```

**Examples:**

As producing cabal-compatible stackage output is the main motivation, let us get right down to it:

```
$ stackage-parse pkgs -f cabal -c append
abstract-deque ==0.3,
abstract-deque-tests ==0.3,
abstract-par ==0.3.3,
AC-Angle ==1.0,
acc ==0.2.0.1,
...
```

* `pkgs` is the command.
* `-f cabal` outputs in `cabal` format i.e. `abstract-par ==0.3.3` instead of `abstract-par-0.3.3`.
* `-c append` appends a comma to each line.

This will produce a list of every package in the specified stackage snapshot (`stackage nightly` in this example). Because we may not want to include _all_ of stackage (e.g. executables, packages relying on external C libs), there are two ways to refine this list.

### Exclude

```
$ stackage-parse --exclude exclude-file pkgs -f cabal -c append
abstract-deque ==0.3,
...
```

Same as before, except any packages in `exclude-file` will be excluded from the package list. We can use this to exclude packages we **know** we do not want (e.g. because they will not build with our package).

The [clc-stackage/exclude](clc-stackage/exclude) directory contains files for building most of stackage, excluding packages like executables or those that require system libs. For instance, to get an appropriate `build-depends` for GHC 9.4.4:

```
$ stackage-parse --nightly 2023-04-05 -e clc-stackage/exclude/nightly-2023-04-05 pkgs -f cabal -c append
abstract-deque ==0.3,
...
```

### Include

```
$ stackage-parse --include include-file pkgs -f cabal -c append
abstract-deque ==0.3,
...
```

Like `--exclude`, except only packages _in_ `include-file` will be included in the package list. We can use this to, say, upgrade only the packages from a previous snapshot without adding any new, potentially unbuildable packages.

See [examples](examples/) for more details.

## Snapshot

**Description:** Prints snapshot metadata as `json`. Here we use the `jq` utility to pretty-print the result.

**Usage:**
```
Usage: stackage-parse snapshot

  Prints snapshot metadata formatted as json.


Available options:
  -h,--help                Show this help text
```

**Examples:**

```
$ stackage-parse snapshot | jq
{
  "compiler": "ghc-9.4.4",
  "created": "2023-03-14",
  "ghc": "9.4.4",
  "name": "nightly-2023-03-14"
}
```

## Full

**Description:** Returns the package list and metadata as json i.e. combined `pkgs` and `snapshot` commands.

**Usage:**

```
Usage: stackage-parse full

  Prints full package list and snapshot metadata formatted as json.


Available options:
  -h,--help                Show this help text
```

**Examples:**

```
$ stackage-parse --nightly 2023-03-14 full | jq
{
  "packages": [
    <extremely long list omitted>
  ],
  "snapshot": {
    "compiler": "ghc-9.4.4",
    "created": "2023-03-14",
    "ghc": "9.4.4",
    "name": "nightly-2023-03-14"
  }
}
```