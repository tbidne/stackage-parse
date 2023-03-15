<div align="center">

# stackage-parse

[![GitHub release (latest SemVer)](https://img.shields.io/github/v/release/tbidne/stackage-parse?include_prereleases&sort=semver)](https://github.com/tbidne/stackage-parse/releases/)
![haskell](https://img.shields.io/static/v1?label=&message=9.4&logo=haskell&logoColor=655889&labelColor=2f353e&color=655889)
[![MIT](https://img.shields.io/github/license/tbidne/stackage-parse?color=blue)](https://opensource.org/licenses/MIT)

[![nix](http://img.shields.io/github/actions/workflow/status/tbidne/stackage-parse/nix.yaml?branch=main&label=nix&logo=nixos&logoColor=85c5e7&labelColor=2f353c)](https://github.com/tbidne/stackage-parse/actions/workflows/nix.yaml)
[![cabal](http://img.shields.io/github/actions/workflow/status/tbidne/stackage-parse/cabal.yaml?branch=main&label=cabal&labelColor=2f353c)](https://github.com/tbidne/stackage-parse/actions/workflows/cabal.yaml)
[![style](http://img.shields.io/github/actions/workflow/status/tbidne/stackage-parse/style.yaml?branch=main&label=style&logoColor=white&labelColor=2f353c)](https://github.com/tbidne/stackage-parse/actions/workflows/style.yaml)

</div>

---

# Introduction

`stackage-parse` is a CLI utility for retrieving `stackage` snapshot data from [stackage.org](www.stackage.org).

```
stackage-parse: A tool for parsing stackage snapshot data.

Usage: stackage-parse [--lts (latest|LTS_STR)] [--nightly (latest|DATE_STR)]
                      COMMAND [-v|--version]

Available options:
  --lts (latest|LTS_STR)   LTS snapshot e.g. 20.14 or the string 'latest'.
                           Overridden by --nightly.
  --nightly (latest|DATE_STR)
                           Nightly snapshot e.g. 2023-03-14 or the string
                           'latest'. Overrides --lts.
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

**Description:** Lists all packages in a given snapshot. Obviously the package list is extremely long, so the examples here are truncated.

**Usage:**

```
Usage: stackage-parse pkgs [-f|--format (short|cabal)]

  Lists all packages in a given snapshot.

Available options:
  -f,--format (short|cabal)
                           Short corresponds to <pkg-name>-<vers> e.g.
                           'text-2.0.1'.Cabal corresponds to format suitable to
                           be pasted into a cabal file's 'build-depends' e.g.
                           'text ==2.0.1'. Defaults to short.
  -h,--help                Show this help text
```

**Examples:**

Omitting the `--format` option is equivalent to `--format short`.

```
$ stackage-parse pkgs
abstract-deque-0.3
abstract-deque-tests-0.3
abstract-par-0.3.3
AC-Angle-1.0
acc-0.2.0.2
...
```

Alternatively, `--format cabal` will produce output suitable for use in a cabal file's `build-depends`:

```
# retrieving stackage lts in cabal format
$ stackage-parse --lts 20.14 pkgs -f cabal
abstract-deque ==0.3
abstract-deque-tests ==0.3
abstract-par ==0.3.3
AC-Angle ==1.0
acc ==0.2.0.1
...
```

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