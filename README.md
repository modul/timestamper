# Timestamper

timestamper is a commandline utility to add timestamps to file or directory names.

## Usage

```sh
timestamper [OPTIONS] [FILE]

Common flags:
  -d    --dry              Don't do anything, just print actions
  -m    --modified         Use the modification time as a timestamp (default)
  -n    --now              Use the current time as a timestamp
  -f    --format=ITEM      Timestamp format (default: %y%m%d-%H%M)
  -b    --before           Put timestamp before the filename (default)
  -a    --after            Put timestamp after the filename
        --textbefore=ITEM  Optional text before filename
        --textafter=ITEM   Optional text after filename
  -h -? --help             Display help message
  -V    --version          Print version information
        --numeric-version  Print just the version number
  -v    --verbose          Loud verbosity
  -q    --quiet            Quiet verbosity

```

## Examples

```sh

timestamper -v -n basic.txt
basic.txt -> 180513-1753-basic.txt

timestamper -v -a --textbefore Test package.yaml
package.yaml -> Test-package-180510-1512.yaml
```

## Installation

Use [stack](https://haskellstack.org) to install:

```sh
stack build
stack install
```
