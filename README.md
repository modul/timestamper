# Timestamper

timestamper is a commandline utility to add timestamps to the names of files or directories.

## Usage

```sh
timestamper [OPTIONS] [FILE]

Common flags:
  -d    --dry              don't do anything, just print actions
  -m    --modified         use the modification time as a timestamp (default)
  -n    --now              use the current time as a timestamp
  -f    --format=ITEM      timestamp format (default: %y%m%d-%H%M)
  -b    --before           put timestamp before the filename (default)
  -a    --after            put timestamp after the filename
        --textbefore=ITEM  optional text before filename
        --textafter=ITEM   optional text after filename
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
