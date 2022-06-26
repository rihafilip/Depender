# Depender

A highly customisable dependency graph generator.  
It is bassed on pure text pattern matching and supports configurable
input scanning and different output types.  

The program takes a configuration file (see below) and based on it, 
it scans specified files, matches the dependencies and outputs
in the specified format.

## Prerequisities
Requires the Stack Haskell build system, optionaly GraphViz for using
the Dot output type. 

## Usage
You can use the program without installing it using

```bash
stack run -- --config=FILE --folder=FOLDER [CONFIGS]
```

To see all available flags, run the program with `--help` flag.  
To specify the config file, use `--config`.  
To use different working directory than the current one, use `--folder`.  
To run specified configurations, list them after the other flags
separated by space.  
If no configuration is specified, the config file is just parsed and
verified.

## Examples
There are several examples prepared in the `example` folder.

### Mermaid example

Generates graph of dependencies of all modules, including external ones
(but not their dependencies).

```bash
stack run -- --config="example/config.yaml" example-mermaid
```
The output will be in `example/dependencies.md`. You can view the output
in for example [Mermaid Live](https://mermaid.live/).

### Dot example

Generates graph of just the insidess of `Depender` package.  
This requires a GraphViz package.

```bash
stack run -- --config="example/config.yaml" example-dot
dot example/local-dependencies.dot -Tpng > example/local-dependencies.png
```

The generated graph is in `example/local-dependencies.png`

### Yaml example

Prints to standart output on which external packages is the current 
project dependent on.

```bash
stack run -- --config="example/config.yaml" example-yaml
```

### C++ example

Generates a depenedncy graph on C++ files in `example/cpp-src`
and their namespaces.
Can be used to visualise global namespace pollution.  
This requires a GraphViz package.

```bash
stack run -- --config="example/config.yaml" example-cpp
dot example/namespaces.dot -Tpng > example/namespaces.png
```

## Configuration file
The basis of this program is a YAML configuration file. An example of one
can be found in `example/config.yaml`. Below is it's specification.

On the top level there can be many configurations, which are specified by
their name.

### One configuration format
```yaml
name:
    files: array of <file>
    patterns: array of <pattern-matcher>
    type: <output-type>
    output: <string>
```

The `output` specifies the output file for the output type.

### Files specification
One file is a string, where several characters have special meaning:

| symbol | meaning |
|---|---|
| `/` | directory separator |
| `*` | match zero or more characters except directory separator |
| `**` | match any file or zero or more nested directories |s
| `?` | match any single character except directory separator |

The working directory is scanned recursively and all files that match
at least one of the file patterns is inputed further.

### Pattern matcher
```yaml
( fixed-name: string | file-name: ( with-extension | without-extension ) | name: <pattern> )
pattern: <pattern>
```

Pattern matcher is composed of the name it uses and the actual pattern.  
If fixed name is specified, dependencies will be assigned to a fixed name.  
If file name is specified, dependencies will be assigned to a name of 
the file (with or wihtout extension).  
If name is specified, dependencied will be assigned to
the first match of given pattern. If no name is matched,
the dependencies will not be assigned to anything.  

### Patterns
There are currently two patterns implemented: `regex` and `yaml-parser`.

#### Regex
Regex is just a plain string, with these special symbols:

| symbol | meaning |
|---|---|
| `\@` | the next match on `\.\+` is the outputed token |
| `\.` | any character except spaces |
| `\_` | any character including spaces |
| `\$` | end of line |
| `\?` | previous item is optional |
| `\*` | previous item is matched zero or more times |
| `\+` | previous item is matched one or more times |
| `\\` | match the symbol `\` |
| `\(` (pattern) `\)` | define a subgroup, working as a single item |
| (pattern) `\\|` (pattern) | alternation between two patterns |
| (space) | match one or more whitespace characters (except newline) |

In every regex pattern, exactly one valid output must be specified, output cannot
be used in (`\?`, `\*`, `\+` ) combinators and must be at least once on each side
of `\|` combinator.  

#### YAML parser
YAML parser specifies the path on which the expected value is and what to iterate on.

```yaml
type: yaml-parser
yaml-path: <string>
iterate-over: ( keys | values | array | string )
```

The value of `path` is a string that specifies the path in the top level object
or array, where the syntax is `.key`, `.'key'` or `."key"`
to access a certain key in a object and `[i]` to access the i-th item in the array.  
These patterns can then be chained together.  

The value of `iterate-over` is then `keys` or `values` to iterate over the keys
or values of the object (the path is expected to lead to an object and on `keys`,
all of them must be strings), `array` to iterate over the given array of string 
or `string` if the given path leads to a single string.

### Output type
Currently there are three output types: `stdout`, `mermaid` and `Dot`.

`stdout` simply outputs the content of the dependency graph to a stdout,
ignoring the output file.  
`mermaid` writes the [MermaidJS](https://mermaid-js.github.io/) graph type output
to the output file.  
`Dot` writes the GraphViz Dot format graph to the output file.

## Known limitations
Currently the program works mostly with base String datatype, so
non-unicode characters can cause problems.  
The program list all folders and subfolders and then filters them, so this
can cause endless loop if in subfolder is a symlink to a parent folder.  
The regex pattern matcher is built from scratch and not optimised,
so there is a possibility it can be slow on very long files.  
The regex matcher has a quite small subsset of usual regex patterns.

## Possible future extensions
There is a possibility to use a runtime code loading library like
[Dyre](https://hackage.haskell.org/package/dyre) so that the user load new
functionality without the need to edit and recompile the source code.

Currently one top-level configuration is transformed into one file,
but it could be possible to specify that each input file will have it's own
output file.

The CLI can be then further extended so that users can create new configurations
from existing templates and configure it in a way that they don't need to edit the
configuration file manualy.
