# NI-AFP Semester Project

This project is a highly customisable dependency graph generator. It is based on a
pure text pattern matching and supports configurable input scanning and different
output types.

## Functionality

The program will accept a configuration file (see below) and based on it, it will
scan the specified folders, match the dependencies and then output the dependency
diagrams.

## Configuration file

The base of the program is a configuration file, below is an example of
first draft of it's syntax, currently in the YAML format.

```yaml
elm-project:
    files:
        - "elm.json"
    patterns:
        - type: module
          fixed-name: "project"
          pattern:
                type: json-parser
                json-path: ".dependencies.direct"
                iterate-over: keys

    type : UML
    output: "doc/project-dependecy.png"

cpp:
    files:
        - "src/*.cpp"
        - "src/*.hpp"

    patterns:
        - pattern: \^#include <\@>
          type: module
          file-name: with-extension

        - pattern: \^#include "\@"
          type: file
          file-name: with-extension

    type: dot
    output: "doc/cpp-dep.dot"

haskell:
    files:
        - "backend/.hs"

    patterns:
        - pattern: \^ import \@
          type: module
          name: \^ module \@ where

    type: dot
    output: "doc/backend-dep.dot"

```

On the top level we can specify many independent configurations. In each of these
configurations we specify the files, patterns, output type and output file. 
Files are specified as list of `.gitignore`-like patterns.  

Patterns is a list, where each element is a map with keys "type", "name" and "pattern".  

Type is either "module" or "file", based on if the dependecy is an absolute path
from inspected file or an identifier of module.  
Name is a pattern and it's output is the name of the node in the graph to which
will the dependencies be tied. A special pattern `file-name` will use the name of
the inspected file instead.  
The key `name` can be replaced with `fixed-name`, which uses a fixed string for
the name instead of a pattern.

## Patterns

Pattern is one of several types of file parsers, where the ones provided by default 
will be regex, JSON and YAML parsers.

### Regex

If the pattern is a plain string, it is evaluated as a grep-like
regular expression pattern.

### JSON, YAML parser

If the pattern is a map with the key `type` as `json-parser` or `yaml-parser`,
the file will be parsed as either one of those. The map then needs to have the keys
`json-path` or `yaml-path` respectively and `iterate-over`.

The value of `-path` is a string that specifies the path in the top level object
or array, where the syntax is `.key` to access a certain key in a object and
`[i]` to access the i-th item in the array. These patterns can then be chained
together.

The value of `iterate-over` is then `keys` or `values` to iterate over the keys
or values of the object, and `array` to iterate over the given array. The values
of these should then be strings.

## Output

Currently planned output types are UML diagrams and Graphviz Dot files.

## Further extension

The code should be written in a way that it is quite easy to extend the built-in
parsers and output types with custom ones. There is a possibility to use a runtime
code loading library like [Dyre](https://hackage.haskell.org/package/dyre) so that
the user load new functionality without the need to edit and recompile the source code.

Currently the plan is to transform one top-level dependecy into one file,
but it could be possible to specify that each input file will have it's own
output file.

The CLI can be then further extended so that users can create new configurations
from existing templates and configure it in a way that they don't need to edit the
configuration file manualy.

## Execution flow

``` mermaid
flowchart TD;
    subgraph Legend
        D[[Data]]
        F[/File\]
        P{{Function}}
    end

    subgraph Patterns Package
        Reg{{Regex pattern}} -.-> LoP[[List of patterns]]
        Json{{JSON pattern}} -.-> LoP
        Yaml{{YAML pattern}} -.-> LoP
    end

    subgraph Configration Package
        MkConf{{makeConfiguration}} -.-> Conf[[Configuration]]
    end

    ConfFile[/Configuration file\] ==> MkConf
    LoP --Patterns syntax--> MkConf
    
    Wd[/Working directory\] ==> FileM
    subgraph FileMatch Package
        Conf --Which files to match--> FileM{{Files matcher}}
        FileM -.-> Files[[Raw files content and which pattern they use]]
    end
    
    subgraph DependencyGraph Package
        Files --> Dep{{Dependency matcher}}
        LoP --Actual matching functions--> Dep
        Dep -.-> Graph[[Dependency graph]]
    end

    subgraph Writter Package
        Dot{{Dot Graph Maker}} --> Write{{Writing function}}
        UML{{UML Graph Maker}} --> Write
    end

    Graph --> Write
    Write ==> Output[/"Output file(s)"\]
```

## Configuration specification

***Configuration***

Many top level configurations in a YAML file

***top level configuration***
```yaml
configuration-name:
    files: array of <file>
    patterns: array of <dependency-pattern>
    type: ( UML | Dot )
    output: <string>
```

***file***
```yaml
<string>
```

Special characters:

| symbol | meaning |
|---|---|
| `/` | directory separator |
| `*` | match zero or more characters except directory separator |
| `**` | match any file or zero or more nested directories |s
| `?` | match any single character except directory separator |

***dependency-pattern***
```yaml
type: ( module | file )
( fixed-name: string | file-name: ( with-extension | without-extension ) | name: <pattern> )
pattern: <pattern>
```

***pattern***
```yaml
( <regex> | <yaml-pattern> | <json-patter> )
```

***regex***
```yaml
<string>
```

Special characters:

| symbol | meaning |
|---|---|
| `\@` | the next match on `\.\+` is the outputed token |
| `\.` | any character except spaces |
| `\_` | any character including spaces |
| `\^` | start of line |
| `\$` | end of line |
| `\?` | previous item is optional |
| `\*` | previous item is matched zero or more times |
| `\+` | previous item is matched one or more times |
| `\\` | match the symbol `\` |
| `\(` (pattern) `\)` | define a subgroup, working as a single item |
| (pattern) `\\|` (pattern) | alternation between two patterns |
| (space) | match one or more whitespace characters (except newline) |

***yaml-pattern***

```yaml
type: yaml-parser
yaml-path: <string>
iterate-over: ( keys | values | array )
```

***json-pattern***

```yaml
type: json-parser
json-path: <string>
iterate-over: ( keys | values | array | string )
```
