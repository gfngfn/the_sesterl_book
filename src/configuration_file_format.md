## Configuration file format

Configuration files must be of the following form. Although configuration files are in the YAML format, their specification is described here by using JSON-like expressions for clarity of the structure:

```
Config := {
  package: String
    # The name of the package. Example: "sesterl_json"

  language: String
    # The minimum version of Sesterl required by the package.
    # Example: "v0.2.0"
    # The Sesterl compiler refers to this field for checking that
    # the compiler is backward-compatible with the required version.
    # This field is optional. No check will be performed if omitted.

  source_directories: Array<String>
    # The list of directories where source files are placed.
    # All the source files (i.e. files that have
    # ".sest", ".erl", or ".app.src" as their extension)
    # that are exactly at one of the specified directories will be used for compilation.
    # Specified directories must be relative to the configuration file.
    # Example: [ "./src", "./src/generated" ]

  main_module: String
    # The name of the main module of the package.
    # The *main module* of a package is defined to be
    # the sole module visible from the outside of the package.

  test_directories: Array<String>
    # The list of directories where test files are placed.
    # Specified directories must be relative to the configuration file.
    # This field is optional. Default: []
    # Example: [ "./test" ]

  dependencies: Array<Dependency>
    # This field is optional. Default: []

  test_dependencies: Array<Dependency>
    # This field is optional. Default: []

  erlang: ErlangConfig
    # This field is optional. Default: {}

  document_outputs: Array<DocumentOutput>
    # Settings for the document generation.
    # This field is optional. Default: []
}

Dependency := {
  name: String
    # The name of the dependency.

  source: (GitSource | LocalSource)
    # Describes how to get the dependency.
}

GitSource := {
  type: "git"

  repository: String
    # The URI of the Git repository.

  spec: (TagSpec | RefSpec | BranchSpec)
    # Describes which commit to use.
}

TagSpec := {
  type: "tag"
  value: String  # Example: "v1.3.0"
}

RefSpec := {
  type: "ref"
  value: String  # A commit hash.
}

BranchSpec := {
  type: "branch"
  value: String  # Example: "master"
}

LocalSource := {
  type: "local"

  directory: String
    # The directory where the dependency is placed.
}

HexSource := {
  type: "hex"

  version: String
    # The version number.
}

ErlangConfig := {
  output_directory: String
    # The directory at which Erlang modules are generated.
    # Must be relative to the configuration file.
    # This field is Optional. Default: "./_generated"

  test_output_directory: String
    # The directory at which Erlang test modules for EUnit are generated.
    # Must be relative to the configuration file.
    # This field is Optional. Default: "./_generated_test"

  erlang_dependencies: Array<ErlangDependency>
    # The Erlang libraries on which the package depends.
    # This field is optional. Default: []

  relx: Relx
    # This field is optional.
    # No `relx` stanza will be written on `rebar.config` if omitted.
}

ErlangDependency := {
  name: String
    # The name of the package. Example: "cowboy"

  source: (HexSource | GitSource)
    # Describes how to get the Erlang library.
}

Relx := {
  release: RelxRelease
  dev_mode: Boolean     # This field is optional. Default: false
}

RelxRelease := {
  name: String
  version: String
  applications: Array<String>
}

DocumentOutput := {
  format: { type: "html" }
    # The format of output documents.
    # Only HTML is supported so far.

  output_directory: String
    # The directory at which documents are generated.
    # Must be relative to the configuration file.
    # Example: [ "./_doc" ]
}
```
