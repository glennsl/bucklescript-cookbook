# The BuckleScript Cookbook

The BuckleScript Cookbook is a collection of simple examples intended to both showcase BuckleScript by example, and to demonstrate good practices for accomplishing common tasks.

This has been heavily inspired by the [Rust Cookbook](https://brson.github.io/rust-cookbook/).

<!-- toc -->

- [Contributing](#contributing)
- [General](#general)
    + [Serialize a record to JSON](#serialize-a-record-to-json)
    + [Deserialize JSON to a record](#deserialize-json-to-a-record)
    + [Encode and decode Base64](#encode-and-decode-base64)
    + [Generate random numbers](#generate-random-numbers)
    + [Log a message to the console](#log-a-message-to-the-console)
    + [Use string interpolation](#use-string-interpolation)
    + [Format a string using Printf](#format-a-string-using-printf)
    + [Make and usa a Map](#make-and-usa-a-map)
- [FFI](#ffi)
    + [Bind to a simple function](#bind-to-a-simple-function)
    + [Bind to a function in another module](#bind-to-a-function-in-another-module)
    + [Define composable bitflags constants](#define-composable-bitflags-constants)
- [Browser-specific](#browser-specific)
    + [Extract all links form a webpage](#extract-all-links-form-a-webpage)
    + [Fetch a json resource from some server (Query the GitHub API?)](#fetch-a-json-resource-from-some-server-query-the-github-api)
- [Node-specific](#node-specific)
    + [Read lines from a text file](#read-lines-from-a-text-file)
    + [Read and parse a JSON file](#read-and-parse-a-json-file)
    + [Find files using a given predicate](#find-files-using-a-given-predicate)
    + [Run an external command](#run-an-external-command)

<!-- tocstop -->

## Contributing

There are primarily two ways to contribute:

1. Suggest an example to include in the cookbook by [creating an issue](https://github.com/glennsl/bucklescript-cookbook/issues/new) to describe the task.
2. Add (or edit) an example by [editing this file directly](https://github.com/glennsl/bucklescript-cookbook/edit/master/README.md) and creating a pull request.

## General

#### Serialize a record to JSON
#### Deserialize JSON to a record
Uses [bs-json](https://github.com/BuckleTypes/bs-json)
```ml
type line = {
  start: point;
  end_: point;
  thickness: int option
}
and point = {
  x: float;
  y: float
}

module Decode = struct
  let point json =
    let open! Json.Decode in {
      x = json |> field "x" float;
      y = json |> field "y" float
    }

  let line json =
    Json.Decode.{
      start     = json |> field "start" point;
      end_      = json |> field "end" point;
      thickness = json |> optional (field "thickness" int)
    }
end

let data = {| {
  "start": { "x": 1.1, "y": -0.4 },
  "end":   { "x": 5.3, "y": 3.8 }
} |}

let line = data |> Js.Json.parseExn
                |> Decode.line
```

#### Encode and decode Base64

#### Generate random numbers


#### Log a message to the console

```ml
Js.log "Hello BuckleScript!"
```

#### Use string interpolation

#### Format a string using Printf

```ml
# Printf.printf ("Foo %d  %s") 2 "bar"
```

#### Make and use a Map

To create a Map, use the Map.Make functor. It expects a module with the folowing signature:
```ml
module type OrderedType = sig type t val compare : t -> t -> int end
```

For instance, to create the map which associate 1 to "ocaml" and 2 to "bs":
```ml
let () = 
  let module IntMap = 
    Map.Make(struct type t = int let compare = compare end) in
  let open IntMap in
    add 1 "ocaml" (add 2 "bs" empty) |> 
    iter (fun k v -> Js.log ("key: " ^ (string_of_int k) ^ " val: " ^ v))
```

## FFI

#### Bind to a simple function
```ml
external random : unit -> float = "Math.random" [@@bs.val]
```

#### Bind to a function in another module
```ml
external leftpad : string -> int -> char -> string = "" [@@bs.val] [@@bs.module "left-pad"]
```

#### Define composable bitflags constants

## Browser-specific

#### Extract all links form a webpage

#### Fetch a json resource from some server (Query the GitHub API?)

## Node-specific

#### Read lines from a text file
#### Read and parse a JSON file
#### Find files using a given predicate
#### Run an external command
