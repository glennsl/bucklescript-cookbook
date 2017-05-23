# The BuckleScript Cookbook

The BuckleScript Cookbook is a collection of simple examples intended to both showcase BuckleScript by example, and to demonstrate good practices for accomplishing common tasks.

This was heavily inspired by the [Rust Cookbook](https://brson.github.io/rust-cookbook/).

## Contributing

There are primarily two ways to contribute:

1. Suggest an example to include in the cookbook by [creating an issue](https://github.com/glennsl/bucklescript-cookbook/issues/new) to describe the task.
2. Add (or edit) an example by [editing this file directly](https://github.com/glennsl/bucklescript-cookbook/edit/master/README.md) and creating a pull request.

## General

#### Serialize a record to JSON
#### Deserialize JSON to a record
```ml
type line = {
  start: point,
  end_: point
}
and type point = {
  x: float,
  y: float
}

module Decode = struct
  let point json =
    let open Json.Decode in {
      x = json |> field "x" float;
      y = json |> field "y" float
    }

  let line json =
    let open Json.Decode in {
      start = json |> field "start" point;
      end_  = json |> field "end" point
    }
end

let data = {| {
  "start": { "x": 1.1, "y": -0.4 },
  "end":   { "x": 5.3, "y": 3.8 }
} |}

let line = Js.Json.parseExn |> Decode.line
```

#### Encode and decode Base64
#### Generate random numbers
#### Log a message to the console
#### Use string interpolation
#### Format a string using Printf
#### Make and usa a Map

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
