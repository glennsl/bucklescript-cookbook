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

To encode and decode Base64, you can bind to Javascript functions `btoa` and `atob`, respectively:

```ml
external  btoa : string -> string = "window.btoa" [@@bs.val]
external  atob : string -> string = "window.atob" [@@bs.val]

let _ =  "Hello World!" |> btoa |> atob |> Js.log
```

Alternatively, if you have [bs-webapi](https://github.com/BuckleTypes/bs-webapi-incubator) installed:

```ml
open ReasonJs.Base64

let _ =  "Hello World!" |> btoa |> atob |> Js.log
```

#### Generate random numbers

Use [Random module](http://caml.inria.fr/pub/docs/manual-ocaml/libref/Random.html) to generate random numbers

```ml
Js.log (Random.int 5)
```

#### Log a message to the console

```ml
Js.log "Hello BuckleScript!"
```

#### Use string interpolation

```ml
let () =
  let world = "World" in
  Js.log {j|Hello，$world!|j}
```

#### Format a string using Printf

Use [Printf module](http://caml.inria.fr/pub/docs/manual-ocaml/libref/Printf.html)

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
  (* create a module IntMap *)
  let module IntMap = 
    Map.Make(struct type t = int let compare = compare end) in
  
  let open IntMap in
    (* create a map with keys 1 and 2 *)
    let map12 = empty |> add 1 "ocaml" |> add 2 "bs" in

    (* print each key, value pair *)
    let printKV k v = 
      let k = string_of_int k in 
      Js.log {j|key:$k, val:$v|j} in
    iter printKV map12;
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

#### Extract all links from a webpage

```ml
open ReasonJs.Dom

let printAllLinks () =
  document
  |> Document.querySelectorAll "a"
  |> NodeList.toArray
  |> Array.iter (fun n -> 
    n 
    |> Element.ofNode
    |> (function
        | None -> failwith "Not an Element"
        | Some el -> Element.innerHTML el)
    |> Js.log)

Window.setOnLoad window printAllLinks
```
#### Fetch a json resource from some server (Query the GitHub API?)
Uses [bs-json](https://github.com/BuckleTypes/bs-json) and [bs-fetch](https://github.com/BuckleTypes/bs-fetch)

```ml
open Bs_fetch  

(* given an array of repositories object as a JSON string *)
(* returns an array of names *)
let names text = 
    match Js.Json.parseExn text with
    | arr -> 
        Json.Decode.(array (field "name" string) arr)
    | exception _ -> failwith ("Error parsing: " ^ text)

(* fetch all public repositories of user [BuckleTypes] *)
(* print their names to the console *)
let printGithubRepos () = Js.Promise.(
    fetch "https://api.github.com/users/BuckleTypes/repos"
    |> then_ Response.text
    |> then_ (fun text -> 
        text 
        |> names
        |> Array.iter Js.log 
        |> resolve)
    |> ignore
)

let () = printGithubRepos ()
```
## Node-specific

#### Read lines from a text file
Uses [bs-node](https://github.com/BuckleTypes/bs-node)
```ml
let () =
    Node.Fs.readFileAsUtf8Sync "README.md"
    |> Js.String.split "\n"
    |> Array.iter Js.log
```

#### Read and parse a JSON file
Uses [bs-json](https://github.com/BuckleTypes/bs-json) and [bs-node](https://github.com/BuckleTypes/bs-node)
```ml
let decodeName text =
    (* parse JSON *)
    Js.Json.parseExn text
    (* decode the field [name] *) 
    |> fun obj -> Json.Decode.(field "name" string obj)

let () =
    (* read [package.json] file *)
     Node.Fs.readFileAsUtf8Sync "package.json"
    |> decodeName
    |> Js.log
```
#### Find files using a given predicate
Uses [bs-glob](https://github.com/BuckleTypes/bs-glob)
```ml
let () =
    (* find all javascript files in  subfolders *)
    Glob.glob "**/*.js" (fun _ files -> Array.iter Js.log files)
```

#### Run an external command
Uses [bs-node](https://github.com/BuckleTypes/bs-node)
```ml
let () =
    (* prints node's version *)
    Node.(ChildProcess.execSync "node -v" (Options.options ~encoding:"utf8" ()))
    |> Js.log
```