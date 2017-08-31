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
    + [Query the GitHub API](#query-the-github-api)
- [Node-specific](#node-specific)
    + [Read lines from a text file](#read-lines-from-a-text-file)
    + [Read and parse a JSON file](#read-and-parse-a-json-file)
    + [Find files using a given predicate](#find-files-using-a-given-predicate)
    + [Run an external command](#run-an-external-command)

<!-- tocstop -->

## Reason

All examples in this document use plain OCaml syntax. If you'd rather have a more [Reason](https://facebook.github.io/reason/)able syntax, the examples can be easily be converted using [reason-tools](https://github.com/reasonml/reason-tools), either by installing the browser extension ([Chrome](https://chrome.google.com/webstore/detail/reason-tools/kmdelnjbembbiodplmhgfjpecibfhadd) | [Firefox](https://addons.mozilla.org/en-US/firefox/addon/reason-tools/)), or [directly](https://reasonml.github.io/reason-tools/popup.html).

## Contributing

There are primarily two ways to contribute:

1. Suggest an example to include in the cookbook by [creating an issue](https://github.com/glennsl/bucklescript-cookbook/issues/new) to describe the task.
2. Add (or edit) an example by [editing this file directly](https://github.com/glennsl/bucklescript-cookbook/edit/master/README.md) and creating a pull request.

## General

#### Serialize a record to JSON
Uses [bs-json](https://github.com/reasonml-community/bs-json)
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

module Encode = struct
  let point r =
    let open! Json.Encode in (
      object_ [
        ("x", float r.x);
        ("y", float r.y)
      ]
    )
  let line r =
    Json.Encode.(
      object_ [
        ("start", point r.start);
        ("end", point r.end_);
        ("thickness", match r.thickness with Some x -> int x | None -> null)
      ]
    )
end

let data = {
  start = { x = 1.1; y = -0.4 };
  end_ = { x = 5.3; y = 3.8 };
  thickness = Some 2
}

let json = data |> Encode.line
                |> Js.Json.stringify
```

#### Deserialize JSON to a record
Uses [bs-json](https://github.com/reasonml-community/bs-json)
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
external btoa : string -> string = "" [@@bs.val]
external atob : string -> string = "" [@@bs.val]

let () =
  let text = "Hello World!" in
  Js.log (text |> btoa);
  Js.log (text |> btoa |> atob)
```

Alternatively, if you have [bs-webapi](https://github.com/reasonml-community/bs-webapi-incubator) installed:

```ml
open ReasonJs.Base64

let () =
  let text = "Hello World!" in
  Js.log (text |> btoa);
  Js.log (text |> btoa |> atob)
```

#### Generate random numbers

Use [Random module](http://caml.inria.fr/pub/docs/manual-ocaml/libref/Random.html) to generate random numbers

```ml
let () =
  Js.log (Random.int 5)
```

#### Log a message to the console

```ml
let () =
  Js.log "Hello BuckleScript!"
```

#### Use string interpolation

```ml
let () =
  for a = 1 to 10 do
    for b = 1 to 10 do
      let product = a * b in
      Js.log {j|$a times $b is $product|j}
    done
  done
```

#### Format a string using Printf

Use [Printf module](http://caml.inria.fr/pub/docs/manual-ocaml/libref/Printf.html)

```ml
(* Prints "Foo 2 bar" *)
let () =
  Printf.printf ("Foo %d %s") 2 "bar"
```

#### Extract specific HTML tags from an HTML document using a Regular Expression

```ml
let input = {|
<html>
  <head>
    <title>A Simple HTML Document</title>
  </head>
  <body>
    <p>This is a very simple HTML document</p>
    <p>It only has two paragraphs</p>
  </body>
</html>
|}

let () = 
  input
  |> Js.String.match_ [%re "/<p\\b[^>]*>(.*?)<\\/p>/gi"]
  |> function
    | Some result -> result
      |> Js.Array.forEach Js.log
    | None ->
      Js.log "no matches"
```

#### Create a map data structure, add or replace an entry, and print each key/value pair

##### Map

Immutable, any key type, cross-platform

```ml
let () = 

  let module StringMap = 
    Map.Make (struct
      type t = string
      let compare = compare
    end) in
  
  let painIndexMap = StringMap.(
  	empty
  	|> add "western paper wasp" 1.0
    |> add "yellowjacket" 2.0
    |> add "honey bee" 2.0
    |> add "red paper wasp" 3.0
    |> add "tarantula hawk" 4.0
    |> add "bullet ant" 4.0
  ) in

  painIndexMap |> StringMap.add "bumble bee" 2.0
  			   |> StringMap.iter (fun k v -> Js.log {j|key:$k, val:$v|j})
```

##### Js.Dict

Mutable, string key type, BuckleScript only

```ml
let painIndexMap =
  Js.Dict.fromList [
  	"western paper wasp", 1.0;
    "yellowjacket", 2.0;
    "honey bee", 2.0;
    "red paper wasp", 3.0;
    "tarantula hawk", 4.0;
    "bullet ant", 4.0
  ]
  
let () =
    Js.Dict.set painIndexMap "bumble bee" 2.0

let () =
  painIndexMap |> Js.Dict.entries
  			   |> Js.Array.forEach (fun (k,v)  -> Js.log {j|key:$k, val:$v|j})
```

##### Associative list

Immutable, any key type, cross-platform

```ml
let painIndexMap = [
  "western paper wasp", 1.0;
  "yellowjacket", 2.0;
  "honey bee", 2.0;
  "red paper wasp", 3.0;
  "tarantula hawk", 4.0;
  "bullet ant", 4.0
]

let addOrReplace (k, v) l =
  let l' = List.remove_assoc k l in
  (k, v) :: l'
  
let () =
  painIndexMap |> addOrReplace ("bumble bee", 2.0)
  			   |> List.iter (fun (k,v)  -> Js.log {j|key:$k, val:$v|j})
```

##### Hashtbl

Mutable, string key type, cross-platform

```ml
let painIndexMap = Hashtbl.create 10
let () =
  Hashtbl.(
    add painIndexMap "western paper wasp" 1.0;
    add painIndexMap "yellowjacket" 2.0;
    add painIndexMap "honey bee" 2.0;
    add painIndexMap "red paper wasp" 3.0;
    add painIndexMap "tarantula hawk" 4.0;
    add painIndexMap "bullet ant" 4.0;
  )

let () = 
  Hashtbl.replace painIndexMap "bumble bee" 2.0

let () =
  painIndexMap |> Hashtbl.iter (fun k v -> Js.log {j|key:$k, val:$v|j})
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

#### Create a Plain Old JavaScript Object
```ml
let person = [%obj {
  name = {
    first = "Bob";
    last = "Zhmith"
  };
  age = 32
}]
```

#### Catch a JavaScript exception
TODO

#### Raise a JavaScript exception
TODO

#### Define composable bitflags constants
TODO

#### Bind to a function that takes a avriableriable number of arguments of different types
```ml
module Arg = struct
  type t

  external int : int -> t = "%identity"
  external string : string -> t = "%identity"
end

external executeCommand : string -> Arg.t array -> unit = "" [@@bs.val] [@@bs.splice]

let () =
  executeCommand "copy" Arg.[|string "text/html"; int 2|]
```

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

let () =
  Window.setOnLoad window printAllLinks
```
#### Query the GitHub API
Uses [bs-json](https://github.com/reasonml-community/bs-json) and [bs-fetch](https://github.com/reasonml-community/bs-fetch)

```ml
open Bs_fetch  

(* given an array of repositories object as a JSON string *)
(* returns an array of names *)
let names text = 
  text
  |> Js.Json.parseExn
  |> Json.Decode.(array (field "name" string))

(* fetch all public repositories of user [reasonml-community] *)
(* print their names to the console *)
let printGithubRepos () = Js.Promise.(
  fetch "https://api.github.com/users/reasonml-community/repos"
  |> then_ Response.text
  |> then_ (fun text -> 
      text 
      |> names
      |> Array.iter Js.log 
      |> resolve)
  |> ignore
)

let () =
  printGithubRepos ()
```
## Node-specific

#### Read lines from a text file
Uses [bs-node](https://github.com/reasonml-community/bs-node)
```ml
let () =
  Node.Fs.readFileAsUtf8Sync "README.md"
  |> Js.String.split "\n"
  |> Array.iter Js.log
```

#### Read and parse a JSON file
Uses [bs-json](https://github.com/reasonml-community/bs-json) and [bs-node](https://github.com/reasonml-community/bs-node)
```ml
let decodeName text =
  Js.Json.parseExn text
  |> Json.Decode.(field "name" string)

let () =
  (* read [package.json] file *)
  Node.Fs.readFileAsUtf8Sync "package.json"
  |> decodeName
  |> Js.log
```
#### Find files using a given predicate
Uses [bs-glob](https://github.com/reasonml-community/bs-glob)
```ml
let () =
  (* find and list all javascript files in subfolders *)
  Glob.glob "**/*.js" (fun _ files -> Array.iter Js.log files)
```

#### Run an external command
Uses [bs-node](https://github.com/reasonml-community/bs-node)
```ml
let () =
  (* prints node's version *)
  Node.(ChildProcess.execSync "node -v" (Options.options ~encoding:"utf8" ()))
  |> Js.log
```

#### Parse command-line arguments
TODO (requires bindings to minimist, commander or a similar library)
