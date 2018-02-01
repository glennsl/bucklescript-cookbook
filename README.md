# The BuckleScript Cookbook

The BuckleScript Cookbook is a collection of simple examples intended to both showcase BuckleScript by example, and to demonstrate good practices for accomplishing common tasks.

This has been heavily inspired by the [Rust Cookbook](https://brson.github.io/rust-cookbook/).

<!-- toc -->

- [Reason](#reason)
- [Contributing](#contributing)
- [General](#general)
    + [Serialize a record to JSON](#serialize-a-record-to-json)
    + [Deserialize JSON to a record](#deserialize-json-to-a-record)
    + [Encode and decode Base64](#encode-and-decode-base64)
    + [Generate random numbers](#generate-random-numbers)
    + [Log a message to the console](#log-a-message-to-the-console)
    + [Use string interpolation](#use-string-interpolation)
    + [Format a string using Printf](#format-a-string-using-printf)
    + [Extract specific HTML tags from an HTML document using a Regular Expression](#extract-specific-html-tags-from-an-html-document-using-a-regular-expression)
    + [Dasherize camelCased identifiers inside string literals using Regular Expression](#dasherize-camelcased-identifiers-inside-string-literals-using-regular-expression)
    + [Create a map data structure, add or replace an entry, and print each key/value pair](#create-a-map-data-structure-add-or-replace-an-entry-and-print-each-keyvalue-pair)
        * [Map](#map)
        * [Js.Dict](#jsdict)
        * [Associative list](#associative-list)
        * [Hashtbl](#hashtbl)
    + [Use a Set in a recursive type](#use-a-set-in-a-recursive-type)
  * [Promises](#promises)
    + [Creating new Promises](#creating-new-promises)
    + [Handling promise values](#handling-promise-values)
    + [Error handling](#error-handling)
- [FFI](#ffi)
    + [Bind to a simple function](#bind-to-a-simple-function)
    + [Bind to a function in another module](#bind-to-a-function-in-another-module)
    + [Create a Plain Old JavaScript Object](#create-a-plain-old-javascript-object)
    + [Raise a javascript exception, then catch it and print its message](#raise-a-javascript-exception-then-catch-it-and-print-its-message)
    + [Define composable bitflags constants](#define-composable-bitflags-constants)
  * [Untagged unions](#untagged-unions)
    + [Consuming values of an untagged union type](#consuming-values-of-an-untagged-union-type)
      - [Bind to a higher-order function that returns a value of several different types (an untagged union)](#bind-to-a-higher-order-function-that-returns-a-value-of-several-different-types-an-untagged-union)
      - [Bind to a higher-order function that takes a function accepting an argument of several different types (an untagged union)](#bind-to-a-higher-order-function-that-takes-a-function-accepting-an-argument-of-several-different-types-an-untagged-union)
    + [Producing values of an untagged union type](#producing-values-of-an-untagged-union-type)
      - [Bind to a function overloaded to take an argument of several different types (an untagged union)](#bind-to-a-function-overloaded-to-take-an-argument-of-several-different-types-an-untagged-union)
        * [Mutiple externals](#mutiple-externals)
        * [bs.unwrap](#bsunwrap)
        * [GADT](#gadt)
      - [Bind to a function that takes a variable number of arguments of different types (an untagged union)](#bind-to-a-function-that-takes-a-variable-number-of-arguments-of-different-types-an-untagged-union)
      - [Bind to a second-order callback that takes an argument of several different types (an untagged union)](#bind-to-a-second-order-callback-that-takes-an-argument-of-several-different-types-an-untagged-union)
- [Browser-specific](#browser-specific)
    + [Extract all links from a webpage](#extract-all-links-from-a-webpage)
    + [Query the GitHub API](#query-the-github-api)
- [Node-specific](#node-specific)
    + [Read lines from a text file](#read-lines-from-a-text-file)
    + [Read and parse a JSON file](#read-and-parse-a-json-file)
    + [Find files using a given predicate](#find-files-using-a-given-predicate)
    + [Run an external command](#run-an-external-command)
    + [Parse command-line arguments](#parse-command-line-arguments)

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
open Webapi.Base64

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

Or using [bs-revamp](https://github.com/glennsl/bs-revamp) with the same input:

```ml
input |> Revamp.matches("<p\\b[^>]*>(.*?)<\\/p>", ~flags=[Revamp.IgnoreCase])
      |> Rebase.Seq.forEach(Js.log);
```

#### Dasherize camelCased identifiers inside string literals using Regular Expression

Uses [bs-revamp](https://github.com/glennsl/bs-revamp)

```ml
let code = {|
  let borderLeftColor = "borderLeftColor";
  let borderRightColor = "borderRightColor";
|}

let () =
  code |> Revamp.replace {|"([^"]*)"|}                (* Matches the content of string literals *)
            (Revamp.replace "[A-Z]" (fun letter ->    (* Matches uppercase letters *)
              "-" ^ letter |> Js.String.toLowerCase)) (* Convert to lower case and prefix with a dash *)
       |> Js.log
       
(* Outputs:
  let borderLeftColor = "border-left-color";
  let borderRightColor = "border-right-color";
*)
```

#### Create a map data structure, add or replace an entry, and print each key/value pair

###### Map

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

###### Js.Dict

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
               |> Js.Array.forEach (fun (k, v) -> Js.log {j|key:$k, val:$v|j})
```

###### Associative list

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
               |> List.iter (fun (k, v) -> Js.log {j|key:$k, val:$v|j})
```

###### Hashtbl

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

#### Use a Set in a recursive type

The task is to make something like this using Set:

```ml
type t = A | B | Union of t Set.t
```

Unfortunately there is no `Set.t`. We need to use the `Set.Make` functor which requires that
we pass it the type the set will contain, but of course we don't have that yet since it's recursive...

Instead we nee to use module recursion (Yay!):

```ml
module rec OrderedType : Set.OrderedType with type t = Type.t = struct
  type t = Type.t
  let compare = compare
end

and Type : sig
  type t = A | B | Union of TypedSet.t
end = Type

and TypedSet : Set.S with type elt = OrderedType.t = Set.Make(OrderedType)

include Type
```

This could have been accomplished with just two modules, `TypedSet` and `OrderedType`, but adding `Type` 
let's us get away with only defining the type once, and to be able to include it such that we can use the
type as if it was defined at the top level, without also including `comapre` and thereby shadow `Pervasives.compare`.

We can now use the type seamlessly, as if there was no complicated module recursion with intermingled types:

```ml
let value = Union (TypedSet.of_list [A; B; A])
```


### Promises

#### Creating new Promises

```ml
let okPromise =
  Js.Promise.make (fun ~resolve ~reject:_ -> (resolve "ok")[@bs])

(* Simpler promise creation for static values *)
let _ : string Js.Promise.t =
  Js.Promise.resolve "easy"

(* Create a promise that resolves much later *)
let _ : _ Js.Promise.t =
  Js.Promise.reject (Invalid_argument "too easy")
  
let timer =
  Js.Promise.make (fun ~resolve ~reject:_ ->
    (* `Js.Global.setTimeout` returns a `timeoutId`, so we assign it to
       `_` to ginore and, and annotate its type to make sure we don't
       accidentally partially apply the function *)
    let _ : Js.Global.timeoutId =
      Js.Global.setTimeout
      	(fun () -> (resolve "Done!")[@bs])
        1000
    in ()
  )
```

#### Handling promise values

```ml
(*
 * Note that we *have* to return a new promise inside of the callback given to then_;
 *)
let _ : unit Js.Promise.t =
  Js.Promise.then_
    (fun value -> Js.Promise.resolve (Js.log value))
    okPromise

(* Chaining *)
let _ : unit Js.Promise.t =
  Js.Promise.then_
  	(fun value -> Js.Promise.resolve (Js.log value))
    (Js.Promise.then_
      (fun value -> Js.Promise.resolve (value + 1))
      (Js.Promise.resolve 1))

(* Better with pipes 😉 *)
let _ : unit Js.Promise.t =
  Js.Promise.resolve 1
  |> Js.Promise.then_ (fun value -> Js.Promise.resolve (value + 1))
  |> Js.Promise.then_ (fun value -> Js.Promise.resolve (Js.log value))

(* And even better with local open *)
let _ : unit Js.Promise.t =
  let open Js.Promise in
  resolve 1 |> then_ (fun value -> resolve (value + 1))
            |> then_ (fun value -> resolve (Js.log value))

(* Waiting for two values *)
let _ : unit Js.Promise.t =
  let open Js.Promise in
  all2 (resolve 1, resolve "a")
  |> then_ (fun (v1, v2) ->
  	   Js.log ("Value 1: " ^ string_of_int v1);
       Js.log ("Value 2: " ^ v2);
       resolve ())

(* Waiting for an array of values *)
let _ : unit Js.Promise.t =
  let open Js.Promise in
   all [|resolve 1; resolve 2; resolve 3|]
   |> then_ (fun vs ->
        vs |> Array.iteri (fun v i -> Js.log {j|Value $i: $v|j});
        resolve ())
```

#### Error handling

```ml
(* Using a built-in OCaml error *)
let notFoundPromise =
  Js.Promise.make (fun ~resolve:_ ~reject  -> (reject Not_found) [@bs])
  
let _ : unit Js.Promise.t =
  notFoundPromise
  |> Js.Promise.then_ (fun value -> Js.Promise.resolve (Js.log value))
  |> (Js.Promise.catch (fun err -> Js.Promise.resolve (Js.log err)))

(* Using a custom error *)
exception Oh_no of string

let ohNoPromise : _ Js.Promise.t =
  Js.Promise.make (fun ~resolve:_ ~reject -> reject (Oh_no ("oh no")) [@bs])
  
let _ : unit Js.Promise.t =
  ohNoPromise |> Js.Promise.catch (fun err -> Js.Promise.resolve (Js.log err))

(**
 * Unfortunately, as one can see - catch expects a very generic `Js.Promise.error` value
 * that doesn't give us much to do with.
 * In general, we should not rely on rejecting/catching errors for control flow;
 * it's much better to use a `result` type instead.
 *)
let betterOk : (string, _) Js.Result.t Js.Promise.t =
  Js.Promise.make (fun ~resolve ~reject:_ ->
    resolve (Js.Result.Ok ("everything's fine")) [@bs])
         
let betterOhNo : (_, string) Js.Result.t Js.Promise.t =
  Js.Promise.make (fun ~resolve ~reject:_ ->
    resolve (Js.Result.Error ("nope nope nope")) [@bs])
         
let handleResult =
  Js.Promise.then_ (fun result ->
    Js.Promise.resolve (
      match result with
      | Js.Result.Ok text -> Js.log ("OK: " ^ text)
      | Js.Result.Error reason -> Js.log ("Oh no: " ^ reason)))
      
let _ : unit Js.Promise.t =
  handleResult betterOk
  
let _ : unit Js.Promise.t =
  handleResult betterOhNo
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

#### Raise a javascript exception, then catch it and print its message
```ml
let () =
  try
    Js.Exn.raiseError "oops!"
  with
  | Js.Exn.Error e ->
    match Js.Exn.message e with
    | Some message -> Js.log {j|Error: $message|j}
    | None -> Js.log "An unknown error occurred"
```

#### Define composable bitflags constants
TODO



### Untagged unions

An untagged union type is a type that can be several different types, but whose values, unlike variants,
contain no information that can be translated to and dealt with directly and safely in OCaml. In TypeScript and flow
such a type could be denoted as `string | number`. With BuckleScript we can take a number of different approaches
depending on the context the types appear in, and what we need to do with them.


#### Consuming values of an untagged union type

##### Bind to a higher-order function that returns a value of several different types (an untagged union)

```ml
(* Bind to the function, using Js.Jsont.t to capture the untagged union *)
external getRandomlyTypedValue : unit -> Js.Json.t = "" [@@bs.val]

(* Override the binding with a function that converts the return value *)
let getRandomlyTypedValue () =
  match Js.Json.classify (getRandomlyTypedValue ()) with
  | Js.Json.JSONNumber n -> `Float n
  | Js.Json.JSONString s -> `String s
  | _ -> failwith "unreachable"

(* The function can now be used safely and idiomatically *)
let () =
  match getRandomlyTypedValue () with
  | `Float n  -> Js.log2 "Float: " n
  | `String s -> Js.log2 "String: " s
```

##### Bind to a higher-order function that takes a function accepting an argument of several different types (an untagged union)

This takes the same pattern used in the previous example and applies it to a wrapped callback, since in this case it's "returned"
as an argument to a callback function.

```ml
(* Bind to the function, using Js.Jsont.t to capture the untagged union *)
external withCallback : (Js.Json.t -> unit) -> unit = "" [@@bs.val]

(* Override the binding with a function that wraps the callback in a function that classifies and wraps the argument *)
let withCallback cb =
  withCallback (fun json  ->
    match Js.Json.classify json with
    | Js.Json.JSONNumber n -> cb (`Float n)
    | Js.Json.JSONString s -> cb (`String s)
    | _ -> failwith "unreachable")

(* The function can now be used safely and idiomatically *)
let () =
  withCallback (function | `Float n -> Js.log n
                         | `String s -> Js.log s)
```


#### Producing values of an untagged union type

##### Bind to a function overloaded to take an argument of several different types (an untagged union)

###### Mutiple externals
```ml
module Date = struct
  type t
  
  external fromValue : float -> t = "Date" [@@bs.new]
  external fromString : string -> t = "Date" [@@bs.new]
end

let date1 = Date.fromValue 107849354.
let date2 = Date.fromString "1995-12-17T03:24:00"
```

###### bs.unwrap
```ml
module Date = struct
  type t
  
  external make : ([`Value of float | `String of string] [@bs.unwrap]) -> t = "Date" [@@bs.new]
end

let date1 = Date.make (`Value 107849354.)
let date2 = Date.make (`String "1995-12-17T03:24:00")
```

###### GADT
```ml
module Date = struct
  type t
  
  type 'a makeArg =
  | Value : float makeArg
  | String : string makeArg
  
  external make : ('a makeArg [@bs.ignore]) -> 'a -> t = "Date" [@@bs.new]
end

let date1 = Date.make Value 107849354.
let date2 = Date.make String "1995-12-17T03:24:00"
```

##### Bind to a function that takes a variable number of arguments of different types (an untagged union)
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

##### Bind to a second-order callback that takes an argument of several different types (an untagged union)

This binds to a function taking a callback, which is passed another callback that should be called with an
untagged union value, such as an async function expecting a response. This function could be used in JavaScript
as follows:

```javascript
withAsyncCallback(done => done("I'm done now"));
// or
withAsyncCallback(done => done(false));
```

In OCaml we could translate that to an option, and would then need to wrap the callback in order to convert it
before passing it on:

```ml
type doneFn = string option -> unit
external withAsyncCallback : ((Js.Json.t -> unit) -> unit) -> unit = "" [@@bs.val]
let withAsyncCallback: (doneFn -> unit) -> unit =
  fun f -> withAsyncCallback
    (fun done_  ->
     f (function | Some value -> value     |> Js.Json.string  |> done_
                 | None       -> Js.false_ |> Js.Json.boolean |> done_))
```



## Browser-specific

#### Extract all links from a webpage

```ml
open Webapi.Dom

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
(* given an array of repositories object as a JSON string *)
(* returns an array of names *)
let names text = 
  text
  |> Js.Json.parseExn
  |> Json.Decode.(array (field "name" string))

(* fetch all public repositories of user [reasonml-community] *)
(* print their names to the console *)
let printGithubRepos () = Js.Promise.(
  Fetch.fetch "https://api.github.com/users/reasonml-community/repos"
  |> then_ Fetch.Response.text
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
