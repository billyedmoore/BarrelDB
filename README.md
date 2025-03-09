# PostMixDB

Simple persistent key-value store based on the [Raik's bitcask paper](https://riak.com/assets/bitcask-intro.pdf),
heavily inspired by [avinassh's py-caskdb](https://github.com/avinassh/py-caskdb).

Previously called "BarrelDB" but I stumbled across another bitcask implementation with this name [here](https://github.com/mr-karan/barreldb),
so I renamed it to [PostMixDB](https://en.wikipedia.org/wiki/Premix_and_postmix).

## Usage 

### Prerequistes 

Install *opam*, the official ocaml package manager. See install instructions [here](https://ocaml.org/docs/installing-ocaml).

### Install dependencies

```
opam install . --deps-only --with-test
```
### Run

Run the REPL with:

```
opam exec -- dune exec PostMixDB
```

Run the tests with:

```
opam exec -- dune runtest
```

## Query Language

PostMixDB supports a minimal query language.

+ `OPEN "folder_name"` - Open database.
+ `CREATE "folder_name"` - Create and open database.
+ `PUT "key" "value"` - Put key, value pair in open database.
+ `GET "key"` - Get the value associated with key in open database.
+ `DELETE "key"` - Remove key from open database.
+ `LIST` - List keys in open database 

The REPL will execute commands when a line ends in a semi-colon, this means commands can 
be split over over multiple lines (newline characters are treated the same as spaces).
The number of spaces/new-lines isn't meaningful.

Commands may be chained with or without a separating semi-colon. When
commands are chained only the last result is returned (although all commands
are evaluated).

For example `OPEN "folder_name"; GET "key"` and `OPEN folder_name GET "key";`
produce equivalent ASTs, both statements would evaluate to the value mapped to the
key `"key"` in the database `"folder_name"`(assuming `"key"` is associated with a value and the database `folder_name`
exists).

Commands may be nested in the case they return a string (only `GET` at present). For
example `PUT GET "new-key" GET "new-value";` is a valid statement meaning:
1. Get the value related to the key `"new-key"` 
2. Get the value related to the key `"new-value"`
3. Put a new value with with the key from 1. and value from 2.

## Outstanding Improvements 
+ Change error handling in `PostMixDB.Tokenization` & `PostMixDB.Parse` to results 
 based errors (errors as values as opposed to errors as exceptions), during this change modify
 the REPL to handle these errors gracefully (currently crashes)
+ Parse brackets to allow for more elegant nested statements
+ Add `PostMixDB.Db.merge` to merge in the background, decide when this should be 
 triggered
