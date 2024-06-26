# YSC2229: Libraries and Examples

This project contains the libraries with the implementations of
algorithms and data structures for the Yale-NUS College course on Introductory
Data Structures and Algorithms (YSC2229). The lecture notes for the
course are available at

https://ilyasergey.net/YSC2229

## Project Structure 

The project contains libraries, which are all are defined in the folder
`lib`, as well as executables (defined in the folder `runners`).

More files will be added as we progress through the class.

**Each branch corresponds to a specific week of the module.**

## Building the Project

Building the project requires OCaml version 4.10.0 or higher. Please, check the
[instructions](https://ilyasergey.net/YSC2229/prerequisites.html) on
installing the necessary software.

To build the project, simply type:

```
make
```

To remove the files generated by the compiler, type

```
make clean
```

## Using the Project as a Library

Once successfully built, you can install the contents of this project
as libraries to use in your own projects of homework assignments.
Check out the example project explaining how to use the libraries:

https://github.com/ysc2229/library

### Installing the project via opam

To install the libraries as an independent package, from the root folder
of the project, run, of instance

```
opam install ysc2229
```

If in the future you need to update the package installation, you can
reinstall it as follows (again, from the root folder of the project):

```
opam reinstall ysc2229
```

To uninstall the package, type in terminal, e.g.,

```
opam uninstall ysc2229
```

## Running with utop

From the console, run

```
dune utop . --profile release
```

See also

* [OCaml graphics demo](https://github.com/ysc2229/ocaml-graphics-demo)

## Submitting changes to the project

You can fork the project on GitHub for experimentation.

If you don't like certain functionality and know how to fix it, you
can also file Pull Requests to the master repository via GitHub
interface.
