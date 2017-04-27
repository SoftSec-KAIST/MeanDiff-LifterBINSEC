BINCOA Translator
=================

Translate BINCOA to Soomin IR


# Install

A write-up of my installation process.

Steps:
 1. Install system dependencies
 2. Install Ocaml libraries
 3. Install BINUTILS

## 1. System dependencies

On my Arch Linux box I had to install the following system packages:
 - `ocaml` - OCaml compiler
 - `camlp4` - Caml preprocessor and pretty-printer
 - `opam` - OCaml Package Manager

## 2. Ocaml libraries

First initialize `~/.opam` with `opam init`.

And following packages, with `opam install`:
 - `piqi`
 - `piqilib`
 - `menhir`
 - `ocamlgraph`
 - `zarith`
 - `zmq`
 - `llvm`

## 3. Install BINUTILS

 1. Download and extract [binsec-0.1](http://binsec.gforge.inria.fr/distrib/binsec-0.1-20170301.tgz)
 2. Build

        # ./configure
        # make binsec
        
 3. Install (root permissions)
 
        $ cd src; make install


# Usage

See `binsec disasm -help` or `binsec-0.1-*/README` for examples.
