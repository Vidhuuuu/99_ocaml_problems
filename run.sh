#!/usr/bin/env bash
mkdir -p build
mkdir -p build
ocamlc -o build/main -I build -c main.ml
ocamlc -o build/main build/main.cmo
./build/main
