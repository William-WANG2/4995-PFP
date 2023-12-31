# Parallel Functional Programming: Maximal Clique

## Build
Dependencies: Haskell toolchain including Cabal for building and packaging the project. <br/>
We use [ThreadScope](https://github.com/haskell/ThreadScope) to profile parallel Haskell programs. Firstly, use `cabal get threadscope` to install it.

### MacOS

GTK+ is required.

```sh
brew install gtk+
```

Then build using cabal:

```sh
cabal --project-file=cabal.project.osx v2-build # to only build the project
cabal --project-file=cabal.project.osx v2-install # to build and install the binary
```

### Linux

GTK+2 is required to be installed. On Ubuntu-like systems:

```sh
sudo apt install libgtk2.0-dev
```

Then you can build threadscope using cabal:

```sh
cabal v2-build   # to only build the project, or
cabal v2-install # to build and install the binary
```

## Run

To find all maximal cliques for a graph

```sh
# +RTS = Run Time System, -N2 = use 2 cores, flags are for the eventlog
# running the algorithm
# generate a graph
# test the output to determine if it's valid given input
cabal exec -- MaximalClique-exe compute ${input_file} ${output_file} ${mode} +RTS -N2 -lfs
cabal exec -- MaximalClique-exe gen <mode> <args>..<args> <outfile>
cabal exec -- MaximalClique-exe test <inputfile> <outputfile>
```

modes for computational functionality
```sh
compute ${input_file} ${output_file} seq
compute ${input_file} ${output_file} par_naive
compute ${input_file} ${output_file} par_basic
compute ${input_file} ${output_file} par_chunked chunkSize
compute ${input_file} ${output_file} par_depthLimited maxDepth
```

modes for generation functionality
```sh
gen random n ${output_file}
gen kclique n k ${output_file}
```

Then a log file with extension `.eventlog` should be generated, which can be accessed using

```sh
threadscope MaximalClique-exe.eventlog
```
