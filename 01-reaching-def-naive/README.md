# Data flow analysis - 01

A naive implementation of the reaching definition analysis in chapter 1 of Nielson, Nielson, and Hankin, _Principles of Program Analysis_.


## Setup

Pull this repo somewhere, e.g.,:

    git pull https://... /path/to/local/repo

Download the latest haskell docker image:

    docker pull haskell:latest

Start it up, mounting the repo:

    cd /path/to/local/repo/01-reaching-def-naive
    docker run --rm -ti -v $(pwd):/external -w /external haskell:latest bash

Update cabal's package list:

    cabal update

Upgrade cabal:

    cabal install cabal-install
    hash -r

Check versions:

    ghc --version
    cabal --version


## Use the Library

Run the REPL:

    cabal repl

Look at the example programs:

    ghci> Examples.factorial
    ghci> Examples.power

Run the round robin algorithm on, say, the factorial example:

    ghci> analysis = Equations.roundRobin Examples.factorial
    ghci> printableVersion = Pretty.print analysis
    ghci> putStr printableVersion

