#!/usr/bin/env bash

cabal build Examples.hs > /dev/null

echo "Quicksort - persistent natural"
./Examples.hs --variant persistent-a quicksort

echo && echo "Quicksort - persistent in-place"
./Examples.hs --variant persistent-b quicksort

echo && echo "Quicksort - mutable in-place"
./Examples.hs --variant mutable quicksort

echo && echo "Dijkstra - persistent in-place"
./Examples.hs --variant persistent dijkstra

echo && echo "Dijkstra - mutable in-place"
./Examples.hs --variant mutable dijkstra
