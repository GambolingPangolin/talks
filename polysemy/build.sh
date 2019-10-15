#! /bin/sh

pandoc \
  --standalone \
  -t Slidy \
  -o dist/slides.html \
  slides.md

haddock \
  -o dist/game-0 \
  --hyperlinked-source \
  game-0/Main.hs

haddock \
  -o dist/game-1 \
  --hyperlinked-source \
  game-1/Main.hs

haddock \
  -o dist/game-2 \
  --hyperlinked-source \
  game-2/Main.hs
