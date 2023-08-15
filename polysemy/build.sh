#! /bin/sh

case $1 in

  slides)

    pandoc \
      --embed-resources \
      --standalone \
      -t Slidy \
      -o dist/slides.html \
      slides.md ;;

  execs)

    cabal v2-build \
      --enable-documentation ;;

  sources)

    mkdir -p dist/game-0
    mkdir -p dist/game-1
    mkdir -p dist/game-2

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
      game-2/Main.hs ;;

  *)

    echo "USAGE: ./build.sh (slides|execs|sources)" ;;

esac
