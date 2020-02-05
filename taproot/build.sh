#! /bin/sh

mkdir -p dist

pandoc \
  --standalone \
  -t beamer \
  -o dist/slides.pdf \
  slides.md
