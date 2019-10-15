#! /bin/sh

pandoc \
  --standalone \
  -t Slidy \
  -o dist/slides.html \
  slides.md
