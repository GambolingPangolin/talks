#! /bin/sh

mkdir -p dist

pandoc \
  --self-contained \
  --standalone \
  -t Slidy \
  -o dist/slides.html \
  slides.md

pandoc \
  --standalone \
  -t beamer \
  -o dist/slides.pdf \
  slides.md
