#!/usr/bin/env bash

[[ ! -d dist ]] && mkdir dist

pandoc \
    --standalone \
    -t revealjs \
    -o dist/slides.html \
    slides.md
