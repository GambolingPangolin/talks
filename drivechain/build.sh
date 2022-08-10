#!/usr/bin/env bash

[[ ! -d dist ]] && mkdir dist

pandoc \
    --self-contained \
    --standalone \
    -t revealjs \
    -o dist/slides.html \
    slides.md
