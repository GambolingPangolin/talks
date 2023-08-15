#!/usr/bin/env bash

[[ ! -d dist ]] && mkdir dist

pandoc \
    --embed-resources \
    --standalone \
    -t revealjs \
    -o dist/slides.html \
    slides.md
