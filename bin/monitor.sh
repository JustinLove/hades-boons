#!/usr/bin/env bash
watch "elm make src/HadesBoons.elm --output public/hades-boons.js" src/ generated/ ../elm-collage/src ../elm-canvas/src
