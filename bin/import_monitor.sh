#!/usr/bin/env bash
watch "elm make src/ImportData.elm --output js/ImportData.js" src/ & watch "node js/import-data.js && elm make src/HadesBoons.elm --output public/hades-boons.js" js/
