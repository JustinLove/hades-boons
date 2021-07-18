#!/usr/bin/env bash
elm make src/ImportData.elm --output js/ImportData.js
node js/import-data.js
elm make src/HadesBoons.elm --optimize --output public/hades-boons.js
