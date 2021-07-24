#!/usr/bin/env bash
watch "lua54 proc/text.lua > output/en.json & cat output/en.json & echo 'done'" proc/
