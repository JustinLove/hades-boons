# Hades Boon Chart

Interactive web chart of boons in [Hades](https://www.supergiantgames.com/games/hades") by [Supergiant Games](https://www.supergiantgames.com/) showing duo and legendary requirements.

## Art

God and boon icons and frames are from Supergiant Games Hades. This site is not developed or approved by Supergiant games.

Social and other svg icons: [IcoMoon - Free](https://icomoon.io/#icons-icomoon) ([CC BY 4.0](http://creativecommons.org/licenses/by/4.0/))

## Development

Built in [Elm](https://elm-lang.org/)

My build command:

> elm make src/HadesBoons.elm --output public/hades-boons.js

`bin/monitor.sh` has a command using the [watch](https://www.npmjs.com/package/watch) CLI

note: `elm reactor` will not work as a dev web server if live serving `.dxf` files. `bin/server.sh` has a python server (this supports caching, you may need to open browser dev tools to disable caching)

God layouts developed in [QCAD](https://qcad.org/en/). The paid version is required to support DXF version with metadata annotations.

Layouts are compiled to elm code for standard runtime (they can be loaded from dxf with a small code change, but this is quite slow) This step uses [Node](https://nodejs.org/en/)

> elm make src/ImportData.elm --output js/ImportData.js
> node js/import-data.js

Game data extraction (`proc` folder) has been more of a one-off setup using [Lua](https://www.lua.org/) command line and little bit of Ruby glue code. It likely has references to my local filesystem paths
