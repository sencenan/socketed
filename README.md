## What is this?

This program send data from stdin to a websocket client which `eval` the said data line by line. This forms a base onto which other stdin to websocket application can be made.

## Install
```
cabal install socketed
```

```
socketed

Usage: socketed [-b|--bind STRING] [-p|--port INT]
  socketed

Available options:
  -h,--help                Show this help text
  -b,--bind STRING         host ip the server will be bind
                           to (default: "0.0.0.0")
  -p,--port INT            port number the server will be running
                           on (default: 3000)
```
