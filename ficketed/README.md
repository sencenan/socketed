## What is this?

This program works with `socketed` and creates a simple static server where if the file path is received from stdin, the served file would refresh in the browser

## Install
```
cabal install socketed ficketed
```

## Data Input Format

```
<file path from server root>
```

## Sample Usage

- start server

```
tee | ficketed -p 8080 -w 3333 . | socketed -p 3333
```
- visit `localhost:8080`
- to refresh a file already openned in a browser type `<filepath>` in stdin and hit enter
