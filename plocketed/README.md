## What is this?

This program works with `socketed` and creates stacked line graph in browser from data taken from stdin.

## Data Input Format

```
<name of the data line> <data value>
```

## Sample Usage

- create script to monitor vm usage of a pidof

```
#!/bin/env bash
# monvm.sh
while v=`grep VmSize /proc/$1/status | grep -o '[0-9]*'`; do echo $1 $v; sleep ${2:-1}; done

```
- plot the vm usage
```
monvm.sh `pidof hsdev` | plocketed | socketed -p 3333
```
- visit `localhost:3333` for result
- monitor multiple processes
```
mkfifo pipe
cat < pipe | plocketed | socketed -p 3333
monvm.sh `pidof hsdev` > pipe
monvm.sh `pidof node` > pipe
```
