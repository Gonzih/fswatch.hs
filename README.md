## FSWatch [![Build Status](https://travis-ci.org/Gonzih/fswatch.hs.svg?branch=master)](https://travis-ci.org/Gonzih/fswatch.hs)

![fswatch.hs](https://dl.dropboxusercontent.com/u/4109351/github/fswatch.png)

### Usage:

FSWatch will execute `sh .fswatch` in current directory on every change providing file path and event type (added, modified, removed).
It's very simple util (at least for now) that leverages all filtering logic to underlying scripts.

### Installation:

```sh
git clone https://github.com/Gonzih/fswatch.hs.git
cd fswatch.hs
cabal install
```

### What if I don't want to use `sh` as language for .fswatch file?

This can be solved like that:

```sh
#.fswatch

fish .fswatch.fish $@
```

Or like that:

```
fswatch --execute-with "fish"
```

### Locking and skipping events while running task

Here is small snippet with locks:

```sh
run_longrunningtask(){
  touch .fswatch.lock
  long_running_command
  rm -f .fswatch.lock
}

if [[ ! -f .fswatch.lock ]]
then
  if [[ $1 == *.myfavoriteextension ]]
  then run_longrunningtask &
  fi
fi
```

It will ignore all events up until long running task is done.
