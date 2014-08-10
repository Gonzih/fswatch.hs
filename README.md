![fswatch.hs](https://dl.dropboxusercontent.com/u/4109351/github/fswatch.png)

### Usage:

FSWatch will execute `sh .fswatch` in current directory on every change providing file path and event type (added, modified, removed).
It's very simple util (at least for now) that leverages all filtering logic to underlying scripts.

### Installation:

* git clone https://github.com/Gonzih/fswatch.hs.git
* cd fswatch.hs
* cabal install

### What if I don't want to use `sh` as language for .fswatch file?

Currently it can be solved like that:

```.fswatch
fish .fswatch.fish $@
```
