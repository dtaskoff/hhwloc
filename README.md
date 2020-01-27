# hhwloc
[![hackage](https://img.shields.io/hackage/v/hhwloc.svg?logo=haskell&label=hhwloc)](https://hackage.haskell.org/package/hhwloc)

Haskell bindings to https://www.open-mpi.org/projects/hwloc

## Prerequisites
You'll need [autoconf](https://www.gnu.org/software/autoconf/), [automake](https://www.gnu.org/software/automake/), [make](https://www.gnu.org/software/make/), and [libtool](https://www.gnu.org/software/libtool/) (glibtool on macOS), in order to build this package.

On *Windows*, if you're using [stack](https://docs.haskellstack.org/en/stable/README/), they can be installed with the following commands:
```
stack exec bash
pacman -Syuu
pacman -S --needed autoconf automake make libtool
```
