# Conditional Restriction Parser

This project includes a library that can be used to handle [OpenStreetMap (OSM) Conditional Restrictions](https://wiki.openstreetmap.org/wiki/Conditional_restrictions)
as well as a console-based application
that can be used for simple parsing and evaluation of conditional restrictions.

Both the library and the application are implemented in [Haskell](https://www.haskell.org/),
using [Stack](https://docs.haskellstack.org/en/stable/README/).
You can find the library code under `src/` and the application code under `app/`.

The library is designed to work with conditional restrictions as
[documented on 14. April 2022 on the OSM Wiki](https://wiki.openstreetmap.org/w/index.php?title=Conditional_restrictions&oldid=2310830).
Conditional restrictions also support OSM Opening Hours, for which only a subset
has been implemented in this project. The versions used are
[Key:opening_hours on 10. April 2022](https://wiki.openstreetmap.org/w/index.php?title=Key:opening_hours&oldid=2309608)
and [Key:opening_hours/specification on 04. November 2021](https://wiki.openstreetmap.org/w/index.php?title=Key:opening_hours/specification&oldid=2215648).

## Compiling the project

To compile the current state of the project, you'll need a working [Stack](https://docs.haskellstack.org/en/stable/README/)
setup and a working [GHC](https://www.haskell.org/ghc/) setup. The easiest way to set those both up is
probably with the [GHCup](https://www.haskell.org/ghcup/) tool.

Following versions have been found to be working for this project:
- GHCup v0.1.17.7 (newer probably possible)
- Stack v2.7.5 (newer probably possible)
- GHC v9.0.2 (should match your installation)

The build has only been tested on (Manjaro) Linux, but other operating systems
should work, as long as this setup is present. All you need to do in order to
compile the library and the application is to execute

``` sh
stack build
```

on a shell of your choice (Stack needs to be in your
[$PATH](https://linuxconfig.org/linux-path-environment-variable)) in the project
folder. Stack will output something like this:

```
Installing library in /home/user/workspace/conditional-restriction-parser/.stack-work/install/x86_64-linux-tinfo6/577aef19c282e0a36f2ce11fb38f5391ea06983467b015a6bfb319f9dd99acfe/9.0.2/lib/x86_64-linux-ghc-9.0.2/conditional-restriction-parser-0.1.0-45ZsUYVvY6oL6ghNWUuJIJ
Installing executable conditional-restriction-parser-exe in /home/user/workspace/conditional-restriction-parser/.stack-work/install/x86_64-linux-tinfo6/577aef19c282e0a36f2ce11fb38f5391ea06983467b015a6bfb319f9dd99acfe/9.0.2/bin
Registering library for conditional-restriction-parser-0.1.0..
```

From which you can see where the built library and where the built application
was stored. If you want to install the application in your $PATH, you can
execute

``` sh
stack install
```

which will install the built application in a dedicated directory. The executable will be named `conditional-restriction-parser-exe`.

## Developer Setup

If you want to develop new features or fix a bug, you'll need to set up a
developement environment. First, follow the steps for [compiling the
project](#compiling-the-project). Any IDE that supports Haskell
works, but this project has been developed with [Doom
Emacs](https://github.com/doomemacs/doomemacs),
[LSP](https://microsoft.github.io/language-server-protocol/)
and [HLS](https://haskell-language-server.readthedocs.io/en/latest/what-is-hls.html) version 1.6.1.0.
Please use [Ormolu](https://github.com/tweag/ormolu) for formatting your code if you want
to contribute your code to the project, this will guarantee consistent style.

Execute unit tests and integration tests using

```sh
stack test
```

in the project folder. If you want to check the test coverage, use

``` sh
stack test --coverage
stack hpc report .
```

## License 

This project is licensed under AGPL version 3 (only).
