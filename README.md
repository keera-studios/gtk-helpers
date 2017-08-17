# gtk-helpers
Auxiliary Gtk+ 2 and Gtk+ 3 operations

Instructions on how to run a game:

1)`git clone https://github.com/keera-studios/gtk-helpers.git`

2)`cd gtk-helpers/gtk2`

_Creating a sandbox (or contained environment for haskell stuff) inside the directory gtk-helpers/gtk2_.

3)`cabal sandbox init`


_Installing the package gtk-helpers in that directory (cabal install finds the only .cabal file in that dir)._

4)`cabal install`

**Note**: Some MacOS users may experience a problem upon executing the above command for which the discussion can be found [here](http://stackoverflow.com/questions/43331920/installing-cabal-packages-returns-errors).

In that case, you need to first execute `cabal install gtk -fhave-quartz-gtk` and then `cabal install`.

If the problem isn't resolved even after executing `cabal install gtk -fhave-quartz-gtk` command, you may want to refer [this](http://stackoverflow.com/questions/43359289/architecture-x86-64-while-running-haskell-code-haskell-osx-iconv/43390487#43390487) for additional information.


_For e.g., if you want to play lights-off game,_

5)`cd examples/lights-off/`


_We move into examples/lights-off, and tell cabal to use the sandbox in gtk-helpers/gtk2 for anything we run in this directory._

6)`cabal sandbox --sandbox=../../.cabal-sandbox/ init`

**Note**: Only for lights-off game, an additional package `IfElse` needs to be installed.
This can be done by running `cabal install IfElse`command  after Step 6 and before Step 7.


_Finally, we compile BoardMain.hs. We use cabal exec to help GHC find the sandbox where all the packages have been installed._

7)`cabal exec -- ghc --make BoardMain.hs`


The logic, math and implementation of these games can be read [here](http://keera.co.uk/blog/2013/03/19/creating-board-games-in-haskell/).

All the credit for the above goes to [Ivan Perez](https://github.com/ivanperez-keera).

