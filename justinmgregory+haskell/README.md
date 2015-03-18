Haskell Chess Engine
====================

By Justin Gregory

jmgregory@gmail.com, @justinmgregory


Intro
-----

I built this some years ago as an attempt to learn more about Haskell and FP.  The goal was to write an engine that could beat me.  (This is not as high a bar as it may sound).  I also intentionally avoided any literature about computer chess or how to write a chess engine, hoping to learn better by figuring things out on my own, and also to avoid any preconceptions based on imperative approaches.

This engine runs pretty slowly, even looking only two moves ahead, but it does make moves with some semblance of intelligence.  Then again, sometimes it commits suicide in the mid-game for no apparent reason, and it is still fairly easy to beat.

An experienced Haskell-er will probably be able to point out lots of better ways to do things.  I ran it to make sure it still works, but I haven't gone back to look at the code, mainly so I can avoid the feelings of revulsion one often gets when reading old code.  All that is to say that the reader may want to take things with a grain of salt.


Requirements
------------

To build, you'll need GHC <https://www.haskell.org/ghc/>, along with the MTL library, which you can install using Cabal <https://www.haskell.org/cabal/>.  You'll also need GNU Make, or you can just read the Makefile to see the commands to run.

This program was designed to work with XBoard <http://www.gnu.org/software/xboard/> as the front end.

If you're on OS X, you can install all these using Homebrew:

	$ brew install ghc cabal-install xboard


Running
-------

Assuming all the requirements are met, you can test your skills against the engine using:

	$ make play

White (that's you) goes first.
