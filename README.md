ConfSchedule
=============

Tool collection for [ConfSchedule.net](http://confschedule.net/ "ConfSchedule").

Setup and Building
--------
1.  Make sure you have a recent version of haskell installed on your machine. ;-) ghc-7.4.2 will do
2.  Clone this repository using `git clone`
3.  `cd configtool`
4.  Configure using `runhaskell Setup.hs configure --prefix=$HOME --user`
5.  If you get any errors, install missing dependencies via `cabal install [PACKAGE]`
6.  Build using `runhaskell Setup.hs build`
7.  Install using `runhaskell Setup.hs install`


Usage
--------
Once you've built the tool, run using `configtool file [file ...]`
