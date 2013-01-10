ConfSchedule
=============

Tool collection for [ConfSchedule.net](http://confschedule.net/ "ConfSchedule").

Setup and Building
--------
1.  Make sure you have haskell setup on your machine. ;-)
2.  Clone this repo using `git clone`
3.  Configure using `runhaskell Setup configure --prefix=$HOME --user`
4.  If you get any errors, install missing dependencies via `cabal install [PACKAGE]`
5.  Build using `runhaskell Setup build`


Usage
--------
Once you've built the tool, run using `configtool file [file ...]`
