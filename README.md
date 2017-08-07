Advanced FP Course
=====================

You
---

* Have completed, or are capable of completing, the [Data61 FP Course](https://github.com/data61/fp-course)
* Have a few months self-study to your name
* Want to know how to build larger applications with statically typed FP
* Are willing to accept that a web application is a sufficient choice

We
--

* Have constructed a sequence of goals of increasing difficulty
* Have provided a framework within which to apply these goals
* Have included relevant components of larger applications:
  - Package dependencies
  - Project configuration
  - Application testing & building
  - Encoding / Decoding messages (JSON & Binary)
  - Persistent storage integration
  - App state & configuration management
  - Error handling & reporting
* Will utilise both type & test driven development techniques
* Will explain architectural and design trade-offs when appropriate

Ultimate Goal
===============

This course aims to teach some of the techniques for building a larger
application with FP, using Haskell. By the end of this course you should be
comfortable tackling more advanced projects, and expanding on the concepts and
choices presented here in your own future efforts.

We will build an ultra basic web application and build upon it.

Subgoals (?)
========
- ghcid
- hedgehog
- cabal files

Goals
======

1 - Death to Strings
---
Start up and Servant introduction.

a) Use Scotty/Warp for the "hello, world" application.
b) Explain that we're using strings for routes and this is bad(TM).
c) Move to Servant, explain why, show type driven dev to explain routes->function relationship.

2 - Faking Global Vars with Science
---
Show that nothing can be changed in the current app without recompliation.
Rework the application so we can change the port values and have general app
config.

a) Read a file in app start up to allow for port config changes.
b) Explain that this can be cumbersome to explicitly pass the config around, there are better ways(TM).
c) Add `mtl` dependency.
d) Natural Transformation required from our new 'ReaderT' to Servant (too much?).

3 - Would you like to play a game?
---
Introduce handling input/output also preempt the inclusion of persistent storage.

a) Add rock-paper-scissors data type. (Don't create Enc/Dec instances yet)
b) Add route to accept an RPS move that always plays paper against the user.
c) Go over what the errors from Servant & GHC are telling us.
d) Add the required instances.
e) Change the function up so that it randomly selects a move, evaluates victory/defeat.

4 - Type safe tantrums
---
Introduce error handling by breaking REST rules by having our application throw
an error when it loses at Rock-Paper-Scissors.

a) Change RPS function to throw an error on defeat, creating required error data types.
b) Discuss the errors that appear RE return values and making Servant respond appropriately.
c) Introduce 'ExceptT', change the 'ReaderT' from earlier to what we want it to be.
d) Discuss the errors that appear. Work through fixing these with type-holes in the Natural Transform.

5 - Elephants
---
We'd like to be able to store a history of RPS games. 

Stringly queries acceptable for now - postgresql-simple

Thing(s) to unravel first...

Exceptions - catching, throwing, gotchas (throw vs throwError)
