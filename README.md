# Advanced Functional Programming Course

<img src="http://i.imgur.com/0h9dFhl.png" height="400" width="640" />

### You:

* Have completed, or are capable of completing, the [Data61 FP Course](https://github.com/data61/fp-course).
* Have a few months self-study to your name.
* Want to know how to build larger applications with statically typed FP.
* Are willing to accept that a web application is a sufficient choice, for now.

### We:

* Have constructed a sequence of goals of increasing difficulty.
* Have provided a framework within which to apply these goals.
* Have included relevant components of larger applications:
  - Package dependencies
  - Project configuration
  - Application testing & building
  - Encoding / Decoding messages (JSON & Binary)
  - Persistent storage integration
  - App state & configuration management
  - Error handling & reporting
* Will utilise both type & test driven development techniques.
* Will explain architectural and design trade-offs when appropriate.

### Setup build tools:

You're welcome to setup your own project with ``$ cabal init`` and follow along
with the general flow of the course, or work through the exercises in the
projects provided.

Each level is a self-contained Haskell application, containing incomplete, or as
yet undefined, data types and functions. There is a cabal file and Nix
configuration files for each level, so you can use either cabal sandboxes or a
``nix-shell``, depending on your preference.

To use a sandbox:
```bash
$ cd <levelN>
$ cabal sandbox init
$ cabal configure
$ cabal install --only-dependencies
```
The normal cabal build commands should then work as expected. We do recommend
using cabal sandboxes as they provide a contained Haskell environment for a
given project. Easy to clean up, and package versions won't conflict with any other
sandboxed project you may be working on. Hence their name.

To use the Nix Shell:
```bash
$ cd <levelN>
$ nix-shell
```
Once that completes you will be in a ``nix-shell`` environment with all the
tools required to build the application for that level. Note that the
levels build on each other, so you can go to the highest level and enter a
nix-shell there, you will then have all the required tools for every level.

The ``shell.nix`` is provided separate from the ``default.nix`` so if you have a
different workflow you can still utilise the derivation.

##### Please note...

These lessons are designed to be completed with an instructor as part of the
Data61 Advanced Functional Programming Course. You are of course welcome to
clone the repository and give it a try, but you may find the tasks more
difficult. If you have any questions we can be contacted in the
Freenode [#nicta-course IRC channel](https://freenode.net). You can use the
free [WebChat client](https://webchat.freenode.net).

#### Subsequent lessons may contain spoilers, don't cheat yourself out of the experience!

There is a ``README.md`` file in each Level project that will provide instructions about
what the goal is for that specific level.

* Level 01 : Simple Hello World web app.
* Level 02 : Define our application spec with types!
* Level 03 : Add some flexible configuration
* Level 04 : Testing & Tools (hspec & ghcid)
* Level 05 : Database layer (sqlite-simple)
* Level 06 : ReaderT & Refactoring
* Level 07 : ExceptT & Refactoring

-- Coming Soon...
* Level 08 : (Bonus Round) Lenses & Refactoring

-- Maybe...
* Level 09 : Add session controls (login, logout) and a protected route. So we
  can have something that resembles application state. For the purposes of
  modelling the state machine and implementing some property based tests.

