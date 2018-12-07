# [Workshop] Levels

This level is the start of the [Workshop] levels. These are levels that _are
still in development_ so they will not be as well structured or tested as
earlier levels.

You are invited to give them a go if you are ready for a challenge. But be
prepared that the instructions may not be as clearly defined as earlier levels.

There is also no code provided as the expectation is that you have a _completed_
Level07 `Application` that you will copy and refactor to meet the requirements.


## Level 08 [Workshop]

This exercise introduces the [lens](https://hackage.haskell.org/package/lens)
package. We will be writing some `Lens`es and `Prism`s for our various data
structures, then refactoring some of our code to take advantage of them.

Building up a solid theoretical foundation of what a `lens` is, is beyond the
scope of this level. We will be making use of a far too popular software
development technique of 'cargo-culting'.

This is not to imply that a solid theoretical foundation of lenses is beyond
you, far from it. But there is a more focused and in-depth course that deals
with this subject to a far more satisfying depth. You would be better served by
delving into the details there.

See [lets-lens](https://github.com/data61/lets-lens) for the juicy details.

We have two main goals:

A) Develop your _intuition_ of what a lens is
B) Crush any notion that lenses are super complicated things, used only by
   wizened Haskellers and skittery mathematicians to write inscrutable code.

Our approach is to:

* Discuss some analogies
* Build some
* Use them

[lens intro examples](https://github.com/ekmett/lens/wiki/Examples)
[derivation of lenses](https://github.com/ekmett/lens/wiki/Derivation)


## Level 09 [Workshop]

Classy MTL

[a talk](https://www.youtube.com/watch?v=GZPup5Iuaqw)
[a slides](https://github.com/gwils/next-level-mtl-with-classy-optics)
[a blog](https://carlo-hamalainen.net/2015/07/20/classy-mtl/)

1) Reimplement AppM with ReaderT & ExceptT 
  You may use GeneralizedNewtypeDeriving to make this easier.

2) Define Prisms and 'AsError' typeclass for 'Error' type

import Control.Lens (Prism', prism')

class AsError s where
  _Error :: Prism' s Error
  
  _EmptyTopic       :: Prism' s ()
  _EmptyCommentText :: Prism' s ()
  _DBError          :: Prism' s SQLiteResponse
  ...
  ...

3) Define Lenses and 'HasEnv' typeclass for 'Env' type
- refer to 'Control.Lens.TH' for more info about these classes

class HasEnv t where
  env :: Lens' t Env

class HasConf t where
  port :: Lens' t Port
  dbFilePath :: Lens' t DBFilePath

3b) Reimplement 'getDBConn' in DB.hs

4) Define 'AsAppM' constraint: 
  'type AsAppM m = ...'
  You'll need 'ConstraintKinds' for this.

5) Begin refactoring function types to utilise these constraints:
  'f :: AsAppM m => ... -> m a'
  Only the top level should need to use concrete 'AppM' type.

We want to be able to write functions similar to:

f :: (AsError e, MonadError e m) => a -> m ()

# Level 10 [Workshop]

Reimplement routes using Servant, integrate with existing AppM

# Level 11 [Workshop]

Implement Hedgehog property tests
** Bonus points: Implement tests to prove your typeclasses satisfy the laws (hedgehog-fn package)

Add Hedgehog dependency
Add separate hedgehog test-suite
