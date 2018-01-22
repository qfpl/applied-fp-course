Use of lenses and classy `mtl` will be left to supplemental material. It would
be nice to be able to use it lenses more but the additional explanation required
might be a bit much and not very predictable ?

### Further lessons

N - Death to Strings
---
Start up and Servant introduction.

* Use Scotty/Warp for the "hello, world" application.
* Explain that we're using strings for routes and this is bad(TM).
* Move to Servant, explain why, show type driven dev to explain routes->function relationship.

N - Would you like to play a game?
---
Introduce handling input/output also pre-empt the inclusion of persistent storage.

* Add rock-paper-scissors data type. (Don't create Enc/Dec instances yet)
* Add route to accept an RPS move that always plays paper against the user.
* Go over what the errors from Servant & GHC are telling us.
* Add the required instances.
* Change the function up so that it randomly selects a move, evaluates victory/defeat.

N - Type safe tantrums
---
Introduce error handling by breaking REST rules by having our application throw
an error when it loses at Rock-Paper-Scissors.

* Change RPS function to throw an error on defeat, creating required error data types.
* Discuss the errors that appear RE return values and making Servant respond appropriately.
* Introduce `ExceptT`, change the `ReaderT` from earlier to what we want it to be.
  * More discussion to be had here regarding the ordering of transformer stacks.
* Discuss the errors that appear. Work through fixing these with type-holes in the Natural Transform.

N - Except when exceptionally excepted
---
Handling, catching, and re-throwing exceptions. Motivate errors as values over exceptions.

* Possible ways postgresql-simple can fail on us, note that the types provide no information
* Introduce exception handling with `catching` and `handling` to our error values.
* Generalise our current DB querying technique so that we take a query and
  handle some exceptional cases.
* Discuss the relevance of handling these errors:
  * Shouldn't they just come up in testing?
  * I'm sure there are other points here, some Haskell applications are built
    not to care about these sorts of errors.

* Discuss how you might implement logging ? Leave as exercise.

N - BOSS FIGHT - A
---
Replace the DB layer with something better:
- [Selda](https://selda.link/)
- [Tisch](https://github.com/k0001/tisch)
- [Opaleye](https://hackage.haskell.org/package/opaleye)
- [Groundhog](https://github.com/lykahb/groundhog)

These packages will force a lot of other things to be picked up at the same
time, to varying degrees:
- Selda : Type level lists, type operators, data structures as tuples initially.
- Tisch : Type level HLists, overloaded labels, type families, familiarity with Opaleye.
- Opaleye : Arrows, full polymorphic records.
- Groundhog : Alien data type definitions using their Template Haskell DSL, is
  an ORM more than a type safe SQL package (if that makes sense).

All packages use generics of some description, not sure how much air time they
need? Passing mention and throw some links around, move on.

N - BOSS FIGHT - B
---
Integration of 'classy mtl' style application design.

* Briefly explain the problem with using a transformer stack or newtype'd stack
  like we have.
  * Concrete types
  * Cannot generalise easily to abstract out larger moving parts
* Discuss what we want is just constraints, PARAMETRICITY driven development!!
* Build intuition for `AsFoo` & `HasFoo`
* `mtl` classes are a start, but how do we make this easier?
* Describe `CanFoo` and introduce `ConstraintKinds`

## Suggestions

* Lens
* Mocking / Free
* Dealing with callbacks (more info needed)
* Streaming / Iteratees (safe resource handling)
* ST / Safe Mutation techniques
* Classy MTL
* Generic / Meta programming
* Daemons
* Ermagerd reel librees!
* Concurrency packages/techniques (book recommendation) - STM
* Property Based Testing (Hedgehog, advanced testing)
* Stats
