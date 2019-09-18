# Applied FP Course

This course was created to fill a gap that between students completing introductory
materials and students being able to build functional applications. I had recently watched
the talk titled: "Your first Haskell app" and that became the basis for what is now the
applied fp course.
 
There were some properties that I thought the course had to have in order to be useful,
within these sort of vague guidelines:

**No one ever reads the README for the individual levels. You'll just have to come to terms with that.**

## Boring is best.

Keep your inner type-astronaut under control, seriously.

No snowflake abstractions or going off with polysemy fun-times. Use boring techniques that
you are likely to see in almost any Haskell application.

## Go into the weeds first. 

If you don't understand the problem, you can't understand the solution. The utility of the
custom monad that students build wouldn't make much sense without the pain of juggling the
`IO`, `Either`, and `IO (Either ...` types.
   
Generally, lead with the problem, this motivates working towards a solution.

### Make them try to solve it first.

Students really appreciate when you take the time to work through a problem for the
room. Talking through each bit to a solution. But only *after* they've tried or completed
the exercise themselves. Otherwise they just tune out, go "huh, okay", then struggle
later.

## Don't hide anything.  

The point is not to provide a totally perfect learning application, but to learn by
building an application. Some packages are going to have pockets of suck, just like any
other language. Part of the course is learning to navigate the ecosystem so encountering
this is helpful, if a little icky.

## Use packages from Hackage

I consider it better to expose students to packages that exist separate to the
course. Without an **extremely** good reason there should not be any purpose built
packages in use in the course.

## Let nothing be thought of as "magic"

Numerous students were convinced that the compiler "knew" about the `ReaderT` structure
and that it must be the *compiler* doing something to inline the `Env` where ever the
`ask` function was used. It took a lot to have them prove to themselves that it wasn't
compiler "magic" and it is just functions all the way down.
   
Alleviating students of the concept of "magic" is an extremely important goal. It is
empowering for students to know that the benefits they can reap are from a deeper and
broader understanding of things they are often already aware of.

# "Pro" tips

## Help students by asking them questions

This is a teaching strategy that I have found to be very effective. I usually start by
asking "what are you tring to achieve?", because their initial question will often be
extremely specific and will have their own assumptions attached. It will be a focused "How
do I make Haskell do `X`", when what we need to work out is where they have 'diverged',
then work with them towards a suitable solution.

When working with the student towards a solution, I try to only ask questions that guide
their thinking. Or better yet cause them to explain their problem more thoroughly and thus
I end up just being a rubber duck, this is the best possible outcome.

With either pen & paper or code comments, writing out the various types involved is
suuuuper helpful. A good exercise is to have the student do some basic substitution so
they can see how the ridiculously abstract types move around in very simple (mostly) ways.

## Typed holes.

Remind them so often they consider telling you to shut up.
   
## Tests are awesome

The Level03 tests aren't much of an exercise on their own. However as the application
becomes more complex, remind the students that they can fill out the tests to ensure the
application is doing what they intended.

Types are awesome, tests are cool, types AND tests is pretty much perfect.

It is also worth calling out the existence of "ruby mode", aka `-fdefer-type-errors`. For
those that want to write tests first and then fill the functionality that eventually type
checks and passes tests.
