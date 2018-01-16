Compilation of instructions and important things to remember for the different
levels.

# Level 1

We're going to be building an extremely simple web server using the 'wai'
framework. Wai is a low level HTTP implementation similar to WSGI in Python or
Rack in Ruby.

- Hackage
  - Primary repository for Haskell packages and their documentation.
  - Package **Index** is extremely useful for discovering and interrogating
    packages.

# Level 2

We're going to start building our own REST application by converting our "spec"
into a data structure that will guide the construction of the rest of the
application.

We'll also demonstrate the usefulness of ``newtype`` and how it can trivially
make code more type safe, as well as providing more descriptive type signatures.

# Level 3

Our application is starting to grow we should add another layer of assurance by
writing some tests.

We will cover:
- How to write a ``test-suite`` in the cabal file.
- Usage of the ``hspec`` library as our test runner and how to feed it our
  application.
- We will also write some tests using the ``hspec-wai`` package to give us some
  basic confidence in our error handling.

# Level 4

This covers adding a database to our application.
- We use SQLite because it requires the least amount of setup on the host machine.
- All of the SQL is provided.

We will have the beginnings of some configuration in our application, but we
will simply hardcode this for the time being as we will have a better solution
in the next level.

We try to emphasise the distinction between data to be used externally in the
Database vs the data we deal with internally in the application through the
construction of our DBComment type.

The sqlite-simple-errors package is used to run our queries so that we have our
errors as values, which is what we are doing in the rest of the application.

Again there will be a lot of documentation diving for this part as people will
need to find the functions that will be used to run the queries against the
database.

This level might be a bit hard to navigate as there are a bunch of files to walk
through.

The steps for this level:
1) ``src/FirstApp/DB/Types.hs``
2) ``src/FirstApp/Types.hs``
3) ``src/FirstApp/DB.hs``
4) ``src/FirstApp/Main.hs``

- Call out the encoding instances & the automatic deriving of the ToJSON instances

# Level 5

We construct a proper method of handling our configuration.

This is a complex level due to having to handle the two configuration datatypes,
plus the monoid instance. Additionally the configuration file is a JSON file
that we manually extract information from using decoding functions from the
aeson package. There will be documentation diving and possibly tricky functions
involved.

I think it is important to not let people become hung up on the Monoid instance
(it is partially completed already) additionally care needs to be taken that
searching for the functions they need from the aeson library isn't too painful.
Give hints or just tell them which functions.

The optparse-applicative library is used to handle the command line parsing but
the students do not have to implement it themselves. It's it a gnarly library
despite its usefullness, so it's there for people to look at and refer back to.
But that's about it.

Exceptions will be discussed when working on the 'readObject' function to load
the file into a JSON Value.

# Level 6

This is "The ReaderT" level.

Students will be required to copy their completed versions of functions from
previous levels that will then break in this level and need to be refactored.
The bulk of the work will most likely be in the DB module as the DB functions no
longer require the DB connection to be passed in manually.

Also there are functions in the FirstApp/Main module that will need to be
updated to handle the new shenanigans.

# Level 7

This is the "ExceptT" level.

Students go back to AppM module to see how the new transformer plugs into our stack, there are some functions to complete in here.

Then as per the previous level, the students have to copy over the completed
functions from the previous level to see what breaks and what needs to be fixed.
There will be a lot to do in the DB and FirstApp/Main modules.


## General Notes - More to add.
- structure repo
- readme in each repo
- monoid instance - rehash single/multiple number of possible instances. Don't let people hang too long on this point.
- how to find the documentation for the Header / ContentType
- more instruction that lead people to hackage documentation for Text/ByteString etc
- mention that import lists may need to be updated for the new types
- some editors will need to jump in and out of the different levels (close, cd, re-open)
- Mention that it's fine to use case statements for Either handling, we make it okay at the end.

### IMPORTANT! 
- Stephen Diehl - What I Wish I Knew Learning Haskell
- Ask students if they would prefer access to a prepared VM with the code & an editor or two.
