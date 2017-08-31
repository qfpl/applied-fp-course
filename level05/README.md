# Level 05

We need a place to store our Comments/Topics, so we're going to use the SQLite
database. We've chosen SQLite because it takes the least amount of setup for the
purposes of the course. There is an example module included for PostgreSQL
however the course will focus on the SQLite implementation.

For reference, the packages we will use to talk to our database are:

- [sqlite-simple](https://hackage.haskell.org/package/sqlite-simple)
- [sqlite-simple-errors](https://hackage.haskell.org/package/sqlite-simple-errors)

Start in ``src/FirstApp/DB.hs``

NB: The PostgreSQL example module is in ``src/FirstApp/DB/PostgreSQL.hs``.
