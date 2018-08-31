# Level 07

This exercise involves introducing a monad transformer (ReaderT) to our
application. The use of the ReaderT in our application is motivated in part by
the expansion of the number of inputs to the various functions involved in
handling different requests.

Since nearly any given request handler will likely require access to either the
general application configuration or the database, it is tedious to have to pass
the information in manually in every instance.

Since we already have an AppM type, we're extending with more functionality,
this is what is known as 'stacking' monad transformers.

The steps for this level:
1) ``src/Level07/AppM.hs``
2) ``src/Level07/DB.hs``
3) ``src/Level07/Core.hs``
