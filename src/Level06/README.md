# Level 06

This exercise involves introducing a monad transformer (ReaderT) to our
application. The use of the ReaderT in our application is motivated in part by
the expansion of the number of inputs to the various functions involved in
handling different requests.

Since nearly any given request handler will likely require access to either the
general application configuration or the database, it is tedious to have to pass
the information in manually in every instance.

Start in ``src/FirstApp/AppM.hs``.
