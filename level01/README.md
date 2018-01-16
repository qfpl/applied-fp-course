# Level 01

The purpose of this exercise to whet our appetite a bit by spinning up an ultra
basic web app. The focus will be on reading the [Hackage] documentation for the
[Wai] framework. Consult the ``src/FirstApp/Main.hs`` to see the parts that are
missing and what we need from the [Wai] package to make our "Hello, World!"
application run.

Our "application" will respond to ALL incoming requests with a 200 status code
response and the message "Hello, World!".

[Hackage]: (https://hackage.haskell.org/)
[Wai]: (https://hackage.haskell.org/package/wai)

## Running the program:

To run the application when you are finished:

```bash
# With Cabal
$ cabal exec level01-exe

# With Stack
$ stack exec level01-exe
```

## Accessing the program:

```bash
# Using curl
$ curl -XGET localhost:<port number>
```

You can also use a browser and go to ``http://localhost:<port number>``.
