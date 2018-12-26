# Level 01

The purpose of this exercise is to whet our appetite by creating a basic web
app. The focus will be on reading the [Hackage] documentation for the [Wai]
framework. Consult the ``src/Level01/Core.hs`` to find the parts that are
missing and what we need from the [Wai] package to build our "Hello, World!"
application.

Our "application" will respond to ALL incoming requests with a 200 status code
response and the message "Hello, World!".

[Hackage]: (https://hackage.haskell.org/)
[Wai]: (https://hackage.haskell.org/package/wai)

## Running the program

To run the application:

```bash
# With Cabal
$ cabal new-run level01-exe

# With Stack
$ stack exec level01-exe
```

## Accessing the program

```bash
# Using curl
$ curl -XGET localhost:<port number>
```

You can also use a browser and go to ``http://localhost:<port number>``.
