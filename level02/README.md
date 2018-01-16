# Level 02

This level focuses on using our application's requirements to design some data
structures, and how we can then use those data structures to guide construction.

By using these data structures we provide a mechanism for utilising the compiler
as a pair programmer. It will inform us when we've forgotten to handle a given
path, tried to use information we don't have access to, or haven't validated our
inputs sufficiently.

To build this REST application we're going to need some requirements:

### Requirements
We have a WebThing(TM) somewhere and we would like, for some unknown reason, to
be able to add comments to various topics on this WebThing(TM).

### "Spec"

This will be a REST only application, there won't be any HTML involved.

Let's pretend we've completed a dozen specification meetings with our Project
Manager, resulting in the specification below:

We have to be able to:
- Comment on a given topic
- View a topic and its comments
- List the current topics

So we will work towards building the following routes:
```
# To comment on a given <topic>
POST /<topic>/add

# To view all the comments on a given <topic>
GET /<topic>/view

# To list all the current topics
GET /list
```

The starting point for this exercise is the ``src/FirstApp/Types.hs``.

### Running the program:

```bash
# Using cabal
$ cabal exec level02-exe

# Using stack
$ stack exec level02-exe
```

### Accessing the program:

Using ``curl``:
```bash
# Running a POST

# Valid request
$ curl -XPOST -v localhost:<port>/puppies/add -d "Puppies are awesome."

# Invalid request (should trigger an error in the program)
$ curl -XPOST -v localhost:<port>/puppies/add

# Running a GET
$ curl -XGET -v localhost:<port>/puppies/view
$ curl -XGET -v localhost:<port>/list
```
