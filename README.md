# noelm-get

A command line tool to share Noelm libraries.

## Install

    cabal install noelm-get

This will install the `noelm-get` executable in `/home/YOU/.cabal/bin`.

## Use

The next two sections will cover the basics of using `noelm-get`.

### Install Libraries

To install a library run:

```bash
noelm-get install user/project       # Install latest version
noelm-get install user/project 0.1   # Install version 0.1
```

`noelm-get` is sandboxed by default, so these commands install the
library in the current working directory. This means it is easy for
different projects to have different dependencies. To actually use the
library in your project, you will need to add it to your
`noelm_dependencies.json` file which tells the compiler where to look
for extra libraries.

### Publish Libraries

Key design guidelines:

* Design for a concrete use case
* Always give functions human readable names
* Avoid gratuitous abstraction

Your `noelm_dependencies.json` file should look
something like this:

```json
{ "version": "0.1"
, "summary": "experimental library for structuring code, based on Arrowized FRP"
, "description": "This is an experimental library for structuring code. It is based on Arrowized FRP, which aimed to provide an API for higher-order signal graphs. There are many ways to structure code though, and it is not yet clear when AFRP is a superior method. So this library is meant to explore the basics of AFRP and find cases where it is a really good fit."
, "license": "BSD3"
, "repository": "https://github.com/evancz/automaton.git"
, "exposed-modules": ["Automaton"]
, "noelm-version": "0.10.1"
, "dependencies":{}
}
```

A couple important notes for filling in these fields for your project are:

  * Keep the `summary` under 80 characters.
  * Make the `description` a useful outline of the library. It should
    cover the problems solved by the library, indicate any
    limitations, and point to related resources that might be helpful.
    The goal is for someone to be able to quickly assess if the
    library will suit their goals.
  * The recommended `license` is LGPLv3, but of course, you can use
    whatever license you want.
  * The `exposed-modules` are the subset of modules that people can
    use when they install your library. Use this to stop internal
    details from polluting your API and cluttering the docs with
    modules that are not meant for users.
  * For now, you cannot publish libraries with `dependencies` on
    community libraries.

Once that is all done, use git to tag the specific commit you want to
release with an appropriate version number. This should match the
`version` listed in `noelm_dependencies.json`. Finally, you can publish
your library with:

    noelm-get publish
