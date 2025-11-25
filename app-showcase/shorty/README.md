# shorty

## Setup

### Requirements

* You need [Racket] since this is a Racket application.
* You need [Node.js] version 20 or higher to build the assets.
* You need access to a couple local [Postgres] databases. One named
  `shorty` and the other `shorty_tests`. The latter is
  exercised by unit tests.

### First-time Setup

    npm install && npm run build
    raco pkg install chief
    raco pkg install shorty/        # install and build the application and its deps
    raco pkg install shorty-tests/  # install and build the tests and their deps

### Development environment

Copy `.env.default` to `.env`. [chief] will automatically load the
variables defined in this file into the environment of the subprocesses
defined in the `Procfile` whenever it is run.

The app expects to be run behind an SSL terminated connection (for
example, behind an nginx instance using a self-signed cert), even for
local development. You can disable this requirement by setting the
`SHORTY_DEBUG` environment variable to `x`.

## Running the app locally

    raco chief start

## Running the console

    racket shorty/dynamic.rkt console


[Postgres]: https://www.postgresql.org/
[Racket]: https://racket-lang.org/
[Node.js]: https://nodejs.org/en/
[argon2]: https://www.argon2.com/
[chief]: https://github.com/Bogdanp/racket-chief
