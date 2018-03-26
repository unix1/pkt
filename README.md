## Welcome to pkt!

### What is pkt?

`pkt` is a very small proof of concept URL shortener written in Erlang/OTP.

It has these features:

- Service, minimal browser UI, data backend to shorten URL and store
- Create short URLs on home page
- Visit short URL, get redirected to destination URL
- Short URLs are always shorter than 8 chars, and use [0-9a-zA-Z-_] alphabet
- Use widest possible space with above 2 constraints
- Append "?r=0" to the short URL to view the destination URL before going to it

### Installation

#### Prerequisites

- `git`
- GNU `make`
- Erlang/OTP (developed and tested on version 20)

#### Commands

This will clone, download dependencies, and compile the application.

```
git clone https://github.com/unix1/pkt.git
cd pkt
make
```

### Usage

#### Run

To run the application in shell, type

    make run

The server is now active at https://localhost:8080.

#### REST API

The server is accessible by the web browser and it implements REST API.

#### Other Usage

To terminate the application and exit from its shell, press Ctrl-C twice in the
application shell.

To run tests

    make tests

To make reference documentation

    make docs

To dialyze

    make dialyze

### Questions?

Don't hesitate to create an issue.
