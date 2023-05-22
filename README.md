# reactb

## What problem does it solve?

The ["React - The Complete Guide (incl Hooks, React Router, Redux)" udemy course](https://www.udemy.com/course/react-the-complete-guide-incl-redux/) uses the [Firebase Realtime Database](https://firebase.google.com/docs/database/) as a simple backend storage to keep some persistent state in several course videos. The issue is that it requires a goog account, which is a big (and undocumented!) prerequisite for the course.

There is the [Firebase Local Emulator Suite](https://firebase.google.com/docs/emulator-suite), but it also requires a goog account and a real firebase project even if you'll never use it. There is no workaround for this.

So `reactb` is a tiny implementation of the firebase API necessary for the course, which can be run locally without any extra requirements.

## Installation

Download the latest binary from the [releases page](https://github.com/eunikolsky/reactb/releases), available for linux and macos, `x86_64`. Or you can build it yourself, see below.

## Usage

Run the binary to start the server: `reactb`, it doesn't accept any arguments. The server is listening on port 8080, this is not configurable at the moment. The data is not persisted between runs — restarting the server will start with an empty database.

The firebase REST documentation is at <https://firebase.google.com/docs/database/rest/start>. `reactb` supports only the `GET`, `POST` and `PUT` methods.

Example interaction:

```bash
# the initial state is empty for all endpoints
$ curl http://localhost:8080/orders.json
{}

# POST creates a new object with a random name
$ curl http://localhost:8080/orders.json -H 'Content-Type: application/json' -d '{"hello":"world"}'
{"name":"MpcITTdbvEqFYsOREHVV"}

# GET returns the created object
$ curl http://localhost:8080/orders.json
{"MpcITTdbvEqFYsOREHVV":{"hello":"world"}}

# POST appends data to the current state
$ curl http://localhost:8080/orders.json -H 'Content-Type: application/json' -d '{"foo":"bar"}'
{"name":"gthjUgfwsfQCSEktquBb"}

# GET returns the current state
$ curl http://localhost:8080/orders.json
{"MpcITTdbvEqFYsOREHVV":{"hello":"world"},"gthjUgfwsfQCSEktquBb":{"foo":"bar"}}

# PUT replaces the current state
$ curl -X PUT http://localhost:8080/orders.json -H 'Content-Type: application/json' -d '{"fresh":"data", "empty_array":[]}'
{"fresh":"data"}

# GET returns the new state
$ curl http://localhost:8080/orders.json
{"fresh":"data"}

# other endpoints are unaffected
$ curl http://localhost:8080/foobar.json
{}
```

The server displays requests in the console like this:

```
GET /orders.json
  Accept: */*
  Status: 200 OK 0.000082s
POST /orders.json
  Request Body: {"hello":"world"}
  Accept: */*
  Status: 200 OK 0.000548s
GET /orders.json
  Accept: */*
  Status: 200 OK 0.000048s
POST /orders.json
  Request Body: {"foo":"bar"}
  Accept: */*
  Status: 200 OK 0.000082s
GET /orders.json
  Accept: */*
  Status: 200 OK 0.000043s
PUT /foo.json
  Request Body: {"fresh":"data", "empty_array":[]}
  Accept: */*
  Status: 200 OK 0.000083s
```

## Notes

- At the top-level, only JSON objects are supported.
- Any endpoint ending with `.json` can be used, for example `/foo.json`.
- Empty arrays are automatically removed.
- The server accepts any CORS request.

## Building

To build the program, you need to install the [Haskell Stack](https://docs.haskellstack.org/en/stable/) and run `stack build` in the project directory.

To execute the (few) tests, run `stack test`.

## TODO

* Provide the default meals for one of the videos.
* Allow to disable the verbose logging.
* Allow to configure the port.
