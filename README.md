# civetwebR

Embedded HTTP server for R using CivetWeb with a single-threaded, event-driven R execution model.

## Overview

`civetwebR` provides a minimal HTTP server where:

- CivetWeb handles network I/O in C
- R executes all request logic
- A driver loop in R controls request processing

This design ensures that all user-defined handlers are executed safely within the R thread, avoiding concurrency issues.

## Architecture

Request flow:

```text
HTTP request
   ↓
C (CivetWeb thread)
   ↓ enqueue
R loop (run_server)
   ↓ dispatch_request()
R handler
   ↓ send_response()
C writes response
```

Only the R loop executes user code. C never calls into R directly.

## Installation

Install from source:

```r
devtools::install()
```

## Usage

### Basic Example

See: `inst/examples/`

## Handlers

Handlers are registered using:

```r
handle(method, path, fun)
```

- `method`: HTTP method (e.g. `"GET"`)
- `path`: route path (must start with `/`)
- `fun`: function(req)

### Request Object

Handler functions receive:

```r
req <- list(
  method = "...",
  path   = "..."
)
```

### Return Values

Handlers must return either:

#### Character

```r
"ok"
```

Converted to:

```r
list(
  status = 200L,
  headers = list("Content-Type" = "text/plain"),
  body = "ok"
)
```

#### List

```r
list(
  status  = 200L,
  headers = list("Content-Type" = "application/json"),
  body    = "data"
)
```

Supported body types:

- `character(1)`
- `raw`

## Server Lifecycle

### Start + Run

```r
serve(port)
```

This function:

- starts the HTTP engine
- runs the driver loop
- blocks until interrupted

### Stop

Interrupt execution:

- RStudio: `Esc`
- Terminal: `Ctrl + C`

Then:

```r
stop_server()
```

## Internals

The server loop uses a polling mechanism:

```r
.next_request(timeout_ms)
```

This avoids blocking inside C and allows R to:

- process interrupts
- remain responsive when idle

## Threading Model

- CivetWeb runs in C threads (I/O only)
- R executes all handlers (single-threaded)
- No handler code runs outside the R thread

## License

MIT