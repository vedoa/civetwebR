# civetwebR

An embedded, performance HTTP server for R based on the [CivetWeb](https://github.com/civetweb/civetweb) C library.

## Key Features

- **Thread Safety:** Uses a "Pull" model where multi-threaded C handles socket I/O and a single R thread processes logic, ensuring the R interpreter never crashes.
- **High Performance:** Minimal overhead between C and R.
- **Routing:** Support for method-based routing and path grouping.
- **Static Content:** Built-in static file serving with directory listing, breadcrumbs, and security guards against directory traversal.
- **Flexible I/O:** Support for custom headers, query parameter parsing, and binary (raw) request/response bodies.
- **Interrupt Friendly:** Fully supports `Ctrl+C` to stop the server gracefully from the R console.

## Installation

```r
# Install from GitHub
# devtools::install_github("vedoa/civetwebR")
```

## Usage

```r
library(civetwebR)

# Start a server on port 8080
serve(port = 8080, host = "127.0.0.1")
```

## Handlers

```r
handle("GET", "/path", function(req) "ok")
```

## Static Files

Serve files directly from disk:

```r
serve(
  port = 8080,
  static = list(
    list(dir = "public", prefix = "/")
  )
)
```

## Routing Groups

Group routes under a common prefix:

```r
group("/api", {
  handle("GET", "/hello", function(req) "hi")
})
```

## Request

```r
req <- list(
  method = "...",
  path   = "..."
)
```

## Response

**Character:**
```r
"ok"
```

→ 200 text/plain

**List:**
```r
list(
  status = 200L,
  headers = list(),
  body = "data"
)
```

## Notes

- All handlers run in the R thread  
- No concurrency in user code  

## License

MIT