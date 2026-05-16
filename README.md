# civetwebR

Embedded HTTP server for R based on CivetWeb.

## Overview

- C handles network I/O  
- R handles all request logic  
- Single-threaded execution  

## Usage

```r
serve(port = 8080)
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