# ---- HANDLERS ----

# Simple GET
handle("GET", "/hello", function(req) {
  "hello world"
})

# GET with query params
handle("GET", "/greet", function(req) {
  name <- req$query_params$name %||% "stranger"
  paste("hello", name)
})

# POST with body + headers
handle("POST", "/echo", function(req) {
  body <- rawToChar(req$body)
  ct   <- req$headers[["Content-Type"]] %||% "unknown"
  paste("content-type:", ct, "| body:", body)
})

# Combined example (everything)
handle("POST", "/all", function(req) {
  paste(
    "query:", req$query_params$a %||% "",
    "| header:", req$headers[["X-Test"]] %||% "",
    "| body:", rawToChar(req$body),
    sep = " "
  )
})

# ---- START SERVER ----
serve()


# GET
# curl "http://127.0.0.1:8080/hello"

# query params
# curl "http://127.0.0.1:8080/greet?name=tom"

# POST body + headers
# curl -X POST http://127.0.0.1:8080/echo \
#   -H "Content-Type: text/plain" \
#   -d "hello server"

# everything
# curl -X POST "http://127.0.0.1:8080/all?a=1" \
#   -H "X-Test: ok" \
#   -d "data"
