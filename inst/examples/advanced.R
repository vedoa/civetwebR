# This example demonstrates the core features of civetwebR:
# - Path routing and groups
# - Custom headers and status codes
# - Request object inspection using S3 helpers

library(civetwebR)

# 1. Clean start: remove any handlers registered in the current session
clear_handlers()

# 2. A basic root handler returning plain text
handle("GET", "/", function(req) {
  "Welcome to the civetwebR example server! Try visiting /whoami?name=RUser"
})

# 3. Demonstrate grouping and structured responses (JSON)
group("/api/v1", {
  
  handle("GET", "/status", function(req) {
    list(
      status = 200L,
      headers = list("Content-Type" = "application/json"),
      body = '{"status": "ok", "engine": "CivetWeb", "thread_safe": true}'
    )
  })
  
  handle("POST", "/echo", function(req) {
    # This handler echoes back whatever binary body was sent
    list(
      status = 201L,
      headers = list("X-Echo-Type" = "Binary"),
      body = req$body
    )
  })
})

# 4. Demonstrate S3 helpers for headers and query parameters
handle("GET", "/whoami", function(req) {
  # Use the S3 helpers to extract information safely
  ua <- req_header(req, "User-Agent")
  name <- req_query(req, "name")
  
  if (is.null(name)) name <- "Anonymous Visitor"
  
  sprintf("Hello %s!\n\nYour Request ID is: %d\nYour User-Agent is: %s", 
          name, req$id, ua)
})

# 5. Start the server
# We use a 10ms timeout for high responsiveness during development
message("Server starting at http://127.0.0.1:8080")
serve(port = 8080, host = "127.0.0.1", timeout_ms = 10L)
