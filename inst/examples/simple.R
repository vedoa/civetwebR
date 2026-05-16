# GET
handle("GET", "/hello", function(req) {
  "hello world"
})

# POST (echo body)
handle("POST", "/hello", function(req) {
  rawToChar(req$body)
})

serve()

# GET
# curl http://127.0.0.1:8080/hello
# → hello world

# POST
# curl -X POST http://127.0.0.1:8080/hello -d "hi server"
# → hi server
