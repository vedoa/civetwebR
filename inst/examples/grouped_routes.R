# define routes
group("/api", {
  handle("GET", "/hello", function(req) {
    "hello from api"
  })

  handle("GET", "/user", function(req) {
    "user endpoint"
  })

  handle("POST", "/user", function(req) {
    list(
      status = 201L,
      headers = list("Content-Type" = "text/plain"),
      body = "user created"
    )
  })
})

serve()
