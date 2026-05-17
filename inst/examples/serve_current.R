library(civetwebR)

serve(
  port = 8080L,
  host = "127.0.0.1",
  static = list(
    list(dir = ".", prefix = "/", list_dirs = TRUE)
  )
)
