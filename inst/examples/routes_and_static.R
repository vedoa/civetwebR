library(civetwebR)

# ------------------------------------------------------------
# Reset state
# ------------------------------------------------------------
if (.is_running()) {
  stop_server()
}
clear_handlers()

# ------------------------------------------------------------
# ROUTES
# ------------------------------------------------------------

handle("GET", "/hello", function(req) {
  "HELLO FROM ROUTE"
})

group("/api", {
  handle("GET", "/world", function(req) {
    "WORLD FROM API"
  })
})

# ------------------------------------------------------------
# STATIC FILE SETUP (you need these dirs/files!)
# ------------------------------------------------------------

# create temporary directories for demo
tmp <- tempdir()

public <- file.path(tmp, "public")
assets <- file.path(tmp, "assets")

dir.create(public, showWarnings = FALSE)
dir.create(assets, showWarnings = FALSE)

# public files
writeLines("root-file", file.path(public, "file.txt"))
writeLines("<h1>INDEX</h1>", file.path(public, "index.html"))

# assets files
writeLines("asset-file", file.path(assets, "file.txt"))

# conflict example (static overrides route)
dir.create(file.path(public, "api"), showWarnings = FALSE)
writeLines("STATIC OVERRIDE", file.path(public, "api", "world"))

# ------------------------------------------------------------
# START SERVER
# ------------------------------------------------------------

cat("Serving from:\n")
cat("public =", public, "\n")
cat("assets =", assets, "\n\n")

serve(
  port = 8080L,
  host = "127.0.0.1",
  static = list(
    list(dir = assets, prefix = "/assets"),  # specific first
    list(dir = public, prefix = "/")         # fallback
  )
)
