library(civetwebR)

# ------------------------------------------------------------
# Reset state
# ------------------------------------------------------------
if (.is_running()) {
  stop_server()
}
clear_handlers()

# ------------------------------------------------------------
# ROUTES (for comparison with static)
# ------------------------------------------------------------

handle("GET", "/hello", function(req) {
  "HELLO FROM ROUTE"
})

group("/api", {
  handle("GET", "/world", function(req) {
    "WORLD FROM ROUTE (should be overridden by static)"
  })
})

# ------------------------------------------------------------
# STATIC FILE SETUP (NO index.html anywhere)
# ------------------------------------------------------------

tmp <- file.path(tempdir(), "civetwebR-browse-demo")
dir.create(tmp, recursive = TRUE, showWarnings = FALSE)

public <- file.path(tmp, "public")
assets <- file.path(tmp, "assets")

dir.create(public, recursive = TRUE, showWarnings = FALSE)
dir.create(assets, recursive = TRUE, showWarnings = FALSE)

# ---- PUBLIC ----
# public/docs/{a.txt,b.txt}
# public/images/{one.txt,two.txt}

dir.create(file.path(public, "docs"), recursive = TRUE)
dir.create(file.path(public, "images"), recursive = TRUE)

writeLines("DOC A (public/docs/a.txt)", file.path(public, "docs", "a.txt"))
writeLines("DOC B (public/docs/b.txt)", file.path(public, "docs", "b.txt"))

writeLines(
  "IMAGE ONE (public/images/one.txt)",
  file.path(public, "images", "one.txt")
)
writeLines(
  "IMAGE TWO (public/images/two.txt)",
  file.path(public, "images", "two.txt")
)

# static override example
dir.create(file.path(public, "api"), recursive = TRUE)
writeLines("STATIC OVERRIDE: /api/world", file.path(public, "api", "world"))


# ---- ASSETS ----
# assets/css/{main.css,theme.css}
# assets/js/{app.js,utils.js}

dir.create(file.path(assets, "css"), recursive = TRUE)
dir.create(file.path(assets, "js"), recursive = TRUE)

writeLines(
  "body { background: #fafafa; }",
  file.path(assets, "css", "main.css")
)
writeLines("h1 { color: #3366cc; }", file.path(assets, "css", "theme.css"))

writeLines('console.log("app.js loaded");', file.path(assets, "js", "app.js"))
writeLines(
  'console.log("utils.js loaded");',
  file.path(assets, "js", "utils.js")
)

# ------------------------------------------------------------
# START SERVER
# ------------------------------------------------------------

cat("Browse these URLs:\n\n")
cat("  Root:\n")
cat("    http://127.0.0.1:8080/\n\n")

cat("  Public folders:\n")
cat("    http://127.0.0.1:8080/docs/\n")
cat("    http://127.0.0.1:8080/images/\n\n")

cat("  Assets folders:\n")
cat("    http://127.0.0.1:8080/assets/\n")
cat("    http://127.0.0.1:8080/assets/css/\n")
cat("    http://127.0.0.1:8080/assets/js/\n\n")

cat("  Static overrides route:\n")
cat("    http://127.0.0.1:8080/api/world\n\n")

cat("  Pure route:\n")
cat("    http://127.0.0.1:8080/hello\n\n")

serve(
  port = 8080L,
  host = "127.0.0.1",
  static = list(
    list(dir = assets, prefix = "/assets", list_dirs = TRUE),
    list(dir = public, prefix = "/", list_dirs = TRUE)
  )
)
