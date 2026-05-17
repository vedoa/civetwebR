sysName <- tolower(Sys.info()[["sysname"]])

repos <- switch(
  sysName,
  "linux" = local({
    # Detect Linux distribution to enable pre-built binary support from PPM
    repo_url <- "https://packagemanager.posit.co/cran/latest"
    if (file.exists("/etc/os-release")) {
      os_info <- readLines("/etc/os-release", warn = FALSE)
      get_val <- function(key) {
        line <- grep(paste0("^", key, "="), os_info, value = TRUE)
        if (length(line)) gsub("[\"']", "", sub(paste0("^", key, "="), "", line)) else NULL
      }
      codename <- get_val("VERSION_CODENAME")
      if (!is.null(codename)) {
        repo_url <- sprintf("https://packagemanager.posit.co/cran/__linux__/%s/latest", codename)
      } else if (!is.null(get_val("ID")) && get_val("ID") %in% c("rhel", "centos", "rocky", "almalinux")) {
        ver <- sub("\\..*", "", get_val("VERSION_ID"))
        repo_url <- sprintf("https://packagemanager.posit.co/cran/__linux__/rhel%s/latest", ver)
      }
    }
    repo_url
  }),
  "https://packagemanager.posit.co/cran/latest"
)

options(repos = c(CRAN = repos))

cat("Detected OS:", sysName, "\n")
cat("Using CRAN:", repos, "\n")

init_env <- new.env(parent = globalenv())
init_env$sysName <- sysName

source("init.R", local = init_env)

rm(sysName, repos, init_env)