sysName <- tolower(Sys.info()[["sysname"]])

repos <- switch(
  sysName,
  "windows" = "https://packagemanager.posit.co/cran/latest",
  "linux"   = "https://packagemanager.posit.co/cran/__linux__/noble/latest",
  "darwin"  = "https://packagemanager.posit.co/cran/latest",
  "https://cloud.r-project.org"
)

options(repos = c(CRAN = repos))

cat("Detected OS:", sysName, "\n")
cat("Using CRAN:", repos, "\n")

init_env <- new.env(parent = globalenv())
init_env$sysName <- sysName

source("init.R", local = init_env)

rm(sysName, repos, init_env)