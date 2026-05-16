if (!exists("sysName")) {
  sysName <- tolower(Sys.info()[["sysname"]])
} else {
  sysName <- tolower(sysName)
}

packagePath <- file.path(getwd(), "env", sysName)

if (!dir.exists(packagePath)) {
  dir.create(packagePath, recursive = TRUE)
  cat(paste0("Path ", packagePath, " created.\n"))
}

.libPaths(packagePath)

rm(packagePath, sysName)

cat(paste0("Environments loaded\n", paste0(.libPaths(), collapse = "\n")))
cat(paste0("\nCRAN set to:\n"), paste0(options()$repos, collapse = "\n"))
cat("\n")
