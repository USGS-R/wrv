file <- if (WINDOWS) "mfusg.exe" else "mfusg"  # working directory is ./src
if (file.exists(file)) {
  path <- file.path(R_PACKAGE_DIR, "bin", R_ARCH)
  dir.create(path, recursive=TRUE, showWarnings=FALSE)
  file.copy(file, path, overwrite=TRUE)
  file.remove(c(file, list.files(pattern="\\.mod$")))
}
