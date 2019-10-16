
require("tools")

# -------------------------------------------------------------------
# check that md2sum works on Unicode paths, in Windows

test <- function() {
  tmp <- tempfile()
  on.exit(unlink(tmp, recursive = TRUE), add = TRUE)
  dir.create(tmp)

  f1 <- file.path(tmp, "simple")
  f2 <- file.path(tmp, "tricky-\u0151")
  writeBin(charToRaw("foo\nbar\n"), f1)
  writeBin(charToRaw("foo\nbar\n"), f2)

  m1 <- md5sum(f1)
  m2 <- md5sum(f2)
  stopifnot(
    !is.na(m1),
    !is.na(m2),
    identical(m1[[1]], m2[[1]])
  )
}

# Only test on Windows currently, to avoid problems on OSes without
# Unicode or a Unicode file system
if (.Platform$OS.type == "windows") test()
