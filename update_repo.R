# derived from the drat package
# License: GPL (>= 2)
# https://github.com/eddelbuettel/drat

print("start update repo")

getPathForPackage <- function(file) {
  pkgtype <- identifyPackageType(file)
  fields <- getPackageInfo(file)
  print("===")
  print(fields)
  print("===")
  rversion <- unname(fields["Rmajor"])
  # Forcibly only submit R 3.3 or 3.4 packages.
  if(!rversion %in% c("3.3", "3.4")) { 
    return(NA) 
  }

  if (pkgtype == "source") {
    ret <- file.path("src", "contrib")
  } else if (pkgtype == "win.binary") {
    ret <- file.path("bin", "windows", "contrib", rversion)
  } else if (pkgtype == "mac.binary") {
    # commenting out the special stuff for mac versions, which wasn't working
    # if (fields["OSflavour"] == "") {
    #   # non-binary package, treated as El-Capitan
    #   message("Note: Non-binary OS X package will be installed in El Capitan path.")
    #   fields["El-Capitan"] <- "yes"
    # }
    # if (unname(fields["El-Capitan"]) == "yes") {
      ret <- file.path("bin", "macosx", "el-capitan", "contrib", rversion)
    # } else {
      # ret <- file.path("bin", "macosx", "contrib", rversion)
    # }
  }
  return(ret)
}

identifyPackageType <- function(file) {
  ##from src/library/tools/R/packages.R
  ret <- if (grepl("_.*\\.tar\\..*$", file)) {
    "source"
  } else if (grepl("_.*\\.tgz$", file)) {
    "mac.binary"
  } else if (grepl("_.*\\.zip$", file)) {
    "win.binary"
  } else {
    stop("Unknown package type", call. = FALSE)
  }
  return(ret)
}

getPackageInfo <- function(file) {
  if (!file.exists(file))
    stop("File ", file, " not found!", call. = FALSE)

  td <- tempdir()
  if (grepl(".zip$", file)) {
    unzip(file, exdir = td)
  } else if (grepl(".tgz$", file)) {
    untar(file, exdir = td)
  } else {
    ##stop("Not sure we can handle ", file, call.=FALSE)
    fields <- c("Source" = TRUE,
                "Rmajor" = NA,
                "El-Capitan" = FALSE)
    return(fields)
  }

  pkgname <- gsub("^([a-zA-Z0-9.]*)_.*", "\\1", basename(file))
  path <- file.path(td, pkgname, "DESCRIPTION")
  builtstring <- read.dcf(path, 'Built')
  unlink(file.path(td, pkgname), recursive = TRUE)

  fields <- strsplit(builtstring, "; ")[[1]]
  names(fields) <- c("Rversion", "OSflavour", "Date", "OS")

  rmajor <-
    gsub("^R (\\d\\.\\d)\\.\\d.*", "\\1", fields["Rversion"])
  isDarwin13 <-
    ifelse(fields["OSflavour"] == "x86_64-apple-darwin13.4.0", "yes", "no")
  fields <-
    c(fields,
      "Rmajor" = unname(rmajor),
      "El-Capitan" = unname(isDarwin13))

  return(fields)
}

path <- ifelse(
  .Platform$OS.type == 'windows',
  file.path('..', Sys.getenv("APPVEYOR_PROJECT_NAME")),
  file.path('..')
)

files <-
  file.path(
    path, dir(
      path, pattern = ifelse(.Platform$OS.type == 'windows', '.zip', '.t*z')))

repodir <- '.'

print("Beginning update_repo.R script.")
for (f in files) {
  print(paste('Processing', f))

  reldir <- getPathForPackage(f)
  if(is.na(reldir)) {
    print("No relative directory, skipping to next file.")
    next
  }
  print(paste("Relative directory:", reldir))

  pkgdir <- file.path(repodir, reldir)

  if (!file.exists(pkgdir)) {
    print(paste("Package directory did not exist. Creating directory", pkgdir))
    ## TODO: this could be in a git branch, need checking
    if (!dir.create(pkgdir, recursive = TRUE)) {
      stop("Directory ", pkgdir, " couldn't be created\n", call. = FALSE)
    }
  }

  print("Beginning file copy into repo.")
  ## copy file into repo
  if (!file.copy(f, pkgdir, overwrite = TRUE)) {
    stop("File ", f, " can not be copied to ", pkgdir, call. = FALSE)
  }
  print("File copy into repo complete, using tools::write_PACKAGES")

  tools::write_PACKAGES(pkgdir, type = identifyPackageType(f))

  print(paste("File", f, "complete"))
}
