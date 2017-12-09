# derived from the drat package
# License: GPL (>= 2)
# https://github.com/eddelbuettel/drat

print("Begin running update_repo.R to update repo.")

getPathForPackage <- function(file) {
  print(paste("Asked to get a path for", file))
  pkgtype <- identifyPackageType(file)
  print(paste("Used identifyPackageType to get the package type: ", pkgtype))

  print("Now, getting package fields...")
  fields <- getPackageInfo(file)
  print("===")
  print(fields)
  print("===")
  rversion <- unname(fields["Rmajor"])
  # Forcibly only submit R 3.3 or 3.4 packages.
  if(!rversion %in% c("3.3", "3.4")) { 
    print("R version did not match our range: 3.3 to 3.4... so we do not submit.")
    return(NA) 
  }

  if (pkgtype == "source") {
    print("Package type was source, so we're setting up the folder src/contrib.")
    ret <- file.path("src", "contrib")
  } else if (pkgtype == "win.binary") {
    print("Package type was win.binary, so we're setting up the folder bin/windows/contrib/<rversion>")
    ret <- file.path("bin", "windows", "contrib", rversion)
  } else if (pkgtype == "mac.binary") {
    print("Package type was mac.binary so we're setting up the folder bin/macosx/el-capitan/contrib/<rversion>")
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
  print("Identifying package type.")
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
  print(paste("Package type: ", ret))
  return(ret)
}

getPackageInfo <- function(file) {
  print(paste("Running getPackageInfo on", file))
  if (!file.exists(file))
    stop("File ", file, " not found!", call. = FALSE)

  td <- tempdir()
  if (grepl(".zip$", file)) {
    print("We found a zip file. We will unzip.")
    unzip(file, exdir = td)
  } else if (grepl(".tgz$", file)) {
    print("We found a tgz file, we will untar.")
    untar(file, exdir = td)
  } else {
    print("We found some other file... hmmm...")
    ##stop("Not sure we can handle ", file, call.=FALSE)
    fields <- c("Source" = TRUE,
                "Rmajor" = NA,
                "El-Capitan" = FALSE)
    return(fields)
  }

  print("Using the unarchived version of the file, let's get some info...")
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

print("Beginning update_repo.R file processing loop.")
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
