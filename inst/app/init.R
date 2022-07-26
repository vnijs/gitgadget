os_type <- Sys.info()["sysname"]

find_home <- function(os_type = Sys.info()["sysname"]) {
  if (os_type == "Windows") {
    ## gives /Users/x and not /Users/x/Documents
    normalizePath(
      file.path(
        Sys.getenv("HOMEDRIVE"),
        Sys.getenv("HOMEPATH")
      ),
      winslash = "/"
    )
  } else {
    Sys.getenv("HOME")
  }
}

homedir <- find_home()
renvirdir <- Sys.getenv("HOME")

## setting up volumes for shinyFiles
gg_volumes <- c(Home = homedir)
git_home <- Sys.getenv("git.home")

## needed if using gitgadget with both native and docker version of Rstudio
if (git_home != "" && !file.exists(git_home)) {
  git_alt <- file.path(homedir, basename(git_home))
  if (file.exists(git_alt)) {
    git_home <- git_alt
  } else {
    git_alt <- file.path(homedir, basename(dirname(git_home)), basename(git_home))
    if (file.exists(git_alt)) {
      git_home <- git_alt
    } else {
      git_home <- ""
    }
  }
}

## setting up volumes for shinyFiles
if (git_home != "") {
  gg_volumes <- setNames(c(git_home, gg_volumes), c(basename(git_home), names(gg_volumes)))
}

is_repo_fun <- function(dr) {
  is_empty(suppressWarnings(system(paste("git -C", dr, "rev-parse --is-inside-work-tree 2>/dev/null"), intern = TRUE))) == FALSE
}

projdir <- basedir <- git_home
if (rstudioapi::isAvailable()) {
  pdir <- rstudioapi::getActiveProject()
  if (!is_empty(pdir)) {
    projdir <- basedir <- pdir
  } else {
    wd <- getwd()
    if (grepl("^/srv/", wd)) wd <- git_home
    if (is_repo_fun(wd)) {
      projdir <- basedir <- wd
    }
  }
} else {
  wd <- getwd()
  if (grepl("^/srv/", wd)) wd <- git_home
  if (is_repo_fun(wd)) {
    projdir <- basedir <- wd
  }
}

if (is_empty(projdir)) {
  projdir <- basedir <- normalizePath(homedir, winslash = "/")
} else {
  if (!projdir %in% gg_volumes) {
    gg_volumes <- setNames(c(projdir, gg_volumes), c(basename(projdir), names(gg_volumes)))
  }
  projdir <- basedir <- normalizePath(projdir, winslash = "/")
}