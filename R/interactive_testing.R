## test section
# main_git__ <- TRUE
main_git__ <- FALSE
if (main_git__) {

  library(curl)
  library(jsonlite)
  library(dplyr)
  source("./R/git.R")

  ## settings
  # server <- Sys.getenv("git.server", "https://gitlab.com/api/v4/")
  server <- Sys.getenv("git.server", "https://rsm-gitlab.ucsd.edu/api/v4/")
  username <- Sys.getenv("git.user")
  token <- Sys.getenv("git.token")
  # groupname <- "rady-mgta-bc-2016"
  groupname <- Sys.getenv("git.group")
  userfile <- "~/git/msba-test-gitlab.csv"
  stopifnot(file.exists(userfile))

  ## to debug code
  permission <- 20

  ## important - name the assignment repo something unique because they will all reside
  ## in the student's namespace, i.e., two faculty might have assignment1
  assignment <- "assignment1"
  type <- "individual"
  # pre <- paste0(groupname,"-")
  # directory <- paste0("~/bc/", groupname)

  ## uncomment to cleanup
  # remove_group(token, groupname, server)

  ## uncomment to remove all student projects!
  ## highly destructive!
  userfile <- "~/git/msba-test-gitlab.csv"
  students <- read.csv(userfile, stringsAsFactors = FALSE)
  gitgadget:::remove_student_projects(userfile, Sys.getenv("git.server", "https://rsm-gitlab.ucsd.edu/api/v4/"))

  ## repo <- "gitgadget-test-repo"
  # id <- projID(paste0("vnijs/",repo), token, server)$project_id
  # remove_project(token, id, server)

  ## To remove students from a group go to the group page, click on members,
  ## search for the students you want and click the delete icon

  ## To remove individual projects cloned to a student's account
  ## use the Create tab and load the file with student information

  ## check tokens
  userfile <- "~/msba-test-gitlab.csv"
  students <- read.csv(userfile, stringsAsFactors = FALSE)

  ## testing if student tokens work
  for (i in seq_len(nrow(students))) {
    i <- 1
    token <- students[i, "token"]
    if (token != "")
      id <- get_allprojects(token, server)
    else
      id$status <- "EMPTY"

    if (id$status == "OKAY") {
      print(paste0("OKAY: ", students[i, "userid"], " ", token))
    } else {
      print(paste0("NOT OKAY: ", students[i, "userid"], " ", token))
    }
  }

  if (file.exists(file.path(directory, assignment))) {
    unlink(file.path(directory, assignment, ".git"), recursive = TRUE, force = TRUE)
    dir.exists(file.path(directory, assignment, ".git"))

    ## create a group for a course where all assignments and cases will be posted
    create_group(
      token, groupname, userfile, permission = permission, server = server
    )

    ## get or create a repo for assignments and cases
    create_repo(
      username, token, assignment, directory, groupname,
      pre = pre, server = server
    )

    assign_work(
      token, groupname, assignment, userfile, type = type,
      pre = pre, server = server
    )

  } else {
    cat("Assignment does not exist")
  }

  ## same steps for a team assignment
  assignment <- "assignment2"
  type <- "team"
  if (file.exists(file.path(directory, assignment))) {
    unlink(file.path(directory, assignment, ".git"), recursive = TRUE, force = TRUE)
    dir.exists(file.path(directory, assignment, ".git"))

    create_repo(
      username, token, assignment, directory, groupname,
      pre = pre, server = server
    )

    assign_work(
      token, groupname, assignment, userfile, type = type,
      pre = pre, server = server
    )
  } else {
    cat("Assignment does not exist")
  }

  ## generate merge (pull) requests
  assignment <- "assignment1"
  type <- "individual"
  if (file.exists(file.path(directory, assignment))) {
    collect_work(
      token, groupname, assignment, userfile, type = type,
      pre = pre, server = server
    )

    fetch_work(
      token, groupname, assignment, pre = pre, server = server
    )
  } else {
    cat("Assignment does not exist")
  }

  ## create a repo on gitlab in the users own namespace
  groupname <- ""
  pre <- ""
  repo <- assignment
  # repo <- "gitgadget-test-repo"
  # directory <- "/Users/vnijs/Desktop/Github"
  if (file.exists(file.path(directory, repo))) {
    unlink(file.path(directory, assignment, ".git"), recursive = TRUE, force = TRUE)
    dir.exists(file.path(directory, assignment, ".git"))
    create_repo(
      username, token, repo, directory, groupname,
      pre = pre, server = server
    )
  }
}
