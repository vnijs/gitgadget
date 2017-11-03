checkerr <- function(code) floor(code/100) == 2
is_not <- function(x) length(x) == 0 || is.na(x)
is_empty <- function(x, empty = "\\s*") if (is_not(x) || grepl(paste0("^",empty,"$"), x)) TRUE else FALSE
not_pressed <- function(x) if (is.null(x) || x == 0) TRUE else FALSE
pressed <- function(x) if (!is.null(x) && x > 0) TRUE else FALSE

connect <- function(username, password, server = "https://gitlab.com/api/v3/") {
  h <- new_handle()
  handle_setopt(h, customrequest = "POST")
  murl <- paste0(server, "session?login=", username, "&password=", password)
  resp <- curl_fetch_memory(murl, h)
  if (checkerr(resp$status_code) == FALSE)
    return(list(status = "SERVER_ERROR", message = fromJSON(rawToChar(resp$content))$message))
  token <- fromJSON(rawToChar(resp$content))$private_token
  list(status = "OKAY", token = token)
}

groupID <- function(name, path, token, server) {
  h <- new_handle()
  handle_setheaders(h, "PRIVATE-TOKEN" = token)
  murl <- paste0(server, "groups")
  resp <- curl_fetch_memory(murl, h)
  if (checkerr(resp$status_code) == FALSE)
    return(list(status = "SERVER_ERROR"))

  resp$content <- fromJSON(rawToChar(resp$content))

  ## check if group exists
  id <- which(name == resp$content$name & path == resp$content$path)

  if (length(id) == 0) {
    list(status = "NOSUCHGROUP")
  } else {
    list(status = "OKAY", group_id = resp$content$id[id])
  }
}

userIDs <- function(ids, token, server) {
  sapply(ids, function(id) {
    resp <- userID(id, token, server)
    ifelse (resp$status == "OKAY", resp$user_id[1], NA)
  })
}

userID <- function(id, token, server) {
  h <- new_handle()
  handle_setopt(h, customrequest = "GET")
  handle_setheaders(h, "PRIVATE-TOKEN" = token)
  resp <- curl_fetch_memory(paste0(server, "users?username=", id), h)

  if (checkerr(resp$status_code) == FALSE)
    return(list(status = "SERVER_ERROR", message = rawToChar(resp$content)))

  uid <- fromJSON(rawToChar(resp$content))$id

  if (length(uid) == 0) {
    list(status = "NO_SUCH_USER")
  } else {
    list(status = "OKAY", user_id = uid[1])
  }
}

groupr <- function(groupname, path, token, server) {
  h <- new_handle()
  handle_setopt(h, customrequest = "POST")
  handle_setheaders(h, "PRIVATE-TOKEN" = token)
  murl <- paste0(server, "groups?name=", groupname, "&path=", path, "&visibility_level=0")
  resp <- curl_fetch_memory(murl, h)
  if (checkerr(resp$status_code) == FALSE)
    return(list(status = "SERVER_ERROR", message = rawToChar(resp$content)))
    # return(list(status = "SERVER_ERROR", message = fromJSON(rawToChar(resp$content))$message))

  group_id <- fromJSON(rawToChar(resp$content))$id
  list(status = "OKAY", group_id = group_id)
}

add_users <- function(user_ids, group_id, token, permission, server) {
  resp <- lapply(user_ids, function(user_id) {
    add_user(user_id, group_id, token, permission, server)$status
  })
  if (resp[[1]] != "OKAY") stop("\nThere was an error adding users\n")
}

add_user <- function(user_id, group_id, token, permission, server) {
  h <- new_handle()
  handle_setopt(h, customrequest = "POST")
  handle_setheaders(h, "PRIVATE-TOKEN" = token)
  murl <- paste0(server, "groups/", group_id, "/members?user_id=", user_id, "&access_level=", permission)
  resp <- curl_fetch_memory(murl,h)
  if (checkerr(resp$status_code) == FALSE) {
    mess <- fromJSON(rawToChar(resp$content))$message
    if (length(mess) > 0 && grepl("already exists", mess, ignore.case = TRUE)) {
      return(list(status = "OKAY", message = mess))
    } else {
      message("There was an error adding user to the group:", mess, "\n")
      return(list(status = "SERVER_ERROR", message = mess))
    }
  }

  resp$content <- fromJSON(rawToChar(resp$content))
  list(status = "OKAY")
}

#' Create a group on gitlab using the API
#'
#' @details See \url{https://github.com/vnijs/gitgadget} for additional documentation
#'
#' @param username Gitlab username
#' @param password Gitlab password
#' @param groupname Group to create on gitlab (defaults to user's namespace)
#' @param userfile A csv file with student information (i.e., username and token)
#' @param permission Permission setting for the group (default is 20, i.e., reporter)
#' @param server The gitlab API server
#'
#' @export
create_group <- function(username, password, groupname = "", userfile = "",
                         permission = 20,
                         server = "https://gitlab.com/api/v3/") {

  resp <- connect(username, password, server)
  if (resp$status != "OKAY")
    stop("Error connecting to server: check username/password/server")

  token <- resp$token
  resp <- groupID(groupname, groupname, token, server)

  if (resp$status == "NOSUCHGROUP")
    resp <- groupr(groupname, groupname, token, server = server)

  if (resp$status != "OKAY") {
    message("Unable to create or get group: ", resp$message)
    return(invisible())
  }

  ## must give users permission in order to fork repo for them
  if (!is_empty(userfile)) {
    course_id <- resp$group_id
    udat <- read.csv(userfile, stringsAsFactor = FALSE)
    uids <- userIDs(udat$userid, token, server)
    add_users(uids, course_id, token, permission, server)
  }
}

get_allprojects <- function(token, server, everything = FALSE, turn = 1) {

  h <- new_handle()
  handle_setopt(h, customrequest = "GET")
  handle_setheaders(h, "PRIVATE-TOKEN" = token)
  resp <- curl_fetch_memory(paste0(server, "projects?per_page=100"), h)

  if (checkerr(resp$status_code) == FALSE) {
    if (turn < 6) {
      message("SERVER_ERROR: Problem getting projects")
      message("Sleeping for 5 seconds and then trying again")
      Sys.sleep(5)
      return(get_allprojects(token, server, everything = FALSE, turn = turn + 1))
    } else {
      message("****************************************************************************")
      message("Tried 5 times and failed to get list of projects. Gitlab message shown below")
      message("****************************************************************************")
      message(rawToChar(resp$content))
    }

    return(list(status = "SERVER_ERROR", message = rawToChar(resp$content)))
    # return(list(status = "SERVER_ERROR", message = fromJSON(rawToChar(resp$content))$message))
  }
  nr_pages <- strsplit(rawToChar(resp$headers), "\n")[[1]] %>%
    .[grepl("X-Total-Pages",.)] %>%
    sub("X-Total-Pages:\\s+","",.) %>%
    as.numeric
  if (is.numeric(nr_pages) && nr_pages > 1) stop("Nr. of projects is > 100. Updates limits in 'get_allprojects")

  mainproj <- fromJSON(rawToChar(resp$content))

  if (everything == FALSE)
    mainproj <- select(mainproj, which(names(mainproj) %in% c("id","name","path_with_namespace","forked_from_project")))

  list(status = "OKAY", repos = mainproj)
}

projID <- function(path_with_namespace, token, server) {

  resp <- get_allprojects(token, server)
  if (resp$status != "OKAY") return(resp)

  mainproj <- resp$repos
  mainproj <- mainproj[mainproj$path_with_namespace == path_with_namespace,]
  if (length(mainproj$id) == 0) {
    message("No such repo: ", path_with_namespace, "  Status code: ", mainproj$message)
    list(status = "NO_SUCH_REPO", message = "No such repo")
  } else {
    list(status = "OKAY", project_id = mainproj$id[1])
  }
}

forkRepo <- function(token, project_id, server) {
  h <- new_handle()
  handle_setopt(h, customrequest = "POST")
  handle_setheaders(h, "PRIVATE-TOKEN" = token)
  murl <- paste0(server, "projects/fork/", project_id)
  resp <- curl_fetch_memory(murl, h)
  if (checkerr(resp$status_code) == FALSE) {
    message("Problem forking")
    list(status = "SERVER_ERROR", message = rawToChar(resp$content))
    # list(status = "SERVER_ERROR", message = fromJSON(rawToChar(resp$content))$message)
  } else {
    content <- fromJSON(rawToChar(resp$content))
    list(status = "OKAY", content = content)
  }
}

## not currently used
renameRepo <- function(project_id, token, newname, server) {
  h <- new_handle()
  handle_setopt(h, customrequest = "PUT")
  handle_setheaders(h, "PRIVATE-TOKEN" = token)
  murl <- paste0(server, "projects/", project_id, "?name=", newname)
  resp <- curl_fetch_memory(murl, h)
  if (checkerr(resp$status_code) == FALSE) {
    message("Problem changing repo name")
    list(status = "SERVER_ERROR", message = rawToChar(resp$content))
    # list(status='SERVER_ERROR', message = fromJSON(rawToChar(resp$content))$message)
  } else {
    list(status = "OKAY", name = fromJSON(rawToChar(resp$content))$name)
  }
}

setupteam <- function(token, others, project_id, server) {
  ##fork if needed for team lead
  resp <- get_allprojects(token, server, everything = TRUE)

  if (!"forked_from_project" %in% names(resp$repos) ||
      !project_id %in% resp$repos$forked_from_project$id) {
    message("Creating fork for team lead")

    resp <- forkRepo(token, project_id, server)
    if (resp$status != "OKAY") {
      message("Error forking for leader")
      return(invisible())
    }
    leader_project_id <- resp$content$id
    leader_project_name <- resp$content$name
    upstream_name <- resp$content$name
  } else {
    message("Team lead already forked the assignment")
    id <- which(resp$repos$forked_from_project$id == project_id)
    if (length(id) == 0) {
      message("Can't find repo ID for team lead")
      return()
    }
    leader_project_id <- resp$repos$id[id]
    leader_project_name <- resp$repos$name[id]
    upstream_name <- resp$repos$forked_from_project$name[id]
  }

  #add others as users
  if (length(others) > 0) {
    message("Adding team members to lead's repo")
    add_team(leader_project_id, token, others, server)
  } else {
    message("No one to add to team")
  }
  return(invisible())
}

add_team <- function(proj_id, token, team_mates, server) {
  h <- new_handle()
  handle_setopt(h, customrequest = "POST")
  handle_setheaders(h, "PRIVATE-TOKEN" = token)

  sapply(team_mates, function(otherid) {
    murl <- paste0(server, "projects/", proj_id, "/members?user_id=", otherid, "&access_level=40")
    resp <- curl_fetch_memory(murl, h)
    if (checkerr(resp$status_code) == TRUE) {
      content <- fromJSON(rawToChar(resp$content))
      message("Adding user ", content$name, " to team")
      list(status = "OKAY", content = content)
    } else {
      message("Error adding ", otherid, " to team: server code ", resp$status_code)
      # content <- fromJSON(rawToChar(resp$content))
      list(status = "SERVER_ERROR", content = rawToChar(resp$content))
    }
  })
}

#' Assign work to each student/team by creating a fork of the main repo
#'
#' @details See \url{https://github.com/vnijs/gitgadget} for additional documentation
#'
#' @param username Gitlab username
#' @param password Gitlab password
#' @param groupname Group to create on gitlab (defaults to user's namespace)
#' @param assignment Name of the assigment to assign
#' @param userfile A csv file with student information (i.e., username and token)
#' @param type Individual or Team work
#' @param pre Pre-amble for the assignment name, usually groupname + "-"
#' @param server The gitlab API server
#'
#' @export
assign_work <- function(username, password, groupname, assignment, userfile,
                        type = "individual", pre = "",
                        server = "https://gitlab.com/api/v3/") {

  resp <- connect(username, password, server)
  if (resp$status != 'OKAY')
    stop("Error connecting to server: check username/password/server")

  token <- resp$token
  upstream_name <- paste0(groupname, "/", paste0(pre, assignment))
  resp <- projID(upstream_name, token, server)

  if (resp$status != "OKAY")
    stop("Error getting assignment ", upstream_name)

  project_id <- resp$project_id
  student_data <- read.csv(userfile, stringsAsFactor = FALSE)
  student_data$user_id <- userIDs(student_data$userid, token, server)

  if (type == "individual")
    student_data$team <- paste("team",1:nrow(student_data))

  setup <- function(dat) {
    # dat$rownum <- sample(1:nrow(dat))
    # leader <- which(with(dat, rownum == min(rownum)))
    dat$rownum <- 1:nrow(dat); leader <- 1
    teamname <- dat$team[leader]
    setupteam(dat$token[leader], dat$user_id[-leader], project_id, server)
    dat$teamname <- teamname
    dat$leader <- dat$userid[leader]
    dat
  }

  resp <- student_data %>%
    group_by_at(.vars = "team") %>%
    do(setup(.))
}

maker <- function(repo_name, token, server, namespace = "") {

  if (namespace == "") {
    namespace_id <- namespace
  } else {
    resp <- groupID(namespace, namespace, token, server)
    if (resp$status != "OKAY")
      return(list(status = "NOSUCHGROUP", content = "Namespace to create repo does not exist"))
    namespace_id <- resp$group_id
  }

  ## check if repo already exists
  h <- new_handle()
  handle_setheaders(h, "PRIVATE-TOKEN" = token)
  murl <- paste0(server, "projects?", "namespace_id=", namespace_id)
  resp <- curl_fetch_memory(murl, h)
  content <- fromJSON(rawToChar(resp$content))
  id <- which(repo_name == content$name)
  if (length(id) > 0) {
    message("Got id for existing repo ", repo_name)
    return(list(status = "OKAY", repo_id = content$id[id[1]]))
  }

  h <- new_handle()
  handle_setopt(h, customrequest = "POST")
  handle_setheaders(h, "PRIVATE-TOKEN" = token)

  murl <- paste0(server, "projects?", "name=", repo_name, "&namespace_id=", namespace_id)
  resp <- curl_fetch_memory(murl, h)
  content <- fromJSON(rawToChar(resp$content))
  if (checkerr(resp$status_code) == TRUE) {
    message("Created repo ", repo_name)
    list(status = "OKAY", repo_id = content$id)
  } else if (content$message$name == "has already been taken") {
    list(status = "OKAY", repo_id = content$id)
  } else {
    message("Error creating repo")
    list(status = "SERVER_ERROR", content = content)
  }
}

#' Create the main repo from a local directory
#'
#' @details See \url{https://github.com/vnijs/gitgadget} for additional documentation
#'
#' @param username Gitlab username
#' @param password Gitlab password
#' @param groupname Group to create on gitlab (defaults to user's namespace)
#' @param assignment Name of the assigment (repo)
#' @param directory Base directory for the repo. file.path(directory, assignment) should exist
#' @param pre Pre-amble for the assignment name, usually groupname + "-"
#' @param server The gitlab API server
#'
#' @export
create_repo <- function(username, password, groupname, assignment, directory,
                        pre = "", server = "https://gitlab.com/api/v3/") {

  resp <- connect(username, password, server);
  if (resp$status != 'OKAY')
    stop("Error connecting to server: check username/password/server")

  token <- resp$token
  gn <- ifelse (groupname == "" || groupname == username, "", groupname)

  message("Making repo ", paste0(pre, assignment), " in group ", ifelse (gn == "", username, gn))
  resp <- maker(paste0(pre, assignment), token, server, gn)

  if (resp$status == "NOSUCHGROUP")
    stop("Add group ", gn, " before pushing assignment ", assignment)

  ## set directory and reset to current on function exit
  adir <- file.path(directory, assignment)
  if (!dir.exists(adir)) {
    dir.create(adir, recursive = TRUE)
    cat("New repo created by gitgadget", file = file.path(adir, "README.md"))
  }

  owd <- setwd(adir)
  on.exit(setwd(owd))

  ## initialize git repo if it doesn't exist yet
  if (!dir.exists(".git")) system2("git", "init")

  ## make sure .gitignore is added before create
  if (!file.exists(".gitignore"))
    cat(".Rproj.user\n.Rhistory\n.RData\n.Ruserdata\n.DS_Store\n", file = ".gitignore")

  ## make project file if needed
  rproj <- list.files(path = adir, pattern = "*.Rproj")
  if (length(rproj) == 0) {
    "Version: 1.0\n\nRestoreWorkspace: No\nSaveWorkspace: No\nAlwaysSaveHistory: Default\n\nEnableCodeIndexing: Yes\nUseSpacesForTab: Yes\nNumSpacesForTab: 2\nEncoding: UTF-8\n\nRnwWeave: knitr\nLaTex: pdfLaTeX\n\nAutoAppendNewline: Yes\n\nBuildType: Package\nPackageUseDevtools: Yes\nPackageInstallArgs: --no-multiarch --with-keep.source\nPackageRoxygenize: rd,collate,namespace\n" %>%
      cat(file = paste0(basename(adir), ".Rproj"))
  }

  if (gn == "") gn <- username
  # murl <- paste0("https://", username, ":", password,"@gitlab.com/", gn, "/", paste0(pre, assignment), ".git")
  murl <- paste0("https://gitlab.com/", gn, "/", paste0(pre, assignment), ".git")
  rorg <- system("git remote -v", intern = TRUE)

  if (length(rorg) == 0) {
    system2("git", c("remote", "add", "origin", murl))
  } else {
    system2("git", c("remote", "set-url", "origin", murl))
  }

  ## allow fetching of MRs
  ## https://gitlab.com/gitlab-org/gitlab-ce/blob/master/doc/workflow/merge_requests.md#checkout-merge-requests-locally
  remote_fetch <- system("git config --get-all remote.origin.fetch", intern = TRUE)
  if (!"+refs/merge-requests/*/head:refs/remotes/origin/merge-requests/*" %in% remote_fetch)
    system("git config --add remote.origin.fetch +refs/merge-requests/*/head:refs/remotes/origin/merge-requests/*")

  system2("git", c("add", "."))
  system2("git", c("commit", "-m", '"Upload repo using gitgadget"'))
  system2("git", c("push", "-u", "origin", "master"))
}

merger <- function(token, to, server,
                   title = "submission",
                   frombranch = "master",
                   tobranch = "master") {

  resp <- get_allprojects(token[1], server)
  forked <- resp$repo[resp$repo$forked_from_project$id == to,]

  if (length(forked) == 0) {
    message("Error trying to find fork")
    message(resp)
    return(list(status = "ERROR", content = resp))
  }

  from <- na.omit(forked$id)[1]

  if (length(from) == 0) {
    message("No fork found")
    return(list(status = "ERROR", content = ""))
  }

  h <- new_handle()
  handle_setopt(h, customrequest = "POST")
  handle_setheaders(h, "PRIVATE-TOKEN" = token[1])
  murl <- paste0(server, "projects/", from, "/merge_requests?source_branch=",
                 frombranch, "&target_branch=", tobranch, "&title=", title,
                 "&target_project_id=", to)

  resp <- curl_fetch_memory(murl, h)
  resp$content <- fromJSON(rawToChar(resp$content))
  if (checkerr(resp$status_code) == TRUE) {
    message("Generating merge request for", token[2])
    list(status = "OKAY", content = resp$content)
  } else if (grepl("This merge request already exists", resp$content)) {
    message("Merge request already exists for ", token[2])
    list(status = "OKAY", content = resp$content)
  } else {
    message("Error creating merge request for ", token[2], " ", resp$status_code)
    list(status = "ERROR", content = rawToChar(resp$content))
  }
}

#' Create merge requests for each student/team
#'
#' @details See \url{https://github.com/vnijs/gitgadget} for additional documentation
#'
#' @param username Gitlab username
#' @param password Gitlab password
#' @param groupname Group containing the assignment
#' @param assignment Name of the assigment. file.path(directory, assignment) should exist
#' @param userfile A csv file with student information (i.e., username and token)
#' @param type Individual or Team work
#' @param pre Pre-amble for the assignment name, usually groupname + "-"
#' @param server The gitlab API server
#'
#' @export
collect_work <- function(username, password, groupname, assignment, userfile,
                         type = "individual", pre = "",
                         server = "https://gitlab.com/api/v3/") {

  resp <- connect(username, password, server)
  if (resp$status != 'OKAY')
    stop("Error connecting to server: check username/password/server")

  token <- resp$token
  upstream_name <- paste0(groupname, "/", paste0(pre, assignment))
  resp <- projID(upstream_name, token, server)

  if (resp$status != "OKAY")
    stop("Error getting assignment ", upstream_name)

  project_id <- resp$project_id
  udat <- read.csv(userfile, stringsAsFactor = FALSE)

  if (type == "individual") {
    udat$team <- paste("ind", 1:nrow(udat))
  } else {
    ## MR only from team lead
    udat <- group_by_at(udat, .vars = "team") %>%
      slice(1)
  }

  udat$user_id <- userIDs(udat$userid, token, server)
  # resp <- sapply(udat$token, merger, project_id, server)
  resp <- apply(udat[,c("token","userid")], 1, merger, project_id, server)
  message("Finished attempt to collect all merge requests. Check the console for messages\n")
}

#' Fetch all merge requests as local branches and link to a remote
#'
#' @details See \url{https://github.com/vnijs/gitgadget} for additional documentation
#'
#' @param username Gitlab username
#' @param password Gitlab password
#' @param groupname Group containing the assignment
#' @param assignment Name of the assigment
#' @param pre Pre-amble for the assignment name, usually groupname + "-"
#' @param server The gitlab API server
#'
#' @export
fetch_work <- function(username, password, groupname, assignment,
                       pre = "",
                       server = "https://gitlab.com/api/v3/") {

  resp <- connect(username, password, server)
  if (resp$status != 'OKAY')
    stop("Error connecting to server: check username/password/server")

  token <- resp$token
  upstream_name <- paste0(groupname, "/", paste0(pre, assignment))
  resp <- projID(upstream_name, token, server)

  if (resp$status != "OKAY")
    stop("Error getting assignment ", upstream_name)

  project_id <- resp$project_id

  h <- new_handle()
  handle_setopt(h, customrequest = "GET")
  handle_setheaders(h, "PRIVATE-TOKEN" = token)

  ## collecting information on (max) 100 merge requests
  # resp <- curl_fetch_memory(paste0(server, "projects/", project_id, "/merge_requests?state=all&page=1&per_page=100"), h)
  resp <- curl_fetch_memory(paste0(server, "projects/", project_id, "/merge_requests?state=all&per_page=100"), h)

  nr_pages <- strsplit(rawToChar(resp$headers), "\n")[[1]] %>%
    .[grepl("X-Total-Pages",.)] %>%
    sub("X-Total-Pages:\\s+","",.) %>%
    as.numeric
  if (is.numeric(nr_pages) && nr_pages > 1) stop("Nr. of merge requests is > 100. Update limits in 'fetch_work'")

  mr <- fromJSON(rawToChar(resp$content))

  mrdat <-
    data_frame(id = mr$iid, un = mr$author$username) %>%
    arrange(un, desc(id)) %>%
    group_by(un) %>%
    slice(1) %>%
    ungroup %>%
    mutate(id = as.character(id))   ## needed to ensure there are no spaces in branch name

  # system("git fetch origin")
  system("git fetch origin +refs/merge-requests/*/head:refs/remotes/origin/merge-requests/*")

  branches <- system("git branch ", intern = TRUE) %>% gsub("[\\* ]+", "", .)

  create_branch <- function(dat) {
    if (any(grepl(dat[["un"]], branches))) {
      # cat("Branch", dat[["un"]], "already exists. To update this branch first delete the current branch in the Branch tab and then click the Fetch button again\n")
      system(paste0("git checkout ", dat[["un"]]))
      system(paste0("git merge origin/merge-requests/", dat[["id"]], " ", dat[["un"]]))
      ## the next two steps will commit, even if there is a merge conflict
      ## that way the process won't stop for a single branch/MR with a conflict
      system("git add .")
      system("git commit -m \"Update local branch with MR\"")
      system(paste0("git branch -d -r origin/merge-requests/", dat[["id"]]))
      system(paste0("git push"))
    } else {
      cat("Creating local and remote branch for ", dat[["un"]], "\n")
      system(paste0("git checkout -b ", dat[["un"]], " origin/merge-requests/", dat[["id"]]))
      system(paste0("git branch -d -r origin/merge-requests/", dat[["id"]]))
      system(paste0("git push --set-upstream origin ", dat["un"]))
    }
  }

  tmp <- apply(mrdat, 1, create_branch)
  message("Finished fetch attempt. Check the console for messages\n")
}

remove_group <- function(token, groupname, server) {
  resp <- groupID(groupname, groupname, token, server)

  id <- resp$group_id
  h <- new_handle()
  handle_setopt(h, customrequest = "DELETE")
  handle_setheaders(h, "PRIVATE-TOKEN" = token)
  resp <- curl_fetch_memory(paste0(server, "groups/", id), h)
}

remove_projects <- function(token, server) {
  ids <- get_allprojects(token, server)
  sapply(ids$repos$id, function(id) {
    remove_project(token, id, server)
  })
}

remove_project <- function(token, id, server) {
  h <- new_handle()
  handle_setopt(h, customrequest = "DELETE")
  handle_setheaders(h, "PRIVATE-TOKEN" = token)
  resp <- curl_fetch_memory(paste0(server, "projects/", id), h)
}

remove_student_projects <- function(userfile, server) {
  udat <- read.csv(userfile, stringsAsFactor = FALSE)
  sapply(udat$token, remove_projects, server)
}

update_file <- function(userfile, assignment, file, path, add = "\n\nAll finished!") {
  file <- "assignment.Rmd"
  path <- file.path(directory, assignment, file)

  content <- paste0(readLines(path), collapse = "\n") %>% paste0(add) %>% base64_enc

  udat <- read.csv(userfile, stringsAsFactor = FALSE)
  udat <- slice(udat, 1)
  id <- projID(paste0(udat$userid,"/",pre, assignment), udat$token, server)$project_id

  h <- new_handle()
  handle_setopt(h, customrequest = "POST")
  handle_setheaders(h, "PRIVATE-TOKEN" = udat$token)
  resp <-
    curl_fetch_memory(
      paste0(server, "projects/", id,"/repository/files?file_path=", file, "&branch_name=master&encoding=base64&commit_message=update file&content=", content)
      ,h
    )
}

## test section
# main_git__ <- TRUE
main_git__ <- FALSE
if (main_git__) {

  library(curl)
  library(jsonlite)
  library(dplyr)

  ## settings
  server <- getOption("git.server", default = "https://gitlab.com/api/v3/")
  username <- getOption("git.user", default = "")
  password <- getOption("git.password", default = "")
  groupname <- "rady-mgta-bc-2016"
  userfile <- "~/bc/rady-mgta-bc-2016/msba-students.csv"
  stopifnot(file.exists(userfile))

  ## to debug code
  permission <- 20

  ## important - name the assignment repo something unique because they will all reside
  ## in the student's namespace, i.e., two faculty might have assignment1
  assignment <- "assignment1"
  type <- "individual"
  pre <- paste0(groupname,"-")
  directory <- paste0("~/bc/", groupname)

  ## uncomment to cleanup
  # token <- connect(username, password, server)$token
  # remove_group(token, "rady-mgta-bc-2016", server)

  ## uncomment to remove all student projects!
  ## highly destructive!
  # remove_student_projects(userfile, server)

  ## repo <- "gitgadget-test-repo"
  # id <- projID(paste0("vnijs/",repo), token, server)$project_id
  # remove_project(token, id, server)

  ## To remove students from a group go to the group page, click on members,
  ## search for the students you want and click the delete icon

  ## removing individual projects cloned to a student's account
  # students <- read.csv(userfile, stringsAsFactors = FALSE)
  # for (i in 1:nrow(students)) {
  #   id <- projID(paste0(students[i, "userid"], "/rady-mgta-bc-2016-assignment1"), students[i,"token"], "https://gitlab.com/api/v3/")
  #   if (id$status == "OKAY")
  #     remove_project(students[i,"token"], id$project_id, "https://gitlab.com/api/v3/")
  # }

  ## check tokens
  userfile <- "~/ict/msba2017-gitlab.csv"
  # userfile <- "~/ict/msba-test-gitlab.csv"
  students <- read.csv(userfile, stringsAsFactors = FALSE)

  for (i in seq_len(nrow(students))) {
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
      username, password, groupname, userfile, permission = permission, server = server
    )

    ## get or create a repo for assignments and cases
    create_repo(
      username, password, groupname, assignment, directory, pre = pre,
      server = server
    )

    assign_work(
      username, password, groupname, assignment, userfile, type = type,
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
      username, password, groupname, assignment, directory, pre = pre,
      server = server
    )

    assign_work(
      username, password, groupname, assignment, userfile, type = type,
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
      username, password, groupname, assignment, userfile, type = type,
      pre = pre, server = server
    )

    fetch_work(
      username, password, groupname, assignment, pre = pre, server = server
    )

    # unlink(file.path(directory, assignment, ".git"), recursive = TRUE, force = TRUE)
    # dir.exists(file.path(directory, assignment, ".git"))
  } else {
    cat("Assignment does not exist")
  }

  ## create a repo on gitlab in the users own namespace
  groupname <- ""
  pre <- ""
  repo <- "gitgadget-test-repo"
  directory <- "/Users/vnijs/Desktop/Github"
  if (file.exists(file.path(directory, repo))) {
    unlink(file.path(directory, assignment, ".git"), recursive = TRUE, force = TRUE)
    dir.exists(file.path(directory, assignment, ".git"))
    create_repo(
      username, password, groupname, repo, directory, pre = pre,
      server = server
    )
    # unlink(file.path(directory, repo, ".git"), recursive = TRUE, force = TRUE)
    # dir.exists(file.path(directory, repo, ".git"))
  }
}
