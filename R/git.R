printf <- function(...) cat(paste0(sprintf(...), collapse = "\n"))
checkerr <- function(code) floor(code/100) == 2

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
  # name <- course_name
  # path <- name

  h <- new_handle()
  handle_setheaders(h, "PRIVATE-TOKEN" = token)
  murl <- paste0(server, "groups")
  resp <- curl_fetch_memory(murl, h)
  if (checkerr(resp$status_code) == FALSE)
    return(list(status ="SERVER_ERROR"))

  resp$content <- fromJSON(rawToChar(resp$content))

  ## check if group exists
  id <- which(name == resp$content$name && path == resp$content$path)

  if (length(id) == 0) {
    list(status = "NO_SUCH_GROUP")
  } else {
    list(status = "OKAY", group_id = resp$content$id[id])
  }
}

userIDs <- function(usernames, token, server) {
  # usernames <- userfile$userid
  sapply(usernames, function(username) {
    resp <- userID(username, token, server)
    ifelse (resp$status == "OKAY", resp$user_id[1], NA)
  })
}

userID <- function(username, token, server) {
  # username <- usernames[1]
  h <- new_handle()
  handle_setopt(h, customrequest = "GET")
  handle_setheaders(h, "PRIVATE-TOKEN" = token)
  resp <- curl_fetch_memory(paste0(server,"users?username=", username), h)

  if (checkerr(resp$status_code) == FALSE)
    return(list(status = "SERVER_ERROR", message = rawToChar(resp$content)))

  uid <- fromJSON(rawToChar(resp$content))$id
  if (length(uid) == 0) {
    list(status = "NO_SUCH_USER")
  } else {
    list(status = "OKAY", user_id = uid[1])
  }
}

groupr <- function(group_name, path, token, server) {

  # server <- SERVER
  # group_name <- course_name

  h <- new_handle()
  handle_setopt(h, customrequest = "POST")
  handle_setheaders(h, "PRIVATE-TOKEN" = token)
  murl <- paste0(server, "groups?name=", group_name, "&path=", path, "&visibility_level=0")
  resp <- curl_fetch_memory(murl, h)
  if (checkerr(resp$status_code) == FALSE)
    return(list(status = "SERVER_ERROR", message = fromJSON(rawToChar(resp$content))$message))

  group_id <- fromJSON(rawToChar(resp$content))$id
  list(status = "OKAY", group_id = group_id)
}

add_users <- function(user_ids, group_id, token, permission, server) {
  # group_id <- course_id
  sapply(user_ids, function(user_id) {
    add_user(user_id, group_id, token, permission, server)$status
  })
}

add_user <- function(user_id, group_id, token, permission, server) {
  # user_id <- user_ids[1]
  # permission <- 20
  h <- new_handle()
  handle_setopt(h, customrequest = "POST")
  handle_setheaders(h, "PRIVATE-TOKEN" = token)
  murl <- paste0(server, "groups/", group_id, "/members?user_id=", user_id, "&access_level=", permission)
  resp <- curl_fetch_memory(murl,h)
  if (checkerr(resp$status_code) == FALSE) {
    mesg <- fromJSON(rawToChar(resp$content))$message
    if (mesg == "Already exists") {
      return(list(status = "OKAY"))
    } else {
      # print(mesg)
      return(list(status = "SERVER_ERROR", message = mesg))
    }
  }

  ## what does this do?
  resp$content <- fromJSON(rawToChar(resp$content))
  list(status = "OKAY")
}

#` export
create_group <- function(username, password, course_name, student_file,
                         pre = "rady-", permission = 20,
                         server = "https://gitlab.com/api/v3/") {

  # pre <- "rady-msba-"
  ## permission 20 --> reporter

  cn <- paste0(pre, course_name)
  resp <- connect(username, password, server)
  if (resp$status != "OKAY")
    stop("Error connecting to server: check username/password/server")

  token <- resp$token
  resp <- groupID(cn, cn, token, server)

  if (resp$status == "NO_SUCH_GROUP")
    resp <- groupr(cn, cn, token, server = server)

  if (resp$status != "OKAY") {
    message("Unable to create or get group: ", resp$message)
    return(invisible())
  }

  ## must give users permission in order to forked it to them
  course_id <- resp$group_id
  userfile <- read.csv(student_file, stringsAsFactor = FALSE)
  user_ids <- userIDs(userfile$userid, token, server)

  add_users(user_ids, course_id, token, permission, server)
}

get_allprojects <- function(token, server, everything = FALSE) {
  h <- new_handle()
  handle_setopt(h, customrequest = "GET")
  handle_setheaders(h, "PRIVATE-TOKEN" = token)
  resp <- curl_fetch_memory(paste0(server, "projects"), h)
  if (checkerr(resp$status_code) == FALSE) {
    message("SERVER_ERROR: Problem getting projects")
    return(list(status="SERVER_ERROR",message=fromJSON(rawToChar(resp$content))$message))
  }
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
  if (length(mainproj$id)==0) {
    message("No such repo: ", path_with_namespace, "  Status code: ",mainproj$message)
    list(status="NO_SUCH_REPO",message='No such repo')
  } else {
    list(status="OKAY",project_id=mainproj$id[1])
  }
}

forkRepo <- function(proj_id, token, server) {
  h <- new_handle()
  handle_setopt(h, customrequest = "POST")
  handle_setheaders(h,"PRIVATE-TOKEN" = token)
  murl <- paste0(server, "projects/fork/", proj_id)
  resp <- curl_fetch_memory(murl, h)
  if (checkerr(resp$status_code) == FALSE) {
    message("Problem forking")
    list(status = "SERVER_ERROR", message = fromJSON(rawToChar(resp$content))$message)
  } else {
    content <- fromJSON(rawToChar(resp$content))
    list(status = "OKAY", content = content)
  }
}

renameRepo <- function(proj_id, token, newname) {
  h <- new_handle()
  handle_setopt(h, customrequest = "PUT")
  handle_setheaders(h, "PRIVATE-TOKEN" = token)
  murl <- paste0(SERVER, "projects/", proj_id, "?name=", newname)
  resp <- curl_fetch_memory(murl, h)
  if (checkerr(resp$status_code) == FALSE) {
    message("Problem changing repo name")
    list(status='SERVER_ERROR', message = fromJSON(rawToChar(resp$content))$message)
  } else {
    list(status = "OKAY", name = fromJSON(rawToChar(resp$content))$name)
  }
}

setupteam <- function(leader_token, other_user_ids, assignment_id, course, team_name, server, pre) {
  ##fork if needed for leader
  resp <- get_allprojects(leader_token, server, everything = TRUE)

  if (!"forked_from_project" %in% names(resp$repos) || !assignment_id %in% resp$repos$forked_from_project$id) {
  # if (is.null(resp$repos$forked_from_project) | !(assignment_id %in% resp$repos$forked_from_project$id)) {
    # printf("fork needs to be created for leader")
    message("Creating fork for team lead")
    resp <- forkRepo(assignment_id, leader_token, server)
    if (resp$status != "OKAY") {
      message("Error forking for leader")
      return()
    }
    leader_project_id <- resp$content$id
    leader_project_name <- resp$content$name
    upstream_name <- resp$content$name
  } else {
    message("Team lead already forked the assignment")
    id <- which(resp$repos$forked_from_project$id == assignment_id)
    if (length(id) == 0) {
      message("Can't find repo ID for team lead")
      return()
    }
    leader_project_id <- resp$repos$id[id]
    leader_project_name <- resp$repos$name[id]
    upstream_name <- resp$repos$forked_from_project$name[id]
  }

  ##rename leader's repo if needed.
  ## VN -- needed?
  # if (upstream_name == leader_project_name) {
  #   # course <- gsub('rady-msba-(.*)','\\1',course)
  #   cn <- gsub(paste0(pre,"-(.*)","\\1",course)
  #   leader_project_name <- paste(cn,'-',team_name,'-',upstream_name,sep='')
  #   resp <- renameRepo(leader_project_id,leader_token,leader_project_name)
  #   if (resp$status=='OKAY') {
  #       printf("Renamed leader's repo to %s",leader_project_name)
  #   } else {
  #     printf("Error renaming leader's repo to %s",leader_project_name)
  #   }
  # } else {
  #   printf("Leader repo has already been renamed to %s",leader_project_name)
  # }

  #add other_user_ids as users
  if (length(other_user_ids) > 0) {
    message("Adding team members to lead's repo")
    add_team(leader_project_id, leader_token, other_user_ids, server)
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
      # printf("\nAdding user %s to team\n",content$name)
      message("Adding user ", content$name, " to team")
      list(status = "OKAY", content = content)
    } else {
      # printf("\nError adding %s to team: server code %d\n",otherid,resp$status_code)
      message("Error adding ", otherid, " to team: server code ", resp$status_code)
      content <- fromJSON(rawToChar(resp$content))
      list(status = "SERVER_ERROR", content = content)
    }
  })
}

#` export
assign_work <- function(username, password, course_name, assignment, student_file,
                        type = "individual", pre = "rady-", server = "https://gitlab.com/api/v3/") {

  cn <- paste0(pre, course_name)

  resp <- connect(username, password, server)
  if (resp$status != 'OKAY')
    stop("Error connecting to server: check username/password/server")

  token <- resp$token
  upstream_name <- paste0(cn, "/", assignment)
  resp <- projID(upstream_name, token, server)

  if (resp$status != "OKAY")
    stop("Error getting assignment ",upstream_name)

  assignment_id <- resp$project_id
  student_data <- read.csv(student_file, stringsAsFactor = FALSE)
  student_data$user_id <- userIDs(student_data$userid, token, server)

  if (type == "individual")
   student_data$team <- paste("team",1:nrow(student_data))

  setup <- function(dat) {
    # dat <- student_data[1:2,]
    dat$rownum <- sample(1:nrow(dat))
    leader <- which(with(dat, rownum == min(rownum)))
    teamname <- dat$team[leader]
    setupteam(leader_token = dat$token[leader], other_user_ids = dat$user_id[-leader],
              assignment_id = assignment_id, course = cn, team_name = teamname, server, pre)
    dat$teamname <- teamname
    dat$leader <- dat$userid[leader]
    dat
  }

  student_data %>% group_by(team) %>% do(setup(.)) %>% print(n = 1000)

  # aggregate(student_data$rownum,by=list(team=student_data$team),function(rownum) {
  #     leader <- min(rownum)
  #     if (length(rownum)!=1) {
  #         others <- rownum[!(rownum %in% leader)]
  #         printf("Setting up team:%s : Leader: %s Others: %s",student_data$team[leader],student_data$userid[leader],
  #                paste(student_data$userid[others],sep=','))
  #     } else {
  #         others <- c()
  #         printf("Setting up team:%s : Leader: %s Others: -",student_data$team[leader],student_data$userid[leader])
  #     }
  #     teamname <- student_data$team[leader]
  #     setupteam(leader_token=student_data$token[leader],
  #               other_user_ids=student_data$user_id[others],
  #               assignment_id=assignment_id,
  #               course=cn,
  #               team_name=teamname)
  # })
  return(NULL)
}

#####################

maker <- function(repo_name, token, server, namespace = "") {

  # namespace <- cn
  # repo_name <- assignment
  # namespace
  # repo_name

  if (namespace == "") {
    namespace_id <- namespace
  } else {
    resp <- groupID(namespace, namespace, token, server)
    if (resp$status != "OKAY")
      return(list(status = "NO_SUCH_GROUP", content = "Namespace to create repo does not exist"))
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

#` export
create_repo <- function(username, password, course_name, assignment, directory,
                        pre = "rady-", server = "https://gitlab.com/api/v3/") {

  resp <- connect(username, password, server);
  if (resp$status != 'OKAY')
    stop("Error connecting to server: check username/password/server")

  token <- resp$token
  if (course_name == "" || course_name == username) {
    cn <- ""
  } else {
    cn <- paste0(pre, course_name)
  }

  # message("Making repo ", assignment, " for course ", cn)
  resp <- maker(assignment, token, server, cn)

  if (resp$status == "NO_SUCH_GROUP")
    stop("Add group ", cn, " before pushing assignment ", assignment)

  ## set directory and reset to current on function exit
  if (!dir.exists(directory)) {
    dir.create(directory, recursive = TRUE)
    cat("New repo created by gitgadget", file = file.path(directory, "README.md"))
  }

  owd <- setwd(directory)
  on.exit(setwd(owd))

  ## initialize git repo if it doesn't exist yet
  if (!dir.exists(".git")) system2("git", "init")

  ## needed? doesn't seem to work on windows
  # system2("unset", "SSH_ASKPASS")
  # system2("unsetenv", "SSH_ASKPASS")

  if (cn != "") cn <- paste0(cn, "/")
  if (cn == "") cn <- paste0(cn, username, "/")
  murl <- paste0("https://", username, ":", password,"@gitlab.com/", cn, assignment, ".git")
  rorg <- system("git remote -v", intern = TRUE)
  rorg
  borg <- "origin"

  if (length(rorg) == 0) {
    system2("git", c("remote", "add", borg, murl))
  } else if (grepl(murl, rorg) %>% sum(.) == 0) {
    borg <- "alternative"
    if (grepl(borg, rorg) %>% sum(.) > 0)
      system2("git", c("remote", "rm", borg))
    system2("git", c("remote", "add", borg, murl))
  }

  system2("git", c("add", "."))
  system2("git", c("commit", "-m", '"Upload repo using gitgadget"'))
  system2("git", c("push", "-u", borg, "master"))
}

##############
main__ <- FALSE

if (main__) {

  setwd("~/gh/gitgadget")
  server <- getOption("git.server", default = "https://gitlab.com/api/v3/")
  username <- getOption("git.user", default = "")

  ## why not ssh with key?
  password <- getOption("git.password", default = "")

  ## rady-msba will be added in course-update
  course_name <- "mgta-bc"
  group_name <- course_name

  ##important - name the assignment something unique. because they will all reside in the student's namespce
  ##so if we dont name it appropriately, two faculty might have assignment1 which would create conflict.
  ##and we wont be able to fork.
  assignment <- "assignment1" %>% paste0(course_name, "-", .)
  student_file <- "~/Desktop/mgba-students.csv"
  # pre = ""
  pre = "rady-mgta460"
  directory <- "/Users/vnijs/Github/rady-mgta-bc"

  ## create a group for a course where all assignments and cases will be posted
  create_group(
    username, password, course_name, student_file,
    pre = pre, server = server
  )

  ## get or create a repo for assignments and cases
  create_repo(
    username, password, course_name, assignment, directory,
    pre = pre, server = server
  )

  assign_work(
    username, password, course_name, assignment, student_file,
    type = "individual", pre = pre, server = server
  )

  assignment <- "assignment2" %>% paste0(course_name, "-", .)

  create_repo(
    username, password, course_name, assignment, directory,
    pre = pre, server = server
  )

  assign_work(
    username, password, course_name, assignment, student_file,
    type = "team", pre = pre, server = server
  )
}

## connect gets the faculties token ... provide directly? Perhaps in .Rprofile?
