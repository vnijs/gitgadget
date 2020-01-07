output$ui_create_token <- renderUI({
  req(input$create_remote)
  if (input$create_remote == "GitLab") {
    passwordInput("create_token", "GitLab token:", value = Sys.getenv("git.token"))
  } else {
    passwordInput("create_token", "GitHub token:", value = Sys.getenv("GITHUB_PAT"))
  }
})

output$ui_create_pre <- renderUI({
  req(input$create_group)
  textInput("create_pre", "Prefix:", value = Sys.getenv("git.prefix"))
})

shinyFiles::shinyDirChoose(input, "create_directory_find", roots = gg_volumes)

output$ui_create_directory <- renderUI({
  init <- projdir
  if (!is.integer(input$create_directory_find)) {
    init <- shinyFiles::parseDirPath(gg_volumes, input$create_directory_find)
  }
  textInput(
    "create_directory", "Local directory:", value = init, 
    placeholder = "Base directory for the git repo"
  )
})

create_uploadfile <- shinyFiles::shinyFileChoose(
  input = input,
  id = "create_file_find",
  roots = gg_volumes,
  session = session,
  filetype = "csv"
)

output$ui_create_user_file <- renderUI({
  init <- Sys.getenv("git.userfile")
  if (!is.integer(input$create_file_find)) {
    chosen <- shinyFiles::parseFilePaths(gg_volumes, input$create_file_find)
    if (nrow(chosen) > 0) {
      init <- chosen$datapath
    }
  }
  textInput(
    "create_user_file", "Upload file with student tokens:", 
    value = init, placeholder = "Open student CSV file"
  )
})

create_ta_uploadfile <- shinyFiles::shinyFileChoose(
  input = input,
  id = "create_tafile_find",
  roots = gg_volumes,
  session = session,
  filetype = "csv"
)

output$ui_create_ta_file <- renderUI({
  init <- Sys.getenv("git.tafile")
  if (!is.integer(input$create_tafile_find)) {
    chosen <- shinyFiles::parseFilePaths(gg_volumes, input$create_tafile_find)
    if (nrow(chosen) > 0) {
      init <- chosen$datapath
    }
  }
  textInput(
    "create_ta_file", "Upload file with TA tokens:", 
    value = init, placeholder = "Open TA CSV file"
  )
})

output$ui_create_buttons <- renderUI({
  if (input$intro_user_type == "faculty" && !is_empty(input$create_user_file)) {
    tagList(
      actionButton(
        "create", "Create", 
        title = "Create a new repo using the gitlab API"
      ),
      actionButton(
        "create_hide_repo", "Hide", 
        title = "Hide class repo from students", class = "btn-warning"
      ),
      actionButton(
        "create_show_repo", "Show", 
        title = "Show class repo to students", class = "btn-success"
      )
    )
  } else {
    actionButton("create", "Create", title = "Create a new repo using the gitlab API")
  }
})

observeEvent(input$create_check_tokens, {
  mess <- ""
  if (!is_empty(input$create_user_file)) {
    withProgress(message = "Checking student tokens on GitLab", value = 0, style = "old", {
      mess <- capture.output(check_tokens(input$create_user_file))
    })
  }
  if (!is_empty(input$create_ta_file)) {
    withProgress(message = "Checking TA tokens on GitLab", value = 0, style = "old", {
      mess <- capture.output(check_tokens(input$create_ta_file)) %>%
        paste0(mess, "\n\n", .)
    })
  }
  showModal(
    modalDialog(title = "Check token messages",
      span(HTML(paste0(mess, collapse = "</br>")))
    )
  )
  message("\nToken check completed. Check the console for messages\n")
})

## Show remove_git modal when button is clicked.
observeEvent(input$remove_git_show, {
  ## See https://shiny.rstudio.com/reference/shiny/latest/modalDialog.html
  showModal(
    modalDialog(title = "Remove local .git directory",
      span("Are you sure you want to remove the local .git directory? Use only if you want to destroy all local history and restart!"),
      footer = tagList(
        modalButton("Cancel"),
        actionButton(
          "remove_git", "Remove .git", 
          title = "Remove previous .git directory if present", 
          class = "btn-danger"
        )
      )
    )
  )
})

remove_git <- observeEvent(input$remove_git, {
  removeModal()
  if (!dir.exists(input$create_directory)) {
    cat("\nThe specified directory does not exist\n")
    return(invisible())
  }

  if (!dir.exists(file.path(input$create_directory, ".git"))) {
    cat("\nNo .git directory found\n")
  } else {
    cat("\nA .git directory was found and removed\n")
    unlink(file.path(input$create_directory, ".git"), recursive = TRUE, force = TRUE)
  }
})

## Show remove_git modal when button is clicked.
observeEvent(input$remove_remote_show, {
  req(input$create_remote)
  ## See https://shiny.rstudio.com/reference/shiny/latest/modalDialog.html
  if (input$create_remote == "GitLab") {
    showModal(
      modalDialog(title = "Remove remote GitLab repo",
        span("Are you sure you want to remove the remote repo on GitLab? Use only if you want to destroy all remote files and history and restart!"),
        footer = tagList(
          with(tags, table(
            align = "right",
            td(modalButton("Cancel")),
            td(conditionalPanel("input.intro_user_type == 'faculty' && input.create_user_file != ''",
              actionButton(
                "remove_forks", "Remove forks", 
                title = "Remove forks from current repo created for students", 
                class = "btn-danger"
              )
            )),
            td(actionButton(
              "remove_gitlab", "Remove remote", 
              title = "Remove previous remote repo if present", 
              class = "btn-danger"
            ))
          ))
        )
      )
    )
  } else {
    showModal(
      modalDialog(title = "Remove remote GitHub repo",
        span("This feature has not yet been implemented for GitHub repos")
      )
    )
  }
})

remove_gitlab <- observeEvent(input$remove_gitlab, {
  removeModal()
  if (is_empty(input$create_token)) {
    cat("Token required to remove remote repo")
    return(invisible())
  }

  if (!dir.exists(input$create_directory)) {
    cat("The specified directory does not exist. Create the directory and try again")
    return(invisible())
  }

  withProgress(message = "Removing remote repo", value = 0, style = "old", {
    create_group_lc <- tolower(input$create_group) %>%
      {ifelse(is_empty(.), Sys.getenv("git.user", ""), .)}

    if (is_empty(create_group_lc)) {
      cat("No group or user name provided. Please provide this information and try again")
      return(invisible())
    }

    create_pre_lc <- tolower(input$create_pre)
    repo <- basename(input$create_directory)

    cat("Removing remote repo ...\n")

    id <- projID(
      paste0(create_group_lc, "/", create_pre_lc, repo), 
      input$create_token, input$create_server
    )
    if (id$status == "OKAY") {
      resp <- remove_project(input$create_token, id$project_id, input$create_server)
      if (checkerr(resp$status_code))
        cat("\nRemote repo successfully removed\n")
      else
        cat("\nProblem removing remote repo. See the console for messages\n")
    } else {
      cat("\nNo remote repo found\n")
    }
  })
})

remove_forks <- observeEvent(input$remove_forks, {
  removeModal()
  if (is_empty(input$create_token)) {
    cat("Token required to remove student forks")
    return(invisible())
  }

  if (!dir.exists(input$create_directory)) {
    cat("The specified directory does not exist. Create the directory and try again")
    return(invisible())
  }

  withProgress(message = "Removing student forks", value = 0, style = "old", {
    create_group_lc <- tolower(input$create_group)
    create_pre_lc <- tolower(input$create_pre)
    repo <- basename(input$create_directory)

    message("Removing student forks ...\n")

    students <- read.csv(input$create_user_file, stringsAsFactors = FALSE)
    for (i in seq_len(nrow(students))) {
      id <- projID(
        paste0(students[i, "userid"], "/", create_pre_lc, repo), 
        students[i, "token"], "https://gitlab.com/api/v4/"
      )
      if (id$status == "OKAY") {
        remove_project(students[i, "token"], id$project_id, "https://gitlab.com/api/v4/")
        message(paste0("Project fork ", id$project_id, " removed for ", students[i, "userid"], " in ", students[i, "team"]))
      }
    }
    message("\nFork removal process complete. Check the console for messages\n")
  })
})

create <- eventReactive(input$create, {

  if (is_empty(input$create_user_name) || is_empty(input$create_token)) {
    cat("Username and token required to create a new repo")
    return(invisible())
  }

  if (!dir.exists(input$create_directory)) {
    cat("The specified directory does not exist. Create the directory and try again")
    return(invisible())
  }

  repo <- basename(input$create_directory)

  if (grepl("[^A-z0-9_\\.\\-]", repo)) {
    cat("The repo name cannot contain spaces or symbols. Please change the name and try again")
    return(invisible())
  }

  directory <- dirname(input$create_directory)
  if (directory == dirname(input$intro_git_home)) {
    mess <- "The local directory cannot be the same as base directory for all repos. Please change the local directory that contains the repo you want to create"
    showModal(
      modalDialog(
        title = "Update the local directory",
        span(mess)
      )
    )
    return(invisible())
  }

  mess <- ""

  if (input$create_remote == "GitLab") {
    withProgress(message = "Creating and forking repo", value = 0, style = "old", {

      create_group_lc <- tolower(input$create_group)
      create_pre_lc <- tolower(input$create_pre)

      if (create_group_lc != "" && create_group_lc != Sys.getenv("git.user")) {
        cat("Creating group ...\n")
        mess <- capture.output(create_group(
          input$create_token, create_group_lc, input$create_user_file,
          permission = 0, server = input$create_server
        ))
      }

      cat("Creating repo ...\n")
      mess <- capture.output(create_repo(
        input$create_user_name, input$create_token, repo, directory, create_group_lc,
        pre = create_pre_lc, ssh = ifelse(isTRUE(input$create_ssh == "ssh"), TRUE, FALSE), 
        server = input$create_server
      )) %>% paste0(mess, "\n\n", .)
      if (!is_empty(input$create_user_file)) {
        if (is_empty(input$create_group)) {
          cat("A groupname is required when assigning work.\n")
          cat("Add a groupname and try again ...\n")
        } else {
          cat("Assigning work ...\n")
          mess <- capture.output(assign_work(
            input$create_token, create_group_lc, repo,
            input$create_user_file,
            tafile = input$create_ta_file,
            type = input$create_type, pre = create_pre_lc,
            server = input$create_server
          )) %>% paste0(mess, "\n\n", .)
        }
      }
    })
    showModal(
      modalDialog(
        title = "Repo creation messages",
        span(HTML(paste0(mess, collapse = "</br>")))
      )
    )
  } else {
    cat("Creating repo ...\n")
    curr <- setwd(input$create_directory)
    on.exit(setwd(curr))
    usethis::use_git()
    usethis::use_github(protocol = "https", auth_token = input$create_token)
  }

  message("\nCreate process complete. Check the console for messages\n")
})

create_repo_name <- reactive({
  repo <- basename(input$create_directory)
  create_pre_lc <- tolower(input$create_pre)
  if (!is_empty(create_pre_lc)) {
    repo <- paste0(create_pre_lc, repo)
  }

  create_group_lc <- tolower(input$create_group)
  if (!is_empty(create_group_lc) && create_group_lc != Sys.getenv("git.user")) {
    repo <- paste0(create_group_lc, "/", repo)
  }
  repo
})

observeEvent(input$create_hide_repo, {
  req(input$create_token, input$create_server, input$create_user_file)
  withProgress(message = "Hiding class repo", value = 0, style = "old", {
    repo <- create_repo_name()
    owd <- setwd(input$repo_directory)
    on.exit(setwd(owd))
    mess <- capture.output(remove_users_repo(input$create_token, repo, input$create_user_file, server = input$create_server))
    cat("\nUser permissions removed ...\n\n")
  })
  showModal(
    modalDialog(
      title = "Hide repo messages",
      span(HTML(paste0(mess, collapse = "</br>")))
    )
  )
})

observeEvent(input$create_show_repo, {
  req(input$create_token, input$create_server, input$create_user_file)
  withProgress(message = "Showing class repo", value = 0, style = "old", {
    repo <- create_repo_name()
    owd <- setwd(input$repo_directory)
    on.exit(setwd(owd))
    mess <- capture.output(add_users_repo(input$create_token, repo, input$create_user_file, permission = 20, server = input$create_server))
    cat("User permissions added ...\n")
  })
  showModal(
    modalDialog(
      title = "Show repo messages",
      span(HTML(paste0(mess, collapse = "</br>")))
    )
  )
})

output$create_output <- renderPrint({
  input$create  ## creating a dependency
  ret <- create()
})
