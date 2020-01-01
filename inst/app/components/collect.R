output$ui_collect_assignment <- renderUI({
  resp <- assignment_name()
  if (length(resp) == 0) {
    HTML("<label>No assignments available for specified input values</label>")
  } else {
    selectInput("collect_assignment", "Assignment name:", choices = resp)
  }

})

collect_file_find <- shinyFiles::shinyFileChoose(
  input = input,
  id = "collect_file_find",
  roots = gg_volumes,
  session = session,
  filetype = "csv"
)

## https://gitlab.com/gitlab-org/gitlab-ce/blob/master/doc/workflow/merge_requests.md#checkout-merge-requests-locally
## setup a branch switcher so you can easily do "git checkout origin/merge-requests/1" for each PR
## can you push back tot the PR as well?
output$ui_collect_user_file <- renderUI({
  init <- Sys.getenv("git.userfile")
  if (!is.integer(input$collect_file_find)) {
    chosen <- shinyFiles::parseFilePaths(gg_volumes, input$collect_file_find)
    if (nrow(chosen) > 0) {
      init <- chosen$datapath
    }
  }
  textInput(
    "collect_user_file", "Upload file with student tokens:", 
    value = init, placeholder = "Open student CSV file"
  )
})

collect_tafile_find <- shinyFiles::shinyFileChoose(
  input = input,
  id = "collect_tafile_find",
  roots = gg_volumes,
  session = session,
  filetype = "csv"
)

## https://gitlab.com/gitlab-org/gitlab-ce/blob/master/doc/workflow/merge_requests.md#checkout-merge-requests-locally
## setup a branch switcher so you can easily do "git checkout origin/merge-requests/1" for each PR
## can you push back tot the PR as well?
output$ui_collect_ta_file <- renderUI({
  init <- Sys.getenv("git.tafile")
  if (!is.integer(input$collect_tafile_find)) {
    chosen <- shinyFiles::parseFilePaths(gg_volumes, input$collect_tafile_find)
    if (nrow(chosen) > 0) {
      init <- chosen$datapath
    }
  }
  textInput("collect_ta_file", "Upload file with TA tokens:", value = init, placeholder = "Open TA CSV file")
})

collect <- eventReactive(input$collect, {
  req(input$collect_token, input$collect_server, input$collect_user_file)

  cat("Generating merge requests ...\n")

  ## pre not used when called from the gadget interface because the full
  ## assignment name is retrieved from gitlab
  # input$collect_token, input$collect_group,
  withProgress(message = "Generating merge requests", value = 0, style = "old", {
    collect_work(
      input$collect_token, input$collect_assignment,
      input$collect_user_file,
      type = input$collect_type, server = input$collect_server
    )
  })

  message("\nGenerating merge requests complete. Check the console for messages. Click the 'Fetch' button to review the merge requests locally or view and comment on gitlab")
})

collect_fetch <- eventReactive(input$collect_fetch, {
  remote_fetch <- system("git config --get-all remote.origin.fetch", intern = TRUE)
  if (!"+refs/merge-requests/*/head:refs/remotes/origin/merge-requests/*" %in% remote_fetch) {
    system("git config --add remote.origin.fetch +refs/merge-requests/*/head:refs/remotes/origin/merge-requests/*")
  }


  # input$collect_token, input$collect_group,
  ## pre not used when called from the gadget interface because the full
  ## assignment name is retrieved from gitlab
  withProgress(message = "Fetching merge requests", value = 0, style = "old", {
    fetch_work(
      input$collect_token, input$collect_assignment,
      server = input$collect_server
    )
  })

  message("\nUse the Git tab in R-studio (click refresh first) to switch between different student assignment submissions\n")
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
    remove_users_repo(input$create_token, repo, input$create_user_file, server = input$create_server)
    cat("\nUser permissions removed ...\n\n")
  })
})


observeEvent(input$create_show_repo, {
  req(input$create_token, input$create_server, input$create_user_file)
  withProgress(message = "Showing class repo", value = 0, style = "old", {
    repo <- create_repo_name()
    add_users_repo(input$create_token, repo, input$create_user_file, permission = 20, server = input$create_server)
    cat("User permissions added ...\n")
  })
})

observeEvent(input$collect_hide_repo, {
  req(input$collect_token, input$collect_server, input$collect_user_file, input$collect_assignment)
  withProgress(message = "Hiding class repo", value = 0, style = "old", {
    remove_users_repo(input$collect_token, input$collect_assignment, input$collect_user_file, server = input$collect_server)
    cat("\nUser permissions removed ...\n")
  })
})

observeEvent(input$collect_show_repo, {
  req(input$collect_token, input$collect_server, input$collect_user_file, input$collect_assignment)
  withProgress(message = "Showing class repo", value = 0, style = "old", {
    add_users_repo(input$collect_token, input$collect_assignment, input$collect_user_file, permission = 20, server = input$create_server)
    cat("User permissions added ...\n\n")
  })
})

observeEvent(input$collect_hide_from_ta, {
  req(input$collect_token, input$collect_server, input$collect_user_file, input$collect_ta_file, input$collect_assignment)
  withProgress(message = "Hiding student forks from TA", value = 0, style = "old", {
    repo <- strsplit(input$collect_assignment, "/")[[1]] %>% {ifelse(length(.) > 1, .[2], .[1])}
    students <- read.csv(input$collect_user_file, stringsAsFactors = FALSE)
    if (input$collect_type == "team") {
      students <- distinct(students, team, .keep_all = TRUE)
    }
    for (i in seq_len(nrow(students))) {
      fork <- paste0(students[i, "userid"], "/", repo)
      remove_users_repo(students[i, "token"], fork, input$collect_ta_file, server = input$collect_server)
      message(paste0("Project fork ", fork, " hidden from TAs"))
    }
    cat("\nStudent forks hidden from TA ...\n")
  })
})

observeEvent(input$collect_show_to_ta, {
  req(input$collect_token, input$collect_server, input$collect_user_file, input$collect_ta_file, input$collect_assignment)
  withProgress(message = "Showing student forks to TA", value = 0, style = "old", {
    repo <- strsplit(input$collect_assignment, "/")[[1]] %>% {ifelse(length(.) > 1, .[2], .[1])}
    students <- read.csv(input$collect_user_file, stringsAsFactors = FALSE)
    if (input$collect_type == "team") {
      students <- distinct(students, team, .keep_all = TRUE)
    }
    for (i in seq_len(nrow(students))) {
      fork <- paste0(students[i, "userid"], "/", repo)
      add_users_repo(students[i, "token"], fork, input$collect_ta_file, permission = 40, server = input$collect_server)
      message(paste0("Project fork ", fork, " shown to TAs"))
    }
    cat("Student forks shown to TA ...\n\n")
  })
})

output$collect_output <- renderPrint({
  if (is_empty(input$collect_assignment) || is_empty(input$collect_user_file)) {
   cat("Provide GitLab token and load the user file with GitLab tokens. You should be in the\nRstudio project used to create the assignment repo or in a clone of that repo (i.e.,\ncheck if the Assignment name shown is correct). Then press the Collect button to generate\nMerge Requests. Click the Fetch button to review the Merge Requests locally as branches") } else {
   if (pressed(input$collect))
     ret <- collect()
   if (pressed(input$collect_fetch)) {
     cat("Fetching merge requests ...\n")
     ret <- collect_fetch()
   }
   if (not_pressed(input$collect) && not_pressed(input$collect_fetch))
     cat("Specify GitLab token and the user file with GitLab tokens. Then press the Collect button")
  }
})
