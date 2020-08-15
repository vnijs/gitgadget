assignment_name <- function(github = FALSE, url = FALSE) {
  assignment <- remote_info()
  assn <- NULL
  if (any(grepl("( git@)", assignment))) {
    server <- gsub("^.* git@([^:]+).*", "https://\\1", assignment)
    assn <- gsub("^.*:(.*)\\.git.*", "\\1", assignment)
  } else if (any(grepl(" https://", assignment))) {
    server <- gsub("^.* https://([^/]+).*", "https://\\1", assignment)
    assn <- gsub("^.* https://[^/]+/(.*)\\.git.*", "\\1", assignment)
  } else {
    cat(assignment)
    stop("Unable to determine the server and assignment name from the information printed above")
  }
  if (url) {
    assn <- paste0(server, "/", assn)
  }
  assn
}

output$ui_collect_assignment <- renderUI({
  resp <- assignment_name()
  if (is_empty(resp)) {
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
    mess <- capture.output(collect_work(
      input$collect_token, input$collect_assignment,
      input$collect_user_file,
      type = input$collect_type, server = input$collect_server
    ))
  })
  if (is_empty(mess)) mess <- "No messages"
  showModal(
    modalDialog(
      title = "Fetching merge request messages",
      span(HTML(paste0(mess, collapse = "</br>")))
    )
  )
  message("\nGenerating merge requests complete. Check the console for messages. Click the 'Fetch' button to review the merge requests locally or view and comment on gitlab")
})

collect_fetch <- eventReactive(input$collect_fetch, {
  remote_fetch <- system(paste("git -C", input$repo_directory, "config --get-all remote.origin.fetch"), intern = TRUE)
  if (!"+refs/merge-requests/*/head:refs/remotes/origin/merge-requests/*" %in% remote_fetch) {
    system(paste("git  -C", input$repo_directory, "config --add remote.origin.fetch +refs/merge-requests/*/head:refs/remotes/origin/merge-requests/*"))
  }

  ## pre not used when called from the gadget interface because the full
  ## assignment name is retrieved from gitlab
  withProgress(message = "Fetching merge requests", value = 0, style = "old", {
    owd <- setwd(input$repo_directory)
    on.exit(setwd(owd))
    mess <- capture.output(fetch_work(
      input$collect_token, input$collect_assignment,
      server = input$collect_server
    ))
  })
  if (is_empty(mess)) mess <- "No messages"
  showModal(
    modalDialog(
      title = "Fetching merge request messages",
      span(HTML(paste0(mess, collapse = "</br>")))
    )
  )
  message("\nUse the Git tab in R-studio (click refresh first) to switch between different student assignment submissions\n")
})

observeEvent(input$collect_hide_repo, {
  req(input$collect_token, input$collect_server, input$collect_user_file, input$collect_assignment)
  withProgress(message = "Hiding class repo", value = 0, style = "old", {
    mess <- capture.output(remove_users_repo(input$collect_token, input$collect_assignment, input$collect_user_file, server = input$collect_server))
    cat("\nUser permissions removed ...\n")
  })
  if (is_empty(mess)) mess <- "No messages"
  showModal(
    modalDialog(
      title = "Hide repo messages",
      span(HTML(paste0(mess, collapse = "</br>")))
    )
  )
})

observeEvent(input$collect_show_repo, {
  req(input$collect_token, input$collect_server, input$collect_user_file, input$collect_assignment)
  withProgress(message = "Showing class repo", value = 0, style = "old", {
    owd <- setwd(input$repo_directory)
    on.exit(setwd(owd))
    mess <- capture.output(add_users_repo(input$collect_token, input$collect_assignment, input$collect_user_file, permission = 20, server = input$create_server))
    cat("User permissions added ...\n\n")
  })
  if (is_empty(mess)) mess <- "No messages"
  showModal(
    modalDialog(
      title = "Show repo messages",
      span(HTML(paste0(mess, collapse = "</br>")))
    )
  )
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
      owd <- setwd(input$repo_directory)
      on.exit(setwd(owd))
      mess <- capture.output(remove_users_repo(students[i, "token"], fork, input$collect_ta_file, server = input$collect_server))
      message(paste0("Project fork ", fork, " hidden from TAs"))
    }
    cat("\nStudent forks hidden from TA ...\n")
  })
  if (is_empty(mess)) mess <- "No messages"
  showModal(
    modalDialog(
      title = "Hide repos from TA messages",
      span(HTML(paste0(mess, collapse = "</br>")))
    )
  )
})

observeEvent(input$collect_show_to_ta, {
  req(input$collect_token, input$collect_server, input$collect_user_file, input$collect_ta_file, input$collect_assignment)
  withProgress(message = "Showing student forks to TA", value = 0, style = "old", {
    repo <- strsplit(input$collect_assignment, "/")[[1]] %>% {ifelse(length(.) > 1, .[2], .[1])}
    students <- read.csv(input$collect_user_file, stringsAsFactors = FALSE)
    if (input$collect_type == "team") {
      students <- distinct(students, team, .keep_all = TRUE)
    }
    owd <- setwd(input$repo_directory)
    on.exit(setwd(owd))
    for (i in seq_len(nrow(students))) {
      fork <- paste0(students[i, "userid"], "/", repo)
      mess <- capture.output(add_users_repo(students[i, "token"], fork, input$collect_ta_file, permission = 40, server = input$collect_server))
      message(paste0("Project fork ", fork, " shown to TAs"))
    }
    cat("Student forks shown to TA ...\n\n")
  })
  if (is_empty(mess)) mess <- "No messages"
  showModal(
    modalDialog(
      title = "Show repos to TA messages",
      span(HTML(paste0(mess, collapse = "</br>")))
    )
  )
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
