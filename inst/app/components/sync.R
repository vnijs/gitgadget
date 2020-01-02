
remote_info <- reactive({
  input$sync; input$sync_unlink; input$branch_link; input$branch_unlink
  req(input$repo_directory)
  suppressWarnings(paste0(system(paste("git -C", input$repo_directory, "remote -v"), intern = TRUE), collapse = "\n")) %>%
    gsub("(\t)|(  ) ", " ", .)
})

assignment_name <- function() {
  # assignment <- capture.output(remote_info())[-1:-2]
  assignment <- remote_info()
  if (any(grepl("(https://github.com)|(git@github.com)", assignment))) {
    message("GitGadget does not (yet) support assignment management on GitHub.com")
    ""
  } else{
    if (any(grepl("https://gitlab.com", assignment))) {
      server <- "https://gitlab.com"
      gsub(paste0("^.*", server, "/(.*).git.*"), "\\1", assignment) %>% unique()
    } else {
      server <- "git@gitlab.com"
      gsub(paste0("^.*", server, ":(.*).git.*"), "\\1", assignment) %>% unique()
    }
  }
}

upstream_info <- function() {
  input$sync; input$sync_unlink
  req(input$repo_directory)
  suppressWarnings(system(paste("git -C", input$repo_directory, "remote -v"), intern = TRUE)) %>%
    .[grepl("^upstream", .)] %>%
    gsub("^upstream\\s+", "", .) %>%
    gsub(" \\(fetch\\)$", "", .)
}

observeEvent(input$sync_commit, {
  req(input$repo_directory)
  cmess <- input$sync_commit_message
  req(input$repo_directory)
  if (is_empty(cmess))
    cmess <- paste0("Updates: ", Sys.time())
  else
    cmess <- gsub("\"", "'", cmess)

  withProgress(message = "Committing all changes locally", value = 0, style = "old", {
    system(paste("git -C", input$repo_directory, "add ."))
    mess <- suppressWarnings(system(paste0("git -C ", input$repo_directory, " commit -m \"", cmess, "\""), intern = TRUE))
    message("\nCommit attempt completed. Check the console for messages\n")
  })

  showModal(
    modalDialog(
      title = "Commit messages",
      span(HTML(paste0(mess, collapse = "</br>")))
    )
  )
  updateTextInput(session = session, "sync_commit_message", value = "")
})

## Show reset modal when button is clicked.
observeEvent(input$sync_undo_commit_show, {
  ## See https://shiny.rstudio.com/reference/shiny/latest/modalDialog.html
  commit_mess <- system(paste("git -C", input$repo_directory, "log -1 --pretty=%B"), intern = TRUE)
  req(input$repo_directory)
  showModal(
    modalDialog(title = "Undo latest local commit",
      span(suppressWarnings(paste0("\"", commit_mess[1], "\""))),
      br(), br(),
      span("Are you sure you want to undo the latest local commit? This will
           leave the latest changes un-staged (see Rstudio Git tab) so you can
           edit them or revert the changes"),
      footer = tagList(
        modalButton("Cancel"),
        actionButton(
          "sync_undo_commit", "Undo", class = "btn-danger", 
          title = "Undo the latest local commit\n\nGit command:\ngit reset HEAD~"
        )
      )
    )
  )
})

observeEvent(input$sync_undo_commit, {
  req(input$repo_directory)
  withProgress(message = "Undo the latest local commit", value = 0, style = "old", {
    system(paste("git -C", input$repo_directory, "reset HEAD~"))
    message("\nPrevious changes are now unstaged. Check the Git tab in Rstudio\n")
  })
  removeModal()
  updateTextInput(
    session = session,
    "sync_commit_message",
    value = system(paste("git -C", input$repo_directory, "log -1 --pretty=%B"), intern = TRUE)
  )
})

observeEvent(input$sync_pull, {
  req(input$repo_directory)
  withProgress(message = "Pull changes from remote", value = 0, style = "old", {
    mess <- suppressWarnings(system(paste("git -C", input$repo_directory, "pull"), intern = TRUE))
    message("\nPull attempt completed. Check the console for messages\n")
  })
  showModal(
    modalDialog(
      title = "Pull messages",
      span(HTML(paste0(mess, collapse = "</br>")))
    )
  )

})

observeEvent(input$sync_push, {
  req(input$repo_directory)
  withProgress(message = "Pushing changes to remote", value = 0, style = "old", {
    mess <- suppressWarnings(system(paste("git -C", input$repo_directory, "push"), intern = TRUE))
    message("\nPush attempt completed. Check the console for messages\n")
  })
  if (is_empty(mess)) mess <- "Push attempt completed"
  showModal(
    modalDialog(
      title = "Push messages",
      span(HTML(paste0(mess, collapse = "</br>")))
    )
  )
})

## Show reset modal when button is clicked.
observeEvent(input$sync_reset_show, {
  ## See https://shiny.rstudio.com/reference/shiny/latest/modalDialog.html
  showModal(
    modalDialog(title = "Reset local repo",
      span("Are you sure you want to reset the local repo? Only use this
           option if you want to destroy the current state of the local repo
           and make a copy of the remote repo!"),
      footer = tagList(
        modalButton("Cancel"),
        actionButton(
          "sync_reset", "Reset", class = "btn-danger", 
          title = "Completely reset local repo to remote master branch\n\nGit commands:\ngit --fetch all\ngit reset --hard origin/master"
        )
      )
    )
  )
})

observeEvent(input$sync_reset, {
  removeModal()
  req(input$repo_directory)
  withProgress(message = "Resetting local repo to remote master branch", value = 0, style = "old", {
    ## Add confirmation dialog
    mess1 <- system(paste("git -C", input$repo_directory, "fetch --all"), intern = TRUE)
    mess2 <- system(paste("git -C", input$repo_directory, "reset --hard origin/master"), intern = TRUE)
    message("\nReset attempt completed. Check the console for messages\n")
  })
  mess <- c(mess1, mess2)
  showModal(
    modalDialog(
      title = "Reset messages",
      span(HTML(paste0(mess, collapse = "</br>")))
    )
  )
})

observeEvent(input$sync, {
  req(input$repo_directory)
  if (!is_empty(input$sync_from)) {
    withProgress(message = "Syncing repo", value = 0, style = "old", {
      if (is_empty(upstream_info())) {
        mess1 <- system(paste("git -C", input$repo_directory, "remote add upstream", input$sync_from), intern = TRUE)
        mess2 <- system(paste("git -C", input$repo_directory, "fetch upstream"), intern = TRUE)
        mess <- c(mess1, mess2)
        showModal(
          modalDialog(
            title = "Reset messages",
            span(HTML(paste0(mess, collapse = "</br>")))
          )
        )
      }
    })
  }
})

observeEvent(input$sync_merge, {
  req(input$repo_directory)
  withProgress(message = "Merging synced repo", value = 0, style = "old", {
    mess1 <- system(paste("git -C", input$repo_directory, "checkout master"), intern = TRUE)
    mess2 <- system(paste("git -C", input$repo_directory, "merge upstream/master"), inern = TRUE)
  })
  mess <- c(mess1, mess2)
  showModal(
    modalDialog(
      title = "Merge messages",
      span(HTML(paste0(mess, collapse = "</br>")))
    )
  )
})

observeEvent(input$synch_abort, {
  req(input$repo_directory)
  mess <- system(paste("git -C", input$repo_directory, "merge --abort"), intern = TRUE)
  showModal(
    modalDialog(
      title = "Merge abort messages",
      span(HTML(paste0(mess, collapse = "</br>")))
    )
  )

})

observeEvent(input$sync_unlink, {
  req(input$repo_directory)
  mess <- system(paste("git -C", input$repo_directory, "remote remove upstream"), intern = TRUE)
  showModal(
    modalDialog(
      title = "Unlink messages",
      span(HTML(paste0(mess, collapse = "</br>")))
    )
  )
})

output$ui_sync_commit_message <- renderUI({
  br <- rbranches()
  if (length(br) == 0) {
    HTML(paste0("<label>", input$repo_directory, " is not a git repo</label></br>"))
  } else {
    textAreaInput(
      "sync_commit_message", "Commit message:", rows = 2, resize = "both", value = "", 
      placeholder = "Provide a commit message that describes the changes you made to the repo"
    )
  }
})

output$ui_sync_from <- renderUI({
  init <- upstream_info()
  textInput(
    "sync_from", "Sync repo with remote it was forked from:", 
    value = ifelse(length(init) == 0, "", init[1]), 
    placeholder = "Provide https or ssh link to original remote repo"
  )
})

output$sync_output <- renderPrint({
  cat("Overview of remotes:\n\n")
  cat(remote_info())
})
