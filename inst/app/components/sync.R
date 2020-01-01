
remote_info <- function() {
  input$sync; input$sync_unlink; input$branch_link; input$branch_unlink
  cat("Overview of remotes:\n\n")
  paste0(system(paste0("git remote -v"), intern = TRUE), collapse = "\n") %>%
    gsub("(\t)|(  ) ", " ", .) %>%
    cat()
}

assignment_name <- function() {
  assignment <- capture.output(remote_info())[-1:-2]
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
  system("git remote -v", intern = TRUE) %>%
    .[grepl("^upstream", .)] %>%
    gsub("^upstream\\s+", "", .) %>%
    gsub(" \\(fetch\\)$", "", .)
}

observeEvent(input$sync_commit, {
  cmess <- input$sync_commit_message
  if (is_empty(cmess))
    cmess <- paste0("Updates: ", Sys.time())
  else
    cmess <- gsub("\"", "'", cmess)
  withProgress(message = "Committing all changes locally", value = 0, style = "old", {
    system("git add .")
    system(paste0("git commit -m \"", cmess, "\""))
    message("\nCommit attempt completed. Check the console for messages\n")
  })
  updateTextInput(session = session, "sync_commit_message", value = "")
})

## Show reset modal when button is clicked.
observeEvent(input$sync_undo_commit_show, {
  ## See https://shiny.rstudio.com/reference/shiny/latest/modalDialog.html
  commit_mess <- system("git log -1 --pretty=%B", intern = TRUE)
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
  withProgress(message = "Undo the latest local commit", value = 0, style = "old", {
    system("git reset HEAD~")
    message("\nPrevious changes are now unstaged. Check the Git tab in Rstudio\n")
  })
  removeModal()
  updateTextInput(
    session = session,
    "sync_commit_message",
    value = system("git log -1 --pretty=%B", intern = TRUE)
  )
})

observeEvent(input$sync_pull, {
  withProgress(message = "Pull changes from remote", value = 0, style = "old", {
    system("git pull")
    message("\nPull attempt completed. Check the console for messages\n")
  })
})

observeEvent(input$sync_push, {
  withProgress(message = "Pushing changes to remote", value = 0, style = "old", {
    system("git push")
    message("\nPush attempt completed. Check the console for messages\n")
  })
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
  withProgress(message = "Resetting local repo to remote master branch", value = 0, style = "old", {
    ## Add confirmation dialog
    system("git fetch --all")
    system("git reset --hard origin/master")
    message("\nReset attempt completed. Check the console for messages\n")
  })
})

observeEvent(input$sync, {
  if (!is_empty(input$sync_from)) {
    withProgress(message = "Syncing repo", value = 0, style = "old", {
      if (is_empty(upstream_info()))
        system(paste("git remote add upstream", input$sync_from))
      system("git fetch upstream")
    })
  }
})

observeEvent(input$sync_merge, {
  withProgress(message = "Merging synced repo", value = 0, style = "old", {
    system("git checkout master")
    system("git merge upstream/master")
  })
})

observeEvent(input$synch_abort, {
  system("git merge --abort")
})

observeEvent(input$sync_unlink, {
  system("git remote remove upstream")
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
  remote_info()
})
