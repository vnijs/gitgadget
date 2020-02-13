remote_info <- reactive({
  input$sync; input$sync_unlink; input$branch_link; input$branch_unlink
  req(input$repo_directory)
  suppressWarnings(paste0(system(paste("git -C", input$repo_directory, "remote -v"), intern = TRUE), collapse = "\n")) %>%
    gsub("(\t)|(  ) ", " ", .)
})


upstream_info <- function() {
  input$sync; input$sync_unlink
  req(input$repo_directory)
  suppressWarnings(system(paste("git -C", input$repo_directory, "remote -v"), intern = TRUE)) %>%
    .[grepl("^upstream", .)] %>%
    gsub("^upstream\\s+", "", .) %>%
    gsub(" \\(fetch\\)$", "", .)
}

observeEvent(input$sync_stage, {
  req(input$repo_directory)
  withProgress(message = "Staging all files", value = 0, style = "old", {
    system(paste("git -C", input$repo_directory, "add ."))
    mess <- suppressWarnings(system(paste0("git -C ", input$repo_directory, " diff --staged"), intern = TRUE))
  })

  mess <- color_diff_html(mess)
  if (is_empty(mess)) mess <- "No changes to commit"

  showModal(
    modalDialog(
      title = "Staged file differences",
      span(HTML(paste0(mess, collapse = "</br>"))),
      size = "l",
      easyClose = TRUE
    )
  )
})

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

output$ui_sync_check <- renderUI({
  req(input$sync_check > 0)
  assn <- assignment_name(github = TRUE, url = TRUE)
  hash <- suppressWarnings(system(paste("git -C", input$repo_directory, "rev-parse --short HEAD"), intern = TRUE))
  if (!is_empty(hash) && length(hash) == 1) {
    assn <- paste0(assn, "/commit/", hash)
  }
  tags$script(paste0("window.open('", assn, "', '_blank')"))
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
    mess2 <- system(paste("git -C", input$repo_directory, "merge upstream/master"), intern = TRUE)
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

color_diff_html <- function(mess) {
  # adapted from https://gist.github.com/stopyoukid/5888146
  html <- "<html> <head> <meta charset=\"utf-8\"> <style> .modal-lg .file-diff>div { width: 100%; } .modal-lg .modal-body { overflow: auto; } .modal-lg pre::-webkit-scrollbar { /* hide scrollbar on chrome, safari, and edge */ display: none; } .modal-lg pre { /* hide scrollbar on firefox */ scrollbar-width: none; } .modal-lg pre { padding: 0; margin: 0; margin-left: 5px; font-size: 12px; text-align: left; border: 0; border-radius: 0; background-color: rgb(255, 255, 255); line-height: 1.75em; } .modal-lg .file { margin-bottom: 1em; border: 1px; } .modal-lg .delete { background-color: #fdd; } .modal-lg .insert { background-color: #dfd; } .modal-lg .info { background-color: #888 } .modal-lg .context { color: #aaa; } </style> </head> <body>"
  # html <- paste0(readLines("~/Desktop/format.html"), collapse = "\n")
  first <- 1
  diffseen <- lastonly <- 0
  currSection <- currFile <- ""

  addDiffToPage <- function(file, section, html) {
    paste0(html, "\n<h2>", file, "</h2>\n") %>%
      paste0("<div class=\"file-diff\">\n") %>%
      paste0(section) %>%
      paste0("\n</div>")
  }

  for (s in mess) {
    # Get beginning of line to determine what type of diff line it is.
    t1 <- substring(s, 0, 1)
    t2 <- substring(s, 0, 2)
    t3 <- substring(s, 0, 3)
    t4 <- substring(s, 0, 4)
    t7 <- substring(s, 0, 7)
    cls <- ""

    # Determine HTML class to use.
    if (t7 == "Only in") {
      cls <- "only"
      if (diffseen == 0) {
        diffseen <- 1
      } else {
        if (lastonly == 0) {
          html <- addDiffToPage(currFile, currSection, html)
        }
      }
      if (lastonly == 0) currSection <- ""
        lastonly <- 1
    } else if (t4 == "diff") {
      cls <- "file"
      s <- sub("^.* b/", ">>> ", s)

      if (diffseen == 1)
        html <- addDiffToPage(currFile, currSection, html)
      diffseen <- 1
      currSection <- ""
      lastonly <- 0
    } else if (t3 == '+++') {
      lastonly <- 0
      next
    } else if (t3 == "---") {
      lastonly <- 0
      next
    } else if (t2 == "@@") {
      cls <- "info"
      lastonly <- 0
    } else if (t1 == "+") {
      s <- paste0(" ", substring(s, 2))
      cls <- "insert"
      lastonly <- 0
    } else if (t1 == "-") {
      s <- paste0(" ", substring(s, 2))
      cls <- "delete"
      lastonly <- 0
    } else {
      cls <- "context"
      lastonly <- 0
    }

    # Convert &, <, > to HTML entities.
    s <- gsub("\\&", "\\&amp;", s) %>%
      gsub("<", "\\&lt;", .) %>%
      gsub(">", "\\&gt;", .)

    if (first == 1) first <- 0

    # Output the line.
    if (cls != "") {
      currSection <- paste0(currSection, "\n<pre class=\"", cls, "\">", s, "</pre>")
    } else {
      currSection <- paste0(currSection, "\n<pre>", s, "</pre>")
    }
  }

  if (currSection != "") {
    html <- addDiffToPage(currFile, currSection, html)
  }

  paste0(html, "\n</body></html>")
}
