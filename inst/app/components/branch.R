branches <- reactive({
  input$branch_delete
  input$branch_create
  input$branch_checkout
  req(input$repo_directory)
  br <- suppressWarnings(system(paste("git -C", input$repo_directory, "branch -a"), intern = TRUE))
  brs <- attr(br, "status")
  ## need both conditions because output on windows and mac differs
  if (length(br) == 0 || (!is.null(brs) && brs == 128)) {
    c()
  } else {
    br %>% gsub("[\\* ]+", "", .) %>%
    {.[!grepl("(^master$)|(^remotes/origin/master$)|(^remotes/origin/HEAD)", .)]}
  }
})

observeEvent(input$branch_create, {
  req(input$repo_directory)
  if (input$branch_create_name != "") {
    withProgress(message = "Creating branch", value = 0, style = "old", {
      suppressWarnings(system(paste("git -C", input$repo_directory, "checkout -b", input$branch_create_name)))
    })
  }
})

observeEvent(input$branch_create_from_mr, {
  req(input$repo_directory)
  remote_fetch <- suppressWarnings(system(paste("git -C", input$repo_directory, "config --get-all remote.origin.fetch"), intern = TRUE))
  if (!"+refs/merge-requests/*/head:refs/remotes/origin/merge-requests/*" %in% remote_fetch) {
    system(paste("git -C", dir, "config --add remote.origin.fetch +refs/merge-requests/*/head:refs/remotes/origin/merge-requests/*"))
  }
})

observeEvent(input$branch_merge, {
  from <- input$branch_merge_from
  into <- input$branch_merge_into
  req(input$repo_directory)
  if (!is.null(from) || !is.null(into)) {
    withProgress(message = "Merging branch", value = 0, style = "old", {
      suppressWarnings(system(paste("git -C", input$repo_directory, "checkout", into)))
      suppressWarnings(system(paste("git -C", input$repo_directory, "merge", from)))
    })
  }
})

observeEvent(input$branch_abort, {
  req(input$repo_directory)
  system(paste("git -C", input$repo_directory, "merge --abort"))
})

observeEvent(input$branch_link, {
  req(input$repo_directory)
  if (input$branch_create_name != "") {
    ## would prefer to do this without 'push' -- however then I can't unlink for some reason
    paste("git -C", input$repo_directory, "push --set-upstream origin", input$branch_create_name) %>%
      system(.)
  }
})

observeEvent(input$branch_unlink, {
  req(input$repo_directory)
  branch <- input$branch_delete_name
  if (is_empty(branch)) input$branch_create_name
  if (!is_empty(branch)) {
    for (ib in branch) {
      system(paste0("git -C ", input$repo_directory, " branch -d -r origin/", ib))
    }
  }
})

observeEvent(input$branch_delete, {
  req(input$repo_directory)
  if (!is.null(input$branch_delete_name)) {
    withProgress(message = "Deleting branch", value = 0, style = "old", {
      system(paste("git -C", input$repo_directory, "checkout master"))
      for (ib in input$branch_delete_name) {
        system(paste("git -C", input$repo_directory, "branch -D", ib))
      }
    })
  }
})

output$ui_branch_create_name <- renderUI({
  br <- rbranches()
  if (length(br) == 0) {
    HTML(paste0("<label>", input$repo_directory, " is not a git repo</label></br>"))
  } else {
    init <- isolate(input$branch_create_name)
    init <- ifelse(is_empty(init), "", init) 
    textInput("branch_create_name", NULL, value = init, placeholder = "Provide a name for the new branch")
  }
})

output$ui_branch_merge_branches <- renderUI({
  br <- c("master", branches()) %>% .[!grepl("origin/", .)]
  if (length(br) == 1) {
    HTML("<label>No branches available to merge</label>")
  } else {
    tagList(
      fillRow(height = "70px", width = "300px",
        selectInput("branch_merge_from", "From:", choices = br, selected = br[2]),
        selectInput("branch_merge_into", "Into:", choices = br, selected = br[1])
      )
    )
  }
})

rbranches <- reactive({
  input$sync; input$sync_unlink; input$branch_link; input$branch_unlink
  input$branch_create; input$branch_checkout; input$branch_delete;
  input$branch_merge; input$collect_fetch; input$collect
  req(input$repo_directory)

  br <- suppressWarnings(system(paste("git -C", input$repo_directory, "branch --all"), intern = TRUE))
  brs <- attr(br, "status")
  ## need both conditions because output on windows and mac differs
  if (length(br) == 0 || (!is.null(brs) && brs == 128)) {
    c()
  } else {
    br %>% {unique(c(.[grepl("\\* ", .)], .))} %>%
    gsub("[\\* ]+", "", .) %>%
    {.[!grepl("(^remotes/origin/master$)|(^remotes/origin/HEAD)", .)]}
  }
})

output$ui_branch_checkout_name <- renderUI({
  input$branch_create
  br <- rbranches()
  if (length(br) == 0) {
    HTML("<label>No branches available</label>")
  } else {
    selectInput("branch_checkout_name", NULL, choices = br)
  }
})

observeEvent(input$branch_checkout, {
  req(input$repo_directory)
  if (!is.null(input$branch_checkout_name)) {
    ## based on solution #1 http://stackoverflow.com/a/29828320/1974918
    withProgress(message = "Checkout branch", value = 0, style = "old", {
      suppressWarnings(system(paste0("git -C ", input$repo_directory, " checkout ", sub("remotes/origin/", "", input$branch_checkout_name))))
    })
  }
})

output$ui_branch_delete_name <- renderUI({
  resp <- branches() %>% .[!grepl("^remotes/origin", .)]
  if (length(resp) == 0) {
    HTML("<label>No branches available to delete</label>")
  } else {
    selectizeInput("branch_delete_name",
      label = NULL,
      selected = resp[1],
      choices = resp,
      multiple = TRUE,
      options = list(placeholder = "Select branch(es) to delete", plugins = list("remove_button"))
    )
  }
})
