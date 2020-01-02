shinyFiles::shinyDirChoose(input, "branch_directory_find", roots = gg_volumes)

output$ui_branch_directory <- renderUI({
  init <- projdir
  if (!is.integer(input$branch_directory_find)) {
    init <- shinyFiles::parseDirPath(gg_volumes, input$branch_directory_find)
  }
  textInput(
    "branch_directory", NULL, value = init, 
    placeholder = "Repo directory"
  )
})

branches <- reactive({
  input$branch_delete
  input$branch_create
  input$branch_checkout
  # dir <- getOption("gitgadget.launch_dir", ".")
  # br <- system(paste0("git -C ", dir, " branch -a"), intern = TRUE)
  br <- system(paste0("git branch -a"), intern = TRUE)
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
  if (input$branch_create_name != "") {
    withProgress(message = "Creating branch", value = 0, style = "old", {
      paste("git checkout -b", input$branch_create_name) %>%
        system(.)
    })
  }
})

observeEvent(input$branch_create_from_mr, {
  remote_fetch <- system("git config --get-all remote.origin.fetch", intern = TRUE)
  if (!"+refs/merge-requests/*/head:refs/remotes/origin/merge-requests/*" %in% remote_fetch) {
    system("git config --add remote.origin.fetch +refs/merge-requests/*/head:refs/remotes/origin/merge-requests/*")
  }
})

observeEvent(input$branch_merge, {
  from <- input$branch_merge_from
  into <- input$branch_merge_into
  if (!is.null(from) || !is.null(into)) {
    withProgress(message = "Merging branch", value = 0, style = "old", {
      system(paste("git checkout", into))
      system(paste("git merge", from))
    })
  }
})

observeEvent(input$branch_abort, {
  system("git merge --abort")
})

observeEvent(input$branch_link, {
  if (input$branch_create_name != "") {
    ## would prefer to do this without 'push' -- however then I can't unlink for some reason
    paste("git push --set-upstream origin", input$branch_create_name) %>%
      system(.)
  }
})

observeEvent(input$branch_unlink, {
  branch <- input$branch_delete_name
  if (is_empty(branch)) input$branch_create_name
  if (!is_empty(branch)) {
    for (ib in branch) {
      system(paste0("git branch -d -r origin/", ib))
    }
  }
})

observeEvent(input$branch_delete, {
  if (!is.null(input$branch_delete_name)) {
    withProgress(message = "Deleting branch", value = 0, style = "old", {
      system("git checkout master")
      for (ib in input$branch_delete_name) {
        system(paste("git branch -D", ib))
      }
    })
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

  br <- system("git branch --all", intern = TRUE)
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
    invisible()
  } else {
    selectInput("branch_checkout_name", NULL, choices = br)
  }
})

observeEvent(input$branch_checkout, {
  if (!is.null(input$branch_checkout_name)) {
    ## based on solution #1 http://stackoverflow.com/a/29828320/1974918
    withProgress(message = "Checkout branch", value = 0, style = "old", {
      system(paste0("git checkout ", sub("remotes/origin/", "", input$branch_checkout_name)))
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
