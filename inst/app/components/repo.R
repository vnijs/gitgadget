shinyFiles::shinyDirChoose(input, "repo_directory_find", roots = gg_volumes)

output$ui_repo_directory <- renderUI({
  input$create
  input$remove_git
  if (!is_empty(projdir) && grepl(homedir, projdir)) {
    init <- projdir
  } else {
    init <- homedir
  }
  if (!is.integer(input$repo_directory_find)) {
    init <- shinyFiles::parseDirPath(gg_volumes, input$repo_directory_find)
  }
  textInput(
    "repo_directory", NULL,
    value = init,
    placeholder = "Repo directory"
  )
})

outputOptions(output, "ui_repo_directory", suspendWhenHidden = FALSE)

is_repo <- reactive({
  input$repo_refresh
  is_repo_fun()
})

output$repo_output <- renderPrint({
  req(input$repo_directory)
  input$repo_refresh
  if (is_repo()) {
    cat(paste(input$repo_directory, "is a git repo"))
  } else {
    cat(paste(input$repo_directory, "is not a git repo"))
  }
})