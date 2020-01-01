observeEvent(grep("([\\]+)|([/]{2,})", input$intro_git_home), {
  if (!is_empty(input$intro_git_home))
    updateTextInput(session = session, "intro_git_home", value = gsub("([\\]+)|([/]{2,})", "/", input$intro_git_home))
})

observeEvent(grep("([\\]+)|([/]{2,})", input$create_directory), {
  updateTextInput(session = session, "create_directory", value = gsub("([\\]+)|([/]{2,})", "/", input$create_directory))
})

observeEvent(grep("([\\]+)|([/]{2,})", input$create_user_file), {
  updateTextInput(session = session, "create_user_file", value = gsub("([\\]+)|([/]{2,})", "/", input$create_user_file))
})

observeEvent(grep("([\\]+)|([/]{2,})", input$clone_into), {
  updateTextInput(session = session, "clone_into", value = gsub("([\\]+)|([/]{2,})", "/", input$clone_into))
})

observeEvent(grep("([\\]+)|([/]{2,})", input$clone_to), {
  updateTextInput(session = session, "clone_to", value = gsub("([\\]+)|([/]{2,})", "/", input$clone_to))
})

observeEvent(grep("([\\]+)|([/]{2,})", input$collect_user_file), {
  updateTextInput(session = session, "collect_user_file", value = gsub("([\\]+)|([/]{2,})", "/", input$collect_user_file))
})
