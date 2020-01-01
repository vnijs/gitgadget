if (!is.null(getOption("git.user"))) {
    showModal(
        modalDialog(title = "Move git settings to .Renviron",
            span("All git related settings should be moved from .Rprofile to .Renviron. Click on the
      \"edit .R files\" button to open both .Rprofile and .Renviron. Move all lines that
      contain 'git.' out of .Rprofile and to the .Renviron file. Then remove 'options' from the
      new lines in .Renviron. For example: \"options(git.user = 'abc123')\" in .Rprofile should
      be \"git.user = 'abc123'\" in .Renviron."),
            br(), br(),
            span("If you run into any problems, please
      post an issue to"),
            HTML("<a href='https://github.com/vnijs/gitgadget/issues' target='_blank'>https://github.com/vnijs/gitgadget/issues</a>"),
            footer = tagList(
                modalButton("Cancel"),
                actionButton("edit_r_files", "Edit .R files")
            )
        )
    )
}

observeEvent(input$edit_r_files, {
    usethis::edit_r_environ()
    usethis::edit_r_profile()
})