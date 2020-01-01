observeEvent(input$help, {
    viewer <- getOption("viewer", default = browseURL)
    viewer("https://github.com/vnijs/gitgadget")
})

## Show remove_git modal when button is clicked.
observeEvent(input$help, {
    ## See https://shiny.rstudio.com/reference/shiny/latest/modalDialog.html
    showModal(
        modalDialog(title = "GitGadget Help",
            markdown::markdownToHTML(
                file.path(system.file(package = "gitgadget"), "app/help/help.md"),
                fragment.only = TRUE,
                options = "",
                stylesheet = ""
            ) %>%
                gsub("<table>", "<table class='table table-condensed table-hover'>", .) %>%
                HTML,
            footer = tagList(modalButton("OK")),
            easyClose = TRUE
        )
    )
})
