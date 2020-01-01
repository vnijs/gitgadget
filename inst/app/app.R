library(shiny)
library(gitgadget)
library(dplyr)
library(miniUI)

is_not <- function(x) length(x) == 0 || (length(x) == 1 && is.na(x))
is_empty <- function(x, empty = "\\s*") {
  is_not(x) || (length(x) == 1 && grepl(paste0("^", empty, "$"), x))
}
pressed <- function(x) !is.null(x) && (is.list(x) || x > 0)
not_pressed <- function(x) !pressed(x)

source("init.R", local = TRUE)
ui <- gitgadget:::gitgadget_ui()
# source("../../R/gitgadget_ui.R"); ui <- gitgadget_ui()
server <- function(input, output, session) {
  source("components/legacy.R", local = TRUE)
  source("components/help.R", local = TRUE)
  source("components/input-validation.R", local = TRUE)
  source("components/intro.R", local = TRUE)
  source("components/create.R", local = TRUE)
  source("components/clone.R", local = TRUE)
  source("components/branch.R", local = TRUE)
  source("components/sync.R", local = TRUE)
  source("components/collect.R", local = TRUE)
  observeEvent(input$done, {
    stopApp(cat("Stopped GitGadget"))
  })
}

# options(gitgadget.launch_dir = "~/git/testing")
# setwd("~/git/testing")
shinyApp(ui = ui, server = server)
