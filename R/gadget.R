## copied from https://github.com/rstudio/shiny
#' noRd
#' export
get_port <- function() {
  randomInt <- function(min, max) {
    min + sample(max - min, 1) - 1
  }

  while (TRUE) {
    port <- randomInt(3000, 8000)
    # Reject ports in this range that are considered unsafe by Chrome
    # http://superuser.com/questions/188058/which-ports-are-considered-unsafe-on-chrome
    # https://github.com/rstudio/shiny/issues/1784
    if (!port %in% c(3659, 4045, 6000, 6665:6669, 6697)) {
      break
    }
  }
  port
}

#' Launch gitgadget in Rstudio viewer if available
#'
#' @details See \url{https://github.com/vnijs/gitgadget} for documentation
#'
#' @param port Port to use for the app
#' @param launch.browser Launch app in viewer (browsers) or only show the URL
#'
#' @export
gitgadget <- function(port = get_port(), launch.browser = TRUE) {
  gitgadget_dir <- system.file(package = "gitgadget")
  source(file.path(gitgadget_dir, "app/init.R"), local = TRUE)
  source(file.path(gitgadget_dir, "app/gitgadget_ui.R"), local = TRUE)
  ui <- gitgadget_ui()
  server <- function(input, output, session) {
    source(file.path(gitgadget_dir, "app/components/legacy.R"), local = TRUE)
    source(file.path(gitgadget_dir, "app/components/help.R"), local = TRUE)
    source(file.path(gitgadget_dir, "app/components/input-validation.R"), local = TRUE)
    source(file.path(gitgadget_dir, "app/components/intro.R"), local = TRUE)
    source(file.path(gitgadget_dir, "app/components/create.R"), local = TRUE)
    source(file.path(gitgadget_dir, "app/components/clone.R"), local = TRUE)
    source(file.path(gitgadget_dir, "app/components/repo.R"), local = TRUE)
    source(file.path(gitgadget_dir, "app/components/branch.R"), local = TRUE)
    source(file.path(gitgadget_dir, "app/components/sync.R"), local = TRUE)
    source(file.path(gitgadget_dir, "app/components/collect.R"), local = TRUE)
    observeEvent(input$done, {
      stopApp(cat("Stopped GitGadget"))
    })
  }

  if (rstudioapi::isAvailable() && launch.browser) {
    runGadget(shinyApp(ui, server), port = port, viewer = shiny::paneViewer(minHeight = 725))
  } else {
    runApp(shinyApp(ui, server), port = port, launch.browser = launch.browser)
  }
}


#' Start gitgadget and show url to open the application in an external browser
#'
#' @details See \url{https://github.com/vnijs/gitgadget} for documentation
#'
#' @export
gitgadget_url <- function() {
  message("Click on the link below to open gitgadget\nin your default browser")
  gitgadget::gitgadget(launch.browser = FALSE)
}

#' Launch gitgadget in a separate process
#'
#' @details Using the \code{callr} package to launch gitgadget in a separate process so
#'   the console is not blocked. Rstudio viewer is used if available. See
#'   \url{https://github.com/vnijs/gitgadget} for documentation
#'
#' @importFrom callr r_bg
#'
#' @export
gitgadget_callr <- function() {
  port <- get_port()
  callr::r_bg(function(port) { gitgadget::gitgadget(port = port) }, args = list(port), user_profile = TRUE)
  Sys.sleep(1)
  getOption("viewer")(paste0("http://localhost:", port, "/"))
}
