# to avoid 'no visible binding for global variable' NOTE
globalVariables(c(".", "un", "directory", "base64_enc", "server", "team", "gitgadget_ui", "status", "name"))

#' gitgadget
#'
#' @name gitgadget
#' @import shiny miniUI curl dplyr
#' @importFrom jsonlite fromJSON
#' @importFrom rstudioapi isAvailable getActiveProject openProject restartSession
#' @importFrom markdown markdownToHTML
#' @importFrom utils read.csv capture.output
#' @importFrom stats na.omit setNames
#' @importFrom utils packageVersion browseURL head
#' @importFrom usethis edit_r_environ edit_r_profile use_git use_github
#' @importFrom shinyFiles shinyDirButton shinyFilesButton parseDirPath
#'   parseFilePaths shinyFileChoose shinyDirChoose
"_PACKAGE"
