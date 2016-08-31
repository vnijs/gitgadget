# to avoid 'no visible binding for global variable' NOTE
globalVariables(c(".", "un", "directory", "base64_enc", "server"))

#' gitgadget
#'
#' @name gitgadget
#' @docType package
#' @import shiny miniUI curl dplyr
#' @importFrom jsonlite fromJSON
#' @importFrom rstudioapi isAvailable getActiveProject
#' @importFrom utils read.csv
#' @importFrom stats na.omit
#' @importFrom methods is
#' @importFrom utils packageVersion browseURL
NULL
