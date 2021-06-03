#' print the last line, again, for use with keyboard shortcut
#'
#' @return none. a print method is used instead
#' @export

printLast <- function() {
  print(.Last.value)
}
