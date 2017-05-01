
logEnv <- new.env()

assign("current_log_file", NULL, envir=logEnv)
assign("indents", c(), envir=logEnv)

#' Setting, appending, and clearing log files
#'
#' @describeIn set_log_file Set the log file name
#' @export
set_log_file <- function(filename) {
  assign("current_log_file", filename, envir=logEnv)
}

#' @describeIn set_log_file Start new log file; overwrite if exists
#' @export
start_log_file <- function(filename) {
  set_log_file(filename)
  close(file(filename, open="w"))
}

#' @describeIn set_log_file Start new log file; append if exists
#' @export
append_log_file <- function(file) {
  set_log_file(file)
}

#' @describeIn set_log_file Delete contents of current log file
#' @export
clear_log_file <- function() {
  set_log_file(NULL)
}

#' Indenting text
#'
#' @describeIn indent_log Add to existing indent
#' @export
indent_log <- function(indent="   ") {
  assign("indents", c(get("indents", envir=logEnv), indent), envir=logEnv)
}

#' @describeIn indent_log Remove last indent
#' @export
unindent_log <- function(n=1) {
  indents <- get("indents", envir=logEnv)
  if (length(indents) - n < 1) {
    assign("indents", c(), envir=logEnv)
  } else {
    assign("indents", indents[1:(length(indents)-n)], envir=logEnv)
  }
}

#' @describeIn indent_log Remove all indents
#' @export
unindent_all_log <- function() {
  unindent_log(1e9)
}

#' Print to screen and log file
#'
#' @describeIn say Print a single string
#' @export
say <- function(string, tofile=T) {
  indents <- get("indents", envir=logEnv)
  current_log_file <- get("current_log_file", envir=logEnv)
  string <- paste0(paste(indents, collapse=""), string, "\n")
  cat(string)
  if (tofile && !is.null(current_log_file)) {
    cat(string, file=current_log_file, append=T)
  }
}

#' @describeIn say Print multiple strings with no separation
#' @export
say0 <- function(...) {
  say(paste0(...))
}

#' @describeIn say Formatted printing to screen and logfile
#' @export
sayf <- function(fmt, ...) {
  say(sprintf(fmt, ...))
}

#' @describeIn say Print strings only to screen
#' @export
say_debug <- function(...) {
  say(..., tofile=F)
}


