debug_msg <- function(msg, origin = "global.R") {
  timestamp <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")
  message(sprintf("[%s] [%s] %s", timestamp, origin, msg))
}