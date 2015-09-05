options(COFFICER.if_test = TRUE)
options(COFFICER.if_save_log = TRUE)
options(COFFICER.test_file_path = "./.pos_.RData")
# pos_ <- COFFICER::COfficerPosition$new(if_test = TRUE, if_shiny = TRUE)
pos_ <- NULL
establish_pos_ <- function(path) {
  f_msg("Build COfficerPosition entity...")
  if (missing(path)) {
    pos_ <<- COFFICER::COfficerPosition$new(if_shiny = TRUE)
  } else {
    stopifnot(typeof(path) == "character", length(path) == 1)
    path <- stringr::str_trim(path)
    load(path, envir = environment())
    pos_ <<- x
  }
  f_msg(sprintf("Done. The based date is %s.", pos_$based_date))
  return(invisible())
}
log_folder <- getOption("COFFICER.log_folder")
