# S3 class for tbl_df_diagnoser

#' Create a object of tbl_df_diagnoser, subclass of tibble
#' @importFrom tibble is_tibble
#' @param x tibble object
#' @param ks_test result of ks.test
#' @param levene_test result of leveneTest
#' @param id_influential row ID at which Cook's distance > 1
new_tibble_diagnoser <- function(x, ks_test = NULL,
                                 levene_test = NULL,
                                 id_influential = NULL) {
  stopifnot(is_tibble(x))
  class(x) <- append("tbl_df_diagnoser", class(x))
  attr(x, "ks_test") <- ks_test
  attr(x, "levene_test") <- levene_test
  attr(x, "id_influential") <- id_influential
  return(x)
}


#' Print of tbl_df_diagnoser
#' @inheritParams tibble::print.tbl_df
print.tbl_df_diagnoser <- function(x, width = NULL, ..., n = NULL,
                                   max_extra_cols = NULL,
                                   max_footer_lines = NULL) {
  NextMethod()
}