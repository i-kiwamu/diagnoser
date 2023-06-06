# S3 class for tbl_df_diag

#' Create a object of tbl_df_diag, subclass of tibble
#' @importFrom tibble is_tibble
#' @param x tibble object
#' @param ks_test result of ks.test
#' @param levene_test result of leveneTest
new_tibble_diag <- function(x, ks_test = NULL, levene_test = NULL) {
  stopifnot(is_tibble(x))
  class(x) <- append("tbl_df_diag", class(x))
  attr(x, "ks_test") <- ks_test
  attr(x, "levene_test") <- levene_test
  return(x)
}


#' Print of tbl_df_diag
#' @inheritParams tibble::print.tbl_df
print.tbl_df_diag <- function(x, width = NULL, ..., n = NULL,
                              max_extra_cols = NULL,
                              max_footer_lines = NULL) {
  NextMethod()
}