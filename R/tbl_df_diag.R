# S3 class for tbl_df_diag

#' Create a object of tbl_df_diag, subclass of tibble
#' @importFrom tibble is_tibble
#' @param x tibble object (level = 1)
#' @param xg tibble object of higher levels
#' @param model model object
new_tibble_diag <- function(x, xg, model) {
  stopifnot(is_tibble(x))
  class(x) <- append("tbl_df_diag", class(x))
  attr(x, "levels") <- xg
  attr(x, "model") <- model
  return(x)
}


#' Print of tbl_df_diag
#' @inheritParams tibble::print.tbl_df
print.tbl_df_diag <- function(x, width = NULL, ..., n = NULL,
                              max_extra_cols = NULL,
                              max_footer_lines = NULL) {
  NextMethod()
}