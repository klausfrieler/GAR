#' supported test
#'
#' Lists the available questionnairs.
#'
#' @export
get_questionnaires <- function(){
  tibble(id = c("GEN","EMO1", "GRV", "ATT"), max_items = c(1, 6, 6, 10))
}
