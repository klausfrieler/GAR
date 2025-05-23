#' supported test
#'
#' Lists the available questionnaires.
#'
#' @export
get_questionnaires <- function(){
  tibble(id = c("GEN","EMO1", "GRV", "MAS", "AAT"), max_items = c(6, 6, 6, 18, 6))
}
