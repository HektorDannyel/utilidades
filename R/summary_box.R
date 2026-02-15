#' @export
#' @importFrom htmltools div
summary_box <- function(string){

  string <- div(string, class = "summary-box")

  return(string)

}
