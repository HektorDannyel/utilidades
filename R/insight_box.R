#' @export
#' @importFrom htmltools div
insight_box <- function(string){

  string <- div(string, class = "insight")

  return(string)

}
