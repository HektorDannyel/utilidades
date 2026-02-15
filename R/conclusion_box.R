#' @export
#' @importFrom htmltools div
conclusion_box <- function(string){

  string <- div(string, class = "conclusion")

  return(string)

}
