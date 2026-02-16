#' @export
#' @import kableExtra
custom_kable <- function(table, bg_col = 1, test_col = NULL, alpha = 0.05){

  colors <- c("#2F5597", "#C96A12", "#1B5E20", "#CC4125", "#483F63", "#B7950B",
              "#45818E", "#E69138", "#3FA34D", "#CC7876", "#8E7CC3", "#C9B458")

  escape <- TRUE

  if(!is.null(test_col)){

    table <- table |>
      mutate_at(vars(test_col), ~cell_spec(round(., 4), color = ifelse(. < alpha, "#000000", "#45818E")))

    escape <- FALSE

  }

  table <- table |>
    kbl(align = "c", escape = escape) |>
    kable_styling(full_width = FALSE, bootstrap_options = c("striped", "hover", "condensed", "responsive")) |>
    row_spec(0, background = colors[bg_col], color = "#FFFFFF")

  return(table)

}
