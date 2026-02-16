#' @export
#' @import dplyr
#' @import stringr
custom_palette <- function(geom){

  colors <- c("#2F5597", "#C96A12", "#1B5E20", "#CC4125", "#483F63", "#B7950B",
              "#45818E", "#E69138", "#3FA34D", "#CC7876", "#8E7CC3", "#C9B458")

  colour <- as_label(geom@mapping$colour)
  fill <- as_label(geom@mapping$fill)

  geom_data <- geom@data

  if(!colour == "NULL"){

    colour_levels <- geom_data |>
      select(all_of(colour)) |>
      distinct() |>
      pull()

    colour_string <- paste0(
      "scale_colour_manual(values = c(", str_remove(paste0("'", colour_levels, "' = '", colors[1:length(colour_levels)], "', ", collapse = ""), ",\\s$"), "))"
    )

    eval(parse(text = paste0(
      "geom <- geom + ",
      colour_string
    )))

  }

  if(!fill == "NULL"){

    fill_levels <- geom_data |>
      select(all_of(fill)) |>
      distinct() |>
      pull()

    fill_string <- paste0(
      "scale_fill_manual(values = c(", str_remove(paste0("'", fill_levels, "' = '", colors[1:length(fill_levels)], "', ", collapse = ""), ",\\s$"), "))"
      )

    eval(parse(text = paste0(
      "geom <- geom + ",
      fill_string
    )))

  }

  if(fill == "NULL" & colour == "NULL"){

    layers <- geom@layers

    for(i in 1:length(layers)){

      detect_geom <- as.character(geom@layers[[i]]$constructor)

      if(any(detect_geom == "annotate")) next

      if(any(str_detect(detect_geom, "aes"))){

        aes_geom <- detect_geom[2]
        detect_geom <- detect_geom[1]

        if(str_detect(detect_geom, "bar|histogram|map")){

          geom@layers[[i]]$aes_params <- list(fill = colors[i])

        } else {

          geom@layers[[i]]$aes_params <- list(colour = colors[i])

        }

      } else {

        detect_geom <- detect_geom[str_detect(detect_geom, "geom")]

        if(str_detect(detect_geom, "bar|histogram|map")){

          geom@layers[[i]]$aes_params <- list(fill = colors[i])

        } else {

          geom@layers[[i]]$aes_params <- list(colour = colors[i])

        }

      }

    }

  }

  return(geom)

}
