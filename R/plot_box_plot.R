# library(dplyr)
library(ggplot2)
# library(prettycheck) # github.com/danielvartan/prettycheck
# library(tidyr)

source(here::here("R", "utils.R"))
source(here::here("R", "utils-plots.R"))
source(here::here("R", "utils-stats.R"))

plot_box_plot <- function(
    data,
    col,
    direction = 1,
    label = col,
    cord_flip = TRUE,
    jitter = FALSE,
    print = TRUE
  ) {
  col_class_options <- c(
    "numeric", "integer", "Duration", "Period", "difftime", "hms", "POSIXt"
  )

  prettycheck:::assert_tibble(data)
  prettycheck:::assert_character(col)
  prettycheck:::assert_subset(col, names(data))
  prettycheck:::assert_multi_class(data[[col]], col_class_options)
  prettycheck:::assert_choice(direction, c(-1, 1))
  prettycheck:::assert_flag(cord_flip)
  prettycheck:::assert_flag(jitter)
  prettycheck:::assert_character(label)
  prettycheck:::assert_flag(print)

  temporal <- prettycheck:::test_temporal(data[[col]])
  names(col) <- label

  data <-
    data |>
    dplyr::select(dplyr::all_of(col)) |>
    tidyr::pivot_longer(dplyr::all_of(col |> unname())) |>
    dplyr::mutate(
      name = factor(
        name,
        levels = col |> rev() |> names(),
        labels = col |> rev() |> names()
      )
    ) |>
    tidyr::drop_na(value)

  plot <-
    {
      if (length(col) == 1) {
        ggplot2::ggplot(
          data = data,
          ggplot2::aes(
            x = name,
            y = value
          )
        )
      } else {
        ggplot2::ggplot(
          data = data,
          ggplot2::aes(
            x = name,
            y = value,
            fill = name
          )
        )
      }
    } +
    ggplot2::geom_boxplot(
      outlier.colour = get_brand_color("red"),
      outlier.shape = 1,
      width = 0.75
    )

  if (isTRUE(cord_flip)) {
    plot <-
      plot +
      ggplot2::coord_flip() +
      ggplot2::theme(
        axis.title.y = ggplot2::element_blank(),
        axis.text.y = ggplot2::element_blank(),
        axis.ticks.y = ggplot2::element_blank()
      ) +
      ggplot2::labs(
        x = "Valor",
        y = "Variável",
        fill = NULL
      )

    if (isTRUE(temporal)) {
      plot <- plot + ggplot2::scale_x_continuous(labels = labels_hms)
    }
  } else {
    plot <-
      plot +
      ggplot2::theme(
        axis.title.x = ggplot2::element_blank(),
        axis.text.x = ggplot2::element_blank(),
        axis.ticks.x = ggplot2::element_blank()
      ) +
      ggplot2::labs(
        x = "Variable",
        y = "Value",
        fill = NULL
      )

    if (isTRUE(temporal)) {
      plot <- plot + ggplot2::scale_y_continuous(labels = labels_hms)
    }
  }

  if (isTRUE(jitter)) {
    plot <-
      plot +
      ggplot2::geom_jitter(
        width = 0.3,
        alpha = 0.1,
        color = get_brand_color("black"),
        size = 0.5
      )
  }

  if (!length(col) == 1) {
    plot <-
      plot +
      scale_fill_brand_d(
        direction = direction,
        breaks = names(col)
      )
  }

  if (isTRUE(print)) print(plot)

  invisible(plot)
}
