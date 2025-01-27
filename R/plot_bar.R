# library(dplyr)
# library(ggplot2)
# library(prettycheck) # github.com/danielvartan/prettycheck
library(rlang)
# library(tidyr)

source(here::here("R", "utils-checks.R"))

plot_bar <- function(
    data,
    col,
    na_rm = TRUE,
    x_label = "Frequency",
    y_label = col,
    print = TRUE
  ) {
  col_class_options <- c("character", "factor")

  prettycheck:::assert_tibble(data)
  prettycheck:::assert_string(col)
  prettycheck:::assert_choice(col, names(data))
  prettycheck:::assert_multi_class(data[[col]], col_class_options)
  prettycheck:::assert_flag(na_rm)
  assert_gg_label(x_label)
  assert_gg_label(y_label)
  prettycheck:::assert_flag(print)

  data <-
    data |>
    dplyr::select(dplyr::all_of(col)) |>
    dplyr::mutate(!!as.symbol(col) := as.factor(!!as.symbol(col)))

  levels <- data |> dplyr::pull(col) |> levels()

  if (isTRUE(na_rm)) data <- data |> tidyr::drop_na()

  plot <-
    data |>
    ggplot2::ggplot(
      ggplot2::aes(y = !!as.symbol(col))
    ) +
    ggplot2::geom_bar() +
    ggplot2::labs(
      x = x_label,
      y = y_label
    ) +
    scale_y_discrete(limits = rev(levels)) +
    ggplot2::theme(legend.position = "none")

  if (isTRUE(print)) print(plot)

  invisible(plot)
}
