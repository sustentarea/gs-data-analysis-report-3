# library(dplyr)
# library(ggplot2)
# library(prettycheck) # github.com/danielvartan/prettycheck
library(rlang)
# library(tidyr)

source(here::here("R", "utils-checks.R"))
source(here::here("R", "utils-plots.R"))

plot_hist <- function(
    data,
    col,
    bins = 30,
    stat = "density",
    density_line = TRUE,
    na_rm = TRUE,
    x_label = "Value",
    y_label = ifelse(stat == "count", "Frequency", "Density"),
    print = TRUE
  ) {
  col_class_options <- c(
    "numeric", "integer", "Duration", "Period", "difftime", "hms", "POSIXt"
  )

  prettycheck:::assert_tibble(data)
  prettycheck:::assert_string(col)
  prettycheck:::assert_choice(col, names(data))
  prettycheck:::assert_multi_class(data[[col]], col_class_options)
  prettycheck:::assert_number(bins, lower = 1)
  prettycheck:::assert_choice(stat, c("count", "density"))
  prettycheck:::assert_flag(density_line)
  prettycheck:::assert_flag(na_rm)
  assert_gg_label(x_label)
  assert_gg_label(y_label)
  prettycheck:::assert_flag(print)

  data <- data |> dplyr::select(dplyr::all_of(col))

  if (isTRUE(na_rm)) data <- data |> tidyr::drop_na()

  plot <-
    data |>
    ggplot2::ggplot(
      ggplot2::aes(x = !!as.symbol(col))
    ) +
    ggplot2::geom_histogram(
      ggplot2::aes(y = ggplot2::after_stat(!!as.symbol(stat))),
      bins = 30,
      color = "white"
    ) +
    ggplot2::labs(
      x = x_label,
      y = y_label
    ) +
    ggplot2::theme(legend.position = "none")

  if (prettycheck:::test_temporal(data[[col]])) {
    plot <- plot + ggplot2::scale_x_continuous(labels = labels_hms)
  }

  if (stat == "density" && isTRUE(density_line)) {
    plot <- plot + ggplot2::geom_density(
      color = get_brand_color("red"),
      linewidth = 1
    )
  }

  if (isTRUE(print)) print(plot)

  invisible(plot)
}
