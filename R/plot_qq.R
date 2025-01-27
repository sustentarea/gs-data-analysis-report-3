# library(dplyr)
library(ggplot2)
# library(prettycheck) # github.com/danielvartan/prettycheck
library(rlang)
# library(tidyr)

source(here::here("R", "utils-plots.R"))

plot_qq <- function(data, col, na_rm = TRUE, print = TRUE) {
  col_class_options <- c(
    "numeric", "integer", "Duration", "Period", "difftime", "hms", "POSIXt"
  )

  prettycheck:::assert_tibble(data)
  prettycheck:::assert_string(col)
  prettycheck:::assert_subset(col, names(data))
  prettycheck:::assert_multi_class(data[[col]], col_class_options)
  prettycheck:::assert_flag(na_rm)
  prettycheck:::assert_flag(print)

  data <- data |> dplyr::select(dplyr::all_of(col))
  if (isTRUE(na_rm)) data <- data |> tidyr::drop_na()

  plot <-
    data |>
    ggplot2::ggplot(ggplot2::aes(sample = !!as.symbol(col))) +
    ggplot2::stat_qq() +
    ggplot2::stat_qq_line(
      color = get_brand_color("red"),
      linewidth = 1
    ) +
    ggplot2::labs(
      x = "Theoretical quantiles (Std. normal)",
      y = "Sample quantiles"
    ) +
    ggplot2::theme(legend.position = "none")

  if (prettycheck:::test_temporal(data[[col]])) {
    plot <- plot + ggplot2::scale_y_continuous(labels = labels_hms)
  }

  if (isTRUE(print)) print(plot)

  invisible(plot)
}
