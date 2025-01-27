# library(patchwork)
# library(prettycheck) # github.com/danielvartan/prettycheck

source(here::here("R", "utils-checks.R"))
source(here::here("R", "plot_box_plot.R"))
source(here::here("R", "plot_hist.R"))
source(here::here("R", "plot_qq.R"))

plot_dist <- function(
    data,
    col,
    bins = 30,
    stat = "density",
    density_line = TRUE,
    jitter = TRUE,
    na_rm = TRUE,
    print = TRUE
  ) {
  prettycheck:::assert_tibble(data)
  prettycheck:::assert_string(col)
  prettycheck:::assert_choice(col, names(data))
  prettycheck:::assert_number(bins, lower = 1)
  prettycheck:::assert_choice(stat, c("count", "density"))
  prettycheck:::assert_flag(density_line)
  prettycheck:::assert_flag(jitter)
  prettycheck:::assert_flag(na_rm)
  prettycheck:::assert_flag(print)

  hist_plot <-
    data |>
    plot_hist(
      col = col,
      bins = bins,
      stat = stat,
      density_line = density_line,
      na_rm = na_rm,
      print = FALSE
    )

  qq_plot <-
    data |>
    plot_qq(
      col = col,
      na_rm = na_rm,
      print = FALSE
    )

  box_plot <-
    data |>
    plot_box_plot(
      col = col,
      label = col,
      cord_flip = FALSE,
      jitter = jitter,
      print = FALSE
    )

  plot <- patchwork::wrap_plots(hist_plot, qq_plot, box_plot, ncol = 3)

  if (isTRUE(print)) print(plot)

  invisible(plot)
}
