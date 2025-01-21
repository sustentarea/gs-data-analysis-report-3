library(ggplot2)
# library(prettycheck) # github.com/danielvartan/prettycheck

source(here::here("R", "utils.R"))
source(here::here("R", "utils-plots.R"))

plot_gamm <- function(
    data,
    model,
    x_lim = c(1.05, -1.8),
    y_lim = c(0, 0.1),
    breaks = seq(y_lim[1], y_lim[2], 0.025),
    title = NULL,
    x_label = "Standardised Precipitation Evapotranspiration Index (12m)",
    y_label = "Predicted",
    print = TRUE
  ) {
  prettycheck:::assert_tibble(data)
  prettycheck:::assert_choice("spei_12m", names(data))
  prettycheck:::assert_class(model, "gam")
  prettycheck:::assert_numeric(x_lim, len = 2)
  prettycheck:::assert_numeric(y_lim, len = 2)
  prettycheck:::assert_multi_class(breaks, c("numeric", "waiver"))
  prettycheck:::assert_string(title, null.ok = TRUE)
  prettycheck:::assert_string(x_label, null.ok = TRUE)
  prettycheck:::assert_string(y_label, null.ok = TRUE)
  prettycheck:::assert_flag(print)

  plot <-
    data |>
    data_plot_gamm(model, type = 1) |>
    ggplot2::ggplot(ggplot2::aes(x = spei_12m, y = pred_response)) +
    ggplot2::geom_line(
      color = get_brand_color("red"),
      linewidth = 1
    ) +
    ggplot2::geom_ribbon(
      ggplot2::aes(ymin = lower, ymax = upper),
      alpha = 0.2,
      fill = get_brand_color("red")
    ) +
    ggplot2::labs(
      title = title,
      x = x_label,
      y = y_label
    ) +
    ggplot2::scale_y_continuous(
      breaks = breaks,
      limits = y_lim
    ) +
    ggplot2::scale_x_reverse(limits = x_lim)

  if (isTRUE(print)) print(plot)

  list(

  )
  invisible(plot)
}

# library(dplyr)
# library(magrittr)
# library(prettycheck) # github.com/danielvartan/prettycheck
# library(stats)

data_plot_gamm <- function(data, model, type = 1, resolution = 200) {
  prettycheck:::assert_tibble(data)
  prettycheck:::assert_choice("spei_12m", names(data))
  prettycheck:::assert_class(model, "gam")
  prettycheck:::assert_choice(type, 1:2)
  prettycheck:::assert_integer_number(resolution, lower = 1)

  if (type == 1) {
    out <- dplyr::tibble(
      spei_12m = seq(
        data |>
          dplyr::pull(spei_12m) |>
          min(na.rm = TRUE),
        data |>
          dplyr::pull(spei_12m) |>
          max(na.rm = TRUE),
        length.out = resolution
      ),
      gdp_per_capita =
        data |>
        dplyr::pull(gdp_per_capita) |>
        mean(na.rm = TRUE), # Keep GDP constant
      year =
        data |>
        dplyr::pull(year) |>
        mean(na.rm = TRUE) # Keep year constant
    )
  } else {
    out <- dplyr::tibble(
      year = seq(
        data |>
          dplyr::pull(year) |>
          min(na.rm = TRUE),
        data |>
          dplyr::pull(year) |>
          max(na.rm = TRUE),
        length.out = resolution
      )
    )
  }

  out |>
    dplyr::mutate(
      pred =
        model |>
        stats::predict(
          newdata = out,
          type = "link",
          se.fit = TRUE
        ) |>
        magrittr::extract2("fit"),
      se =
        model |>
        stats::predict(
          newdata = out,
          type = "link",
          se.fit = TRUE
        ) |>
        magrittr::extract2("se.fit"),
      # For beta regression (logit link)
      pred_response = pred |> stats::plogis(),
      upper = (pred + 1.96 * se) |> stats::plogis(),
      lower = (pred - 1.96 * se) |> stats::plogis()
    )
}

library(ggplot2)
library(patchwork)
# library(prettycheck) # github.com/danielvartan/prettycheck

plot_gamm_misfs <- function(
    data,
    gamm_models,
    type = 1,
    x_lim = NULL,
    y_lim = NULL,
    breaks = NULL,
    title = NULL,
    x_label = "Standardised Precipitation Evapotranspiration Index (12m)",
    y_label = "Predicted",
    print = TRUE
  ) {
  categories <- c("A", "B", "C", "D")

  prettycheck:::assert_tibble(data)
  prettycheck:::assert_choice("spei_12m", names(data))
  prettycheck:::assert_list(gamm_models)
  prettycheck:::assert_choice(type, 1:2)
  prettycheck:::assert_set_equal(categories, names(gamm_models))
  prettycheck:::assert_numeric(x_lim, null_ok = TRUE)
  prettycheck::assert_length(x_lim, 2, null_ok = TRUE)
  prettycheck:::assert_numeric(y_lim, null_ok = TRUE)
  prettycheck::assert_length(y_lim, 2, null_ok = TRUE)
  prettycheck:::assert_string(title, null.ok = TRUE)
  prettycheck:::assert_string(x_label, null.ok = TRUE)
  prettycheck:::assert_string(y_label, null.ok = TRUE)
  prettycheck:::assert_flag(print)

  prettycheck:::assert_multi_class(
    breaks, c("numeric", "waiver"), null.ok = TRUE
  )

  if (is.null(x_lim)) {
    if (type == 1) {
      x_lim <- c(1.05, -1.8)
    } else {
      x_lim <- c(2008, 2019)
    }
  }

  if (is.null(y_lim)) {
    y_lim <-c(
      0,
      0.15
      # find_gamm_misfs_y_max(data, gamm_models) |> round(2)
    )
  }

  if (is.null(breaks)) breaks <- seq(y_lim[1], y_lim[2], 0.05)

   if (type == 1) {
     gamm_plots <- plot_gamm_misfs_type_1(
      data = data,
      gamm_models = gamm_models,
      x_lim = x_lim,
      y_lim = y_lim,
      breaks = breaks,
      x_label = x_label,
      y_label = y_label
    )
  } else {
    gamm_plots <- plot_gamm_misfs_type_2(
      data = data,
      gamm_models = gamm_models,
      x_lim = x_lim,
      y_lim = y_lim,
      breaks = breaks,
      x_label = x_label,
      y_label = y_label
    )
  }

  plot <-
    patchwork::wrap_plots(
      A = gamm_plots$A,
      B = gamm_plots$B,
      C = gamm_plots$C,
      D = gamm_plots$D,
      ncol = 2,
      nrow = 2,
      axis_titles = "collect"
    ) +
    patchwork::plot_annotation(
      title = title
      # tag_levels = "A"
    )

  if (isTRUE(print)) print(plot)

  list(
    patchwork = plot,
    plots = gamm_plots
  ) |>
    invisible()
}

plot_gamm_misfs_type_1  <- function(
    data,
    gamm_models,
    x_lim = c(1.05, -1.8),
    y_lim = c(0, 0.15),
    breaks = NULL,
    x_label = "Standardised Precipitation Evapotranspiration Index (12m)",
    y_label = "Predicted"
  ) {
  categories <- c("A", "B", "C", "D")
  gamm_plots <- list()

  for (i in categories) {
    i_data <-
      data |>
      dplyr::filter(misf == i)

    i_plot <-
      plot_gamm(
        data = i_data,
        model = gamm_models[[i]],
        x_lim = x_lim,
        y_lim = y_lim,
        breaks = breaks,
        title = i,
        x_label = x_label,
        y_label = y_label,
        print = FALSE
      ) +
      ggplot2::theme(plot.title = element_text(hjust = 0.5))

    gamm_plots[[i]] <- i_plot
  }

  invisible(gamm_plots)
}

library(ggplot2)
library(mgcv)
library(rlang)

source(here::here("R", "utils.R"))
source(here::here("R", "utils-plots.R"))

plot_gamm_misfs_type_2  <- function(
    data,
    gamm_models,
    x_lim = c(2008, 2019),
    y_lim = c(0, 0.15),
    breaks = NULL,
    x_label = "Years",
    y_label = "Predicted"
  ) {
  categories <- c("A", "B", "C", "D")
  gamm_plots <- list()

  for (i in categories) {
    i_data <-
      data |>
      dplyr::filter(misf == i)

    i_plot <-
      i_data |>
      data_plot_gamm(gamm_models[[i]], type = 2) |>
      ggplot2::ggplot(aes(x = year, y = pred_response)) +
      ggplot2::geom_line(
        color = get_brand_color("red"),
        linewidth = 1
      ) +
      ggplot2::geom_ribbon(
        ggplot2::aes(ymin = lower, ymax = upper),
        alpha = 0.2,
        fill = get_brand_color("red")
      ) +
      ggplot2::labs(
        title = i,
        x = x_label,
        y = y_label
      ) +
      ggplot2::scale_y_continuous(
        breaks = breaks,
        limits = y_lim
      ) +
      ggplot2::scale_x_continuous(
        breaks = ~ grDevices::axisTicks(.x, log = FALSE)
        # limits = ~ grDevices::axisTicks(.x, log = FALSE)
      ) +
      ggplot2::theme(plot.title = element_text(hjust = 0.5))

    gamm_plots[[i]] <- i_plot
  }

  invisible(gamm_plots)
}

# library(prettycheck) # github.com/danielvartan/prettycheck

# # Helper
#
# data |>
#   find_gamm_misfs_y_max(
#     list(
#       A = mbepr_model_gamm,
#       B = beipr_model_gamm,
#       C = maper_model_gamm,
#       D = mpepr_model_gamm
#     )
#   )

find_gamm_misfs_y_max <- function(data, gamm_models) {
  categories <- c("A", "B", "C", "D")

  prettycheck:::assert_tibble(data)
  prettycheck:::assert_choice("spei_12m", names(data))
  prettycheck:::assert_list(gamm_models)
  prettycheck:::assert_set_equal(categories, names(gamm_models))

  y_max <- numeric()

  for (i in categories) {
    i_data <-
      data |>
      dplyr::filter(misf == i)

    i_data_plot_gamm <- data_plot_gamm(i_data, gamm_models[[i]])

    y_max <- c(y_max, i_data_plot_gamm$upper) |> max()
  }

  y_max
}

