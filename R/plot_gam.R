library(ggplot2)
# library(prettycheck) # github.com/danielvartan/prettycheck
library(rlang)

source(here::here("R", "gam_str_fun.R"))
source(here::here("R", "utils.R"))
source(here::here("R", "utils-plots.R"))

plot_gam <- function(
    data,
    model,
    type = 1,
    x_lim = c(1.05, -1.8),
    y_lim = c(0, 0.175),
    breaks = seq(y_lim[1], y_lim[2], 0.025),
    title = NULL,
    subtitle = gam_str_fun(model, fix_all_but = 1),
    x_label = "SPEI (12 months)",
    y_label = "Predicted probability",
    print = TRUE
  ) {
  prettycheck:::assert_tibble(data)
  prettycheck:::assert_choice("spei_12m", names(data))
  prettycheck:::assert_class(model, "gam")
  prettycheck:::assert_integer_number(type, lower = 1)
  prettycheck:::assert_numeric(x_lim, len = 2)
  prettycheck:::assert_numeric(y_lim, len = 2)
  prettycheck:::assert_multi_class(breaks, c("numeric", "waiver"))
  prettycheck:::assert_string(title, null.ok = TRUE)
  prettycheck:::assert_string(x_label, null.ok = TRUE)
  prettycheck:::assert_string(y_label, null.ok = TRUE)
  prettycheck:::assert_flag(print)

  prettycheck:::assert_multi_class(
    subtitle,
    c("character", "latexexpression"),
    null.ok = TRUE
  )

  if (!is.null(subtitle)) if (nchar(subtitle) > 60) subtitle <- NULL

  if (type %in% 1:3) {
    x_var = "spei_12m"
  } else if (type %in% 4:6) {
    x_var = "year"
  }

  response_var <-
    model$formula |>
    all.vars() |>
    magrittr::extract(1)

  plot <-
    data |>
    data_plot_gam(model, type = type) |>
    ggplot2::ggplot(
      ggplot2::aes(x = !!as.symbol(x_var), y = pred_response)
    ) +
    ggplot2::geom_point(
      mapping = ggplot2::aes(
        x = !!as.symbol(x_var),
        y = !!as.symbol(response_var)
      ),
      data = data,
      na.rm = TRUE,
      inherit.aes = FALSE,
      size = 0.5,
      colour = get_brand_color_tint(900, "black"),
      alpha = 0.25
    ) +
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
      subtitle = subtitle,
      x = x_label,
      y = y_label
    ) +
    ggplot2::scale_y_continuous(
      breaks = breaks,
      limits = y_lim
    ) +
    {
      if (x_var == "spei_12m") {
        ggplot2::scale_x_reverse(limits = x_lim)
      } else if (x_var == "year") {
        ggplot2::scale_x_continuous(
          breaks = ~ grDevices::axisTicks(.x, log = FALSE)
        )
      }
    }

  if (isTRUE(print)) print(plot)

  invisible(plot)
}

# library(dplyr)
# library(magrittr)
# library(prettycheck) # github.com/danielvartan/prettycheck
# library(stats)

data_plot_gam <- function(data, model, type = 1, resolution = 500) {
  prettycheck:::assert_tibble(data)
  prettycheck:::assert_choice("spei_12m", names(data))
  prettycheck:::assert_class(model, "gam")
  prettycheck:::assert_integer_number(type, lower = 1)
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
      gini_index =
        data |>
        dplyr::pull(gini_index) |>
        mean(na.rm = TRUE), # Keep Gini Index constant
      gdp_per_capita =
        data |>
        dplyr::pull(gdp_per_capita) |>
        mean(na.rm = TRUE), # Keep GDP constant
      year =
        data |>
        dplyr::pull(year) |>
        mean(na.rm = TRUE) # Keep year constant
    )
  } else if (type %in% 2:3) {
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
      gini_index =
        data |>
        dplyr::pull(gini_index) |>
        mean(na.rm = TRUE), # Keep Gini Index constant
      gdp_per_capita =
        data |>
        dplyr::pull(gdp_per_capita) |>
        mean(na.rm = TRUE), # Keep GDP constant
      year =
        data |>
        dplyr::pull(year) |>
        as.character() |>
        as.integer() |>
        mean(na.rm = TRUE) |> # Keep year constant
        round()
    )
  } else if (type %in% 4) {
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
  } else if (type %in% 5:6) {
    out <- dplyr::tibble(
      year = seq.int(
        data |>
          dplyr::pull(year) |>
          as.character() |>
          as.integer() |>
          min(na.rm = TRUE),
        data |>
          dplyr::pull(year) |>
          as.character() |>
          as.integer() |>
          max(na.rm = TRUE)
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

plot_gam_misfs <- function(
    data,
    gam_models,
    type = 1,
    x_lim = c(1.05, -1.8),
    y_lim = c(0, 0.3),
    breaks = seq(y_lim[1], y_lim[2], 0.05),
    title = NULL,
    subtitle = TRUE,
    x_label = "SPEI (12 months)",
    y_label = "Predicted probability",
    print = TRUE
  ) {
  categories <- c("A", "B", "C", "D")

  prettycheck:::assert_tibble(data)
  prettycheck:::assert_choice("spei_12m", names(data))
  prettycheck:::assert_list(gam_models)
  prettycheck:::assert_set_equal(categories, names(gam_models))
  prettycheck:::assert_integer_number(type, lower = 1)
  prettycheck:::assert_numeric(x_lim, len = 2)
  prettycheck:::assert_numeric(y_lim, len = 2)
  prettycheck:::assert_multi_class(breaks, c("numeric", "waiver"))
  prettycheck:::assert_string(title, null.ok = TRUE)
  prettycheck:::assert_flag(subtitle)
  prettycheck:::assert_string(x_label, null.ok = TRUE)
  prettycheck:::assert_string(y_label, null.ok = TRUE)
  prettycheck:::assert_flag(print)

  gam_plots <- list()

  for (i in categories) {
    i_data <-
      data |>
      dplyr::filter(misf == i)

    if (isTRUE(subtitle)) {
      i_subtitle <- gam_str_fun(gam_models[[i]])
      if (nchar(i_subtitle) > 60) i_subtitle <- NULL
    } else {
      i_subtitle <- NULL
    }

    i_plot <-
      plot_gam(
        data = i_data,
        model = gam_models[[i]],
        type = type,
        x_lim = x_lim,
        y_lim = y_lim,
        breaks = breaks,
        title = i,
        subtitle = i_subtitle,
        x_label = x_label,
        y_label = y_label,
        print = FALSE
      ) +
      ggplot2::theme(plot.title = element_text(hjust = 0.5))

    gam_plots[[i]] <- i_plot
  }

  plot <-
    patchwork::wrap_plots(
      A = gam_plots$A,
      B = gam_plots$B,
      C = gam_plots$C,
      D = gam_plots$D,
      ncol = 2,
      nrow = 2,
      axis_titles = "collect"
    ) +
    patchwork::plot_annotation(
      title = title
    )

  if (isTRUE(print)) print(plot)

  list(
    patchwork = plot,
    plots = gam_plots
  ) |>
    invisible()
}

# library(prettycheck) # github.com/danielvartan/prettycheck

# # Helper
#
# data |>
#   find_gam_misfs_y_max(
#     list(
#       A = mbepr_model_gam,
#       B = beipr_model_gam,
#       C = maper_model_gam,
#       D = mpepr_model_gam
#     )
#   )

find_gam_misfs_y_max <- function(data, gam_models) {
  categories <- c("A", "B", "C", "D")

  prettycheck:::assert_tibble(data)
  prettycheck:::assert_choice("spei_12m", names(data))
  prettycheck:::assert_list(gam_models)
  prettycheck:::assert_set_equal(categories, names(gam_models))

  y_max <- numeric()

  for (i in categories) {
    i_data <-
      data |>
      dplyr::filter(misf == i)

    i_data_plot_gam <- data_plot_gam(i_data, gam_models[[i]])

    y_max <- c(y_max, i_data_plot_gam$upper) |> max()
  }

  y_max
}

