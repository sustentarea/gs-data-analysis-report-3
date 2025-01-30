# library(cli)
# library(clipr)
# library(dplyr)
# library(glue)
# library(mgcv)
# library(prettycheck) # github.com/danielvartan/prettycheck
# library(readr)
# library(rutils) # github.com/danielvartan/rutils

source(here::here("R", "utils.R"))
source(here::here("R", "utils-checks.R"))

tabset_panel_gam_by_misfs <- function(
    data,
    rhs_formula,
    cols = gam_cols(),
    col_short_labels = gam_col_short_labels(),
    col_long_labels = gam_col_long_labels(),
    type = 1,
    plot_x_labels = gam_plot_x_labels(type),
    plot_y_labels = gam_plot_y_labels(type),
    plot_captions = gam_plot_captions(type),
    source = rep("Created by the authors.", length(cols)),
    heading = "####",
    data_name = deparse(substitute(data)),
    suffix = "",
    root = ".",
    write = TRUE,
    verbose = TRUE
  ) {
  prettycheck:::assert_tibble(data)
  prettycheck:::assert_string(rhs_formula)
  prettycheck:::assert_character(cols, min.len = 1, any.missing = FALSE)
  prettycheck:::assert_subset(cols, names(data))
  prettycheck:::assert_character(col_short_labels, len = length(cols))
  prettycheck:::assert_character(col_long_labels, len = length(cols))
  prettycheck:::assert_integer_number(type, lower = 1)
  prettycheck:::assert_string(heading, pattern = "^#*")
  prettycheck:::assert_string(data_name)
  prettycheck:::assert_atomic(suffix, len = 1)
  prettycheck:::assert_string(root)
  prettycheck:::assert_flag(write)
  prettycheck:::assert_flag(verbose)

  for (i in c("plot_x_labels", "plot_y_labels", "plot_captions", "source")) {
    prettycheck:::assert_character(
      get(i),
      len = length(cols),
      null.ok = TRUE
    )
  }

  col_labels <- paste0(col_short_labels, " – ", col_long_labels)

  if (!file.exists(here::here("qmd"))) dir.create(here::here("qmd"))
  if (!suffix == "") suffix <- paste0("-", suffix)

  file <- here::here(
    "qmd",
    glue::glue("_panel-tabset-gam-{type}-by-misfs{suffix}.qmd")
  )

  libraries <-
    c(
      "cli", "clipr", "dplyr", "ggplot2", "glue", "here", "magrittr", "mgcv",
      "prettycheck", "readr", "rutils"
    ) |>
    sort() %>%
    paste0("library(", ., ")", collapse = "\n")

  scripts <-
    c(
      "plot_gam.R", "summarise_r2.R", "summarise_coefs.R", "utils.R",
      "utils-plots.R"
    ) |>
    sort() %>%
    paste0("source(here::here('R', '", ., "'))", collapse = "\n")

  out <- ":::: {.panel-tabset}\n"

  for (i in seq_along(cols)) {
    i_var <- glue::glue("{cols[i]}_gam_{type}_by_misfs")
    i_var_fix <- i_var |> stringr::str_replace_all("_", "-")
    i_formula <- glue::glue("{cols[i]} ~ {rhs_formula}")

    i_plot_x_label <- ifelse(
      test_null_na(plot_x_labels[i]),
      "NULL",
      glue::glue(plot_x_labels[i])
    )

    i_plot_y_label <- ifelse(
      test_null_na(plot_y_labels[i]),
      "NULL",
      glue::glue(plot_y_labels[i])
    )

    i_plot_caption <- ifelse(
      test_null_na(plot_captions[i]),
      "",
      glue::glue(plot_captions[i])
    )

    i_source <- ifelse(
      test_null_na(source[i]),
      "",
      glue::glue("[Source: {source}]{{.legend}}")
    )

    out <- c(
      out,
      glue::glue(
        '
      {heading} {col_labels[i]}

      ```{{r}}
      {i_var} <-
        {data_name} |>
        gam_misfs({i_formula})
      ```

      ```{{r paged.print=FALSE}}
      #| code-fold: false

      {data_name} |>
        summarise_gam_misfs({i_var})
      ```

      ::: {{#fig-{i_var_fix}-prediction-versus-spei{suffix}}}
      ```{{r}}
      {data_name} |>
        dplyr::mutate(
          dplyr::across(
            .cols = dplyr::matches("^year$"),
            .fns = ~ .x |> as.character() |> as.numeric()
          )
        ) |>
        plot_gam_misfs(
          gam_models = {i_var},
          type = {type},
          x_label = "{i_plot_x_label}",
          y_label = "{i_plot_y_label}"
        )
      ```

      {i_source}

      {i_plot_caption}
      :::
      '
      )
    )

    if (!i == length(cols)) out <- c(out, "\n\n")
  }

  out <- c(out, "\n::::") |> paste0(collapse = "")
  if (isTRUE(write)) out |> readr::write_lines(file)

  if (isTRUE(verbose)) {
    include_string <- glue::glue(
      "{{{{< include {to_relative_path(file, root)} >}}}}"
    )

    cli::cli_alert_info(
      glue:::glue(
        "Use `{{include_string}}` to include ",
        "the panel in the file (Copied to clipboard).",
        "\n\n",
        "Also, don't forget call the libraries and to source the scripts ",
        "below in the file.",
        "\n\n",
        libraries,
        "\n\n",
        scripts,
        wrap = TRUE
      )
    )

    clipr::write_clip(include_string)
  }

  invisible(out)
}
