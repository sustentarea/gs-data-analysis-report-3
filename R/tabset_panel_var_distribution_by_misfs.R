# library(cli)
# library(clipr)
# library(dplyr)
# library(glue)
# library(prettycheck) # github.com/danielvartan/prettycheck

source(here::here("R", "tabset_panel_var_distribution.R"))

tabset_panel_var_distribution_by_misfs <- function(
    data,
    cols,
    col_labels = NULL,
    jitter = TRUE,
    source = "Created by the authors.",
    heading = "####",
    root = ".",
    write = TRUE,
    verbose = TRUE
  ) {
  prettycheck:::assert_tibble(data)
  prettycheck:::assert_character(cols, min.len = 1, any.missing = FALSE)
  prettycheck:::assert_subset(c(cols, "misf"), names(data))
  prettycheck:::assert_character(col_labels, len = length(cols), null.ok = TRUE)
  prettycheck:::assert_flag(jitter)
  prettycheck:::assert_string(source, null.ok = TRUE)
  prettycheck:::assert_string(heading, pattern = "^#*")
  prettycheck:::assert_string(root)
  prettycheck:::assert_flag(write)
  prettycheck:::assert_flag(verbose)

  if (is.null(col_labels)) col_labels <- cols
  if (!file.exists(here::here("qmd"))) dir.create(here::here("qmd"))

  file <- here::here(
    "qmd",
    glue::glue("_panel-tabset-var-distribution-by-misfs.qmd")
  )

  libraries <-
    c(
      "cli", "clipr", "dplyr", "ggplot2", "glue", "here", "magrittr",
      "prettycheck", "readr", "rlang", "summarytools", "tidyr"
    ) |>
    sort() %>%
    paste0("library(", ., ")", collapse = "\n")

  scripts <-
    c(
      "plot_bar.R", "plot_dist.R", "utils.R", "utils-checks.R",
      "utils-plots.R", "utils-stats.R"
    ) |>
    sort() %>%
    paste0("source(here::here('R', '", ., "'))", collapse = "\n")

  categories <- c("A", "B", "C", "D")
  out <- character()

  for (i in categories) {
    i_data_name  <- paste0("data_misfs_", tolower(i))

    if (!i_data_name %in% ls(envir = .GlobalEnv)) {
      assign(
        i_data_name,
        data |> dplyr::filter(misf == i),
        envir = .GlobalEnv
      )
    }

    out <-
      c(
        out,
        paste0(
          glue::glue("{heading} {i}"),
          "\n\n",
          tabset_panel_var_distribution(
            data = get(i_data_name),
            cols = cols,
            col_labels = col_labels,
            jitter = jitter,
            source = source,
            heading = paste0(heading, '#'),
            data_name = i_data_name,
            suffix = tolower(i),
            root = root,
            write = FALSE,
            verbose = FALSE
          )
        )
    )

    if (!i == "D") out <- c(out, "\n\n")
  }

  out <- out |> paste0(collapse = "")
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
