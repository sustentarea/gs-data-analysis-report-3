# library(cli)
# library(clipr)
# library(dplyr)
# library(glue)
# library(prettycheck) # github.com/danielvartan/prettycheck
# library(stringr)

source(here::here("R", "utils.R"))

tabset_panel_brazil_municipality <- function(
    col,
    caption,
    source = "Created by the authors.",
    years = 2008:2019,
    heading = "###",
    suffix = "",
    root = ".",
    write = TRUE,
    verbose = TRUE
  ) {
  prettycheck:::assert_string(col)
  prettycheck:::assert_string(caption)
  prettycheck:::assert_string(source, null.ok = TRUE)
  prettycheck:::assert_integerish(years, any.missing = FALSE)
  prettycheck:::assert_string(heading, pattern = "^#*")
  prettycheck:::assert_atomic(suffix, len = 1)
  prettycheck:::assert_string(root)
  prettycheck:::assert_flag(write)
  prettycheck:::assert_flag(verbose)

  if (stringr::str_sub(caption, -1) == ".") {
    caption <- stringr::str_sub(caption, 1, -2)
  }

  col <- stringr::str_replace_all(col, "_", "-")
  years <- c(glue::glue("{dplyr::first(years)}-{dplyr::last(years)}"), years)

  if (!file.exists(here::here("qmd"))) dir.create(here::here("qmd"))
  if (!suffix == "") suffix <- paste0("-", suffix)

  file <- here::here(
    "qmd",
    glue::glue("_panel-tabset-brazil-municipality-{col}{suffix}.qmd")
  )

  libraries <-
    c(
      "cli", "clipr", "dplyr", "geobr", "ggplot2", "glue", "here", "lubridate",
      "magrittr", "prettycheck", "readr", "rutils"
    ) |>
    sort() %>%
    paste0("library(", ., ")", collapse = "\n")

  scripts <-
    c(
      "plot_brazil.R", "utils.R", "utils-plots.R"
    ) |>
    sort() %>%
    paste0("source(here::here('R', '", ., "'))", collapse = "\n")

  out <- ":::: {.panel-tabset}\n"

  for (i in seq_along(years)) {
    if (i == 1) {
      header_text <- "Animation"
      md_image <- glue::glue("![]({root}/images/{col}-animation.gif)")
    } else {
      header_text <- glue::glue("{years[i]}")
      md_image <- glue::glue("![]({root}/images/{col}-{years[i]}.png)")
    }

    out <- c(
      out,
      glue::glue(
        '
        {heading} {header_text}

        ::: {{#fig-brazil_municipality-{col}-{tolower(header_text)}}}
        {md_image}

        [Source: {source}]{{.legend}}

        {caption} ({years[i]}).
        :::
        '
      )
    )

    if (!i == length(years)) out <- c(out, "\n\n")
  }

  out <- c(out, "\n::::") |>  paste0(collapse = "")
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
