# library(cli)
# library(clipr)
# library(dplyr)
# library(glue)
# library(prettycheck) # github.com/danielvartan/prettycheck
# library(summarytools)

source(here::here("R", "plot_bar.R"))
source(here::here("R", "plot_dist.R"))
source(here::here("R", "stats_summary.R"))
source(here::here("R", "utils.R"))

panel_tabset_var_distribution <- function(
    data,
    cols,
    col_labels = NULL,
    jitter = TRUE,
    source = "Created by the authors.",
    heading = "###",
    data_name = deparse(substitute(data)),
    suffix = "",
    root = ".",
    qual_chart = TRUE,
    write = TRUE,
    verbose = TRUE
  ) {
  prettycheck:::assert_tibble(data)
  prettycheck:::assert_character(cols, min.len = 1, any.missing = FALSE)
  prettycheck:::assert_subset(cols, names(data))
  prettycheck:::assert_character(col_labels, len = length(cols), null.ok = TRUE)
  prettycheck:::assert_flag(jitter)
  prettycheck:::assert_string(source, null.ok = TRUE)
  prettycheck:::assert_string(heading, pattern = "^#*")
  prettycheck:::assert_string(data_name)
  prettycheck:::assert_atomic(suffix)
  prettycheck:::assert_string(root)
  prettycheck:::assert_flag(qual_chart)
  prettycheck:::assert_flag(write)
  prettycheck:::assert_flag(verbose)

  sample <- sample(1000:9999, 1)
  suffix <- as.character(suffix)

  if (is.null(col_labels)) col_labels <- cols
  if (!file.exists(here::here("qmd"))) dir.create(here::here("qmd"))
  if (!suffix == "") suffix <- paste0("-", suffix)

  file <- here::here(
    "qmd",
    glue::glue("_panel-tabset-var-distribution{suffix}.qmd")
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
      "plot_bar.R", "plot_dist.R", "stats_summary.R", "utils.R",
      "utils-checks.R", "utils-plots.R", "utils-stats.R"
    ) |>
    sort() %>%
    paste0("source(here::here('R', '", ., "'))", collapse = "\n")

  out <- ":::: {.panel-tabset}\n"

  for (i in seq_along(cols)) {
    col_fix <- cols[i] |> stringr::str_replace_all("_", "-")

    if (isTRUE(jitter))
      box_plot_caption <- glue::glue(
        "Histogram of the `{cols[i]}` variable with a kernel density ",
        "estimate, a quantile-quantile (Q-Q) plot comparing the variable to ",
        "the theoretical normal distribution, and a boxplot highlighting ",
        "outliers in red and jittered data points in black."
      )
    else {
      box_plot_caption <- glue::glue(
        "Histogram of the `{cols[i]}` variable with a kernel density ",
        "estimate, a quantile-quantile (Q-Q) plot comparing the variable to ",
        "the theoretical normal distribution, and a boxplot highlighting ",
        "outliers in red."
      )
    }

    if (!is.numeric(data[[cols[i]]])) {
      out <- c(
        out,
        glue::glue(
        "
        {heading} `{col_labels[i]}`

        ::: {{#tbl-var-distribution-freqs-{col_fix}-{sample}}}
        ```{{r}}
        #| code-fold: true
        #| output: asis

        {data_name} |>
          summarytools::freq(
            var = {cols[i]},
            style = 'rmarkdown',
            plain.ascii = FALSE,
            headings = FALSE
          )
        ```

        [Source: {source}]{{.legend}}

        Frequencies of the `{cols[i]}` variable.
        :::
        "
        )
      )

      if (isTRUE(qual_chart)) {
        out <- c(
          out,
          "\n",
          glue::glue(
        "

        ::: {{#fig-var-distribution-charts-{col_fix}-{sample}}}
        ```{{r}}
        #| code-fold: true

        {data_name} |>
          plot_bar(
            col = '{cols[i]}',
            y_label = '{col_labels[i]}'
          )
        ```

        [Source: {source}]{{.legend}}

        Bar plot of the `{cols[i]}` variable.
        :::
        "
          )
        )
      }

    } else {
      out <- c(
        out,
        glue::glue(
      "
      {heading} `{col_labels[i]}`

      ::: {{#tbl-var-distribution-stats-{col_fix}-{sample}}}
      ```{{r}}
      #| code-fold: true
      #| output: asis

      {data_name} |>
        summarytools::descr(
          var = {cols[i]},
          style = 'rmarkdown',
          plain.ascii = FALSE,
          headings = FALSE
        )
      ```

      [Source: {source}]{{.legend}}

      Statistics for the `{cols[i]}` variable.
      :::

      ::: {{#fig-var-distribution-charts-{col_fix}-{sample}}}
      ```{{r}}
      #| code-fold: true

      {data_name} |>
        plot_dist(
          col = '{cols[i]}',
          jitter = {jitter}
        )
      ```

      [Source: {source}]{{.legend}}

      {box_plot_caption}
      :::
      "
        )
      )
    }

    if (!i == length(cols)) out <- c(out, "\n\n")
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
