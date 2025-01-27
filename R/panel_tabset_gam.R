# library(cli)
# library(clipr)
# library(dplyr)
# library(glue)
# library(mgcv)
# library(pal) # gitlab.com/rpkg.dev/pal
# library(prettycheck) # github.com/danielvartan/prettycheck
# library(readr)
# library(rutils) # github.com/danielvartan/rutils

source(here::here("R", "utils.R"))

panel_tabset_gam <- function(
    data,
    rhs_formula = "s(spei_12m)",
    cols = c("mbepr", "beipr", "mbepr_beipr", "maper", "mpepr", "maper_mpepr"),
    source = "Created by the authors.",
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
  prettycheck:::assert_string(source, null.ok = TRUE)
  prettycheck:::assert_string(heading, pattern = "^#*")
  prettycheck:::assert_string(data_name)
  prettycheck:::assert_integer_number(suffix)
  prettycheck:::assert_string(root)
  prettycheck:::assert_flag(write)
  prettycheck:::assert_flag(verbose)

  col_short_labels <- c(
    "MBEPR", "BEIPR", "MBEPR & BEIPR", "MAPER", "MPEPR", "MAPER & MPEPR"
  )

  col_labels <- c(
    paste(
      "MBEPR – Very Short Stature for Age",
      "(*Muito Baixa Estatura Para a Idade*)"
    ),
    "BEIPR – Short Stature for Age (*Baixa Estatura Para Idade*)",
    paste(
      "MBEPR & BEIPR – Very Short/Short Stature for Age",
      "(*Muito Baixa/Baixa Estatura Para Idade*)"
    ),
    paste(
      "MAPER – Severe Thinness for Height",
      "(*Magreza Acentuada Para a Estatura*)"
    ),
    "MPEPR – Thinness for Height (*Magreza Por Estatura*)",
    paste(
      "MAPER & MPEPR – Severe/Moderate Thinness for Height",
      "(*Magreza Acentuada/Moderada Para a Estatura*)"
    )
  )

  if (suffix %in% 1:3) {
    plot_x_label <- paste0(
      "Standardised Precipitation Evapotranspiration Index (12 months)"
    )
  } else if (suffix %in% 4:6) {
    plot_x_label <- "Years"
  }

  plot_titles <- c(
    glue::glue(
      "Very Short Stature for Age versus (MBEPR) versus ",
      ifelse(suffix %in% 1:3, "\\n ", ""),
      "{plot_x_label}",
    ),
    glue::glue(
      "Short Stature for Age (BEIPR) versus ",
      ifelse(suffix %in% 1:3, "\\n ", ""),
      "{plot_x_label}",
    ),
    glue::glue(
      "Very Short/Short Stature for Age (MBEPR & BEIPR) versus ",
      ifelse(suffix %in% 1:3, "\\n ", ""),
      "{plot_x_label}",
    ),
    glue::glue(
      "Severe Thinness for Height (MAPER) versus ",
      ifelse(suffix %in% 1:3, "\\n ", ""),
      "{plot_x_label}",
    ),
    glue::glue(
      "Thinness for Height (MPEPR) versus ",
      ifelse(suffix %in% 1:3, "\\n ", ""),
      "{plot_x_label}"
    ),
    glue::glue(
      "Severe/Moderate Thinness for Height (MAPER & MPEPR) versus ",
      ifelse(suffix %in% 1:3, "\\n ", ""),
      "{plot_x_label}",
    )
  )

  prettycheck:::assert_character(col_labels, len = length(cols))

  if (!file.exists(here::here("qmd"))) dir.create(here::here("qmd"))

  file <- here::here(
    "qmd",
    paste0(
      "_panel-tabset-gam",
      ifelse(!suffix == "", "-", ""),
      suffix,
      ".qmd"
    )
  )

  libraries <-
    c(
      "cli", "clipr", "dplyr", "ggplot2", "glue", "here", "magrittr", "mgcv",
      "prettycheck", "readr"
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
    if (i == length(cols)) {
      end <- ""
    } else {
      end <- "\n\n"
    }

    i_var <- glue::glue("{cols[i]}_gam_{suffix}")
    i_var_fix <- i_var |> stringr::str_replace_all("_", "-")
    i_formula <- glue::glue("{cols[i]} ~ {rhs_formula}")

    if (suffix %in% 1:3) {
      plot_caption <- glue::glue(
        "Predicted probability of {glue::backtick(cols[i])} as a function ",
        "of the Standardized Precipitation Evapotranspiration Index (SPEI) ",
        "(12 months) for the `{glue::backtick(i_var)}` model. The shaded ",
        "area represents the 95% prediction confidence interval, with ",
        "all other variables except SPEI held constant at their mean values."
      )
    } else if (suffix %in% 4:6) {
      plot_caption <- glue::glue(
        "Predicted probability of {glue::backtick(cols[i])} as a function ",
        "of the year for the {glue::backtick(i_var)} model. The shaded ",
        "area represents the 95% prediction confidence interval."
      )
    }

    out <- c(
      out,
      glue::glue(
      "
      {heading} {col_labels[i]}

      ```{{r}}
      {i_var} <- mgcv::gam(
        {i_formula},
        data = {data_name},
        family = mgcv::betar(link = 'logit')
      )
      ```

      ```{{r}}
      #| code-fold: false

      {i_var} |> summary()
      ```

      ::: {{#tbl-{i_var_fix}-r2}}
      ```{{r}}
      #| output: asis

      {i_var} |>
        summarise_r2(
          {data_name} |>
            nrow()
        ) |>
        md_named_tibble()
      ```

      [Source: {source}]{{.legend}}

      Confidence interval for the adjusted $\\text{{R}}^{{2}}$ of the
      `{i_var}` model.
      :::

      ::: {{#tbl-{i_var_fix}-summary-stats}}
      ```{{r}}
      #| output: asis

      {i_var} |>
        broom::glance() |>
        tidyr::pivot_longer(dplyr::everything()) |>
        md_named_tibble()
      ```

      [Source: {source}]{{.legend}}

      Summary statistics of the `{i_var}` model.
      :::

      ::: {{#tbl-{i_var_fix}-coef-stats}}
      ```{{r}}
      #| output: asis

      {i_var} |>
        summarise_coefs() |>
        md_named_tibble()
      ```

      [Source: {source}]{{.legend}}

      Value of the coefficients in the `{i_var}` model.
      :::

      ```{{r}}
      {i_var} |> mgcv::gam.check()
      ```

      ::: {{#fig-{i_var_fix}-prediction-versus-spei}}
      ```{{r}}
      {data_name} |>
        plot_gam(
          model = {i_var},
          type = {as.integer(suffix)},
          title = '{plot_titles[i]}',
          x_label = '{plot_x_label}',
          y_label = 'Predicted probability of {col_short_labels[i]}'
        )
      ```

      [Source: {source}]{{.legend}}

      {plot_caption}
      :::
      "
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
