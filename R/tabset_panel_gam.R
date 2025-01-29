# library(cli)
# library(clipr)
# library(dplyr)
# library(glue)
# library(mgcv)
# library(mgcViz)
# library(pal) # gitlab.com/rpkg.dev/pal
# library(prettycheck) # github.com/danielvartan/prettycheck
# library(readr)
# library(rutils) # github.com/danielvartan/rutils

source(here::here("R", "utils.R"))
source(here::here("R", "utils-checks.R"))

tabset_panel_gam <- function(
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
    glue::glue("_panel-tabset-gam-{type}{suffix}.qmd")
  )

  libraries <-
    c(
      "cli", "clipr", "dplyr", "ggplot2", "glue", "here", "magrittr", "mgcv",
      "mgcViz", "prettycheck", "readr", "pal", "rutils"
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
    i_var <- glue::glue("{cols[i]}_gam_{type}")
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
      {i_var} <- mgcv::gam(
        {i_formula},
        data = {data_name},
        family = mgcv::betar(link = "logit"),
        method = "REML"
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

      {i_source}

      Confidence interval for the adjusted $\\text{{R}}^{{2}}$ of the
      `{i_var}` model.
      :::

      ::: {{#tbl-{i_var_fix}-summary-stats{suffix}}}
      ```{{r}}
      #| output: asis

      {i_var} |>
        broom::glance() |>
        tidyr::pivot_longer(dplyr::everything()) |>
        md_named_tibble()
      ```

      {i_source}

      Summary statistics of the `{i_var}` model.
      :::

      ::: {{#tbl-{i_var_fix}-coef-stats{suffix}}}
      ```{{r}}
      #| output: asis

      {i_var} |>
        summarise_coefs() |>
        md_named_tibble()
      ```

      {i_source}

      Value of the coefficients in the `{i_var}` model.
      :::
      '
      )
    )

    if (stringr::str_detect(rhs_formula, ".*\\(")) {
      out <- c(
        out,
        "\n\n",
        glue::glue(
          '
      ::: {{#fig-{i_var_fix}-effects{suffix}}}
      ```{{r}}
      plot <- {i_var} |>  mgcViz::getViz() |> plot(allTerms = TRUE)

      for(i in plot$plots) print(i)
      ```

      {i_source}

      Effects of each term in the `{i_var}` model.
      :::

      ::: {{#tbl-{i_var_fix}-concurvity{suffix}}}
      ```{{r}}
      #| output: asis

      {i_var} |>
        mgcv::concurvity(TRUE) |>
        as.data.frame() |>
        pal::pipe_table(label = NA, digits = 10) |>
        pal::cat_lines()
      ```

      {i_source}

      Concurvity measures of the `{i_var}` model terms.
      :::
       '
        )
      )
    }

    out <- c(
      out,
      "\n\n",
      glue::glue(
        '
      ::: {{#fig-{i_var_fix}-diagnostics{suffix}}}
      ```{{r}}
      {i_var} |> mgcViz::getViz() |> mgcViz::check.gamViz()
      ```

      {i_source}

      Diagnostics for the `{i_var}` model.
      :::

      ::: {{#fig-{i_var_fix}-prediction-versus-spei{suffix}}}
      ```{{r}}
      {data_name} |>
        dplyr::mutate(
          dplyr::across(
            .cols = dplyr::matches("^year$"),
            .fns = ~ .x |> as.character() |> as.numeric()
          )
        ) |>
        plot_gam(
          model = {i_var},
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


gam_cols <- function() {
  c("mbepr", "beipr", "mbepr_beipr", "maper", "mpepr", "maper_mpepr")
}

gam_col_short_labels <- function() {
  # col_short_labels <- col_labels |> stringr::str_extract(".*(?= – )")

  c(
    "MBEPR", "BEIPR", "MBEPR & BEIPR", "MAPER", "MPEPR", "MAPER & MPEPR"
  )
}

gam_col_long_labels <- function() {
  # col_long_labels  <- col_labels |> stringr::str_extract("(?<= – )[A-Za-z].*")

  c(
    "Very Short Stature for Age (*Muito Baixa Estatura Para a Idade*)",
    "Short Stature for Age (*Baixa Estatura Para Idade*)",
    paste0(
      "Very Short/Short Stature for Age ",
      "(*Muito Baixa/Baixa Estatura Para Idade*)"
    ),
    "Severe Thinness for Height (*Magreza Acentuada Para a Estatura*)",
    "Thinness for Height (*Magreza Por Estatura*)",
    paste0(
      "Severe/Moderate Thinness for Height ",
      "(*Magreza Acentuada/Moderada Para a Estatura*)"
    )
  )
}

# library(prettycheck) # github.com/danielvartan/prettycheck

gam_plot_x_labels <- function(type) {
  prettycheck:::assert_integer_number(type, lower = 1)

  if (type %in% 1:3) {
    paste0(
      "Standardised Precipitation Evapotranspiration Index (12 months)"
    ) |>
      rep(length(gam_cols()))
  } else if (type %in% 4:6) {
    "Years" |> rep(length(gam_cols()))
  } else {
    NULL
  }
}

# library(prettycheck) # github.com/danielvartan/prettycheck

gam_plot_y_labels <- function(type) {
  prettycheck:::assert_integer_number(type, lower = 1)

  paste0(
    "Predicted probability of {col_short_labels[i]}"
  ) |>
    rep(length(gam_cols()))
}

# library(prettycheck) # github.com/danielvartan/prettycheck

gam_plot_captions <- function(type) {
  prettycheck:::assert_integer_number(type, lower = 1)

  if (type %in% 1:3) {
    paste0(
      strwrap(
      "
      Predicted probability of {col_short_labels[i]} ({col_long_labels[i]}) as
      a function of the smoothed 12-month Standardized Precipitation
      Evapotranspiration Index (SPEI), based on the `{i_var}` model. All other
      variables are held constant at their mean values except SPEI. Shaded
      areas indicates the pointwise 95% prediction confidence interval, while
      the faded dots in the background represent the observed data.
      "
      ),
      collapse = " "
    ) |>
      rep(length(gam_cols()))
  } else if (type %in% 4:6) {
    paste0(
      strwrap(
      "
      Predicted probability of {col_short_labels[i]} ({col_long_labels[i]}) as
      a function of the smoothed 12-month Standardized Precipitation
      Evapotranspiration Index (SPEI), based on the `{i_var}` model. Shaded
      areas indicates the pointwise 95% prediction confidence interval, while
      the faded dots in the background represent the observed data.
      "
      ),
      collapse = " "
    ) |>
      rep(length(gam_cols()))
  } else {
    NULL
  }
}
