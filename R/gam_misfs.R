# library(mgcv)
# library(prettycheck) # github.com/danielvartan/prettycheck

gam_misfs <- function(data, var, type = 1) {
  prettycheck:::assert_tibble(data)
  prettycheck:::assert_choice(var, names(data))
  prettycheck:::assert_choice(type, 1:2)

  categories <- c("A", "B", "C", "D")

  if (type == 1) {
    prettycheck:::assert_subset(
      c("spei_12m", "gdp_per_capita", "year"),
      names(data)
    )

    formula <-
      paste0(
        "formula(",
        var,
        " ~ s(spei_12m) + gdp_per_capita + s(year, bs = 're')",
        ")"
      ) |>
      str2expression() |>
      eval()
  } else {
    prettycheck:::assert_subset("year", names(data))

    formula <-
      paste0(
        "formula(",
        var,
        " ~ s(year)",
        ")"
      ) |>
      str2expression() |>
      eval()
  }

  gam_models <- list()

  for (i in categories) {
    i_data <-
      data |>
      dplyr::filter(misf == i)

    i_model <- mgcv::gam(
      formula = formula,
      data = i_data,
      family = mgcv::betar(link = "logit")
    )

    gam_models[[i]] <- i_model
  }

  invisible(gam_models)
}

# library(cli)
# library(prettycheck) # github.com/danielvartan/prettycheck
# library(utils)

# Use `paged.print=FALSE` in the code chunk!

summarise_gam_misfs <- function(data, gam_models, n, ci_level = 0.95) {
  categories <- c("A", "B", "C", "D")
  vars <- c("spei_12m", "gdp_per_capita", "year")

  prettycheck:::assert_tibble(data)
  prettycheck:::assert_subset(vars, names(data))
  prettycheck:::assert_list(gam_models)
  prettycheck:::assert_set_equal(categories, names(gam_models))
  prettycheck:::assert_number(ci_level, lower = 0, upper = 1)

  pillar_sigfig <- getOption("pillar.sigfig")
  options(pillar.sigfig = 10)

  for (i in categories) {
    i_data <-
      data |>
      dplyr::filter(misf == i)

    cli::cli_h1("Model {i}")

    gam_models[[i]] |>
      summary() |>
      print()

    cat("\n")

    gam_models[[i]] |>
      summarise_r2(n = nrow(i_data)) |>
      print()
  }

  options(pillar.sigfig = pillar_sigfig)

  invisible()
}
