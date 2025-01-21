# library(mgcv)
# library(prettycheck) # github.com/danielvartan/prettycheck

model_gamm_misfs <- function(data, model, type = 1) {
  prettycheck:::assert_tibble(data)
  prettycheck:::assert_choice("spei_12m", names(data))
  prettycheck:::assert_class(model, "gam")
  prettycheck:::assert_choice(type, 1:2)

  categories <- c("A", "B", "C", "D")
  var <- model$formula |> all.vars() |> magrittr::extract(1)

  if (type == 1) {
    formula <-
      paste0(
        "formula(",
        var,
        " ~ s(spei_12m) + gdp_per_capita + s(year, bs = 're'))"
      ) |>
      str2expression() |>
      eval()
  } else {
    formula <-
      paste0(
        "formula(",
        var,
        " ~ s(year))"
      ) |>
      str2expression() |>
      eval()
  }

  gamm_models <- list()

  for (i in categories) {
    i_data <-
      data |>
      dplyr::filter(misf == i)

    i_model <- mgcv::gam(
      formula = formula,
      data = i_data,
      family = mgcv::betar(link = "logit")
    )

    gamm_models[[i]] <- i_model
  }

  invisible(gamm_models)
}

summarise_model_gamm_misfs <- function(gamm_models) {
  categories <- c("A", "B", "C", "D")

  prettycheck:::assert_list(gamm_models)
  prettycheck:::assert_set_equal(categories, names(gamm_models))

  for (i in categories) {
    cli::cli_h1("Model {i}")

    gamm_models[[i]] |>
      summary() |>
      print()
  }

  invisible()
}

summarise_model_gamm_misfs_2 <- function(gamm_models) {
  categories <- c("A", "B", "C", "D")

  prettycheck:::assert_list(gamm_models)
  prettycheck:::assert_set_equal(categories, names(gamm_models))

  out <- dplyr::tibble()

  for (i in categories) {
    i_summary <- gamm_models[[i]] |> summary()

    out <-
      out |>
      dplyr::bind_rows(
        dplyr::tibble(
          misf = i,
          edf = i_summary$s.table[1, "edf"],
          p_value = i_summary$s.table[1, "p-value"]
        )
      )
  }

  out
}
