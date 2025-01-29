# library(dplyr)
library(magrittr)
# library(prettycheck) # github.com/danielvartan/prettycheck
# library(purrr)
# library(stats)

summarise_coefs <- function(model) {
  prettycheck:::assert_class(model, "gam")

  coef_names <-
    stats::coef(model) |>
    names() |>
    stringr::str_remove("\\.[0-9]*$") |>
    unique()

  dplyr::tibble(
    name = c(
      "[Mean]",
      coef_names,
      stats::coef(model) |> names() |> setdiff(coef_names)
    ),
    value = c(
      coef_mean(model),
      coef_names |>
        stringr::str_escape() |>
        purrr::map_dbl(~ coef_mean(model, .x)),
      stats::coef(model)[setdiff(stats::coef(model) |> names(), coef_names)]
    ),
  ) |>
    dplyr::mutate(
      name = dplyr::case_when(
        stringr::str_detect(name, "^.*\\(.*\\)$") ~
          paste0("mean(", name, ")"),
        TRUE ~ name
      )
    )
}

library(magrittr)
# library(prettycheck) # github.com/danielvartan/prettycheck
# library(rutils) # github.com/danielvartan/rutils
# library(stats)

coef_mean <- function(model, pattern = NULL) {
  prettycheck:::assert_class(model, "gam")
  prettycheck:::assert_string(pattern, null.ok = TRUE)

  if (is.null(pattern)) {
    model |>
      coef() |>
      mean()
  } else {
    stats::coef(model) |>
      names() |>
      stringr::str_subset(pattern) %>%
      magrittr::extract(stats::coef(model), .) |>
      mean(na.rm = TRUE) |>
      rutils:::clear_names()
  }
}

# library(dplyr)
# library(magrittr)
# library(prettycheck) # github.com/danielvartan/prettycheck

summarise_coefs_misfs <- function(gamm_models) {
  categories <- c("A", "B", "C", "D")

  prettycheck:::assert_list(gamm_models)
  prettycheck:::assert_set_equal(categories, names(gamm_models))
  for (i in gamm_models) prettycheck:::assert_class(i, "gam")

  out <- list()

  for (i in categories) {
    i_tibble <-
      gamm_models[[i]] |>
      summarise_coefs() |>
      magrittr::set_names(c("name", i))

    out <- c(out, list(i_tibble))
  }

  purrr::reduce(out, dplyr::left_join, by = "name")
}

# library(dplyr)
library(magrittr)
# library(prettycheck) # github.com/danielvartan/prettycheck
# library(rutils) # github.com/danielvartan/rutils
# library(stats)

coef_means_misfs <- function(gamm_models, pattern = NULL) {
  prettycheck:::assert_list(gamm_models)
  prettycheck:::assert_set_equal(c("A", "B", "C", "D"), names(gamm_models))
  for (i in gamm_models) prettycheck:::assert_class(i, "gam")
  prettycheck:::assert_string(pattern, null.ok = TRUE)

  dplyr::tibble(
    A = coef_mean(gamm_models[["A"]], pattern),
    B = coef_mean(gamm_models[["B"]], pattern),
    C = coef_mean(gamm_models[["C"]], pattern),
    D = coef_mean(gamm_models[["D"]], pattern)
  )
}
