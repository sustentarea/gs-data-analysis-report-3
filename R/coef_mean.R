# library(prettycheck) # github.com/danielvartan/prettycheck

coef_mean <- function(model) {
  prettycheck:::assert_class(model, "gam")

  model |>
    coef() |>
    mean()
}

# library(magrittr)
# library(prettycheck) # github.com/danielvartan/prettycheck

coef_spei12m_mean <- function(model) {
  prettycheck:::assert_class(model, "gam")

  model |>
    coef() |>
    magrittr::extract(grep("s\\(spei_12m\\)", names(coef(model)))) |>
    mean()
}

# library(dplyr)
# library(prettycheck) # github.com/danielvartan/prettycheck

coef_means_misfs <- function(gamm_models) {
  prettycheck:::assert_list(gamm_models)
  prettycheck:::assert_set_equal(c("A", "B", "C", "D"), names(gamm_models))

  dplyr::tibble(
    A = coef_mean(gamm_models[["A"]]),
    B = coef_mean(gamm_models[["B"]]),
    C = coef_mean(gamm_models[["C"]]),
    D = coef_mean(gamm_models[["D"]])
  )
}

# library(dplyr)
# library(prettycheck) # github.com/danielvartan/prettycheck

coef_spei12m_means_misfs <- function(gamm_models) {
  prettycheck:::assert_list(gamm_models)
  prettycheck:::assert_set_equal(c("A", "B", "C", "D"), names(gamm_models))

  dplyr::tibble(
    A = coef_spei12m_mean(gamm_models[["A"]]),
    B = coef_spei12m_mean(gamm_models[["B"]]),
    C = coef_spei12m_mean(gamm_models[["C"]]),
    D = coef_spei12m_mean(gamm_models[["D"]])
  )
}
