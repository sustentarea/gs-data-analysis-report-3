# library(mgcv)
# library(prettycheck) # github.com/danielvartan/prettycheck

model_gamm_misfs <- function(data, model, type = 1) {
  vars <- c("spei_12m", "gdp_per_capita", "year")

  prettycheck:::assert_tibble(data)
  prettycheck:::assert_subset(vars, names(data))
  prettycheck:::assert_choice("spei_12m", names(data))
  prettycheck:::assert_class(model, "gam")
  prettycheck:::assert_choice(type, 1:2)

  categories <- c("A", "B", "C", "D")

  var <- model$formula |> all.vars() |> magrittr::extract(1)
  prettycheck:::assert_choice(var, names(data))

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

# library(cli)
# library(prettycheck) # github.com/danielvartan/prettycheck
# library(utils)

# Use `paged.print=FALSE` in the code chunk!

summarise_model_gamm_misfs <- function(data, gamm_models, n, ci_level = 0.95) {
  categories <- c("A", "B", "C", "D")
  vars <- c("spei_12m", "gdp_per_capita", "year")

  prettycheck:::assert_tibble(data)
  prettycheck:::assert_subset(vars, names(data))
  prettycheck:::assert_list(gamm_models)
  prettycheck:::assert_set_equal(categories, names(gamm_models))
  prettycheck:::assert_number(ci_level, lower = 0, upper = 1)

  pillar_sigfig <- getOption("pillar.sigfig")
  options(pillar.sigfig = 10)

  for (i in categories) {
    i_data <-
      data |>
      dplyr::filter(misf == i)

    cli::cli_h1("Model {i}")

    gamm_models[[i]] |>
      summary() |>
      print()

    cat("\n")

    gamm_models[[i]] |>
      summarise_r2(n = nrow(i_data)) |>
      print()
  }

  options(pillar.sigfig = pillar_sigfig)

  invisible()
}

# library(dplyr)
# library(effectsize)
# library(magrittr)
# library(prettycheck) # github.com/danielvartan/prettycheck
# library(psychometric)
# library(rutils) # github.com/danielvartan/rutils

summarise_r2 <- function(
    model,
    n,
    k = (model$formula |> all.vars() |> length()) - 1,
    ci_level = 0.95,
    rules = "cohen1988"
  ) {
  rules_choices <- c("cohen1988", "falk1992", "chin1998", "hair2011")

  prettycheck:::assert_class(model, "gam")
  prettycheck:::assert_integer_number(k, lower = 1)
  prettycheck:::assert_number(ci_level, lower = 0, upper = 1)
  prettycheck:::assert_choice(rules, rules_choices)

  r2 <-summary(model) |> magrittr::extract2("r.sq") # Only for `gam()`
  if (r2 <= 0) r2 <- 0

  summary(model) |>
    magrittr::extract2("r.sq") |>
    psychometric::CI.Rsq(n = n, k = k, level = ci_level) |>
    tidyr::pivot_longer(cols = dplyr::everything()) |>
    dplyr::transmute(
      name = c("R2", "SE", "Lower CI", "Upper CI"),
      value = dplyr::if_else(rep(r2, 4) <= 0, 0, value),
      interpretation =
        value |>
        effectsize::interpret_r2(rules = rules) |>
        as.character(),
      rule = rep(rules, 4) |> magrittr::inset(2, NA),
      interpretation =
        dplyr::case_when(
          value <= 0 ~ "no effect",
          rule == "cohen1988" & interpretation == "very weak" ~
            "very weak (negligible)",
          TRUE ~ interpretation
        ) |>
        magrittr::inset(2, NA)
    ) |>
    rutils::shush()
}
