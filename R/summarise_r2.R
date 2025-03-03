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
