# library(latex2exp)
library(magrittr)
# library(prettycheck) # github.com/danielvartan/prettycheck
# library(rutils) # github.com/danielvartan/rutils
# library(stats)

gam_str_fun <- function(
    model,
    digits = 3,
    latex2exp = FALSE,
    fix_all_but = NULL, # Ignore the intercept coefficient
    fix_fun = "Mean",
    coef_names = NULL, # Ignore the intercept coefficient
    coef_labels = NULL  # Ignore the intercept coefficient
  ) {
  prettycheck:::assert_multi_class(model, "gam")
  prettycheck:::assert_number(digits)
  prettycheck:::assert_flag(latex2exp)
  prettycheck:::assert_string(fix_fun)

  coef_length <-
    model$formula |>
    all.vars() |>
    magrittr::extract(-1) |>
    length()

  prettycheck:::assert_number(
    fix_all_but,
    lower = 1,
    upper = coef_length,
    null.ok = TRUE
  )

  prettycheck:::assert_character(
    coef_names,
    any.missing = FALSE,
    len = coef_length,
    null.ok = TRUE
  )

  prettycheck:::assert_character(
    coef_labels,
    any.missing = FALSE,
    len = coef_length,
    null.ok = TRUE
  )

  if (is.null(coef_names)) {
    coef_names <-
      model$formula |>
      all.vars() |>
      magrittr::extract(-1)
  }

  if (is.null(coef_labels)) {
    coef_labels <-
      model$formula |>
      as.character() |>
      magrittr::extract(3) |>
      stringr::str_split(" [+*] ") |>
      unlist()
  }

  coef <- numeric()

  for (i in coef_names) {
    coef[[i]] <-
      stats::coef(model) |>
      names() |>
      stringr::str_subset(i) %>%
      magrittr::extract(stats::coef(model), .) |>
      mean(na.rm = TRUE) |>
      unname() |>
      round(digits)
  }

  if (!is.null(fix_all_but)) {
    for (i in seq_along(coef_labels)[-fix_all_but]) {
      coef_labels[i] <- paste0(fix_fun, "(", coef_labels[i], ")")
    }
  }

  if (isTRUE(latex2exp)) {
    coef_labels <-
      coef_labels |>
      stringr::str_replace_all("\\_|\\.", " ") |>
      stringr::str_to_title() |>
      stringr::str_replace_all(" ", "")
  } else {
    coef_labels <- coef_labels
  }

  out <- paste0(
    ifelse(isTRUE(latex2exp), "$", ""),
    "y =", " ",
    round(stats::coef(model)[1] |> unname(), digits), " + ",
    paste(
      coef,
      ifelse(isTRUE(latex2exp), "\\times", "×"),
      coef_labels,
      # paste0("\\text{", coef_labels, "}"),
      collapse = " + "
    ),
    ifelse(isTRUE(latex2exp), "$", "")
  )

  out <- out |> stringr::str_replace("\\+ \\-", "\\- ")

  if (isTRUE(latex2exp)) {
    out |>latex2exp::TeX()
  } else {
    out
  }
}
