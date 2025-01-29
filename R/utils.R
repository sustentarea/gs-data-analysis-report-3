long_string <- function(x) {
  prettycheck:::assert_string(x)

  x |>
    strwrap() |>
    paste0(collapse = " ") |>
    gsub(x = _, pattern = "\\s+", replacement = " ")
}

# library(magrittr)
# library(pal) # gitlab.com/rpkg.dev/pal
# library(prettycheck) # github.com/danielvartan/prettycheck
# library(stringr)
# library(rutils) # github.com/danielvartan/rutils

md_named_tibble <- function(data, col_names = NULL, row_names = NULL) {
  prettycheck:::assert_tibble(data, min.cols = 2)
  prettycheck:::assert_character(data[[1]])

  if (is.null(col_names)) {
    col_names <-
      data[-1] |>
      names() |>
      stringr::str_replace_all("\\_", " ") |>
      stringr::str_to_title()
  }

  if (is.null(row_names)) row_names <- data[[1]]

  data[-1] |>
    as.data.frame() |>
    magrittr::set_colnames(col_names) |>
    magrittr::set_rownames(row_names) |>
    pal::pipe_table(label = NA, digits = 10) |>
    pal::cat_lines() |>
    rutils::shush()
}

# library(here)
# library(prettycheck) # github.com/danielvartan/prettycheck
# library(stringr)

to_relative_path <- function(path, root = ".") {
  prettycheck:::assert_string(path)
  prettycheck:::assert_string(root)

  path <- stringr::str_remove(path, here::here())

  paste0(root, path)
}

# library(janitor)
# library(polyglotr)
# library(prettycheck) # github.com/danielvartan/prettycheck

regularize_col_names <- function(
    data,
    translate = TRUE,
    source_language = "pt",
    target_language = "en"
) {
  prettycheck:::assert_tibble(data)
  prettycheck:::assert_flag(translate)
  prettycheck:::assert_string(source_language, n.chars = 2)
  prettycheck:::assert_string(target_language, n.chars = 2)

  names(data) <- polyglotr::google_translate(
    names(data),
    target_language = "en",
    source_language = "pt"
  ) |>
    as.character()

  data |> janitor::clean_names()
}

# library(dplyr)
# library(prettycheck) # github.com/danielvartan/prettycheck
# library(tidyr)

list_as_tibble <- function(list) {
  prettycheck:::assert_list(list)

  list |>
    dplyr::as_tibble() |>
    dplyr::mutate(
      dplyr::across(
        .cols = dplyr::everything(),
        .fns = as.character
      )
    ) |>
    tidyr::pivot_longer(cols = dplyr::everything())
}

grab_fun_par <- function() {
  args_names <- ls(envir = parent.frame(), all.names = TRUE, sorted = FALSE)

  if ("..." %in% args_names) {
    dots <- eval(quote(list(...)), envir = parent.frame())
  } else {
    dots = list()
  }

  args_names <- sapply(setdiff(args_names, "..."), as.name)

  if(length(args_names)) {
    not_dots <- lapply(args_names, eval, envir = parent.frame())
  } else {
    not_dots <- list()
  }

  out <- c(not_dots, dots)

  out[names(out) != ""]
}

# library(prettycheck) # github.com/danielvartan/prettycheck

clean_arg_list <- function(list) {
  prettycheck:::assert_multi_class(list, c("list", "pairlist"))
  prettycheck:::assert_list(as.list(list), names = "named")

  list <- list |> nullify_list()

  out <- list()

  for (i in seq_along(list)) {
    if (!names(list[i]) %in% names(out)) {
      out <- c(out, list[i])
    }
  }

  out
}

# library(prettycheck) # github.com/danielvartan/prettycheck

nullify_list <- function(list) {
  prettycheck:::assert_multi_class(list, c("list", "pairlist"))
  prettycheck:::assert_list(as.list(list), names = "named")

  for (i in names(list)) {
    if (!is.null(list[[i]]) && is.atomic(list[[i]])) {
      if (any(list[[i]] == "", na.rm = TRUE)) {
        list[i] <- list(NULL)
      }
    }
  }

  list
}

# library(clipr)
library(magrittr)
# library(prettycheck) # github.com/danielvartan/prettycheck
# library(stringr)

normalize_names <- function(
    path = clipr::read_clip(),
    exceptions = c(
      "^README", "^OFL.txt$", "^DESCRIPTION", "^Google README.txt$"
    ),
    just_dirs = FALSE
) {
  prettycheck:::assert_string(path)
  prettycheck:::assert_directory_exists(path)
  prettycheck:::assert_character(exceptions)
  prettycheck:::assert_flag(just_dirs)

  dirs <- path |> list.dirs()
  if (length(dirs) == 0) dirs <- path

  if (!length(dirs) == 0 && isTRUE(just_dirs)) {
    dirs <- dirs |> magrittr::extract(-1)

    new_dir_name <-
      dirs |>
      basename() |>
      tolower() |>
      stringr::str_replace_all(" - ", "_") |>
      stringr::str_replace_all(" ", "-") |>
      stringr::str_squish() %>%
      file.path(dirname(dirs), .)

    for (i in rev(seq_along(dirs))) {
      if (!dirs[i] == new_dir_name[i]) {
        paste(
          "mv",
          glue::single_quote(dirs[i]),
          glue::single_quote(new_dir_name[i])
        ) |>
          system()
      }
    }

    dirs <-
      path |>
      list.dirs() |>
      magrittr::extract(-1)
  } else {
    for (i in dirs) {
      files <- list.files(i)

      for (j in exceptions) {
        files <- files |> stringr::str_subset(j, negate = TRUE)
      }

      if (length(files) == 0) {
        next
      } else {
        new_name <-
          files |>
          tolower() |>
          stringr::str_replace_all(" - ", "_") |>
          stringr::str_replace_all("_", "-") |>
          stringr::str_replace_all(" ", "-") |>
          stringr::str_squish()

        file.rename(file.path(i, files), file.path(i, new_name))
      }
    }
  }

  invisible()
}

# library(clipr)
# library(prettycheck) # github.com/danielvartan/prettycheck

vector_to_c <- function(x, quote = TRUE, clipboard = TRUE) {
  prettycheck:::assert_atomic(x)

  if (isTRUE(quote)) x <- paste0('"', x, '"')

  out <- paste0("c(", paste(x, collapse = ", "), ")")

  if (isTRUE(clipboard)) {
    cli::cli_alert_info("Copied to clipboard.")

    out |> clipr::write_clip()
  }

  cat(out)

  invisible(out)
}
