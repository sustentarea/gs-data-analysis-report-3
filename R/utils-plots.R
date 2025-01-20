# library(ggplot2)
# library(prettycheck) # github.com/danielvartan/prettycheck

source(here::here("R", "utils.R"))

# See <https://ggplot2-book.org/extensions#sec-new-scales> to learn more.

scale_brand <- function(
    aesthetics = "color",
    scale_type = "c",
    color_type = "seq",
    direction = 1,
    na.value = NA, # Must follow ggplot2 arg names. # "grey50"
    reverse = FALSE,
    ...
  ) {
  # See <https://ggplot2.tidyverse.org/reference/scale_viridis.html>.
  scale_type_choices <- c(
    "d", "discrete",
    "c", "continuous",
    "b", "binned"
  )

  # See <https://ggplot2.tidyverse.org/reference/scale_brewer.html>.
  color_type_choices <- c(
    "seq", "sequential",
    "div", "diverging",
    "qual", "qualitative"
  )

  prettycheck:::assert_string(aesthetics)
  prettycheck:::assert_choice(scale_type, scale_type_choices)
  prettycheck:::assert_choice(color_type, color_type_choices)
  prettycheck:::assert_choice(direction, c(-1, 1))
  prettycheck:::assert_color(na.value, na_ok = TRUE)
  prettycheck:::assert_flag(reverse)

  if (color_type %in% c("seq", "sequential")) {
    palette <- \(x) color_brand_sequential(x, direction = direction)
  } else if (color_type %in% c("div", "diverging")) {
    palette <- \(x) color_brand_diverging(x, direction = direction)
  } else if (color_type %in% c("qual", "qualitative")) {
    palette <- \(x) color_brand_qualitative(x, direction = direction)
  }

  if (scale_type %in% c("d", "discrete")) {
    scale_fun <- ggplot2::discrete_scale
    guide <- ggplot2::guide_legend(reverse = reverse)
  } else if (scale_type %in% c("c", "continuous")) {
    scale_fun <- ggplot2::continuous_scale
    guide <- ggplot2::guide_colourbar(reverse = reverse)
  } else if (scale_type %in% c("b", "binned")) {
    scale_fun <- ggplot2::binned_scale
    guide <- ggplot2::guide_colorsteps(reverse = reverse)
  }

  arg_list <- list(
    aesthetics = aesthetics,
    palette = palette,
    na.value = na.value,
    guide = guide
  )

  do.call(
    what = scale_fun,
    args = c(
      list(...)[names(list(...)) %in% names(formals(scale_fun))],
      arg_list
    ) |>
      clean_arg_list()
  )
}

source(here::here("R", "utils.R"))

scale_color_brand_d <- function(
    aesthetics = "color",
    scale_type = "d",
    color_type = "seq",
    direction = 1,
    ...
  ) {
  do.call("scale_brand", grab_fun_par())
}

scale_color_brand_c <- function(
    aesthetics = "color",
    scale_type = "c",
    color_type = "seq",
    direction = 1,
    ...
  ) {
  do.call("scale_brand", grab_fun_par())
}

scale_color_brand_b <- function(
    aesthetics = "color",
    scale_type = "b",
    color_type = "seq",
    direction = 1,
    ...
  ) {
  do.call("scale_brand", grab_fun_par())
}

scale_colour_brand_d <- scale_color_brand_d
scale_colour_brand_c <- scale_color_brand_c
scale_colour_brand_b <- scale_color_brand_b

scale_fill_brand_d <- function(
    aesthetics = "fill",
    scale_type = "d",
    color_type = "seq",
    direction = 1,
    ...
  ) {
  do.call("scale_brand", grab_fun_par())
}

scale_fill_brand_c <- function(
    aesthetics = "fill",
    scale_type = "c",
    color_type = "seq",
    direction = 1,
    ...
  ) {
  do.call("scale_brand", grab_fun_par())
}

scale_fill_brand_b <- function(
    aesthetics = "fill",
    scale_type = "b",
    color_type = "seq",
    direction = 1,
    ...
  ) {
  do.call("scale_brand", grab_fun_par())
}

# library(dplyr)
# library(prettycheck) # github.com/danielvartan/prettycheck

color_brand_sequential <- function(n, direction = 1) {
  prettycheck:::assert_numeric(n, lower = 0, min_len = 1)
  prettycheck:::assert_choice(direction, c(-1, 1))

  colors <- c(
    get_brand_color("primary"),
    get_brand_color("secondary")
  )

  if (length(n > 1) && all(dplyr::between(n, 0, 1), na.rm = TRUE)) {
    make_color_vector(
      n_prop = n,
      direction = direction,
      colors = colors
    )
  } else {
    make_color_vector(
      n = n,
      direction = direction,
      colors = colors
    )
  }
}

# library(prettycheck) # github.com/danielvartan/prettycheck

color_brand_diverging <- function(n, direction = 1) {
  prettycheck:::assert_numeric(n, lower = 0, min_len = 1)
  prettycheck:::assert_choice(direction, c(-1, 1))

  colors <- c(
    get_brand_color("primary"),
    get_brand_color("white"),
    get_brand_color("secondary")
  )

  if (length(n > 1) && all(dplyr::between(n, 0, 1), na.rm = TRUE)) {
    make_color_vector(
      n_prop = n,
      direction = direction,
      colors = colors
    )
  } else {
    make_color_vector(
      n = n,
      direction = direction,
      colors = colors
    )
  }
}

# library(prettycheck) # github.com/danielvartan/prettycheck

color_brand_qualitative <- function(n, direction = 1) {
  prettycheck:::assert_integerish(n, lower = 1, min.len = 1)
  prettycheck:::assert_choice(direction, c(-1, 1))

  base <- c(
    "#006909", # get_brand_color("primary")
    "#ED6B4D", # get_brand_color("secondary")
    "#513024", # get_brand_color("tertiary")
    "#9ECD7A", # get_brand_color("light-green")
    "#B44522", # get_brand_color("dark-red")
    "#272B33" # get_brand_color("black")
  )

  if (direction == -1) base <- rev(base)

  rep(base, length.out = n)
}

# library(here)
# library(prettycheck) # github.com/danielvartan/prettycheck
# library(yaml)

get_brand_color <- function(color) {
  brands_list <- yaml::read_yaml(here::here("_brand.yml"))

  palette_names <- brands_list$color$palette |> names()
  theme_names <- brands_list$color |> names()

  choices <- c(palette_names, theme_names)

  prettycheck:::assert_choice(color, choices)

  if (color %in% theme_names) {
    for (i in theme_names) {
      if (color == i) {
        color <- brands_list$color[[i]]
      }
    }
  }

  brands_list$color$palette[[color]]
}

# library(here)
# library(prettycheck) # github.com/danielvartan/prettycheck
# library(yaml)

get_brand_font <- function(type) {
  brands_list <- yaml::read_yaml(here::here("_brand.yml"))

  choices <- c(
    "base", "headings", "monospace", "monospace-inline", "monospace-block"
  )

  choices <- choices[choices %in% names(brands_list$typography)]

  prettycheck:::assert_choice(type, choices)

  if (is.null(names(brands_list$typography[[type]]))) {
    brands_list$typography[[type]]
  } else {
    brands_list$typography[[type]]$family
  }
}

# library(grDevices)
# library(prettycheck) # github.com/danielvartan/prettycheck

# # Helper
#
# get_brand_color_tint(c(1, seq(100, 1000, 100))) |> vector_to_c()
#
# c(1, seq(100, 1000, 100))
# #> [1]    1  100  200  300  400  500  600  700  800  900 1000
#
# c("#000000", "#2E1700", "#5D2F00", "#8C4700", "#BA5F00", "#E97600",
#   "#EE9233", "#F2AD66", "#F6C899", "#FAE3CC", "#FFFFFF")

get_brand_color_tint <- function(
    position = 500,
    color = "primary",
    n = 1000
  ) {
  prettycheck:::assert_integerish(position, lower = 0, upper = 1000)
  prettycheck:::assert_integer_number(n, lower = 1)

  color <- get_brand_color(color)

  color_fun <- grDevices::colorRampPalette(c("black", color, "white"))
  color_values <- color_fun(n)

  color_values[position]
}

# library(grDevices)
# library(prettycheck) # github.com/danielvartan/prettycheck

# # Helpers
#
# make_color_vector(10) |> vector_to_c()
# make_color_vector(n_prop = c(0.1, 0.5, 0.99)) |> vector_to_c()

make_color_vector <- function(
    n = NULL,
    colors = c(get_brand_color("primary"), get_brand_color("secondary")),
    direction = 1,
    values = NULL,
    n_prop = NULL,
    n_prop_res = 10000,
    ...
  ) {
  prettycheck:::assert_integerish(n, lower = 1, null.ok = TRUE)
  for (i in colors) prettycheck:::assert_color(i)
  prettycheck:::assert_choice(direction, c(-1, 1))

  prettycheck:::assert_numeric(
    n_prop, lower = 0, upper = 1, null_ok = TRUE
  )

  prettycheck:::assert_integer_number(n_prop_res, lower = 1)

  if (direction == -1) colors <- rev(colors)

  color_fun <- function(n) {
    color_ramp_fun <- grDevices::colorRampPalette(colors, ...)

    dplyr::case_when(
      n == 0 ~ color_ramp_fun(1),
      is.na(n) ~ NA,
      TRUE ~ color_ramp_fun(n)
    )
  }

  if (!is.null(n_prop)) {
    n <- n_prop * n_prop_res
    n <- ifelse(n == 0, 1, n)

    color_values <- color_fun(n_prop_res)[n]
    n <- length(n)
  } else {
    color_values <- color_fun(n)
  }

  if (!is.null(values)){
    prettycheck:::assert_atomic(values, len = n)

    names(color_values) <- as.character(values)
  }

  color_values
}

# library(grDevices)

gg_color_hue <- function(n) {
  hues = seq(15, 375, length = n + 1)
  grDevices::hcl(h = hues, l = 65, c = 100)[1:n]
}

# library(ggplot2)

# Colors based on a data visualization found in @roenneberg2019b.

scale_fill_rainbow <- function(direction = 1) {
  colors <- c(
    "#FF1E00", "#FF7C00", "#FDD400", "#00CB00", "#009DF5",
    "#0040F7", "#981EAF"
  )

  if (direction == -1) colors <- rev(colors)

  ggplot2::scale_fill_manual(values = colors)
}
