## Based on <https://github.com/hadley/r4ds/blob/main/_common.R>.

# Load packages -----

library(downlit)
library(ggplot2)
# library(here)
# library(knitr)
library(lubridate)
library(magrittr)
library(ragg)
library(rlang)
# library(rutils) # github.com/danielvartan/rutils
library(showtext)
library(sysfonts)
library(targets)
library(xml2)
# library(yaml)

## Load functions -----

source(here::here("R", "utils.R"))
source(here::here("R", "utils-plots.R"))

# Set general options -----

options(
  dplyr.print_min = 6,
  dplyr.print_max = 6,
  pillar.max_footer_lines = 2,
  pillar.min_chars = 15,
  scipen = 10,
  digits = 10,
  stringr.view_n = 6,
  pillar.bold = TRUE,
  width = 77 # 80 - 3 for #> comment
)

# Set variables -----

set.seed(2025)

# Set knitr -----

knitr::clean_cache() |> rutils:::shush()

knitr::opts_chunk$set(
  comment = "#>",
  collapse = TRUE,
  root.dir = here::here(),
  dev = "ragg_png",
  dev.args = list(bg = "transparent"),
  fig.showtext = TRUE
)

# Set and load graph fonts -----

sysfonts::font_paths(here::here("ttf"))

sysfonts::font_add(
  family = "poppins",
  regular = here::here("ttf", "poppins-regular.ttf"),
  bold = here::here("ttf", "poppins-bold.ttf"),
  italic = here::here("ttf", "poppins-italic.ttf"),
  bolditalic = here::here("ttf", "poppins-bolditalic.ttf"),
  symbol = NULL
)

sysfonts::font_add(
  family = "dm-sans",
  regular = here::here("ttf", "dmsans-regular.ttf"),
  bold = here::here("ttf", "dmsans-bold.ttf"),
  italic = here::here("ttf", "dmsans-italic.ttf"),
  bolditalic = here::here("ttf", "dmsans-bolditalic.ttf"),
  symbol = NULL
)

sysfonts::font_add(
  family = "dm-mono",
  regular = here::here("ttf", "dmmono-medium.ttf"),
  italic = here::here("ttf", "dmmono-mediumitalic.ttf"),
  symbol = NULL
)

showtext::showtext_auto()

# Set `ggplot2` theme -----

ggplot2::theme_set(
  ggplot2::theme_bw() +
    ggplot2::theme(
      text = ggplot2::element_text(
        color = get_brand_color("grey"),
        family = "dm-sans",
        face = "plain"
      ),
      panel.background = ggplot2::element_rect(fill = "transparent"),
      plot.background = ggplot2::element_rect(
        fill = "transparent", color = NA
      ),
      panel.grid.major = ggplot2::element_blank(),
      panel.grid.minor = ggplot2::element_blank(),
      legend.background = ggplot2::element_rect(fill = "transparent"),
      legend.box.background = ggplot2::element_rect(
        fill = "transparent", color = NA
      ),
      legend.frame = ggplot2::element_blank(),
      legend.ticks = ggplot2::element_line(color = "white")
    )
)

# thematic::thematic_on(
#   bg = NULL,
#   fg = get_brand_color("grey"),
#   accent = get_brand_color("red"),
#   font = "dm-sans"
# )

# Run `rbbt` -----

# (2024-08-25)
# This function should work with any version of BetterBibTeX (BBT) for Zotero.
# Verify if @wmoldham PR was merged in the `rbbt` package (see issue #47
# <https://github.com/paleolimbot/rbbt/issues/47>). If not, install `rbbt`
# from @wmoldham fork `renv::install("wmoldham/rbbt")`.

rutils:::bbt_write_quarto_bib(
  bib_file = here::here("references.bib"),
  dir = c("."),
  pattern = "\\.qmd$",
  wd = here::here()
)
