# gs-data-analysis-report-3

<!-- badges: start -->
[![Project Status: WIP – Initial development is in progress, but there has not yet been a stable, usable release suitable for the public.](https://www.repostatus.org/badges/latest/wip.svg)](https://www.repostatus.org/#wip)
[![License: MIT](https://aimg.shields.io/badge/license-MIT-green)](https://choosealicense.com/licenses/mit/)
<!-- badges: end -->

## Overview

🍚🌾🌱🌍🇧🇷📊📈🚜🌽💧☀️🌦️👩‍🌾👨‍🌾🌳📅

This repository contains a report focuses on exploring associations between data from Brazil's Food and Nutrition Surveillance System ([SISVAN](https://sisaps.saude.gov.br/sisvan/)) and climate variables.

The report is available [here](https://sustentarea.github.io/gs-data-analysis-report-3/).

## How to use

The analyses contained in this document are 100% reproducible. They were made using the [R programming language](https://www.r-project.org/) and the  [Quarto](https://quarto.org/) publishing system. The [`renv`](https://rstudio.github.io/renv/) package was used to ensure that the R environment used can be restored (see `renv.lock`). The computational notebook is available in the `index.qmd` file.

To reproduce the analyses do the following steps:

1. Clone this repository.
1. Open the R project (`gs-data-analysis-report-3.Rproj`).
1. Run [`renv::restore()`](https://rstudio.github.io/renv//reference/restore.html) to install all software dependencies.
1. Open and run the analysis in the computational notebook.

## How to cite

To cite this work, please use the following format:

Vartanian, D., & Magalhães, A. R.(2024). *Global syndemic data analysis report 3: SISVAN data analysis*. Sustentarea Research and Extension Group at the University of São Paulo. https://sustentarea.github.io/gs-data-analysis-report-3

A BibTeX entry for LaTeX users is

```         
@techreport{magalhaes2024,
  title = {Global syndemic data analysis report 3: SISVAN data analysis},
  author = {{Daniel Vartanian} and {Arthur Ramalho Magalhães}},
  year = {2025},
  address = {São Paulo},
  institution = {Sustentarea Research and Extension Group at the University of São Paulo},
  langid = {en},
  url = {https://sustentarea.github.io/gs-data-analysis-report-3}
}
```

## Acknowledgments

<table>
  <tr>
    <td width="30%">
      <br>
      <p align="center">
        <a href="https://www.gov.br/cnpq/"><img src="images/sustentarea-logo.png" width="120em"/></a>
      </p>
      <br>
    </td>
    <td width="70%">
      This analysis is part of the <a href="https://www.fsp.usp.br/sustentarea">Sustentarea</a> Research and Extension Group's project: <em>Global syndemic: the impact of anthropogenic climate change on the health and nutrition of children under five years old attended by Brazil's public health system (SUS)</em>.
    </td>
  </tr>
</table>

<table>
  <tr>
    <td width="30%"">
      <br>
      <p align="center">
        <br> <a href="https://www.gov.br/cnpq/"><img src="images/cnpq-logo.png" width="150em"/></a> 
      </p>
      <br>
    </td>
    <td width="70%">
      This research was supported by the Conselho Nacional de Desenvolvimento Científico e Tecnológico - Brazil (<a href="https://www.gov.br/cnpq/">CNPq</a>).
    </td>
  </tr>
</table>
