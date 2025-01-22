# gs-data-analysis-report-3

<!-- badges: start -->
[![Project Status: Inactive – The project has reached a stable, usable state but is no longer being actively developed; support/maintenance will be provided as time allows.](https://www.repostatus.org/badges/latest/inactive.svg)](https://www.repostatus.org/#inactive)
[![License: MIT](https://img.shields.io/badge/License-MIT-green.svg)](https://opensource.org/licenses/MIT)
<!-- badges: end -->

## Overview

🌧️🌡️🌍👶📆🇧🇷🧮📉🌾🌱🍚🌽🚜🌳💧☀️🌦️📈

This repository contains a report exploring potential associations between undernutrition in children and the Standardised Precipitation Evapotranspiration Index (SPEI) in Brazilian municipalities from 2008 to 2019. Access the report [here](https://sustentarea.github.io/gs-data-analysis-report-3/).

## How to use

The analyses contained in the report are 100% reproducible. They were made using the [R programming language](https://www.r-project.org/) and the  [Quarto](https://quarto.org/) publishing system. The [`renv`](https://rstudio.github.io/renv/) package was used to ensure that the R environment used can be restored (see `renv.lock`). The computational notebook is available in the `index.qmd` file.

To reproduce the analyses do the following steps:

1. Clone this repository.
1. Open the R project (`gs-data-analysis-report-3.Rproj`).
1. Run [`renv::restore()`](https://rstudio.github.io/renv//reference/restore.html) to install all software dependencies.
1. Open and run the analysis in the computational notebook.

## How to cite

To cite this work, please use the following format:

Magalhães, A. R & Vartanian, D. (2025). *Global syndemic data analysis: Report 3: SISVAN data analysis*. Sustentarea Research and Extension Group at the University of São Paulo. https://sustentarea.github.io/gs-data-analysis-report-3

A BibTeX entry for LaTeX users is

```         
@techreport{magalhaes2025,
  title = {Global syndemic project: data analysis: Report 3 - SISVAN data analysis},
  author = {{Arthur Ramalho Magalhães} and {Daniel Vartanian}},
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
        <a href="https://www.gov.br/cnpq/"><img src="images/sustentarea-icon-rgb-150-dpi.png" width="120em"/></a>
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
