
<!-- README.md is generated from README.Rmd. Please edit that file -->

# biodivercity <a href='https://ecological-cities.github.io/biodivercity/'><img src='man/figures/logo.png' align="right" height="139" /></a>

<!-- badges: start -->

[![License: GPL
v3](https://img.shields.io/badge/License-GPLv3-blue.svg)](https://www.gnu.org/licenses/gpl-3.0)
[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://www.tidyverse.org/lifecycle/#experimental)
[![CRAN
status](https://www.r-pkg.org/badges/version/biodivercity)](https://CRAN.R-project.org/package=biodivercity)
[![R-CMD-check](https://github.com/ecological-cities/biodivercity/workflows/R-CMD-check/badge.svg)](https://github.com/ecological-cities/biodivercity/actions)
<!-- badges: end -->

Assess the diversity of animal species in urban areas. Refer to [package
website](https://ecological-cities.github.io/biodivercity/) for
demonstrations of how the package may be used.

<br>

## Overview

Multiple frameworks to assess urban biodiversity have been proposed in
both research and practice. Some conceptualise ‘biodiversity’ to include
genetic variation, landscape structure and other human-centric
dimensions such as climate regulation and ecosystem functioning. While
these offer a more comprehensive view of ‘total biodiversity’, certain
components may have shortcomings in terms of their (1) causal proximity
(e.g. indirect effect of the landscape on the diversity of other
species); (2) measurability at-scale (e.g. genetic diversity, plant
species diversity); and (3) precision (e.g. estimating water or thermal
regulation by vegetation). In decision-making, unnecessary complexity
may also risk contributing to mistrust or ignorance of the overall
assessment.

`biodivercity` is an R package for assessing the diversity of animal
species in urban areas. It provides a way to directly assess the
*habitat value* of urban landscapes, based on their empirical effect on
chosen animal groups (taxa). Examples for birds, butterflies, odonates
and amphibians are provided. The key features include:

-   Step-by-step protocols for random point sampling of an animal group
    within areas of interest.
-   Generate data summaries of the animals surveyed at multiple levels
    (e.g. areas, periods, animal groups, species).
-   Download and process landscape predictors within areas of interest
    (e.g. satellite imagery, OpenStreetMap data).
-   Build and validate predictive models to assess local (alpha),
    community (beta) and total (gamma) diversity for an animal group.
-   Convert vector data generated from future design scenarios into
    formats suitable for model predictions.
-   Use models to make pixel-based spatial predictions across new areas,
    for both monitoring and future scenario planning purposes.
-   Summarise predictions for city-wide scoring and benchmarking
    (i.e. allow comparisons to be made between zones used in city
    planning).

<br>

<img src="man/figures/framework.jpeg" width="70%" style="display: block; margin: auto;" />
<center>
<b> Figure: Broad overview of the data workflow for an animal group </b>
</center>

<br>

## Setup

Install the development version from [GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("ecological-cities/biodivercity", ref = "main")
```

Load the package:

``` r
library(biodivercity)
```

<br>

## Citation

To cite `home2park` or acknowledge its use, please cite the following:

*Tan, E. Y. W., Song, X. P., Sim, H. J., Nai, J., Chong, K. Y. (2022).
biodivercity: An R package to assess the diversity of animal species in
urban areas. R package version 0.0.1 (unreleased).
<https://github.com/ecological-cities/biodivercity>.*

To get a BibTex entry, run `citation("biodivercity")`.

<br>

## Data sources

Development of a Biodiversity Index for Residential Towns using
Biodiversity Field Surveys. 14 June 2016, 6.5 years, Ministry of
National Development Research Fund (MNDRF) Grant. Awarded to the
National University of Singapore (host institution) and the Singapore
Housing & Development Board.

<br>

## References

Baker, D. J., Maclean, I. M., Goodall, M., & Gaston, K. J. (2021).
Species distribution modelling is needed to support ecological impact
assessments. *Journal of Applied Ecology*, *58*(1), 21-26.

Soto-Navarro, C. A., Harfoot, M., Hill, S. L. L., Campbell, J., Mora,
F., Campos, C., … & Burgess, N. D. (2021). Towards a multidimensional
biodiversity index for national application. *Nature Sustainability*,
1-10.
