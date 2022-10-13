# COVID-19 Mask Wearing: Longitudinal Analyses

Using longitudinal random-intercept cross-lagged panel modelling to study the antecedents of mask wearing in the United States.

## Getting Started

### Installing

To run this code, you will need to [install R](https://www.r-project.org/) and the following R packages:

```
install.packages(c("arm", "broom.mixed", "cowplot", "dagitty", "ggdag", 
                   "ggeffects", "ggraph", "huxtable", "jtools", "knitr",
                   "kableExtra", "lavaan", "lme4", "lubridate", "MuMIn",
                   "papaja", "rnaturalearth", "rnaturalearthdata", "scales", 
                   "semTools", "sf", "targets", "tarchetypes", "tidyverse",
                   "zipcodeR"))
```

### Executing code

1. Set the working directory to this code repository `setwd("myPath/covidMaskWearing")`
2. Load the `targets` package with `library(targets)`
3. To run all analyses, run `tar_make()`
4. To load individual targets into your environment, run `tar_load(riclpm)` etc.
5. You can then view the model parameters by loading the `lavaan` package with `library(lavaan)` and then running `summary(riclpm)`

## Help

Any issues, please email scott.claessens@gmail.com.

## Authors

Scott Claessens, scott.claessens@gmail.com
