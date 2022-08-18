library(tidyverse)
library(targets)
library(tarchetypes)
source("R/functions.R")
options(tidyverse.quiet = TRUE)
tar_option_set(packages = c("arm", "cowplot", "dagitty", "ggdag", "ggeffects", 
                            "ggraph", "huxtable", "jtools", "knitr", "kableExtra", 
                            "lavaan", "lme4", "lubridate", "MuMIn", "papaja", 
                            "rnaturalearth", "rnaturalearthdata", "scales", 
                            "sf", "zipcodeR"))
# workflow
list(
  # files
  tar_target(fileData, "data/cus_t1-15_noIDs_280422av2337.tsv", format = "file"),
  tar_target(fileFIPS, "data/ZIP-COUNTY-FIPS_2018-03.csv", format = "file"),
  tar_target(fileElec, "data/countypres_2020_HC.csv", format = "file"),
  tar_target(filePrev, "data/United_States_COVID-19_Community_Levels_by_County.csv", format = "file"),
  tar_target(fileOwid, "data/owid-covid-data.csv", format = "file"),
  # load data
  tar_target(d, loadData(fileData, fileFIPS, fileElec, filePrev)),
  # load US covid counts from our world in data
  tar_target(owid, loadOwid(fileOwid)),
  # t-tests: construct validity
  tar_target(constructValDes, testConstructValidity(d, type = "Descriptive")),
  tar_target(constructValInj, testConstructValidity(d, type = "Injunctive")),
  # multilevel modelling
  # correlations between behaviour and norms
  tar_target(m1.1, fitCorBehNorm(d, predictor = "DesNorms")),
  tar_target(m1.2, fitCorBehNorm(d, predictor = "InjNorms")),
  # sensitivity to cdc guidelines
  tar_target(m2.1, fitCDCSens(d, outcome = "ContactsMask")),
  tar_target(m2.2, fitCDCSens(d, outcome = "DesNorms")),
  tar_target(m2.3, fitCDCSens(d, outcome = "InjNorms")),
  # difference between levels of descriptive and injunctive norms
  tar_target(m3.1, fitNormCompare(d)),
  # confidence intervals
  tar_target(conf1.1, confint(m1.1, method = "Wald")),
  tar_target(conf1.2, confint(m1.2, method = "Wald")),
  tar_target(conf2.1, confint(m2.1, method = "Wald")),
  tar_target(conf2.2, confint(m2.2, method = "Wald")),
  tar_target(conf2.3, confint(m2.3, method = "Wald")),
  tar_target(conf3.1, confint(m3.1, method = "Wald")),
  # random-intercept cross-lagged panel model
  tar_target(riclpm, fitRICLPM(d, var1 = "ContactsMask", var2 = "InjNorms", var3 = "DesNorms")),
  tar_target(fitMeasures, fitMeasures(riclpm)),
  # plots
  tar_target(plot1, plotUSMap(d)),
  tar_target(plot2, plotAttrition(d)),
  tar_target(plot3, plotTimeline(d, owid)),
  tar_target(plot4, plotDAG()),
  tar_target(plot5, plotCorBehNorm(m1.1, m1.2)),
  tar_target(plot6, plotCDCSens(m2.1, m2.2, m2.3)),
  tar_target(plot7, plotNormCompare(m3.1)),
  tar_target(plot8, plotRICLPM(riclpm)),
  # tables
  tar_target(itemTable, makeItemTable()),
  tar_target(changePointsTable, makeChangePointsTable(m2.1, m2.2, m2.3)),
  tar_target(lavaanTable, makeLavaanTable(riclpm)),
  # manuscript
  tar_render(manuscript, "manuscript.Rmd"),
  # print session info for reproducibility
  tar_target(sessionInfo, writeLines(capture.output(sessionInfo()), "sessionInfo.txt"))
)