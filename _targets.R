library(targets)
library(tarchetypes)
source("R/functions.R")
options(tidyverse.quiet = TRUE)
tar_option_set(packages = c("cowplot", "dagitty", "ggdag", "ggraph", "lavaan", "lme4", 
                            "lubridate", "rnaturalearth", "rnaturalearthdata", "scales",
                            "sf", "tidyverse", "zipcodeR"))
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
  # compare average levels of descriptive and injunctive norms
  tar_target(desInj, fitNormCompare(d)),
  tar_target(conf, confint(desInj)),
  # fit riclpm
  tar_target(riclpm, fitRICLPM(d, var1 = "ContactsMask", var2 = "InjNorms", var3 = "DesNorms")),
  tar_target(fitMeasures, fitMeasures(riclpm)),
  # plots
  tar_target(plot1, plotUSMap(d)),
  tar_target(plot2, plotAttrition(d)),
  tar_target(plot3, plotTimeline(d, owid)),
  tar_target(plot4, plotDAG()),
  tar_target(plot5, plotRICLPM(riclpm))
)