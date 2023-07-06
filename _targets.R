library(tidyverse)
library(targets)
library(tarchetypes)
source("R/functions.R")
options(tidyverse.quiet = TRUE)
tar_option_set(packages = c("arm", "broom.mixed", "cowplot", "dagitty", "emmeans",
                            "ggarchery", "ggdag", "ggeffects", "ggraph", "huxtable",
                            "jtools", "knitr","kableExtra", "lavaan", "lme4", 
                            "lmerTest", "lubridate", "MuMIn", "ordinal", "papaja", 
                            "psych", "rnaturalearth", "rnaturalearthdata", "scales",
                            "semTools", "sf", "zipcodeR"))
# workflow
list(
  # files
  tar_target(fileData, "data/usCohortCleanData_t1-t18.csv", format = "file"),
  tar_target(fileOwid, "data/owid-covid-data.csv", format = "file"),
  tar_target(fileStates, "data/states.csv", format = "file"),
  # load data
  tar_target(d, loadData(fileData, fileStates)),
  # load US covid counts from our world in data
  tar_target(owid, loadOwid(fileOwid)),
  # construct validity
  tar_target(constructVal, fitConstructValidityModel(d)),
  # interaction frequency
  tar_target(intFreq24hrs, fitIntFreqModel(d, pred = "Contacts24hrs")),
  tar_target(intFreq7days, fitIntFreqModel(d, pred = "Contacts7days")),
  # multilevel modelling
  # correlations between behaviour and norms
  tar_target(m1.1, fitCorBehNorm(d, predictor = "DesNorms")),
  tar_target(m1.2, fitCorBehNorm(d, predictor = "InjNorms")),
  # sensitivity to cdc guidelines
  tar_target(m2.1, fitCDCSens(d, outcome = "ContactsMask")),
  tar_target(m2.2, fitCDCSens(d, outcome = "DesNorms")),
  tar_target(m2.3, fitCDCSens(d, outcome = "InjNorms")),
  # confidence intervals
  tar_target(conf1.1, confint(m1.1, method = "Wald")),
  tar_target(conf1.2, confint(m1.2, method = "Wald")),
  tar_target(conf2.1, confint(m2.1, method = "Wald")),
  tar_target(conf2.2, confint(m2.2, method = "Wald")),
  tar_target(conf2.3, confint(m2.3, method = "Wald")),
  # change point parameters
  tar_target(pars2.1, getChangePointPars(m2.1, outcome = "Mask wearing")),
  tar_target(pars2.2, getChangePointPars(m2.2, outcome = "Descriptive norms")),
  tar_target(pars2.3, getChangePointPars(m2.3, outcome = "Injunctive norms")),
  # random-intercept cross-lagged panel models
  # unconstrained models
  tar_target(riclpm1_uncon, fitRICLPM1(d, constrained = FALSE)), # full model (direct causal effects)
  tar_target(riclpm2_uncon, fitRICLPM2(d, constrained = FALSE)), # reduced model without factual and personal beliefs (total causal effects)
  tar_target(riclpm3_uncon, fitRICLPM3(d, constrained = FALSE)), # reduced model without any covariates
  # constrained models
  tar_target(riclpm1_con, fitRICLPM1(d, constrained = TRUE)), # full model (direct causal effects)
  tar_target(riclpm2_con, fitRICLPM2(d, constrained = TRUE)), # reduced model without factual and personal beliefs (total causal effects)
  tar_target(riclpm3_con, fitRICLPM3(d, constrained = TRUE)), # reduced model without any covariates
  tar_target(riclpm4_con, fitRICLPM4(d, constrained = TRUE)), # multigroup: republican / democrat split
  # model comparisons
  tar_target(compare1, anova(riclpm1_uncon, riclpm1_con)),
  tar_target(compare2, anova(riclpm2_uncon, riclpm2_con)),
  tar_target(compare3, anova(riclpm3_uncon, riclpm3_con)),
  # fit measures
  tar_target(fitMeasures1_uncon, fitMeasures(riclpm1_uncon)),
  tar_target(fitMeasures2_uncon, fitMeasures(riclpm2_uncon)),
  tar_target(fitMeasures3_uncon, fitMeasures(riclpm3_uncon)),
  tar_target(fitMeasures1_con,   fitMeasures(riclpm1_con)),
  tar_target(fitMeasures2_con,   fitMeasures(riclpm2_con)),
  tar_target(fitMeasures3_con,   fitMeasures(riclpm3_con)),
  tar_target(fitMeasures4_con,   fitMeasures(riclpm4_con)),
  # plots
  tar_target(plot01, plotUSMap(d)),
  tar_target(plot02, plotAttrition(d)),
  tar_target(plot03, plotTimeline(d, owid)),
  tar_target(plot04, plotDAG()),
  tar_target(plot05, plotCorBehNorm(m1.1, m1.2)),
  tar_target(plot06, plotCDCSens(m2.1, m2.2, m2.3)),
  tar_target(plot07, plotRICLPMPath(riclpm1_uncon)),
  tar_target(plot08, plotRICLPMPath(riclpm2_uncon)),
  tar_target(plot09, plotRICLPMPath(riclpm3_uncon)),
  tar_target(plot10, plotRICLPMCoef(riclpm1_uncon)),
  tar_target(plot11, plotRICLPMCoef(riclpm2_uncon)),
  tar_target(plot12, plotRICLPMCoef(riclpm3_uncon)),
  tar_target(plot13, plotAttritionBreakdown(d)),
  tar_target(plot14, plotIntFreqModels(intFreq24hrs, intFreq7days)),
  # tables
  tar_target(constrainedTable1, makeConstrainedTable(riclpm1_con)),
  tar_target(constrainedTable2, makeConstrainedTable(riclpm2_con)),
  tar_target(constrainedTable3, makeConstrainedTable(riclpm3_con)),
  tar_target(constrainedTable4, makeConstrainedTable(riclpm4_con, multigroup = TRUE)),
  tar_target(itemTable, makeItemTable(d)),
  tar_target(changePointsTable, makeChangePointsTable(m2.1, m2.2, m2.3, pars2.1, pars2.2, pars2.3)),
  tar_target(unconstrainedTable1, makeUnconstrainedTable(riclpm1_uncon)),
  tar_target(unconstrainedTable2, makeUnconstrainedTable(riclpm2_uncon)),
  tar_target(unconstrainedTable3, makeUnconstrainedTable(riclpm3_uncon)),
  tar_target(constructValTable, makeConstructValTable(constructVal)),
  # manuscript
  tar_render(manuscript, "manuscript.Rmd"),
  # print session info for reproducibility
  tar_target(sessionInfo, writeLines(capture.output(sessionInfo()), "sessionInfo.txt"))
)