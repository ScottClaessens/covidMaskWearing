# custom functions

loadData <- function(fileData, fileStates) {
  # load states data
  states <- read_csv(file = fileStates, show_col_types = FALSE)
  # load data
  out <-
    read_csv(file = fileData, show_col_types = FALSE) %>%
    # fix some mask motives items (7 coded as 8 in qualtrics)
    mutate_at(vars(contains("MaskMotives")), function(x) ifelse(x == 8, 7, x)) %>%
    mutate_at(vars(contains("MaskMotivates")), function(x) ifelse(x == 8, 7, x)) %>%
    # fix mask legality items
    mutate_at(vars(contains("MaskLegal")), function(x) ifelse(x %in% 1:2, x - 1, NA)) %>%
    # create composite measures
    mutate(
      # injunctive norms
      InjNorms.2  = (MaskRespect.2    + MaskEncouraged.2 ) / 2,
      InjNorms.3  = (MaskRespect.3    + MaskEncouraged.3 ) / 2,
      InjNorms.5  = (MaskRespect.5    + MaskEncouraged.5 ) / 2,
      InjNorms.9  = (MaskRespect.9    + MaskEncouraged.9 ) / 2,
      InjNorms.11 = (MaskRespect.11   + MaskEncouraged.11) / 2,
      InjNorms.13 = (MaskRespect.13   + MaskEncouraged.13) / 2,
      InjNorms.14 = (MaskRespect.14   + MaskEncouraged.14) / 2,
      InjNorms.15 = (MaskRespect.15   + MaskEncouraged.15) / 2,
      InjNorms.16 = (MaskRespect.16   + MaskEncouraged.16) / 2,
      InjNorms.17 = (MaskRespect.17   + MaskEncouraged.17) / 2,
      InjNorms.18 = (MaskRespect.18   + MaskEncouraged.18) / 2,
      # descriptive norms
      DesNorms.2  = (NeighborMask1.2  + NeighborMask2.2  ) / 2,
      DesNorms.3  = (NeighborMask1.3  + NeighborMask2.3  ) / 2,
      DesNorms.5  = (NeighborMask1.5  + NeighborMask2.5  ) / 2,
      DesNorms.9  = (NeighborMask1.9  + NeighborMask2.9  ) / 2,
      DesNorms.11 = (NeighborMask1.11 + NeighborMask2.11 ) / 2,
      DesNorms.13 = (NeighborMask1.13 + NeighborMask2.13 ) / 2,
      DesNorms.14 = (NeighborMask1.14 + NeighborMask2.14 ) / 2,
      DesNorms.15 = (NeighborMask1.15 + NeighborMask2.15 ) / 2,
      DesNorms.16 = (NeighborMask1.16 + NeighborMask2.16 ) / 2,
      DesNorms.17 = (NeighborMask1.17 + NeighborMask2.17 ) / 2,
      DesNorms.18 = (NeighborMask1.18 + NeighborMask2.18 ) / 2,
      # factual beliefs
      FactBeliefs.2  = (MaskMotives1.2  + MaskMotives2.2 ) / 2,
      FactBeliefs.4  = (MaskMotives1.4  + MaskMotives2.4 ) / 2,
      FactBeliefs.5  = (MaskMotives1.5  + MaskMotives2.5 ) / 2,
      FactBeliefs.7  = (MaskMotives1.7  + MaskMotives2.7 ) / 2,
      FactBeliefs.9  = (MaskMotives1.9  + MaskMotives2.9 ) / 2,
      FactBeliefs.11 = (MaskMotives1.11 + MaskMotives2.11) / 2,
      FactBeliefs.13 = (MaskMotives1.13 + MaskMotives2.13) / 2,
      FactBeliefs.14 = (MaskMotives1.14 + MaskMotives2.14) / 2,
      FactBeliefs.15 = (MaskMotives1.15 + MaskMotives2.15) / 2,
      FactBeliefs.16 = (MaskMotives1.16 + MaskMotives2.16) / 2,
      FactBeliefs.17 = (MaskMotivates1.17 + MaskMotivates2.17) / 2,
      FactBeliefs.18 = (MaskMotivates1.18 + MaskMotivates2.18) / 2,
      # personal norms
      PersNorms.2  = MaskMotives3.2,
      PersNorms.4  = MaskMotives3.4,
      PersNorms.5  = MaskMotives3.5,
      PersNorms.7  = MaskMotives3.7,
      PersNorms.9  = MaskMotives3.9,
      PersNorms.11 = MaskMotives3.11,
      PersNorms.13 = MaskMotives3.13,
      PersNorms.14 = MaskMotives3.14,
      PersNorms.15 = MaskMotives3.15,
      PersNorms.16 = MaskMotives3.16,
      PersNorms.17 = MaskMotivates3.17,
      PersNorms.18 = MaskMotivates3.18
    )
  # add data on zip codes
  zip <- zipcodeR::reverse_zipcode(unique(out$Zip.1)) %>% mutate(zipcode = as.numeric(zipcode))
  out <- 
    out %>%
    left_join(zip, by = c("Zip.1" = "zipcode")) %>%
    # state-level election results
    left_join(states, by = c("state" = "StateAbbr"))
  return(out)
}

# load US covid counts from our world in data
loadOwid <- function(fileOwid) {
  read_csv(fileOwid) %>%
    # lubridate
    mutate(date = dmy(date))
}

# construct validity
fitConstructValidityModel <- function(d) {
  # vector of items
  items <- c(
    "1.7" = "MaskEncouraged",
    "2.7" = "MaskRespect",
    "3.7" = "NeighborMask2",
    "4.7" = "NeighborMask1"
  )
  # modify data
  out <-
    d %>%
    mutate(id = 1:nrow(d)) %>%
    dplyr::select(id, starts_with("DescriptiveLearning") | starts_with("InjunctiveLearning")) %>%
    pivot_longer(cols = !id) %>%
    mutate(
      value = value,
      Type = ifelse(str_starts(name, "Descriptive"), "Descriptive", "Injunctive"),
      Item = as.character(items[str_sub(name, -3, -1)])
    ) %>%
    drop_na() %>%
    # fit model
    lmer(value ~ Type*Item + (1 | id), data = .) %>%
    emmeans(., ~ Item | Type) %>%
    pairs()
  return(out)
}

# interaction frequency
fitIntFreqModel <- function(d, pred = "") {
  # modify data
  out <-
    d %>%
    mutate(
      # unique participant ids
      id = 1:nrow(d),
      # characters to numeric for analysis
      Contacts24hrs.17 = as.numeric(Contacts24hrs.17),
      Contacts24hrs.18 = as.numeric(Contacts24hrs.18),
      Contacts7days.17 = as.numeric(Contacts7days.17),
      Contacts7days.18 = as.numeric(Contacts7days.18)
      ) %>%
    dplyr::select(id, starts_with("Contacts24hrs") | starts_with("Contacts7days") | starts_with("ContactsMask")) %>%
    pivot_longer(
      cols = !id,
      names_to = c(".value", "time"),
      names_sep = "\\."
    ) %>% 
    drop_na() %>%
    mutate(
      logContacts24hrs = log(Contacts24hrs + 1),
      logContacts7days = log(Contacts7days + 1)
      ) %>%
    # fit model
    lmer(as.formula(paste0("ContactsMask ~ log", pred, " + (1 | id) + (1 | time)")), data = .)
  return(out)
}

# plot interaction frequency models
plotIntFreqModels <- function(intFreq24hrs, intFreq7days) {
  # model predictions - 24 hrs
  pA <-
    ggpredict(intFreq24hrs, terms = "logContacts24hrs") %>%
    ggplot(aes(x, predicted)) +
    geom_jitter(data = intFreq24hrs@frame, aes(x = logContacts24hrs, y = ContactsMask),
                width = 0.5, height = 0.4, alpha = 0.05, size = 0.5) +
    geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = 0.2) +
    geom_line() +
    scale_y_continuous(name = "Mask wearing", breaks = 1:5, limits = c(0.5, 5.5)) +
    scale_x_continuous(name = "ln(Number of in-person interactions\nin last 24 hours + 1)") +
    theme_classic()
  # model predictions - 7 days
  pB <-
    ggpredict(intFreq7days, terms = "logContacts7days") %>%
    ggplot(aes(x, predicted)) +
    geom_jitter(data = intFreq7days@frame, aes(x = logContacts7days, y = ContactsMask),
                width = 0.5, height = 0.4, alpha = 0.05, size = 0.5) +
    geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = 0.2) +
    geom_line() +
    scale_y_continuous(name = "", breaks = 1:5, limits = c(0.5, 5.5)) +
    scale_x_continuous(name = "ln(Number of in-person interactions\nin last 7 days + 1)") +
    theme_classic()
  # put together
  out <- plot_grid(pA, pB, nrow = 1, labels = c("a", "b"))
  # save
  ggsave(out, filename = "figures/pdf/intFreq.pdf", width = 8, height = 4)
  ggsave(out, filename = "figures/png/intFreq.png", width = 8, height = 4)
  return(out)
}

# correlations between behaviour and norms
fitCorBehNorm <- function(d, predictor) {
  # get data in long format
  dLong <-
    d %>%
    dplyr::select(starts_with("DesNorms") | starts_with("InjNorms") | starts_with("ContactsMask")) %>%
    mutate(id = 1:nrow(.)) %>%
    pivot_longer(cols = !id, 
                 names_to = c(".value", "time"),
                 names_sep = "\\.")
  # fit multilevel model
  f <- formula(paste0("ContactsMask ~ 1 + ", predictor, " + (1 + ", predictor, 
               " | id) + (1 + ", predictor, " | time)"))
  out <- lmer(f, data = dLong)
  return(out)
}

# cdc sensitivity
fitCDCSens <- function(d, outcome) {
  # vector of dates
  dates <- c("2020-09-27", "2020-10-27", "2020-11-28",
             "2020-12-28", "2021-01-27", "2021-02-26",
             "2021-03-28", "2021-04-27", "2021-05-27",
             "2021-06-26", "2021-07-26", "2021-08-26",
             "2021-10-25", "2021-12-16", "2022-02-25",
             "2022-04-26", "2022-06-25", "2022-08-29")
  # get data in long format
  dLong <-
    d %>%
    dplyr::select(starts_with("DesNorms") | starts_with("InjNorms") | starts_with("ContactsMask")) %>%
    mutate(id = 1:nrow(.)) %>%
    pivot_longer(cols = !id, 
                 names_to = c(".value", "time"),
                 names_sep = "\\.") %>%
    # time as numeric dates, scaled between 0 and 1
    mutate(timeCont = as.numeric(ymd(dates[as.numeric(time)])),
           timeCont = timeCont - min(timeCont),
           timeCont = timeCont / max(timeCont))
  # formula with cutpoints at cdc events
  f <- formula(paste0(outcome, " ~ 1 + timeCont + I(pmax(0,timeCont-0.231)) + I(pmax(0,timeCont-0.432)) + I(pmax(0,timeCont-0.687)) + (1 | id)"))
  # fit model
  out <- lmer(f, data = dLong)
  return(out)
}

# full model
fitRICLPM1 <- function(d, constrained = FALSE) {
  # prepare covariates for modelling
  d <-
    d %>%
    # fill in missing age observation
    mutate(Age.1 = ifelse(is.na(Age.1), 57, Age.1)) %>%
    # prepare covariates
    mutate(
      Sex.1 = ifelse(Sex.1 == 3, NA, Sex.1 - 1),
      AgeStd.1 = as.numeric(scale(Age.1)),
      EthnicityBlack.1 = ifelse(Ethnicity.1 == "Black or African American", 1, 0),
      EthnicityAsian.1 = ifelse(Ethnicity.1 == "Asian or Pacific Islander", 1, 0),
      EthnicityHispanic.1 = ifelse(Ethnicity.1 == "Hispanic or Latino/a", 1, 0),
      EthnicityOther.1 = ifelse(Ethnicity.1 %in% c("Native American", "Other"), 1, 0),
      PoliticalOrientationStd.1 = as.numeric(scale(PoliticalOrientation.1)),
      # SES is mean average of education, income, and subjective SES
      SESStd.1 = as.numeric(scale((Education.1 + Income.1 + SSES.1) / 3))
    )
  # model code for riclpm
  # https://jeroendmulder.github.io/RI-CLPM/lavaan.html
  model <- '# Create between components (random intercepts)
            ri1 =~ 1*ContactsMask.2 + 1*ContactsMask.5 + 1*ContactsMask.9 + 1*ContactsMask.11 + 1*ContactsMask.13 + 1*ContactsMask.14 + 1*ContactsMask.15 + 1*ContactsMask.16 + 1*ContactsMask.17 + 1*ContactsMask.18
            ri2 =~ 1*InjNorms.2 + 1*InjNorms.5 + 1*InjNorms.9 + 1*InjNorms.11 + 1*InjNorms.13 + 1*InjNorms.14 + 1*InjNorms.15 + 1*InjNorms.16 + 1*InjNorms.17 + 1*InjNorms.18
            ri3 =~ 1*DesNorms.2 + 1*DesNorms.5 + 1*DesNorms.9 + 1*DesNorms.11 + 1*DesNorms.13 + 1*DesNorms.14 + 1*DesNorms.15 + 1*DesNorms.16 + 1*DesNorms.17 + 1*DesNorms.18
            ri4 =~ 1*FactBeliefs.2 + 1*FactBeliefs.5 + 1*FactBeliefs.9 + 1*FactBeliefs.11 + 1*FactBeliefs.13 + 1*FactBeliefs.14 + 1*FactBeliefs.15 + 1*FactBeliefs.16 + 1*FactBeliefs.17 + 1*FactBeliefs.18
            ri5 =~ 1*PersNorms.2 + 1*PersNorms.5 + 1*PersNorms.9 + 1*PersNorms.11 + 1*PersNorms.13 + 1*PersNorms.14 + 1*PersNorms.15 + 1*PersNorms.16 + 1*PersNorms.17 + 1*PersNorms.18
            
            # Create within-person centered variables
            w1_02 =~ 1*ContactsMask.2
            w1_05 =~ 1*ContactsMask.5
            w1_09 =~ 1*ContactsMask.9
            w1_11 =~ 1*ContactsMask.11
            w1_13 =~ 1*ContactsMask.13
            w1_14 =~ 1*ContactsMask.14
            w1_15 =~ 1*ContactsMask.15
            w1_16 =~ 1*ContactsMask.16
            w1_17 =~ 1*ContactsMask.17
            w1_18 =~ 1*ContactsMask.18
            
            w2_02 =~ 1*InjNorms.2
            w2_05 =~ 1*InjNorms.5
            w2_09 =~ 1*InjNorms.9
            w2_11 =~ 1*InjNorms.11
            w2_13 =~ 1*InjNorms.13
            w2_14 =~ 1*InjNorms.14
            w2_15 =~ 1*InjNorms.15
            w2_16 =~ 1*InjNorms.16
            w2_17 =~ 1*InjNorms.17
            w2_18 =~ 1*InjNorms.18
            
            w3_02 =~ 1*DesNorms.2
            w3_05 =~ 1*DesNorms.5
            w3_09 =~ 1*DesNorms.9
            w3_11 =~ 1*DesNorms.11
            w3_13 =~ 1*DesNorms.13
            w3_14 =~ 1*DesNorms.14
            w3_15 =~ 1*DesNorms.15
            w3_16 =~ 1*DesNorms.16
            w3_17 =~ 1*DesNorms.17
            w3_18 =~ 1*DesNorms.18
            
            w4_02 =~ 1*FactBeliefs.2
            w4_05 =~ 1*FactBeliefs.5
            w4_09 =~ 1*FactBeliefs.9
            w4_11 =~ 1*FactBeliefs.11
            w4_13 =~ 1*FactBeliefs.13
            w4_14 =~ 1*FactBeliefs.14
            w4_15 =~ 1*FactBeliefs.15
            w4_16 =~ 1*FactBeliefs.16
            w4_17 =~ 1*FactBeliefs.17
            w4_18 =~ 1*FactBeliefs.18
            
            w5_02 =~ 1*PersNorms.2
            w5_05 =~ 1*PersNorms.5
            w5_09 =~ 1*PersNorms.9
            w5_11 =~ 1*PersNorms.11
            w5_13 =~ 1*PersNorms.13
            w5_14 =~ 1*PersNorms.14
            w5_15 =~ 1*PersNorms.15
            w5_16 =~ 1*PersNorms.16
            w5_17 =~ 1*PersNorms.17
            w5_18 =~ 1*PersNorms.18
            
            # Regression of observed variables on time-invariant controls
            ContactsMask.2 + ContactsMask.5 + ContactsMask.9 + ContactsMask.11 + ContactsMask.13 + ContactsMask.14 + ContactsMask.15 + ContactsMask.16 + ContactsMask.17 + ContactsMask.18 ~ Sex.1 + AgeStd.1 + EthnicityBlack.1 + EthnicityAsian.1 + EthnicityHispanic.1 + EthnicityOther.1 + PoliticalOrientation.1 + SESStd.1
            InjNorms.2 + InjNorms.5 + InjNorms.9 + InjNorms.11 + InjNorms.13 + InjNorms.14 + InjNorms.15 + InjNorms.16 + InjNorms.17 + InjNorms.18 ~ Sex.1 + AgeStd.1 + EthnicityBlack.1 + EthnicityAsian.1 + EthnicityHispanic.1 + EthnicityOther.1 + PoliticalOrientation.1 + SESStd.1
            DesNorms.2 + DesNorms.5 + DesNorms.9 + DesNorms.11 + DesNorms.13 + DesNorms.14 + DesNorms.15 + DesNorms.16 + DesNorms.17 + DesNorms.18 ~ Sex.1 + AgeStd.1 + EthnicityBlack.1 + EthnicityAsian.1 + EthnicityHispanic.1 + EthnicityOther.1 + PoliticalOrientation.1 + SESStd.1
            FactBeliefs.2 + FactBeliefs.5 + FactBeliefs.9 + FactBeliefs.11 + FactBeliefs.13 + FactBeliefs.14 + FactBeliefs.15 + FactBeliefs.16 + FactBeliefs.17 + FactBeliefs.18 ~ Sex.1 + AgeStd.1 + EthnicityBlack.1 + EthnicityAsian.1 + EthnicityHispanic.1 + EthnicityOther.1 + PoliticalOrientation.1 + SESStd.1
            PersNorms.2 + PersNorms.5 + PersNorms.9 + PersNorms.11 + PersNorms.13 + PersNorms.14 + PersNorms.15 + PersNorms.16 + PersNorms.17 + PersNorms.18 ~ Sex.1 + AgeStd.1 + EthnicityBlack.1 + EthnicityAsian.1 + EthnicityHispanic.1 + EthnicityOther.1 + PoliticalOrientation.1 + SESStd.1
            
            # Estimate the lagged effects between the within-person centered variables (with time-variant control)
            w1_05 ~ b11*w1_02 + b12*w2_02 + b13*w3_02 + b14*w4_02 + b15*w5_02
            w1_09 ~ b11*w1_05 + b12*w2_05 + b13*w3_05 + b14*w4_05 + b15*w5_05
            w1_11 ~ b11*w1_09 + b12*w2_09 + b13*w3_09 + b14*w4_09 + b15*w5_09
            w1_13 ~ b11*w1_11 + b12*w2_11 + b13*w3_11 + b14*w4_11 + b15*w5_11
            w1_14 ~ b11*w1_13 + b12*w2_13 + b13*w3_13 + b14*w4_13 + b15*w5_13
            w1_15 ~ b11*w1_14 + b12*w2_14 + b13*w3_14 + b14*w4_14 + b15*w5_14
            w1_16 ~ b11*w1_15 + b12*w2_15 + b13*w3_15 + b14*w4_15 + b15*w5_15
            w1_17 ~ b11*w1_16 + b12*w2_16 + b13*w3_16 + b14*w4_16 + b15*w5_16
            w1_18 ~ b11*w1_17 + b12*w2_17 + b13*w3_17 + b14*w4_17 + b15*w5_17
            
            w2_05 ~ b21*w1_02 + b22*w2_02 + b23*w3_02 + b24*w4_02 + b25*w5_02
            w2_09 ~ b21*w1_05 + b22*w2_05 + b23*w3_05 + b24*w4_05 + b25*w5_05
            w2_11 ~ b21*w1_09 + b22*w2_09 + b23*w3_09 + b24*w4_09 + b25*w5_09
            w2_13 ~ b21*w1_11 + b22*w2_11 + b23*w3_11 + b24*w4_11 + b25*w5_11
            w2_14 ~ b21*w1_13 + b22*w2_13 + b23*w3_13 + b24*w4_13 + b25*w5_13
            w2_15 ~ b21*w1_14 + b22*w2_14 + b23*w3_14 + b24*w4_14 + b25*w5_14
            w2_16 ~ b21*w1_15 + b22*w2_15 + b23*w3_15 + b24*w4_15 + b25*w5_15
            w2_17 ~ b21*w1_16 + b22*w2_16 + b23*w3_16 + b24*w4_16 + b25*w5_16
            w2_18 ~ b21*w1_17 + b22*w2_17 + b23*w3_17 + b24*w4_17 + b25*w5_17
            
            w3_05 ~ b31*w1_02 + b32*w2_02 + b33*w3_02 + b34*w4_02 + b35*w5_02
            w3_09 ~ b31*w1_05 + b32*w2_05 + b33*w3_05 + b34*w4_05 + b35*w5_05
            w3_11 ~ b31*w1_09 + b32*w2_09 + b33*w3_09 + b34*w4_09 + b35*w5_09
            w3_13 ~ b31*w1_11 + b32*w2_11 + b33*w3_11 + b34*w4_11 + b35*w5_11
            w3_14 ~ b31*w1_13 + b32*w2_13 + b33*w3_13 + b34*w4_13 + b35*w5_13
            w3_15 ~ b31*w1_14 + b32*w2_14 + b33*w3_14 + b34*w4_14 + b35*w5_14
            w3_16 ~ b31*w1_15 + b32*w2_15 + b33*w3_15 + b34*w4_15 + b35*w5_15
            w3_17 ~ b31*w1_16 + b32*w2_16 + b33*w3_16 + b34*w4_16 + b35*w5_16
            w3_18 ~ b31*w1_17 + b32*w2_17 + b33*w3_17 + b34*w4_17 + b35*w5_17
            
            w4_05 ~ b41*w1_02 + b42*w2_02 + b43*w3_02 + b44*w4_02 + b45*w5_02
            w4_09 ~ b41*w1_05 + b42*w2_05 + b43*w3_05 + b44*w4_05 + b45*w5_05
            w4_11 ~ b41*w1_09 + b42*w2_09 + b43*w3_09 + b44*w4_09 + b45*w5_09
            w4_13 ~ b41*w1_11 + b42*w2_11 + b43*w3_11 + b44*w4_11 + b45*w5_11
            w4_14 ~ b41*w1_13 + b42*w2_13 + b43*w3_13 + b44*w4_13 + b45*w5_13
            w4_15 ~ b41*w1_14 + b42*w2_14 + b43*w3_14 + b44*w4_14 + b45*w5_14
            w4_16 ~ b41*w1_15 + b42*w2_15 + b43*w3_15 + b44*w4_15 + b45*w5_15
            w4_17 ~ b41*w1_16 + b42*w2_16 + b43*w3_16 + b44*w4_16 + b45*w5_16
            w4_18 ~ b41*w1_17 + b42*w2_17 + b43*w3_17 + b44*w4_17 + b45*w5_17
            
            w5_05 ~ b51*w1_02 + b52*w2_02 + b53*w3_02 + b54*w4_02 + b55*w5_02
            w5_09 ~ b51*w1_05 + b52*w2_05 + b53*w3_05 + b54*w4_05 + b55*w5_05
            w5_11 ~ b51*w1_09 + b52*w2_09 + b53*w3_09 + b54*w4_09 + b55*w5_09
            w5_13 ~ b51*w1_11 + b52*w2_11 + b53*w3_11 + b54*w4_11 + b55*w5_11
            w5_14 ~ b51*w1_13 + b52*w2_13 + b53*w3_13 + b54*w4_13 + b55*w5_13
            w5_15 ~ b51*w1_14 + b52*w2_14 + b53*w3_14 + b54*w4_14 + b55*w5_14
            w5_16 ~ b51*w1_15 + b52*w2_15 + b53*w3_15 + b54*w4_15 + b55*w5_15
            w5_17 ~ b51*w1_16 + b52*w2_16 + b53*w3_16 + b54*w4_16 + b55*w5_16
            w5_18 ~ b51*w1_17 + b52*w2_17 + b53*w3_17 + b54*w4_17 + b55*w5_17
            
            # Estimate the covariance between the within-person centered variables at the first wave
            w1_02 ~~ w2_02
            w1_02 ~~ w3_02
            w1_02 ~~ w4_02
            w1_02 ~~ w5_02
            w2_02 ~~ w3_02
            w2_02 ~~ w4_02
            w2_02 ~~ w5_02
            w3_02 ~~ w4_02
            w3_02 ~~ w5_02
            w4_02 ~~ w5_02
            
            # Estimate the covariances between the residuals of the within-person centered variables
            w1_05 ~~ cov12*w2_05
            w1_09 ~~ cov12*w2_09
            w1_11 ~~ cov12*w2_11
            w1_13 ~~ cov12*w2_13
            w1_14 ~~ cov12*w2_14
            w1_15 ~~ cov12*w2_15
            w1_16 ~~ cov12*w2_16
            w1_17 ~~ cov12*w2_17
            w1_18 ~~ cov12*w2_18
            
            w1_05 ~~ cov13*w3_05
            w1_09 ~~ cov13*w3_09
            w1_11 ~~ cov13*w3_11
            w1_13 ~~ cov13*w3_13
            w1_14 ~~ cov13*w3_14
            w1_15 ~~ cov13*w3_15
            w1_16 ~~ cov13*w3_16
            w1_17 ~~ cov13*w3_17
            w1_18 ~~ cov13*w3_18
            
            w1_05 ~~ cov14*w4_05
            w1_09 ~~ cov14*w4_09
            w1_11 ~~ cov14*w4_11
            w1_13 ~~ cov14*w4_13
            w1_14 ~~ cov14*w4_14
            w1_15 ~~ cov14*w4_15
            w1_16 ~~ cov14*w4_16
            w1_17 ~~ cov14*w4_17
            w1_18 ~~ cov14*w4_18
            
            w1_05 ~~ cov15*w5_05
            w1_09 ~~ cov15*w5_09
            w1_11 ~~ cov15*w5_11
            w1_13 ~~ cov15*w5_13
            w1_14 ~~ cov15*w5_14
            w1_15 ~~ cov15*w5_15
            w1_16 ~~ cov15*w5_16
            w1_17 ~~ cov15*w5_17
            w1_18 ~~ cov15*w5_18
            
            w2_05 ~~ cov23*w3_05
            w2_09 ~~ cov23*w3_09
            w2_11 ~~ cov23*w3_11
            w2_13 ~~ cov23*w3_13
            w2_14 ~~ cov23*w3_14
            w2_15 ~~ cov23*w3_15
            w2_16 ~~ cov23*w3_16
            w2_17 ~~ cov23*w3_17
            w2_18 ~~ cov23*w3_18
            
            w2_05 ~~ cov24*w4_05
            w2_09 ~~ cov24*w4_09
            w2_11 ~~ cov24*w4_11
            w2_13 ~~ cov24*w4_13
            w2_14 ~~ cov24*w4_14
            w2_15 ~~ cov24*w4_15
            w2_16 ~~ cov24*w4_16
            w2_17 ~~ cov24*w4_17
            w2_18 ~~ cov24*w4_18
            
            w2_05 ~~ cov25*w5_05
            w2_09 ~~ cov25*w5_09
            w2_11 ~~ cov25*w5_11
            w2_13 ~~ cov25*w5_13
            w2_14 ~~ cov25*w5_14
            w2_15 ~~ cov25*w5_15
            w2_16 ~~ cov25*w5_16
            w2_17 ~~ cov25*w5_17
            w2_18 ~~ cov25*w5_18
            
            w3_05 ~~ cov34*w4_05
            w3_09 ~~ cov34*w4_09
            w3_11 ~~ cov34*w4_11
            w3_13 ~~ cov34*w4_13
            w3_14 ~~ cov34*w4_14
            w3_15 ~~ cov34*w4_15
            w3_16 ~~ cov34*w4_16
            w3_17 ~~ cov34*w4_17
            w3_18 ~~ cov34*w4_18
            
            w3_05 ~~ cov35*w5_05
            w3_09 ~~ cov35*w5_09
            w3_11 ~~ cov35*w5_11
            w3_13 ~~ cov35*w5_13
            w3_14 ~~ cov35*w5_14
            w3_15 ~~ cov35*w5_15
            w3_16 ~~ cov35*w5_16
            w3_17 ~~ cov35*w5_17
            w3_18 ~~ cov35*w5_18
            
            w4_05 ~~ cov45*w5_05
            w4_09 ~~ cov45*w5_09
            w4_11 ~~ cov45*w5_11
            w4_13 ~~ cov45*w5_13
            w4_14 ~~ cov45*w5_14
            w4_15 ~~ cov45*w5_15
            w4_16 ~~ cov45*w5_16
            w4_17 ~~ cov45*w5_17
            w4_18 ~~ cov45*w5_18
            
            # Estimate the variance and covariance of the random intercepts
            ri1 ~~ ri1
            ri2 ~~ ri2
            ri3 ~~ ri3
            ri4 ~~ ri4
            ri5 ~~ ri5
            
            ri1 ~~ ri2
            ri1 ~~ ri3
            ri1 ~~ ri4
            ri1 ~~ ri5
            ri2 ~~ ri3
            ri2 ~~ ri4
            ri2 ~~ ri5
            ri3 ~~ ri4
            ri3 ~~ ri5
            ri4 ~~ ri5
            
            # Estimate the (residual) variance of the within-person centered variables
            w1_02 ~~ w1_02
            w1_05 ~~ var1*w1_05
            w1_09 ~~ var1*w1_09
            w1_11 ~~ var1*w1_11
            w1_13 ~~ var1*w1_13
            w1_14 ~~ var1*w1_14
            w1_15 ~~ var1*w1_15
            w1_16 ~~ var1*w1_16
            w1_17 ~~ var1*w1_17
            w1_18 ~~ var1*w1_18
            
            w2_02 ~~ w2_02
            w2_05 ~~ var2*w2_05
            w2_09 ~~ var2*w2_09
            w2_11 ~~ var2*w2_11
            w2_13 ~~ var2*w2_13
            w2_14 ~~ var2*w2_14
            w2_15 ~~ var2*w2_15
            w2_16 ~~ var2*w2_16
            w2_17 ~~ var2*w2_17
            w2_18 ~~ var2*w2_18
            
            w3_02 ~~ w3_02
            w3_05 ~~ var3*w3_05
            w3_09 ~~ var3*w3_09
            w3_11 ~~ var3*w3_11
            w3_13 ~~ var3*w3_13
            w3_14 ~~ var3*w3_14
            w3_15 ~~ var3*w3_15
            w3_16 ~~ var3*w3_16
            w3_17 ~~ var3*w3_17
            w3_18 ~~ var3*w3_18
            
            w4_02 ~~ w4_02
            w4_05 ~~ var4*w4_05
            w4_09 ~~ var4*w4_09
            w4_11 ~~ var4*w4_11
            w4_13 ~~ var4*w4_13
            w4_14 ~~ var4*w4_14
            w4_15 ~~ var4*w4_15
            w4_16 ~~ var4*w4_16
            w4_17 ~~ var4*w4_17
            w4_18 ~~ var4*w4_18
            
            w5_02 ~~ w5_02
            w5_05 ~~ var5*w5_05
            w5_09 ~~ var5*w5_09
            w5_11 ~~ var5*w5_11
            w5_13 ~~ var5*w5_13
            w5_14 ~~ var5*w5_14
            w5_15 ~~ var5*w5_15
            w5_16 ~~ var5*w5_16
            w5_17 ~~ var5*w5_17
            w5_18 ~~ var5*w5_18
  
            # Estimate the means
            ContactsMask.2 + ContactsMask.5 + ContactsMask.9 + ContactsMask.11 + ContactsMask.13 + ContactsMask.14 + ContactsMask.15 + ContactsMask.16 + ContactsMask.17 + ContactsMask.18 ~ 1
            InjNorms.2 + InjNorms.5 + InjNorms.9 + InjNorms.11 + InjNorms.13 + InjNorms.14 + InjNorms.15 + InjNorms.16 + InjNorms.17 + InjNorms.18 ~ 1
            DesNorms.2 + DesNorms.5 + DesNorms.9 + DesNorms.11 + DesNorms.13 + DesNorms.14 + DesNorms.15 + DesNorms.16 + DesNorms.17 + DesNorms.18 ~ 1
            FactBeliefs.2 + FactBeliefs.5 + FactBeliefs.9 + FactBeliefs.11 + FactBeliefs.13 + FactBeliefs.14 + FactBeliefs.15 + FactBeliefs.16 + FactBeliefs.17 + FactBeliefs.18 ~ 1
            PersNorms.2 + PersNorms.5 + PersNorms.9 + PersNorms.11 + PersNorms.13 + PersNorms.14 + PersNorms.15 + PersNorms.16 + PersNorms.17 + PersNorms.18 ~ 1'
  # remove constraints for unconstrained model
  if (!constrained) {
    constraints <- c("b11","b12","b13","b14","b15",
                     "b21","b22","b23","b24","b25",
                     "b31","b32","b33","b34","b35",
                     "b41","b42","b43","b44","b45",
                     "b51","b52","b53","b54","b55",
                     "cov12","cov13","cov14","cov15","cov23",
                     "cov24","cov25","cov34","cov35","cov45",
                     "var1","var2","var3","var4","var5")
    for (i in constraints) model <- str_replace_all(model, fixed(paste0(i, "*")), "")
  }
  # fit model
  out <- lavaan(model, data = d, missing = "fiml", meanstructure = TRUE, int.ov.free = TRUE)
  return(out)
}

# reduced model (without factual and personal beliefs)
fitRICLPM2 <- function(d, constrained = FALSE) {
  # prepare covariates for modelling
  d <-
    d %>%
    # fill in missing age observation
    mutate(Age.1 = ifelse(is.na(Age.1), 57, Age.1)) %>%
    # prepare covariates
    mutate(
      Sex.1 = ifelse(Sex.1 == 3, NA, Sex.1 - 1),
      AgeStd.1 = as.numeric(scale(Age.1)),
      EthnicityBlack.1 = ifelse(Ethnicity.1 == "Black or African American", 1, 0),
      EthnicityAsian.1 = ifelse(Ethnicity.1 == "Asian or Pacific Islander", 1, 0),
      EthnicityHispanic.1 = ifelse(Ethnicity.1 == "Hispanic or Latino/a", 1, 0),
      EthnicityOther.1 = ifelse(Ethnicity.1 %in% c("Native American", "Other"), 1, 0),
      PoliticalOrientationStd.1 = as.numeric(scale(PoliticalOrientation.1)),
      # SES is mean average of education, income, and subjective SES
      SESStd.1 = as.numeric(scale((Education.1 + Income.1 + SSES.1) / 3))
    )
  # model code for riclpm
  # https://jeroendmulder.github.io/RI-CLPM/lavaan.html
  model <- '# Create between components (random intercepts)
            ri1 =~ 1*ContactsMask.2 + 1*ContactsMask.5 + 1*ContactsMask.9 + 1*ContactsMask.11 + 1*ContactsMask.13 + 1*ContactsMask.14 + 1*ContactsMask.15 + 1*ContactsMask.16 + 1*ContactsMask.17 + 1*ContactsMask.18
            ri2 =~ 1*InjNorms.2 + 1*InjNorms.5 + 1*InjNorms.9 + 1*InjNorms.11 + 1*InjNorms.13 + 1*InjNorms.14 + 1*InjNorms.15 + 1*InjNorms.16 + 1*InjNorms.17 + 1*InjNorms.18
            ri3 =~ 1*DesNorms.2 + 1*DesNorms.5 + 1*DesNorms.9 + 1*DesNorms.11 + 1*DesNorms.13 + 1*DesNorms.14 + 1*DesNorms.15 + 1*DesNorms.16 + 1*DesNorms.17 + 1*DesNorms.18
            
            # Create within-person centered variables
            w1_02 =~ 1*ContactsMask.2
            w1_05 =~ 1*ContactsMask.5
            w1_09 =~ 1*ContactsMask.9
            w1_11 =~ 1*ContactsMask.11
            w1_13 =~ 1*ContactsMask.13
            w1_14 =~ 1*ContactsMask.14
            w1_15 =~ 1*ContactsMask.15
            w1_16 =~ 1*ContactsMask.16
            w1_17 =~ 1*ContactsMask.17
            w1_18 =~ 1*ContactsMask.18
            
            w2_02 =~ 1*InjNorms.2
            w2_05 =~ 1*InjNorms.5
            w2_09 =~ 1*InjNorms.9
            w2_11 =~ 1*InjNorms.11
            w2_13 =~ 1*InjNorms.13
            w2_14 =~ 1*InjNorms.14
            w2_15 =~ 1*InjNorms.15
            w2_16 =~ 1*InjNorms.16
            w2_17 =~ 1*InjNorms.17
            w2_18 =~ 1*InjNorms.18
            
            w3_02 =~ 1*DesNorms.2
            w3_05 =~ 1*DesNorms.5
            w3_09 =~ 1*DesNorms.9
            w3_11 =~ 1*DesNorms.11
            w3_13 =~ 1*DesNorms.13
            w3_14 =~ 1*DesNorms.14
            w3_15 =~ 1*DesNorms.15
            w3_16 =~ 1*DesNorms.16
            w3_17 =~ 1*DesNorms.17
            w3_18 =~ 1*DesNorms.18
            
            # Regression of observed variables on time-invariant controls
            ContactsMask.2 + ContactsMask.5 + ContactsMask.9 + ContactsMask.11 + ContactsMask.13 + ContactsMask.14 + ContactsMask.15 + ContactsMask.16 + ContactsMask.17 + ContactsMask.18 ~ Sex.1 + AgeStd.1 + EthnicityBlack.1 + EthnicityAsian.1 + EthnicityHispanic.1 + EthnicityOther.1 + PoliticalOrientation.1 + SESStd.1
            InjNorms.2 + InjNorms.5 + InjNorms.9 + InjNorms.11 + InjNorms.13 + InjNorms.14 + InjNorms.15 + InjNorms.16 + InjNorms.17 + InjNorms.18 ~ Sex.1 + AgeStd.1 + EthnicityBlack.1 + EthnicityAsian.1 + EthnicityHispanic.1 + EthnicityOther.1 + PoliticalOrientation.1 + SESStd.1
            DesNorms.2 + DesNorms.5 + DesNorms.9 + DesNorms.11 + DesNorms.13 + DesNorms.14 + DesNorms.15 + DesNorms.16 + DesNorms.17 + DesNorms.18 ~ Sex.1 + AgeStd.1 + EthnicityBlack.1 + EthnicityAsian.1 + EthnicityHispanic.1 + EthnicityOther.1 + PoliticalOrientation.1 + SESStd.1
            
            # Estimate the lagged effects between the within-person centered variables (with time-variant control)
            w1_05 ~ b11*w1_02 + b12*w2_02 + b13*w3_02
            w1_09 ~ b11*w1_05 + b12*w2_05 + b13*w3_05
            w1_11 ~ b11*w1_09 + b12*w2_09 + b13*w3_09
            w1_13 ~ b11*w1_11 + b12*w2_11 + b13*w3_11
            w1_14 ~ b11*w1_13 + b12*w2_13 + b13*w3_13
            w1_15 ~ b11*w1_14 + b12*w2_14 + b13*w3_14
            w1_16 ~ b11*w1_15 + b12*w2_15 + b13*w3_15
            w1_17 ~ b11*w1_16 + b12*w2_16 + b13*w3_16
            w1_18 ~ b11*w1_17 + b12*w2_17 + b13*w3_17
            
            w2_05 ~ b21*w1_02 + b22*w2_02 + b23*w3_02
            w2_09 ~ b21*w1_05 + b22*w2_05 + b23*w3_05
            w2_11 ~ b21*w1_09 + b22*w2_09 + b23*w3_09
            w2_13 ~ b21*w1_11 + b22*w2_11 + b23*w3_11
            w2_14 ~ b21*w1_13 + b22*w2_13 + b23*w3_13
            w2_15 ~ b21*w1_14 + b22*w2_14 + b23*w3_14
            w2_16 ~ b21*w1_15 + b22*w2_15 + b23*w3_15
            w2_17 ~ b21*w1_16 + b22*w2_16 + b23*w3_16
            w2_18 ~ b21*w1_17 + b22*w2_17 + b23*w3_17
            
            w3_05 ~ b31*w1_02 + b32*w2_02 + b33*w3_02
            w3_09 ~ b31*w1_05 + b32*w2_05 + b33*w3_05
            w3_11 ~ b31*w1_09 + b32*w2_09 + b33*w3_09
            w3_13 ~ b31*w1_11 + b32*w2_11 + b33*w3_11
            w3_14 ~ b31*w1_13 + b32*w2_13 + b33*w3_13
            w3_15 ~ b31*w1_14 + b32*w2_14 + b33*w3_14
            w3_16 ~ b31*w1_15 + b32*w2_15 + b33*w3_15
            w3_17 ~ b31*w1_16 + b32*w2_16 + b33*w3_16
            w3_18 ~ b31*w1_17 + b32*w2_17 + b33*w3_17
            
            # Estimate the covariance between the within-person centered variables at the first wave
            w1_02 ~~ w2_02
            w1_02 ~~ w3_02
            w2_02 ~~ w3_02
            
            # Estimate the covariances between the residuals of the within-person centered variables
            w1_05 ~~ cov12*w2_05
            w1_09 ~~ cov12*w2_09
            w1_11 ~~ cov12*w2_11
            w1_13 ~~ cov12*w2_13
            w1_14 ~~ cov12*w2_14
            w1_15 ~~ cov12*w2_15
            w1_16 ~~ cov12*w2_16
            w1_17 ~~ cov12*w2_17
            w1_18 ~~ cov12*w2_18
            
            w1_05 ~~ cov13*w3_05
            w1_09 ~~ cov13*w3_09
            w1_11 ~~ cov13*w3_11
            w1_13 ~~ cov13*w3_13
            w1_14 ~~ cov13*w3_14
            w1_15 ~~ cov13*w3_15
            w1_16 ~~ cov13*w3_16
            w1_17 ~~ cov13*w3_17
            w1_18 ~~ cov13*w3_18
            
            w2_05 ~~ cov23*w3_05
            w2_09 ~~ cov23*w3_09
            w2_11 ~~ cov23*w3_11
            w2_13 ~~ cov23*w3_13
            w2_14 ~~ cov23*w3_14
            w2_15 ~~ cov23*w3_15
            w2_16 ~~ cov23*w3_16
            w2_17 ~~ cov23*w3_17
            w2_18 ~~ cov23*w3_18
            
            # Estimate the variance and covariance of the random intercepts
            ri1 ~~ ri1
            ri2 ~~ ri2
            ri3 ~~ ri3
            
            ri1 ~~ ri2
            ri1 ~~ ri3
            ri2 ~~ ri3
            
            # Estimate the (residual) variance of the within-person centered variables
            w1_02 ~~ w1_02
            w1_05 ~~ var1*w1_05
            w1_09 ~~ var1*w1_09
            w1_11 ~~ var1*w1_11
            w1_13 ~~ var1*w1_13
            w1_14 ~~ var1*w1_14
            w1_15 ~~ var1*w1_15
            w1_16 ~~ var1*w1_16
            w1_17 ~~ var1*w1_17
            w1_18 ~~ var1*w1_18
            
            w2_02 ~~ w2_02
            w2_05 ~~ var2*w2_05
            w2_09 ~~ var2*w2_09
            w2_11 ~~ var2*w2_11
            w2_13 ~~ var2*w2_13
            w2_14 ~~ var2*w2_14
            w2_15 ~~ var2*w2_15
            w2_16 ~~ var2*w2_16
            w2_17 ~~ var2*w2_17
            w2_18 ~~ var2*w2_18
            
            w3_02 ~~ w3_02
            w3_05 ~~ var3*w3_05
            w3_09 ~~ var3*w3_09
            w3_11 ~~ var3*w3_11
            w3_13 ~~ var3*w3_13
            w3_14 ~~ var3*w3_14
            w3_15 ~~ var3*w3_15
            w3_16 ~~ var3*w3_16
            w3_17 ~~ var3*w3_17
            w3_18 ~~ var3*w3_18
            
            # Estimate the means
            ContactsMask.2 + ContactsMask.5 + ContactsMask.9 + ContactsMask.11 + ContactsMask.13 + ContactsMask.14 + ContactsMask.15 + ContactsMask.16 + ContactsMask.17 + ContactsMask.18 ~ 1
            InjNorms.2 + InjNorms.5 + InjNorms.9 + InjNorms.11 + InjNorms.13 + InjNorms.14 + InjNorms.15 + InjNorms.16 + InjNorms.17 + InjNorms.18 ~ 1
            DesNorms.2 + DesNorms.5 + DesNorms.9 + DesNorms.11 + DesNorms.13 + DesNorms.14 + DesNorms.15 + DesNorms.16 + DesNorms.17 + DesNorms.18 ~ 1'
  # remove constraints for unconstrained model
  if (!constrained) {
    constraints <- c("b11","b12","b13",
                     "b21","b22","b23",
                     "b31","b32","b33",
                     "b41","b42","b43",
                     "b51","b52","b53",
                     "cov12","cov13","cov23",
                     "var1","var2","var3")
    for (i in constraints) model <- str_replace_all(model, fixed(paste0(i, "*")), "")
  }
  # fit model
  out <- lavaan(model, data = d, missing = "fiml", meanstructure = TRUE, int.ov.free = TRUE)
  return(out)
}

# reduced model (without any covariates)
fitRICLPM3 <- function(d, constrained = FALSE) {
  # model code for riclpm
  # https://jeroendmulder.github.io/RI-CLPM/lavaan.html
  model <- '# Create between components (random intercepts)
            ri1 =~ 1*ContactsMask.2 + 1*ContactsMask.5 + 1*ContactsMask.9 + 1*ContactsMask.11 + 1*ContactsMask.13 + 1*ContactsMask.14 + 1*ContactsMask.15 + 1*ContactsMask.16 + 1*ContactsMask.17 + 1*ContactsMask.18
            ri2 =~ 1*InjNorms.2 + 1*InjNorms.5 + 1*InjNorms.9 + 1*InjNorms.11 + 1*InjNorms.13 + 1*InjNorms.14 + 1*InjNorms.15 + 1*InjNorms.16 + 1*InjNorms.17 + 1*InjNorms.18
            ri3 =~ 1*DesNorms.2 + 1*DesNorms.5 + 1*DesNorms.9 + 1*DesNorms.11 + 1*DesNorms.13 + 1*DesNorms.14 + 1*DesNorms.15 + 1*DesNorms.16 + 1*DesNorms.17 + 1*DesNorms.18
            
            # Create within-person centered variables
            w1_02 =~ 1*ContactsMask.2
            w1_05 =~ 1*ContactsMask.5
            w1_09 =~ 1*ContactsMask.9
            w1_11 =~ 1*ContactsMask.11
            w1_13 =~ 1*ContactsMask.13
            w1_14 =~ 1*ContactsMask.14
            w1_15 =~ 1*ContactsMask.15
            w1_16 =~ 1*ContactsMask.16
            w1_17 =~ 1*ContactsMask.17
            w1_18 =~ 1*ContactsMask.18
            
            w2_02 =~ 1*InjNorms.2
            w2_05 =~ 1*InjNorms.5
            w2_09 =~ 1*InjNorms.9
            w2_11 =~ 1*InjNorms.11
            w2_13 =~ 1*InjNorms.13
            w2_14 =~ 1*InjNorms.14
            w2_15 =~ 1*InjNorms.15
            w2_16 =~ 1*InjNorms.16
            w2_17 =~ 1*InjNorms.17
            w2_18 =~ 1*InjNorms.18
            
            w3_02 =~ 1*DesNorms.2
            w3_05 =~ 1*DesNorms.5
            w3_09 =~ 1*DesNorms.9
            w3_11 =~ 1*DesNorms.11
            w3_13 =~ 1*DesNorms.13
            w3_14 =~ 1*DesNorms.14
            w3_15 =~ 1*DesNorms.15
            w3_16 =~ 1*DesNorms.16
            w3_17 =~ 1*DesNorms.17
            w3_18 =~ 1*DesNorms.18
            
            # Estimate the lagged effects between the within-person centered variables (with time-variant control)
            w1_05 ~ b11*w1_02 + b12*w2_02 + b13*w3_02
            w1_09 ~ b11*w1_05 + b12*w2_05 + b13*w3_05
            w1_11 ~ b11*w1_09 + b12*w2_09 + b13*w3_09
            w1_13 ~ b11*w1_11 + b12*w2_11 + b13*w3_11
            w1_14 ~ b11*w1_13 + b12*w2_13 + b13*w3_13
            w1_15 ~ b11*w1_14 + b12*w2_14 + b13*w3_14
            w1_16 ~ b11*w1_15 + b12*w2_15 + b13*w3_15
            w1_17 ~ b11*w1_16 + b12*w2_16 + b13*w3_16
            w1_18 ~ b11*w1_17 + b12*w2_17 + b13*w3_17
            
            w2_05 ~ b21*w1_02 + b22*w2_02 + b23*w3_02
            w2_09 ~ b21*w1_05 + b22*w2_05 + b23*w3_05
            w2_11 ~ b21*w1_09 + b22*w2_09 + b23*w3_09
            w2_13 ~ b21*w1_11 + b22*w2_11 + b23*w3_11
            w2_14 ~ b21*w1_13 + b22*w2_13 + b23*w3_13
            w2_15 ~ b21*w1_14 + b22*w2_14 + b23*w3_14
            w2_16 ~ b21*w1_15 + b22*w2_15 + b23*w3_15
            w2_17 ~ b21*w1_16 + b22*w2_16 + b23*w3_16
            w2_18 ~ b21*w1_17 + b22*w2_17 + b23*w3_17
            
            w3_05 ~ b31*w1_02 + b32*w2_02 + b33*w3_02
            w3_09 ~ b31*w1_05 + b32*w2_05 + b33*w3_05
            w3_11 ~ b31*w1_09 + b32*w2_09 + b33*w3_09
            w3_13 ~ b31*w1_11 + b32*w2_11 + b33*w3_11
            w3_14 ~ b31*w1_13 + b32*w2_13 + b33*w3_13
            w3_15 ~ b31*w1_14 + b32*w2_14 + b33*w3_14
            w3_16 ~ b31*w1_15 + b32*w2_15 + b33*w3_15
            w3_17 ~ b31*w1_16 + b32*w2_16 + b33*w3_16
            w3_18 ~ b31*w1_17 + b32*w2_17 + b33*w3_17
            
            # Estimate the covariance between the within-person centered variables at the first wave
            w1_02 ~~ w2_02
            w1_02 ~~ w3_02
            w2_02 ~~ w3_02
            
            # Estimate the covariances between the residuals of the within-person centered variables
            w1_05 ~~ cov12*w2_05
            w1_09 ~~ cov12*w2_09
            w1_11 ~~ cov12*w2_11
            w1_13 ~~ cov12*w2_13
            w1_14 ~~ cov12*w2_14
            w1_15 ~~ cov12*w2_15
            w1_16 ~~ cov12*w2_16
            w1_17 ~~ cov12*w2_17
            w1_18 ~~ cov12*w2_18
            
            w1_05 ~~ cov13*w3_05
            w1_09 ~~ cov13*w3_09
            w1_11 ~~ cov13*w3_11
            w1_13 ~~ cov13*w3_13
            w1_14 ~~ cov13*w3_14
            w1_15 ~~ cov13*w3_15
            w1_16 ~~ cov13*w3_16
            w1_17 ~~ cov13*w3_17
            w1_18 ~~ cov13*w3_18
            
            w2_05 ~~ cov23*w3_05
            w2_09 ~~ cov23*w3_09
            w2_11 ~~ cov23*w3_11
            w2_13 ~~ cov23*w3_13
            w2_14 ~~ cov23*w3_14
            w2_15 ~~ cov23*w3_15
            w2_16 ~~ cov23*w3_16
            w2_17 ~~ cov23*w3_17
            w2_18 ~~ cov23*w3_18
            
            # Estimate the variance and covariance of the random intercepts
            ri1 ~~ ri1
            ri2 ~~ ri2
            ri3 ~~ ri3
            
            ri1 ~~ ri2
            ri1 ~~ ri3
            ri2 ~~ ri3
            
            # Estimate the (residual) variance of the within-person centered variables
            w1_02 ~~ w1_02
            w1_05 ~~ var1*w1_05
            w1_09 ~~ var1*w1_09
            w1_11 ~~ var1*w1_11
            w1_13 ~~ var1*w1_13
            w1_14 ~~ var1*w1_14
            w1_15 ~~ var1*w1_15
            w1_16 ~~ var1*w1_16
            w1_17 ~~ var1*w1_17
            w1_18 ~~ var1*w1_18
            
            w2_02 ~~ w2_02
            w2_05 ~~ var2*w2_05
            w2_09 ~~ var2*w2_09
            w2_11 ~~ var2*w2_11
            w2_13 ~~ var2*w2_13
            w2_14 ~~ var2*w2_14
            w2_15 ~~ var2*w2_15
            w2_16 ~~ var2*w2_16
            w2_17 ~~ var2*w2_17
            w2_18 ~~ var2*w2_18
            
            w3_02 ~~ w3_02
            w3_05 ~~ var3*w3_05
            w3_09 ~~ var3*w3_09
            w3_11 ~~ var3*w3_11
            w3_13 ~~ var3*w3_13
            w3_14 ~~ var3*w3_14
            w3_15 ~~ var3*w3_15
            w3_16 ~~ var3*w3_16
            w3_17 ~~ var3*w3_17
            w3_18 ~~ var3*w3_18
            
            # Estimate the means
            ContactsMask.2 + ContactsMask.5 + ContactsMask.9 + ContactsMask.11 + ContactsMask.13 + ContactsMask.14 + ContactsMask.15 + ContactsMask.16 + ContactsMask.17 + ContactsMask.18 ~ 1
            InjNorms.2 + InjNorms.5 + InjNorms.9 + InjNorms.11 + InjNorms.13 + InjNorms.14 + InjNorms.15 + InjNorms.16 + InjNorms.17 + InjNorms.18 ~ 1
            DesNorms.2 + DesNorms.5 + DesNorms.9 + DesNorms.11 + DesNorms.13 + DesNorms.14 + DesNorms.15 + DesNorms.16 + DesNorms.17 + DesNorms.18 ~ 1'
  # remove constraints for unconstrained model
  if (!constrained) {
    constraints <- c("b11","b12","b13",
                     "b21","b22","b23",
                     "b31","b32","b33",
                     "cov12","cov13","cov23",
                     "var1","var2","var3")
    for (i in constraints) model <- str_replace_all(model, fixed(paste0(i, "*")), "")
  }
  # fit model
  out <- lavaan(model, data = d, missing = "fiml", meanstructure = TRUE, int.ov.free = TRUE)
  return(out)
}

# multigroup model
fitRICLPM4 <- function(d, constrained = FALSE) {
  # prepare covariates for modelling
  d <-
    d %>%
    # fill in missing age observation
    mutate(Age.1 = ifelse(is.na(Age.1), 57, Age.1)) %>%
    # prepare covariates
    mutate(
      Sex.1 = ifelse(Sex.1 == 3, NA, Sex.1 - 1),
      AgeStd.1 = as.numeric(scale(Age.1)),
      EthnicityBlack.1 = ifelse(Ethnicity.1 == "Black or African American", 1, 0),
      EthnicityAsian.1 = ifelse(Ethnicity.1 == "Asian or Pacific Islander", 1, 0),
      EthnicityHispanic.1 = ifelse(Ethnicity.1 == "Hispanic or Latino/a", 1, 0),
      EthnicityOther.1 = ifelse(Ethnicity.1 %in% c("Native American", "Other"), 1, 0),
      PoliticalOrientationStd.1 = as.numeric(scale(PoliticalOrientation.1)),
      # SES is mean average of education, income, and subjective SES
      SESStd.1 = as.numeric(scale((Education.1 + Income.1 + SSES.1) / 3))
    )
  # model code for riclpm
  # https://jeroendmulder.github.io/RI-CLPM/lavaan.html
  model <- '# Create between components (random intercepts)
            ri1 =~ 1*ContactsMask.2 + 1*ContactsMask.5 + 1*ContactsMask.9 + 1*ContactsMask.11 + 1*ContactsMask.13 + 1*ContactsMask.14 + 1*ContactsMask.15 + 1*ContactsMask.16 + 1*ContactsMask.17 + 1*ContactsMask.18
            ri2 =~ 1*InjNorms.2 + 1*InjNorms.5 + 1*InjNorms.9 + 1*InjNorms.11 + 1*InjNorms.13 + 1*InjNorms.14 + 1*InjNorms.15 + 1*InjNorms.16 + 1*InjNorms.17 + 1*InjNorms.18
            ri3 =~ 1*DesNorms.2 + 1*DesNorms.5 + 1*DesNorms.9 + 1*DesNorms.11 + 1*DesNorms.13 + 1*DesNorms.14 + 1*DesNorms.15 + 1*DesNorms.16 + 1*DesNorms.17 + 1*DesNorms.18
            ri4 =~ 1*FactBeliefs.2 + 1*FactBeliefs.5 + 1*FactBeliefs.9 + 1*FactBeliefs.11 + 1*FactBeliefs.13 + 1*FactBeliefs.14 + 1*FactBeliefs.15 + 1*FactBeliefs.16 + 1*FactBeliefs.17 + 1*FactBeliefs.18
            ri5 =~ 1*PersNorms.2 + 1*PersNorms.5 + 1*PersNorms.9 + 1*PersNorms.11 + 1*PersNorms.13 + 1*PersNorms.14 + 1*PersNorms.15 + 1*PersNorms.16 + 1*PersNorms.17 + 1*PersNorms.18
            
            # Create within-person centered variables
            w1_02 =~ 1*ContactsMask.2
            w1_05 =~ 1*ContactsMask.5
            w1_09 =~ 1*ContactsMask.9
            w1_11 =~ 1*ContactsMask.11
            w1_13 =~ 1*ContactsMask.13
            w1_14 =~ 1*ContactsMask.14
            w1_15 =~ 1*ContactsMask.15
            w1_16 =~ 1*ContactsMask.16
            w1_17 =~ 1*ContactsMask.17
            w1_18 =~ 1*ContactsMask.18
            
            w2_02 =~ 1*InjNorms.2
            w2_05 =~ 1*InjNorms.5
            w2_09 =~ 1*InjNorms.9
            w2_11 =~ 1*InjNorms.11
            w2_13 =~ 1*InjNorms.13
            w2_14 =~ 1*InjNorms.14
            w2_15 =~ 1*InjNorms.15
            w2_16 =~ 1*InjNorms.16
            w2_17 =~ 1*InjNorms.17
            w2_18 =~ 1*InjNorms.18
            
            w3_02 =~ 1*DesNorms.2
            w3_05 =~ 1*DesNorms.5
            w3_09 =~ 1*DesNorms.9
            w3_11 =~ 1*DesNorms.11
            w3_13 =~ 1*DesNorms.13
            w3_14 =~ 1*DesNorms.14
            w3_15 =~ 1*DesNorms.15
            w3_16 =~ 1*DesNorms.16
            w3_17 =~ 1*DesNorms.17
            w3_18 =~ 1*DesNorms.18
            
            w4_02 =~ 1*FactBeliefs.2
            w4_05 =~ 1*FactBeliefs.5
            w4_09 =~ 1*FactBeliefs.9
            w4_11 =~ 1*FactBeliefs.11
            w4_13 =~ 1*FactBeliefs.13
            w4_14 =~ 1*FactBeliefs.14
            w4_15 =~ 1*FactBeliefs.15
            w4_16 =~ 1*FactBeliefs.16
            w4_17 =~ 1*FactBeliefs.17
            w4_18 =~ 1*FactBeliefs.18
            
            w5_02 =~ 1*PersNorms.2
            w5_05 =~ 1*PersNorms.5
            w5_09 =~ 1*PersNorms.9
            w5_11 =~ 1*PersNorms.11
            w5_13 =~ 1*PersNorms.13
            w5_14 =~ 1*PersNorms.14
            w5_15 =~ 1*PersNorms.15
            w5_16 =~ 1*PersNorms.16
            w5_17 =~ 1*PersNorms.17
            w5_18 =~ 1*PersNorms.18
            
            # Regression of observed variables on time-invariant controls
            ContactsMask.2 + ContactsMask.5 + ContactsMask.9 + ContactsMask.11 + ContactsMask.13 + ContactsMask.14 + ContactsMask.15 + ContactsMask.16 + ContactsMask.17 + ContactsMask.18 ~ Sex.1 + AgeStd.1 + EthnicityBlack.1 + EthnicityAsian.1 + EthnicityHispanic.1 + EthnicityOther.1 + PoliticalOrientation.1 + SESStd.1
            InjNorms.2 + InjNorms.5 + InjNorms.9 + InjNorms.11 + InjNorms.13 + InjNorms.14 + InjNorms.15 + InjNorms.16 + InjNorms.17 + InjNorms.18 ~ Sex.1 + AgeStd.1 + EthnicityBlack.1 + EthnicityAsian.1 + EthnicityHispanic.1 + EthnicityOther.1 + PoliticalOrientation.1 + SESStd.1
            DesNorms.2 + DesNorms.5 + DesNorms.9 + DesNorms.11 + DesNorms.13 + DesNorms.14 + DesNorms.15 + DesNorms.16 + DesNorms.17 + DesNorms.18 ~ Sex.1 + AgeStd.1 + EthnicityBlack.1 + EthnicityAsian.1 + EthnicityHispanic.1 + EthnicityOther.1 + PoliticalOrientation.1 + SESStd.1
            FactBeliefs.2 + FactBeliefs.5 + FactBeliefs.9 + FactBeliefs.11 + FactBeliefs.13 + FactBeliefs.14 + FactBeliefs.15 + FactBeliefs.16 + FactBeliefs.17 + FactBeliefs.18 ~ Sex.1 + AgeStd.1 + EthnicityBlack.1 + EthnicityAsian.1 + EthnicityHispanic.1 + EthnicityOther.1 + PoliticalOrientation.1 + SESStd.1
            PersNorms.2 + PersNorms.5 + PersNorms.9 + PersNorms.11 + PersNorms.13 + PersNorms.14 + PersNorms.15 + PersNorms.16 + PersNorms.17 + PersNorms.18 ~ Sex.1 + AgeStd.1 + EthnicityBlack.1 + EthnicityAsian.1 + EthnicityHispanic.1 + EthnicityOther.1 + PoliticalOrientation.1 + SESStd.1
            
            # Estimate the lagged effects between the within-person centered variables (with time-variant control)
            w1_05 ~ c(b11_1, b11_2)*w1_02 + c(b12_1, b12_2)*w2_02 + c(b13_1, b13_2)*w3_02 + c(b14_1, b14_2)*w4_02 + c(b15_1, b15_2)*w5_02
            w1_09 ~ c(b11_1, b11_2)*w1_05 + c(b12_1, b12_2)*w2_05 + c(b13_1, b13_2)*w3_05 + c(b14_1, b14_2)*w4_05 + c(b15_1, b15_2)*w5_05
            w1_11 ~ c(b11_1, b11_2)*w1_09 + c(b12_1, b12_2)*w2_09 + c(b13_1, b13_2)*w3_09 + c(b14_1, b14_2)*w4_09 + c(b15_1, b15_2)*w5_09
            w1_13 ~ c(b11_1, b11_2)*w1_11 + c(b12_1, b12_2)*w2_11 + c(b13_1, b13_2)*w3_11 + c(b14_1, b14_2)*w4_11 + c(b15_1, b15_2)*w5_11
            w1_14 ~ c(b11_1, b11_2)*w1_13 + c(b12_1, b12_2)*w2_13 + c(b13_1, b13_2)*w3_13 + c(b14_1, b14_2)*w4_13 + c(b15_1, b15_2)*w5_13
            w1_15 ~ c(b11_1, b11_2)*w1_14 + c(b12_1, b12_2)*w2_14 + c(b13_1, b13_2)*w3_14 + c(b14_1, b14_2)*w4_14 + c(b15_1, b15_2)*w5_14
            w1_16 ~ c(b11_1, b11_2)*w1_15 + c(b12_1, b12_2)*w2_15 + c(b13_1, b13_2)*w3_15 + c(b14_1, b14_2)*w4_15 + c(b15_1, b15_2)*w5_15
            w1_17 ~ c(b11_1, b11_2)*w1_16 + c(b12_1, b12_2)*w2_16 + c(b13_1, b13_2)*w3_16 + c(b14_1, b14_2)*w4_16 + c(b15_1, b15_2)*w5_16
            w1_18 ~ c(b11_1, b11_2)*w1_17 + c(b12_1, b12_2)*w2_17 + c(b13_1, b13_2)*w3_17 + c(b14_1, b14_2)*w4_17 + c(b15_1, b15_2)*w5_17
            
            w2_05 ~ c(b21_1, b21_2)*w1_02 + c(b22_1, b22_2)*w2_02 + c(b23_1, b23_2)*w3_02 + c(b24_1, b24_2)*w4_02 + c(b25_1, b25_2)*w5_02
            w2_09 ~ c(b21_1, b21_2)*w1_05 + c(b22_1, b22_2)*w2_05 + c(b23_1, b23_2)*w3_05 + c(b24_1, b24_2)*w4_05 + c(b25_1, b25_2)*w5_05
            w2_11 ~ c(b21_1, b21_2)*w1_09 + c(b22_1, b22_2)*w2_09 + c(b23_1, b23_2)*w3_09 + c(b24_1, b24_2)*w4_09 + c(b25_1, b25_2)*w5_09
            w2_13 ~ c(b21_1, b21_2)*w1_11 + c(b22_1, b22_2)*w2_11 + c(b23_1, b23_2)*w3_11 + c(b24_1, b24_2)*w4_11 + c(b25_1, b25_2)*w5_11
            w2_14 ~ c(b21_1, b21_2)*w1_13 + c(b22_1, b22_2)*w2_13 + c(b23_1, b23_2)*w3_13 + c(b24_1, b24_2)*w4_13 + c(b25_1, b25_2)*w5_13
            w2_15 ~ c(b21_1, b21_2)*w1_14 + c(b22_1, b22_2)*w2_14 + c(b23_1, b23_2)*w3_14 + c(b24_1, b24_2)*w4_14 + c(b25_1, b25_2)*w5_14
            w2_16 ~ c(b21_1, b21_2)*w1_15 + c(b22_1, b22_2)*w2_15 + c(b23_1, b23_2)*w3_15 + c(b24_1, b24_2)*w4_15 + c(b25_1, b25_2)*w5_15
            w2_17 ~ c(b21_1, b21_2)*w1_16 + c(b22_1, b22_2)*w2_16 + c(b23_1, b23_2)*w3_16 + c(b24_1, b24_2)*w4_16 + c(b25_1, b25_2)*w5_16
            w2_18 ~ c(b21_1, b21_2)*w1_17 + c(b22_1, b22_2)*w2_17 + c(b23_1, b23_2)*w3_17 + c(b24_1, b24_2)*w4_17 + c(b25_1, b25_2)*w5_17
            
            w3_05 ~ c(b31_1, b31_2)*w1_02 + c(b32_1, b32_2)*w2_02 + c(b33_1, b33_2)*w3_02 + c(b34_1, b34_2)*w4_02 + c(b35_1, b35_2)*w5_02
            w3_09 ~ c(b31_1, b31_2)*w1_05 + c(b32_1, b32_2)*w2_05 + c(b33_1, b33_2)*w3_05 + c(b34_1, b34_2)*w4_05 + c(b35_1, b35_2)*w5_05
            w3_11 ~ c(b31_1, b31_2)*w1_09 + c(b32_1, b32_2)*w2_09 + c(b33_1, b33_2)*w3_09 + c(b34_1, b34_2)*w4_09 + c(b35_1, b35_2)*w5_09
            w3_13 ~ c(b31_1, b31_2)*w1_11 + c(b32_1, b32_2)*w2_11 + c(b33_1, b33_2)*w3_11 + c(b34_1, b34_2)*w4_11 + c(b35_1, b35_2)*w5_11
            w3_14 ~ c(b31_1, b31_2)*w1_13 + c(b32_1, b32_2)*w2_13 + c(b33_1, b33_2)*w3_13 + c(b34_1, b34_2)*w4_13 + c(b35_1, b35_2)*w5_13
            w3_15 ~ c(b31_1, b31_2)*w1_14 + c(b32_1, b32_2)*w2_14 + c(b33_1, b33_2)*w3_14 + c(b34_1, b34_2)*w4_14 + c(b35_1, b35_2)*w5_14
            w3_16 ~ c(b31_1, b31_2)*w1_15 + c(b32_1, b32_2)*w2_15 + c(b33_1, b33_2)*w3_15 + c(b34_1, b34_2)*w4_15 + c(b35_1, b35_2)*w5_15
            w3_17 ~ c(b31_1, b31_2)*w1_16 + c(b32_1, b32_2)*w2_16 + c(b33_1, b33_2)*w3_16 + c(b34_1, b34_2)*w4_16 + c(b35_1, b35_2)*w5_16
            w3_18 ~ c(b31_1, b31_2)*w1_17 + c(b32_1, b32_2)*w2_17 + c(b33_1, b33_2)*w3_17 + c(b34_1, b34_2)*w4_17 + c(b35_1, b35_2)*w5_17
            
            w4_05 ~ c(b41_1, b41_2)*w1_02 + c(b42_1, b42_2)*w2_02 + c(b43_1, b43_2)*w3_02 + c(b44_1, b44_2)*w4_02 + c(b45_1, b45_2)*w5_02
            w4_09 ~ c(b41_1, b41_2)*w1_05 + c(b42_1, b42_2)*w2_05 + c(b43_1, b43_2)*w3_05 + c(b44_1, b44_2)*w4_05 + c(b45_1, b45_2)*w5_05
            w4_11 ~ c(b41_1, b41_2)*w1_09 + c(b42_1, b42_2)*w2_09 + c(b43_1, b43_2)*w3_09 + c(b44_1, b44_2)*w4_09 + c(b45_1, b45_2)*w5_09
            w4_13 ~ c(b41_1, b41_2)*w1_11 + c(b42_1, b42_2)*w2_11 + c(b43_1, b43_2)*w3_11 + c(b44_1, b44_2)*w4_11 + c(b45_1, b45_2)*w5_11
            w4_14 ~ c(b41_1, b41_2)*w1_13 + c(b42_1, b42_2)*w2_13 + c(b43_1, b43_2)*w3_13 + c(b44_1, b44_2)*w4_13 + c(b45_1, b45_2)*w5_13
            w4_15 ~ c(b41_1, b41_2)*w1_14 + c(b42_1, b42_2)*w2_14 + c(b43_1, b43_2)*w3_14 + c(b44_1, b44_2)*w4_14 + c(b45_1, b45_2)*w5_14
            w4_16 ~ c(b41_1, b41_2)*w1_15 + c(b42_1, b42_2)*w2_15 + c(b43_1, b43_2)*w3_15 + c(b44_1, b44_2)*w4_15 + c(b45_1, b45_2)*w5_15
            w4_17 ~ c(b41_1, b41_2)*w1_16 + c(b42_1, b42_2)*w2_16 + c(b43_1, b43_2)*w3_16 + c(b44_1, b44_2)*w4_16 + c(b45_1, b45_2)*w5_16
            w4_18 ~ c(b41_1, b41_2)*w1_17 + c(b42_1, b42_2)*w2_17 + c(b43_1, b43_2)*w3_17 + c(b44_1, b44_2)*w4_17 + c(b45_1, b45_2)*w5_17
            
            w5_05 ~ c(b51_1, b51_2)*w1_02 + c(b52_1, b52_2)*w2_02 + c(b53_1, b53_2)*w3_02 + c(b54_1, b54_2)*w4_02 + c(b55_1, b55_2)*w5_02
            w5_09 ~ c(b51_1, b51_2)*w1_05 + c(b52_1, b52_2)*w2_05 + c(b53_1, b53_2)*w3_05 + c(b54_1, b54_2)*w4_05 + c(b55_1, b55_2)*w5_05
            w5_11 ~ c(b51_1, b51_2)*w1_09 + c(b52_1, b52_2)*w2_09 + c(b53_1, b53_2)*w3_09 + c(b54_1, b54_2)*w4_09 + c(b55_1, b55_2)*w5_09
            w5_13 ~ c(b51_1, b51_2)*w1_11 + c(b52_1, b52_2)*w2_11 + c(b53_1, b53_2)*w3_11 + c(b54_1, b54_2)*w4_11 + c(b55_1, b55_2)*w5_11
            w5_14 ~ c(b51_1, b51_2)*w1_13 + c(b52_1, b52_2)*w2_13 + c(b53_1, b53_2)*w3_13 + c(b54_1, b54_2)*w4_13 + c(b55_1, b55_2)*w5_13
            w5_15 ~ c(b51_1, b51_2)*w1_14 + c(b52_1, b52_2)*w2_14 + c(b53_1, b53_2)*w3_14 + c(b54_1, b54_2)*w4_14 + c(b55_1, b55_2)*w5_14
            w5_16 ~ c(b51_1, b51_2)*w1_15 + c(b52_1, b52_2)*w2_15 + c(b53_1, b53_2)*w3_15 + c(b54_1, b54_2)*w4_15 + c(b55_1, b55_2)*w5_15
            w5_17 ~ c(b51_1, b51_2)*w1_16 + c(b52_1, b52_2)*w2_16 + c(b53_1, b53_2)*w3_16 + c(b54_1, b54_2)*w4_16 + c(b55_1, b55_2)*w5_16
            w5_18 ~ c(b51_1, b51_2)*w1_17 + c(b52_1, b52_2)*w2_17 + c(b53_1, b53_2)*w3_17 + c(b54_1, b54_2)*w4_17 + c(b55_1, b55_2)*w5_17
            
            # Estimate the covariance between the within-person centered variables at the first wave
            w1_02 ~~ w2_02
            w1_02 ~~ w3_02
            w1_02 ~~ w4_02
            w1_02 ~~ w5_02
            w2_02 ~~ w3_02
            w2_02 ~~ w4_02
            w2_02 ~~ w5_02
            w3_02 ~~ w4_02
            w3_02 ~~ w5_02
            w4_02 ~~ w5_02
            
            # Estimate the covariances between the residuals of the within-person centered variables
            w1_05 ~~ c(cov12_1, cov12_2)*w2_05
            w1_09 ~~ c(cov12_1, cov12_2)*w2_09
            w1_11 ~~ c(cov12_1, cov12_2)*w2_11
            w1_13 ~~ c(cov12_1, cov12_2)*w2_13
            w1_14 ~~ c(cov12_1, cov12_2)*w2_14
            w1_15 ~~ c(cov12_1, cov12_2)*w2_15
            w1_16 ~~ c(cov12_1, cov12_2)*w2_16
            w1_17 ~~ c(cov12_1, cov12_2)*w2_17
            w1_18 ~~ c(cov12_1, cov12_2)*w2_18
            
            w1_05 ~~ c(cov13_1, cov13_2)*w3_05
            w1_09 ~~ c(cov13_1, cov13_2)*w3_09
            w1_11 ~~ c(cov13_1, cov13_2)*w3_11
            w1_13 ~~ c(cov13_1, cov13_2)*w3_13
            w1_14 ~~ c(cov13_1, cov13_2)*w3_14
            w1_15 ~~ c(cov13_1, cov13_2)*w3_15
            w1_16 ~~ c(cov13_1, cov13_2)*w3_16
            w1_17 ~~ c(cov13_1, cov13_2)*w3_17
            w1_18 ~~ c(cov13_1, cov13_2)*w3_18
            
            w1_05 ~~ c(cov14_1, cov14_2)*w4_05
            w1_09 ~~ c(cov14_1, cov14_2)*w4_09
            w1_11 ~~ c(cov14_1, cov14_2)*w4_11
            w1_13 ~~ c(cov14_1, cov14_2)*w4_13
            w1_14 ~~ c(cov14_1, cov14_2)*w4_14
            w1_15 ~~ c(cov14_1, cov14_2)*w4_15
            w1_16 ~~ c(cov14_1, cov14_2)*w4_16
            w1_17 ~~ c(cov14_1, cov14_2)*w4_17
            w1_18 ~~ c(cov14_1, cov14_2)*w4_18
            
            w1_05 ~~ c(cov15_1, cov15_2)*w5_05
            w1_09 ~~ c(cov15_1, cov15_2)*w5_09
            w1_11 ~~ c(cov15_1, cov15_2)*w5_11
            w1_13 ~~ c(cov15_1, cov15_2)*w5_13
            w1_14 ~~ c(cov15_1, cov15_2)*w5_14
            w1_15 ~~ c(cov15_1, cov15_2)*w5_15
            w1_16 ~~ c(cov15_1, cov15_2)*w5_16
            w1_17 ~~ c(cov15_1, cov15_2)*w5_17
            w1_18 ~~ c(cov15_1, cov15_2)*w5_18
            
            w2_05 ~~ c(cov23_1, cov23_2)*w3_05
            w2_09 ~~ c(cov23_1, cov23_2)*w3_09
            w2_11 ~~ c(cov23_1, cov23_2)*w3_11
            w2_13 ~~ c(cov23_1, cov23_2)*w3_13
            w2_14 ~~ c(cov23_1, cov23_2)*w3_14
            w2_15 ~~ c(cov23_1, cov23_2)*w3_15
            w2_16 ~~ c(cov23_1, cov23_2)*w3_16
            w2_17 ~~ c(cov23_1, cov23_2)*w3_17
            w2_18 ~~ c(cov23_1, cov23_2)*w3_18
            
            w2_05 ~~ c(cov24_1, cov24_2)*w4_05
            w2_09 ~~ c(cov24_1, cov24_2)*w4_09
            w2_11 ~~ c(cov24_1, cov24_2)*w4_11
            w2_13 ~~ c(cov24_1, cov24_2)*w4_13
            w2_14 ~~ c(cov24_1, cov24_2)*w4_14
            w2_15 ~~ c(cov24_1, cov24_2)*w4_15
            w2_16 ~~ c(cov24_1, cov24_2)*w4_16
            w2_17 ~~ c(cov24_1, cov24_2)*w4_17
            w2_18 ~~ c(cov24_1, cov24_2)*w4_18
            
            w2_05 ~~ c(cov25_1, cov25_2)*w5_05
            w2_09 ~~ c(cov25_1, cov25_2)*w5_09
            w2_11 ~~ c(cov25_1, cov25_2)*w5_11
            w2_13 ~~ c(cov25_1, cov25_2)*w5_13
            w2_14 ~~ c(cov25_1, cov25_2)*w5_14
            w2_15 ~~ c(cov25_1, cov25_2)*w5_15
            w2_16 ~~ c(cov25_1, cov25_2)*w5_16
            w2_17 ~~ c(cov25_1, cov25_2)*w5_17
            w2_18 ~~ c(cov25_1, cov25_2)*w5_18
            
            w3_05 ~~ c(cov34_1, cov34_2)*w4_05
            w3_09 ~~ c(cov34_1, cov34_2)*w4_09
            w3_11 ~~ c(cov34_1, cov34_2)*w4_11
            w3_13 ~~ c(cov34_1, cov34_2)*w4_13
            w3_14 ~~ c(cov34_1, cov34_2)*w4_14
            w3_15 ~~ c(cov34_1, cov34_2)*w4_15
            w3_16 ~~ c(cov34_1, cov34_2)*w4_16
            w3_17 ~~ c(cov34_1, cov34_2)*w4_17
            w3_18 ~~ c(cov34_1, cov34_2)*w4_18
            
            w3_05 ~~ c(cov35_1, cov35_2)*w5_05
            w3_09 ~~ c(cov35_1, cov35_2)*w5_09
            w3_11 ~~ c(cov35_1, cov35_2)*w5_11
            w3_13 ~~ c(cov35_1, cov35_2)*w5_13
            w3_14 ~~ c(cov35_1, cov35_2)*w5_14
            w3_15 ~~ c(cov35_1, cov35_2)*w5_15
            w3_16 ~~ c(cov35_1, cov35_2)*w5_16
            w3_17 ~~ c(cov35_1, cov35_2)*w5_17
            w3_18 ~~ c(cov35_1, cov35_2)*w5_18
            
            w4_05 ~~ c(cov45_1, cov45_2)*w5_05
            w4_09 ~~ c(cov45_1, cov45_2)*w5_09
            w4_11 ~~ c(cov45_1, cov45_2)*w5_11
            w4_13 ~~ c(cov45_1, cov45_2)*w5_13
            w4_14 ~~ c(cov45_1, cov45_2)*w5_14
            w4_15 ~~ c(cov45_1, cov45_2)*w5_15
            w4_16 ~~ c(cov45_1, cov45_2)*w5_16
            w4_17 ~~ c(cov45_1, cov45_2)*w5_17
            w4_18 ~~ c(cov45_1, cov45_2)*w5_18
            
            # Estimate the variance and covariance of the random intercepts
            ri1 ~~ ri1
            ri2 ~~ ri2
            ri3 ~~ ri3
            ri4 ~~ ri4
            ri5 ~~ ri5
            
            ri1 ~~ ri2
            ri1 ~~ ri3
            ri1 ~~ ri4
            ri1 ~~ ri5
            ri2 ~~ ri3
            ri2 ~~ ri4
            ri2 ~~ ri5
            ri3 ~~ ri4
            ri3 ~~ ri5
            ri4 ~~ ri5
            
            # Estimate the (residual) variance of the within-person centered variables
            w1_02 ~~ w1_02
            w1_05 ~~ c(var1_1, var1_2)*w1_05
            w1_09 ~~ c(var1_1, var1_2)*w1_09
            w1_11 ~~ c(var1_1, var1_2)*w1_11
            w1_13 ~~ c(var1_1, var1_2)*w1_13
            w1_14 ~~ c(var1_1, var1_2)*w1_14
            w1_15 ~~ c(var1_1, var1_2)*w1_15
            w1_16 ~~ c(var1_1, var1_2)*w1_16
            w1_17 ~~ c(var1_1, var1_2)*w1_17
            w1_18 ~~ c(var1_1, var1_2)*w1_18
            
            w2_02 ~~ w2_02
            w2_05 ~~ c(var2_1, var2_2)*w2_05
            w2_09 ~~ c(var2_1, var2_2)*w2_09
            w2_11 ~~ c(var2_1, var2_2)*w2_11
            w2_13 ~~ c(var2_1, var2_2)*w2_13
            w2_14 ~~ c(var2_1, var2_2)*w2_14
            w2_15 ~~ c(var2_1, var2_2)*w2_15
            w2_16 ~~ c(var2_1, var2_2)*w2_16
            w2_17 ~~ c(var2_1, var2_2)*w2_17
            w2_18 ~~ c(var2_1, var2_2)*w2_18
            
            w3_02 ~~ w3_02
            w3_05 ~~ c(var3_1, var3_2)*w3_05
            w3_09 ~~ c(var3_1, var3_2)*w3_09
            w3_11 ~~ c(var3_1, var3_2)*w3_11
            w3_13 ~~ c(var3_1, var3_2)*w3_13
            w3_14 ~~ c(var3_1, var3_2)*w3_14
            w3_15 ~~ c(var3_1, var3_2)*w3_15
            w3_16 ~~ c(var3_1, var3_2)*w3_16
            w3_17 ~~ c(var3_1, var3_2)*w3_17
            w3_18 ~~ c(var3_1, var3_2)*w3_18
            
            w4_02 ~~ w4_02
            w4_05 ~~ c(var4_1, var4_2)*w4_05
            w4_09 ~~ c(var4_1, var4_2)*w4_09
            w4_11 ~~ c(var4_1, var4_2)*w4_11
            w4_13 ~~ c(var4_1, var4_2)*w4_13
            w4_14 ~~ c(var4_1, var4_2)*w4_14
            w4_15 ~~ c(var4_1, var4_2)*w4_15
            w4_16 ~~ c(var4_1, var4_2)*w4_16
            w4_17 ~~ c(var4_1, var4_2)*w4_17
            w4_18 ~~ c(var4_1, var4_2)*w4_18
            
            w5_02 ~~ w5_02
            w5_05 ~~ c(var5_1, var5_2)*w5_05
            w5_09 ~~ c(var5_1, var5_2)*w5_09
            w5_11 ~~ c(var5_1, var5_2)*w5_11
            w5_13 ~~ c(var5_1, var5_2)*w5_13
            w5_14 ~~ c(var5_1, var5_2)*w5_14
            w5_15 ~~ c(var5_1, var5_2)*w5_15
            w5_16 ~~ c(var5_1, var5_2)*w5_16
            w5_17 ~~ c(var5_1, var5_2)*w5_17
            w5_18 ~~ c(var5_1, var5_2)*w5_18
  
            # Estimate the means
            ContactsMask.2 + ContactsMask.5 + ContactsMask.9 + ContactsMask.11 + ContactsMask.13 + ContactsMask.14 + ContactsMask.15 + ContactsMask.16 + ContactsMask.17 + ContactsMask.18 ~ 1
            InjNorms.2 + InjNorms.5 + InjNorms.9 + InjNorms.11 + InjNorms.13 + InjNorms.14 + InjNorms.15 + InjNorms.16 + InjNorms.17 + InjNorms.18 ~ 1
            DesNorms.2 + DesNorms.5 + DesNorms.9 + DesNorms.11 + DesNorms.13 + DesNorms.14 + DesNorms.15 + DesNorms.16 + DesNorms.17 + DesNorms.18 ~ 1
            FactBeliefs.2 + FactBeliefs.5 + FactBeliefs.9 + FactBeliefs.11 + FactBeliefs.13 + FactBeliefs.14 + FactBeliefs.15 + FactBeliefs.16 + FactBeliefs.17 + FactBeliefs.18 ~ 1
            PersNorms.2 + PersNorms.5 + PersNorms.9 + PersNorms.11 + PersNorms.13 + PersNorms.14 + PersNorms.15 + PersNorms.16 + PersNorms.17 + PersNorms.18 ~ 1'
  # remove constraints for unconstrained model
  if (!constrained) {
    constraints <- c("c(b11_1, b11_2)","c(b12_1, b12_2)","c(b13_1, b13_2)","c(b14_1, b14_2)","c(b15_1, b15_2)",
                     "c(b21_1, b21_2)","c(b22_1, b22_2)","c(b23_1, b23_2)","c(b24_1, b24_2)","c(b25_1, b25_2)",
                     "c(b31_1, b31_2)","c(b32_1, b32_2)","c(b33_1, b33_2)","c(b34_1, b34_2)","c(b35_1, b35_2)",
                     "c(b41_1, b41_2)","c(b42_1, b42_2)","c(b43_1, b43_2)","c(b44_1, b44_2)","c(b45_1, b45_2)",
                     "c(b51_1, b51_2)","c(b52_1, b52_2)","c(b53_1, b53_2)","c(b54_1, b54_2)","c(b55_1, b55_2)",
                     "c(cov12_1, cov12_2)","c(cov13_1, cov13_2)","c(cov14_1, cov14_2)","c(cov15_1, cov15_2)","c(cov23_1, cov23_2)",
                     "c(cov24_1, cov24_2)","c(cov25_1, cov25_2)","c(cov34_1, cov34_2)","c(cov35_1, cov35_2)","c(cov45_1, cov45_2)",
                     "c(var1_1, var1_2)","c(var2_1, var2_2)","c(var3_1, var3_2)","c(var4_1, var4_2)","c(var5_1, var5_2)")
    for (i in constraints) model <- str_replace_all(model, fixed(paste0(i, "*")), "")
  }
  # fit model
  out <- lavaan(model, data = d, missing = "fiml", 
                group = "StateElectionResult2020",
                meanstructure = TRUE, int.ov.free = TRUE)
  return(out)
}

# plot US map from zip codes
plotUSMap <- function(d) {
  # get whole world
  world <- ne_countries(scale = "medium", returnclass = "sf")
  # plot US map
  out <-
    world %>%
    filter(admin == "United States of America") %>%
    ggplot() +
    geom_sf() +
    scale_y_continuous(limits = c(25, 50)) +
    scale_x_continuous(limits = c(-125, -68)) +
    # add study data
    geom_point(data = d, aes(x = lng, y = lat), colour = "red", size = 0.5) +
    theme_void()
  # save plot
  ggsave(out, filename = "figures/pdf/map.pdf", height = 3, width = 5)
  ggsave(out, filename = "figures/png/map.png", height = 3, width = 5)
  return(out)
}

# plot attrition across all timepoints
plotAttrition <- function(d) {
  # vector of dates for plot
  dates <- c("2020-09-27", "2020-10-27", "2020-11-28",
             "2020-12-28", "2021-01-27", "2021-02-26",
             "2021-03-28", "2021-04-27", "2021-05-27",
             "2021-06-26", "2021-07-26", "2021-08-26",
             "2021-10-25", "2021-12-16", "2022-02-25",
             "2022-04-26", "2022-06-25", "2022-08-29")
  # plot
  out <- 
    d %>%
    # pivot into long format
    rename(StartDate.5 = StartDate_A, 
           StartDate.7 = StartDate...1230,
           StartDate.16 = StartDate...2919) %>%
    pivot_longer((starts_with("StartDate"))) %>%
    separate(name, c("var", "time")) %>%
    # keep only relevant vars
    transmute(time = as.numeric(time), value) %>%
    # convert time to date
    mutate(time = ymd(dates[time])) %>%
    # calculate attrition
    group_by(time) %>%
    summarise(count = sum(!is.na(value)), .groups = "drop") %>%
    # plot
    ggplot(aes(x = time, y = count)) +
    geom_point() +
    geom_line() +
    scale_x_date(name = NULL, date_labels = "%b\n%Y", date_breaks = "2 month", limits = c(ymd("2020-09-20"), ymd("2022-09-05"))) +
    scale_y_continuous(name = "Number of participants in study", limits = c(0, 1000)) +
    theme_classic()
  # save
  ggsave(out, filename = "figures/pdf/attrition.pdf", width = 5.5, height = 3)
  ggsave(out, filename = "figures/png/attrition.png", width = 5.5, height = 3)
  return(out)
}

# plot sample breakdown over time
plotAttritionBreakdown <- function(d) {
  # vector of dates for plot
  dates <- c("2020-09-27", "2020-10-27", "2020-11-28",
             "2020-12-28", "2021-01-27", "2021-02-26",
             "2021-03-28", "2021-04-27", "2021-05-27",
             "2021-06-26", "2021-07-26", "2021-08-26",
             "2021-10-25", "2021-12-16", "2022-02-25",
             "2022-04-26", "2022-06-25", "2022-08-29")
  # data with demographic breakdown over time
  dDem <-
    d %>%
    # pivot into long format
    rename(StartDate.5 = StartDate_A, 
           StartDate.7 = StartDate...1230,
           StartDate.16 = StartDate...2919) %>%
    pivot_longer((starts_with("StartDate"))) %>%
    separate(name, c("var", "time")) %>%
    # keep only relevant vars
    transmute(sex = Sex.1, age = Age.1, ethnicity = Ethnicity.1,
              time = as.numeric(time), value) %>%
    # convert time to date, and empty demographic vars if no response from participant
    mutate(
      time = ymd(dates[time]),
      sex = ifelse(is.na(value), NA, sex),
      age = ifelse(is.na(value), NA, age),
      ethnicity = ifelse(is.na(value), NA, ethnicity),
      ) %>%
    # calculate attrition
    group_by(time) %>%
    summarise(
      # sex
      propMale = mean(sex == 1, na.rm = TRUE),
      # age
      meanAge = mean(age, na.rm = TRUE),
      # ethnicity
      `propWhite or Caucasian` = mean(ethnicity == "White or Caucasian", na.rm = TRUE),
      `propAsian or Pacific Islander` = mean(ethnicity == "Asian or Pacific Islander", na.rm = TRUE),
      `propBlack or African American` = mean(ethnicity == "Black or African American", na.rm = TRUE),
      `propHispanic or Latino/a` = mean(ethnicity == "Hispanic or Latino/a", na.rm = TRUE),
      `propNative American` = mean(ethnicity == "Native American", na.rm = TRUE),
      `propOther` = mean(ethnicity == "Other", na.rm = TRUE),
      .groups = "drop"
      )
  # plot sex
  pA <-
    ggplot(dDem, aes(x = time, y = propMale)) +
    geom_point() +
    geom_line() +
    scale_x_date(name = NULL, date_labels = "%b\n%Y", date_breaks = "3 month", limits = c(ymd("2020-09-20"), ymd("2022-09-05"))) +
    scale_y_continuous(name = "Proportion of sample male", limits = c(0, 1)) +
    theme_classic()
  # plot age
  pB <-
    ggplot(dDem, aes(x = time, y = meanAge)) +
    geom_point() +
    geom_line() +
    scale_x_date(name = NULL, date_labels = "%b\n%Y", date_breaks = "3 month", limits = c(ymd("2020-09-20"), ymd("2022-09-05"))) +
    scale_y_continuous(name = "Mean age of sample", limits = c(18, 81), breaks = seq(20, 80, by = 10)) +
    theme_classic()
  # plot ethnicity
  pC <-
    dDem %>%
    dplyr::select(-propMale) %>%
    pivot_longer(starts_with("prop"), names_to = "Ethnicity", values_to = "prop") %>%
    mutate(
      Ethnicity = str_remove(Ethnicity, "prop"),
      Ethnicity = factor(Ethnicity, levels = unique(d$Ethnicity.1)[c(1:4,6,5)])
      ) %>%
    ggplot(aes(x = time, y = prop, fill = Ethnicity)) +
    geom_bar(position = "stack", stat = "identity", width = 20) +
    scale_x_date(name = NULL, date_labels = "%b\n%Y", date_breaks = "3 month", limits = c(ymd("2020-09-20"), ymd("2022-09-05"))) +
    scale_y_continuous(name = "Proportion of sample", limits = c(0, 1)) +
    theme_classic()
  # combine
  top <- plot_grid(pA, pB, nrow = 1)
  bot <- plot_grid(NULL, pC, nrow = 1, rel_widths = c(0.25, 1))
  out <- plot_grid(top, bot, nrow = 2)
  ggsave(out, file = "figures/pdf/attritionBreakdown.pdf", height = 6, width = 7)
  ggsave(out, file = "figures/png/attritionBreakdown.png", height = 6, width = 7)
  return(out)
}

# plot mask wearing, injunctive and descriptive norms, and covid cases over time
plotTimeline <- function(d, owid) {
  # vector of dates for plot
  dates <- c("2020-09-27", "2020-10-27", "2020-11-28",
             "2020-12-28", "2021-01-27", "2021-02-26",
             "2021-03-28", "2021-04-27", "2021-05-27",
             "2021-06-26", "2021-07-26", "2021-08-26",
             "2021-10-25", "2021-12-16", "2022-02-25",
             "2022-04-26", "2022-06-25", "2022-08-29")
  # first plot
  pA <- 
    d %>%
    # pivot into long format
    pivot_longer((starts_with("ContactsMask"))) %>%
    separate(name, c("var", "time")) %>%
    # keep only relevant vars
    transmute(var, time = as.numeric(time), value) %>%
    # convert time to date
    mutate(time = ymd(dates[time])) %>%
    # amend var name for plot
    mutate(var = "Mask wearing behavior") %>%
    # calculate mean and standard errors
    group_by(var, time) %>%
    summarise(m = mean(value, na.rm = T),
              se = sd(value, na.rm = T) / sqrt(sum(!is.na(value))),
              .groups = "drop") %>%
    # plot
    ggplot(aes(x = time, y = m, ymin = m - 2*se, ymax = m + 2*se)) +
    # add important dates
    geom_vline(xintercept = ymd("2020-12-11"), linetype = "dashed", colour = "grey") +
    geom_vline(xintercept = ymd("2021-03-08"), linetype = "dashed", colour = "grey") +
    geom_vline(xintercept = ymd("2021-07-27"), linetype = "dashed", colour = "grey") +
    geom_vline(xintercept = ymd("2021-11-29"), linetype = "dashed", colour = "grey") +
    geom_vline(xintercept = ymd("2022-03-03"), linetype = "dashed", colour = "grey") +
    geom_vline(xintercept = ymd("2022-03-03"), linetype = "dashed", colour = "grey") +
    geom_vline(xintercept = ymd("2022-05-12"), linetype = "dashed", colour = "grey") +
    annotate("text", x = ymd("2020-11-08"), y = 2.8, colour = "grey", size = 2.8, angle = 90, lineheight = 0.85,
             label = "FDA authorizes\nfirst COVID-19\nvaccine") +
    annotate("text", x = ymd("2021-02-03"), y = 2.5, colour = "grey", size = 2.8, angle = 90, lineheight = 0.85,
             label = "CDC: fully vaccinated\npeople can gather\nindoors without masks") +
    annotate("text", x = ymd("2021-06-07"), y = 2.0, colour = "grey", size = 2.8, angle = 90, lineheight = 0.85,
             label = "CDC: guidelines\nreinstated for\nindoor mask\nuse in high\nrisk areas") +
    annotate("text", x = ymd("2021-10-26"), y = 2.2, colour = "grey", size = 2.8, angle = 90, lineheight = 0.85,
             label = "CDC: recommend\nbooster shots\nfor adults") +
    annotate("text", x = ymd("2022-01-22"), y = 2.2, colour = "grey", size = 2.8, angle = 90, lineheight = 0.85,
             label = "CDC: update\ncommunity levels\nand ease mask\nguidelines") +
    annotate("text", x = ymd("2022-04-10"), y = 1.8, colour = "grey", size = 2.8, angle = 90, lineheight = 0.85,
             label = "US hits\n1,000,000\ndeaths") +
    # add main geoms
    geom_line(colour = "#D55E00") +
    geom_pointrange(size = 0.5, fatten = 1.5, colour = "#D55E00") +
    # axes and themes
    scale_x_date(name = NULL, date_labels = "%b\n%Y", date_breaks = "2 month", limits = c(ymd("2020-09-20"), ymd("2022-09-05"))) +
    scale_y_continuous(name = "Self-reported\nmask wearing\nbehavior", limits = c(1, 5), breaks = 1:5) +
    theme_classic() +
    theme(legend.position = "none")
  # second plot
  pB <- 
    d %>%
    # pivot into long format
    pivot_longer((starts_with("InjNorms") | starts_with("DesNorms"))) %>%
    separate(name, c("var", "time")) %>%
    # keep only relevant vars
    transmute(var, time = as.numeric(time), value) %>%
    # convert time to date
    mutate(time = ymd(dates[time])) %>%
    # amend var name for plot
    mutate(var = ifelse(var == "DesNorms", "Descriptive mask\nwearing norm", "Injunctive mask\nwearing norm"),
           var = factor(var, levels = c("Injunctive mask\nwearing norm", "Descriptive mask\nwearing norm"))) %>%
    # calculate mean and standard errors
    group_by(var, time) %>%
    summarise(m = mean(value, na.rm = T),
              se = sd(value, na.rm = T) / sqrt(sum(!is.na(value))),
              .groups = "drop") %>%
    # plot
    ggplot(aes(x = time, y = m, ymin = m - 2*se, ymax = m + 2*se, colour = var)) +
    # add important dates
    geom_vline(xintercept = ymd("2020-12-11"), linetype = "dashed", colour = "grey") +
    geom_vline(xintercept = ymd("2021-03-08"), linetype = "dashed", colour = "grey") +
    geom_vline(xintercept = ymd("2021-07-27"), linetype = "dashed", colour = "grey") +
    geom_vline(xintercept = ymd("2021-11-29"), linetype = "dashed", colour = "grey") +
    geom_vline(xintercept = ymd("2022-03-03"), linetype = "dashed", colour = "grey") +
    geom_vline(xintercept = ymd("2022-03-03"), linetype = "dashed", colour = "grey") +
    geom_vline(xintercept = ymd("2022-05-12"), linetype = "dashed", colour = "grey") +
    # add main geoms
    geom_line() +
    geom_pointrange(size = 0.5, fatten = 1.5) +
    # axes and themes
    scale_x_date(name = NULL, date_labels = "%b\n%Y", date_breaks = "2 month", limits = c(ymd("2020-09-20"), ymd("2022-09-05"))) +
    scale_y_continuous(name = "Perceived\nstrength of\nsocial norm", limits = c(1, 7), breaks = 1:7) +
    guides(colour = guide_legend(byrow = TRUE)) +
    scale_colour_manual(values = c("#009E73", "#0072B2")) +
    theme_classic() +
    theme(legend.title = element_blank(),
          legend.position = c(0.2, 0.3),
          legend.margin = margin(-2, 1, 2, 0),
          legend.box.background = element_rect(colour = "black", size = 1))
  # third plot
  pC <-
    # plot
    ggplot(owid, aes(x = date, y = new_cases_smoothed)) +
    # add important dates
    geom_vline(xintercept = ymd("2020-12-11"), linetype = "dashed", colour = "grey") +
    geom_vline(xintercept = ymd("2021-03-08"), linetype = "dashed", colour = "grey") +
    geom_vline(xintercept = ymd("2021-07-27"), linetype = "dashed", colour = "grey") +
    geom_vline(xintercept = ymd("2021-11-29"), linetype = "dashed", colour = "grey") +
    geom_vline(xintercept = ymd("2022-03-03"), linetype = "dashed", colour = "grey") +
    geom_vline(xintercept = ymd("2022-03-03"), linetype = "dashed", colour = "grey") +
    geom_vline(xintercept = ymd("2022-05-12"), linetype = "dashed", colour = "grey") +
    # add geom
    geom_line() +
    # axes and theme
    scale_x_date(name = NULL, date_labels = "%b\n%Y", date_breaks = "2 month", limits = c(ymd("2020-09-20"), ymd("2022-09-05"))) +
    scale_y_log10(name = "Daily new\nCOVID-19 cases\nin United States", limits = c(1e+04, 1e+06), labels = trans_format("log10", math_format(10^.x))) +
    theme_classic()
  # put together
  out <- plot_grid(pA, pB, pC, nrow = 3, align = "v", labels = c("a","b","c"))
  # save
  ggsave(out, filename = "figures/pdf/timeline.pdf", width = 6, height = 6)
  ggsave(out, filename = "figures/png/timeline.png", width = 6, height = 6)
  return(out)
}

# plot directed acyclic graph
plotDAG <- function() {
  # coordinates for plot
  dag_coords <- tibble(
    name = c("Mask", "DesNorm", "InjNorm", "Fact", "PersNorm", "NormSens", "Demographics"),
    x    = c(0, -1, 1, -0.75, 0.75, 0, 0),
    y    = c(1, 0, 0, 0.75, 0.75, -1, 0)
  )
  # plot
  out <-
    dagify(Mask ~ DesNorm + InjNorm + Fact + PersNorm + Demographics,
           DesNorm ~ NormSens + Demographics,
           InjNorm ~ NormSens + Demographics,
           Fact ~ DesNorm + InjNorm + Demographics,
           PersNorm ~ DesNorm + InjNorm + Demographics,
           NormSens ~ Demographics,
           latent = c("NormSens"),
           coords = dag_coords
    ) %>%
    ggplot(aes(x = x, y = y, xend = xend, yend = yend)) +
    geom_dag_point(
      data = function(x) filter(x, name != "NormSens"),
      size = 38, show.legend = FALSE, colour = "black", fill = "white", pch = 21
    ) +
    geom_dag_point(
      data = function(x) filter(x, name == "NormSens"),
      alpha = 0.5, size = 38, show.legend = FALSE, colour = "lightgrey"
    ) +
    geom_dag_text(colour = "black") +
    geom_dag_edges(start_cap = circle(14.5, 'mm'), 
                   end_cap = circle(14.5, 'mm'),
                   arrow_directed = grid::arrow(length = grid::unit(8, "pt"), type = "closed")) +
    ylim(c(-1.3, 1.3)) +
    theme_void()
  # save
  ggsave(out, filename = "figures/pdf/dag.pdf", height = 8, width = 8)
  ggsave(out, filename = "figures/png/dag.png", height = 8, width = 8)
  return(out)
}

# plot model results - correlation between behaviour and norms
plotCorBehNorm <- function(m1.1, m1.2) {
  # first plot
  pA <-
    plot(ggpredict(m1.1, terms = "DesNorms [1:7 by=0.01]")) +
    scale_x_continuous(name = "Perceived strength of\ndescriptive norms", limits = c(1, 7), breaks = 1:7) +
    scale_y_continuous(name = "Self-reported mask wearing behavior", limits = c(1, 5), breaks = 1:5) +
    ggtitle(NULL) +
    theme_classic()
  # second plot
  pB <-
    plot(ggpredict(m1.2, terms = "InjNorms [1:7 by=0.01]")) +
    scale_x_continuous(name = "Perceived strength of\ninjunctive norms", limits = c(1, 7), breaks = 1:7) +
    scale_y_continuous(name = "", limits = c(1, 5), breaks = 1:5) +
    ggtitle(NULL) +
    theme_classic()
  # update alphas and colours of ribbons
  pA$layers[[2]]$aes_params$alpha <- 0.35
  pB$layers[[2]]$aes_params$alpha <- 0.35
  pA$layers[[2]]$aes_params$fill <- "#0072B2"
  pB$layers[[2]]$aes_params$fill <- "#009E73"
  # combine
  out <- plot_grid(pA, pB, nrow = 1, labels = c("a", "b"))
  # save
  ggsave(out, filename = "figures/pdf/corBehNorm.pdf", width = 6.5, height = 3.8)
  ggsave(out, filename = "figures/png/corBehNorm.png", width = 6.5, height = 3.8)
  return(out)
}

# plot model results - change points regression
plotCDCSens <- function(m2.1, m2.2, m2.3) {
  # first plot
  pA <-
    plot(ggpredict(m2.1, terms = "timeCont [0:1 by=0.01]")) +
    geom_vline(xintercept = 0.231, linetype = "dashed", colour = "grey") +
    geom_vline(xintercept = 0.432, linetype = "dashed", colour = "grey") +
    geom_vline(xintercept = 0.687, linetype = "dashed", colour = "grey") +
    scale_x_continuous(name = "", limits = c(0, 1), breaks = seq(0, 1, by = 0.25)) +
    scale_y_continuous(name = "Self-reported\nmask wearing\nbehavior", limits = c(1, 5), breaks = 1:5) +
    ggtitle(NULL) +
    theme_classic()
  # second plot
  pB <-
    plot(ggpredict(m2.2, terms = "timeCont [0:1 by=0.01]")) +
    geom_vline(xintercept = 0.231, linetype = "dashed", colour = "grey") +
    geom_vline(xintercept = 0.432, linetype = "dashed", colour = "grey") +
    geom_vline(xintercept = 0.687, linetype = "dashed", colour = "grey") +
    scale_x_continuous(name = "", limits = c(0, 1), breaks = seq(0, 1, by = 0.25)) +
    scale_y_continuous(name = "Perceived\nstrength of\ndescriptive norms", limits = c(1, 7), breaks = 1:7) +
    ggtitle(NULL) +
    theme_classic()
  # second plot
  pC <-
    plot(ggpredict(m2.3, terms = "timeCont [0:1 by=0.01]")) +
    geom_vline(xintercept = 0.231, linetype = "dashed", colour = "grey") +
    geom_vline(xintercept = 0.432, linetype = "dashed", colour = "grey") +
    geom_vline(xintercept = 0.687, linetype = "dashed", colour = "grey") +
    scale_x_continuous(name = "Time", limits = c(0, 1), breaks = seq(0, 1, by = 0.25)) +
    scale_y_continuous(name = "Perceived\nstrength of\ninjunctive norms", limits = c(1, 7), breaks = 1:7) +
    ggtitle(NULL) +
    theme_classic()
  # update alphas and colours of ribbons
  pA$layers[[2]]$aes_params$alpha <- 0.35
  pB$layers[[2]]$aes_params$alpha <- 0.35
  pC$layers[[2]]$aes_params$alpha <- 0.35
  pA$layers[[2]]$aes_params$fill <- "#D55E00"
  pB$layers[[2]]$aes_params$fill <- "#0072B2"
  pC$layers[[2]]$aes_params$fill <- "#009E73"
  # combine
  out <- plot_grid(pA, pB, pC, ncol = 1, labels = letters[1:3])
  # save
  ggsave(out, filename = "figures/pdf/CDCSens.pdf", height = 6, width = 4)
  ggsave(out, filename = "figures/png/CDCSens.png", height = 6, width = 4)
  return(out)
}

# plot model results - riclpm autoregressive and cross-lagged effects as path diagram
plotRICLPMPath <- function(model) {
  # get parameter estimates
  pars <- parameterEstimates(model)
  stds <- standardizedSolution(model)
  # variables
  var1 <- "Mask wearing"
  var2 <- "Injunctive norm"
  var3 <- "Descriptive norm"
  col1 <- "#D55E00"
  col2 <- "#009E73"
  col3 <- "#0072B2"
  # vector of dates for plot
  dates <- c("2020-09-27", "2020-10-27", "2020-11-28",
             "2020-12-28", "2021-01-27", "2021-02-26",
             "2021-03-28", "2021-04-27", "2021-05-27",
             "2021-06-26", "2021-07-26", "2021-08-26",
             "2021-10-25", "2021-12-16", "2022-02-25",
             "2022-04-26", "2022-06-25", "2022-08-29")
  # main measurement occasions
  m <- 
    tibble(
      date = as.Date(rep(dates[c(2,5,9,11,13:18)], times = 3)),
      var = factor(rep(c(var1, var2, var3), each = 10), levels = c(var1, var3, var2)),
      col = factor(rep(c(col1, col2, col3), each = 10), levels = c(col1, col3, col2)),
      varNum = rep(c(3,1,2), each = 10)
    )
  # standardized coefficients and significance
  sc <- 
    stds %>%
    as_tibble() %>%
    left_join(as_tibble(pars), by = c("lhs", "op", "rhs")) %>%
    filter(substr(lhs, 1, 3) %in% c("w1_", "w2_", "w3_") &
             substr(rhs, 1, 3) %in% c("w1_", "w2_", "w3_") &
             op == "~") %>%
    dplyr::select(lhs, rhs, est.std, pvalue.y) %>%
    # loop over coefficients and get arrow data
    mutate(
      x      = as.Date(dates[as.numeric(substr(rhs, 4, 5))]),
      xend   = as.Date(dates[as.numeric(substr(lhs, 4, 5))]),
      y      = c(3, 1, 2)[as.numeric(substr(rhs, 2, 2))],
      yend   = c(3, 1, 2)[as.numeric(substr(lhs, 2, 2))],
      size   = ifelse(est.std >= 0, est.std, 0), # for visualisation, min effect = 0
      colour = ifelse(pvalue.y < 0.05, "black", "lightgrey")
    ) %>%
    arrange(desc(colour))
  # number to multiply y-axis by
  i <- 150
  # plot
  p <-
    # draw initial plot
    ggplot(m, aes(x = date, y = varNum*i)) + 
    geom_blank() +
    # add arrows
    geom_arrowsegment(data = sc, aes(x = x, y = y*i, xend = xend, yend = yend*i, 
                                     colour = colour, fill = colour, size = size),
                      arrows = arrow(length = unit(0.3, "cm"), type = "closed", angle = 15), 
                      position = position_attractsegment(end_shave = 11, type_shave = "distance"),
                      show.legend = TRUE) +
    # add measurement occasions
    geom_point(aes(colour = col), size = 3.5) +
    # set colours, fills, sizes, guides, and theme
    scale_colour_identity() +
    scale_fill_identity() +
    scale_size_continuous(range = c(0, 1.5), breaks = c(0.00, 0.25, 0.50)) +
    scale_y_continuous(limits = c(0.7, 3.3)*i, breaks = 1:3*i,
                       labels = function(x) c(var2, var3, var1)[x/i]) +
    scale_x_date(date_labels = "%b\n%Y", date_breaks = "2 month", limits = c(ymd("2020-09-20"), ymd("2022-09-05"))) +
    guides(size = guide_legend(title = "Std. effect size")) +
    coord_fixed() +
    theme_classic() +
    theme(axis.line.y = element_blank(),
          axis.ticks.y = element_blank(),
          axis.title = element_blank(),
          legend.key.width = unit(2, "cm"))
  # legend not working - get actual legend
  # (replace geom_arrowsegment for geom_segment in plot above)
  legend <-
    cowplot::get_legend(
      # draw initial plot
      ggplot(m, aes(x = date, y = varNum*i)) + 
        geom_blank() +
        # add arrows
        geom_segment(data = sc, aes(x = x, y = y*i, xend = xend, yend = yend*i, 
                                    colour = colour, size = size),
                     arrow = arrow(length = unit(0.3, "cm"), type = "closed", angle = 15), 
                     show.legend = TRUE) +
        scale_colour_identity() +
        scale_size_continuous(range = c(0, 1.5), breaks = c(0.00, 0.25, 0.50)) +
        guides(size = guide_legend(title = "Std. effect size")) +
        theme_classic() +
        theme(legend.key.width = unit(2, "cm"))
    )
  # replace legend
  p <- plot_grid(p + theme(legend.position = "none"), legend, nrow = 1, rel_widths = c(1, 0.2))
  # save
  ggsave(p, file = paste0("figures/pdf/path_", substitute(model), ".pdf"), width = 7.5, height = 3.5)
  ggsave(p, file = paste0("figures/png/path_", substitute(model), ".png"), width = 7.5, height = 3.5)
  return(p)
}

# plot model results - riclpm autoregressive and cross-lagged effects as coefficients
plotRICLPMCoef <- function(model) {
  # get parameter estimates
  pars <- standardizedSolution(model)
  # vector of dates for plot
  dates <- c("2020-09-27", "2020-10-27", "2020-11-28",
             "2020-12-28", "2021-01-27", "2021-02-26",
             "2021-03-28", "2021-04-27", "2021-05-27",
             "2021-06-26", "2021-07-26", "2021-08-26",
             "2021-10-25", "2021-12-16", "2022-02-25",
             "2022-04-26", "2022-06-25", "2022-08-29")
  # plot
  out <-
    pars %>%
    as_tibble() %>%
    filter(substr(lhs, 1, 3) %in% c("w1_") &
             substr(rhs, 1, 3) %in% c("w2_", "w3_") &
             op == "~") %>%
    dplyr::select(lhs, rhs, est.std, ci.lower, ci.upper) %>%
    mutate(
      Timepoint = as.Date(dates[as.numeric(substr(rhs, 4, 5))]),
      Predictor = ifelse(str_starts(rhs, "w2_"), "Injunctive norms", "Descriptive norms")
      ) %>%
    ggplot(aes(x = Timepoint, y = est.std, ymin = ci.lower, ymax = ci.upper, colour = Predictor)) +
    geom_hline(yintercept = 0, linetype = "dashed") +
    geom_pointrange(size = 0.2, position = position_dodge(width = 20), linewidth = 0.9) +
    scale_colour_manual(values = c("#0072B2", "#009E73")) +
    scale_x_date(name = NULL, date_labels = "%b\n%Y", date_breaks = "2 month",
                 limits = c(ymd("2020-09-20"), ymd("2022-08-05"))) +
    scale_y_continuous(name = "Standardized\ncross-lagged coefficient") +
    theme_classic()
  # save
  ggsave(out, file = paste0("figures/pdf/coef_", substitute(model), ".pdf"), width = 6.5, height = 3)
  ggsave(out, file = paste0("figures/png/coef_", substitute(model), ".png"), width = 6.5, height = 3)
  return(out)
}

# make supp lavaan table for constrained model
makeConstrainedTable <- function(riclpm, multigroup = FALSE) {
  # produce table
  out <-
    parameterEstimates(riclpm) %>%
    as_tibble() %>%
    filter(
      lhs %in% paste0("w", 1:3, "_05") & 
        op == "~" &
        rhs %in% paste0("w", 1:3, "_02")) %>%
    mutate_if(is.numeric, function(x) format(round(x, 2), nsmall = 2)) %>%
    arrange(rhs) %>%
    mutate(lhs = str_replace(lhs, fixed("w1_05"), "Mask wearing"),
           lhs = str_replace(lhs, fixed("w2_05"), "Injunctive norms"),
           lhs = str_replace(lhs, fixed("w3_05"), "Descriptive norms"),
           rhs = str_replace(rhs, fixed("w1_02"), "Mask wearing"),
           rhs = str_replace(rhs, fixed("w2_02"), "Injunctive norms"),
           rhs = str_replace(rhs, fixed("w3_02"), "Descriptive norms"))
  if (!multigroup) {
    transmute(
      out,
      Parameter = paste0(rhs, " (ref:rightArrow) ", lhs),
      Estimate = est,
      SE = se,
      `2.5%` = ci.lower,
      `97.5%` = ci.upper,
      p = pvalue
    )
  } else {
    transmute(
      out,
      Group = ifelse(as.numeric(group) == 1, "Democrat", "Republican"),
      Parameter = paste0(rhs, " (ref:rightArrow) ", lhs),
      Estimate = est,
      SE = se,
      `2.5%` = ci.lower,
      `97.5%` = ci.upper,
      p = pvalue
    ) %>%
      arrange(Group)
  }
}

# make supp item table
makeItemTable <- function(d) {
  tibble(
    Interpretation = c("Provides descriptive information", "", "", "",
                       "Provides injunctive information", "", "", ""),
    `Item` = c("Descriptive", "", "Injunctive", "",
               "Descriptive", "", "Injunctive", ""),
    Question = c(paste0("Does noticing the proportion of people in your area that wear ",
                        "a mask while doing recreational/social activities indoors (e.g., ",
                        "going to the gym, eating at a restaurant, attending a party) tell ",
                        "you what everyone is doing?"),
                 paste0("Does noticing the proportion of people in your area that wear a mask ",
                        "while doing routine activities indoors (e.g., running errands, shopping, ",
                        "going to work) tell you what everyone is doing?"),
                 paste0("Do mask-wearing rules encouraged in your area (e.g., by local or state ",
                        "government officials, businesses, etc.) tell you what everyone is doing?"),
                 paste0("Does how often you see people that you respect and trust wearing a mask ",
                        "(e.g., on tv, news, etc.) tell you what everyone is doing?"),
                 paste0("Does noticing the proportion of people in your area that wear a mask while ",
                        "doing recreational/social activities indoors (e.g., going to the gym, eating ",
                        "at a restaurant, attending a party) tell you what everyone should be doing?"),
                 paste0("Does noticing the proportion of people in your area that wear a mask while ",
                        "doing routine activities indoors (e.g., running errands, shopping, going to ",
                        "work) tell you what everyone should be doing?"),
                 paste0("Do mask-wearing rules encouraged in your area (e.g., by local or state ",
                        "government officials, businesses, etc.) tell you what everyone should be doing?"),
                 paste0("Does how often you see people that you respect and trust wearing a mask (e.g., ",
                        "on tv, news, etc.) tell you what everyone should be doing?")
                 ),
    Mean = c(
      mean(d$DescriptiveLearning3.7, na.rm = TRUE),
      mean(d$DescriptiveLearning4.7, na.rm = TRUE),
      mean(d$DescriptiveLearning1.7, na.rm = TRUE),
      mean(d$DescriptiveLearning2.7, na.rm = TRUE),
      mean(d$InjunctiveLearning3.7, na.rm = TRUE),
      mean(d$InjunctiveLearning4.7, na.rm = TRUE),
      mean(d$InjunctiveLearning1.7, na.rm = TRUE),
      mean(d$InjunctiveLearning2.7, na.rm = TRUE)
    ),
    SD = c(
      sd(d$DescriptiveLearning3.7, na.rm = TRUE),
      sd(d$DescriptiveLearning4.7, na.rm = TRUE),
      sd(d$DescriptiveLearning1.7, na.rm = TRUE),
      sd(d$DescriptiveLearning2.7, na.rm = TRUE),
      sd(d$InjunctiveLearning3.7, na.rm = TRUE),
      sd(d$InjunctiveLearning4.7, na.rm = TRUE),
      sd(d$InjunctiveLearning1.7, na.rm = TRUE),
      sd(d$InjunctiveLearning2.7, na.rm = TRUE)
    )
  )
}

# function for extracting change point pars
getChangePointPars <- function(model, outcome) {
  set.seed(2113)
  s <- sim(model, n.sims = 2000)
  tibble(
    Intercept = s@fixef[,1],
    Slope1    = s@fixef[,2],
    Slope2    = s@fixef[,2] + s@fixef[,3],
    Slope3    = s@fixef[,2] + s@fixef[,3] + s@fixef[,4],
    Slope4    = s@fixef[,2] + s@fixef[,3] + s@fixef[,4] + s@fixef[,5]
  ) %>%
    pivot_longer(cols = everything(), names_to = "par") %>%
    group_by(par) %>%
    summarise(
      median = median(value),
      lower95 = quantile(value, 0.025),
      upper95 = quantile(value, 0.975)
    ) %>%
    mutate(
      value = paste0(format(round(median, 2), nsmall = 2), ", 95% CI [",
                     format(round(lower95, 2), nsmall = 2), " ",
                     format(round(upper95, 2), nsmall = 2), "]")
    ) %>%
    transmute(outcome = outcome, par, median, lower95, upper95, value)
}

# make supp change points table
makeChangePointsTable <- function(m2.1, m2.2, m2.3, pars2.1, pars2.2, pars2.3) {
  # extract pars for all models
  pars2.1 %>%
    dplyr::select(outcome, par, value) %>%
    bind_rows(pars2.2 %>% dplyr::select(outcome, par, value)) %>%
    bind_rows(pars2.3 %>% dplyr::select(outcome, par, value)) %>%
    pivot_wider(names_from = outcome) %>%
    # attach N for all models
    bind_rows(tibble(par = "N", 
                     `Mask wearing`      = as.character(summary(m2.1)$devcomp$dims["N"]),
                     `Descriptive norms` = as.character(summary(m2.2)$devcomp$dims["N"]),
                     `Injunctive norms`  = as.character(summary(m2.3)$devcomp$dims["N"]))) %>%
    # attach marginal r squared for all models
    bind_rows(tibble(par = "R2 (fixed)", 
                     `Mask wearing`      = as.character(round(r.squaredGLMM(m2.1)[1], 2)),
                     `Descriptive norms` = as.character(round(r.squaredGLMM(m2.2)[1], 2)),
                     `Injunctive norms`  = as.character(round(r.squaredGLMM(m2.3)[1], 2)))) %>%
    # attach total r squared for all models
    bind_rows(tibble(par = "R2 (total)", 
                     `Mask wearing`      = as.character(round(r.squaredGLMM(m2.1)[2], 2)),
                     `Descriptive norms` = as.character(round(r.squaredGLMM(m2.2)[2], 2)),
                     `Injunctive norms`  = as.character(round(r.squaredGLMM(m2.3)[2], 2)))) %>%
    rename(` ` = par)
}

# make supp lavaan table for unconstrained model
makeUnconstrainedTable <- function(riclpm) {
  parameterEstimates(riclpm) %>%
    as_tibble() %>%
    filter(op == "~" & !(rhs %in% c("Sex.1","AgeStd.1","EthnicityBlack.1",
                                    "EthnicityAsian.1","EthnicityHispanic.1",
                                    "EthnicityOther.1","PoliticalOrientation.1",
                                    "SESStd.1"))) %>%
    arrange(rhs) %>%
    mutate_if(is.numeric, function(x) format(round(x, 2), nsmall = 2)) %>%
    mutate(lhs = str_replace(lhs, fixed("w1"), "MaskWearing"),
           lhs = str_replace(lhs, fixed("w2"), "InjunctiveNorms"),
           lhs = str_replace(lhs, fixed("w3"), "DescriptiveNorms"),
           lhs = str_replace(lhs, fixed("w4"), "FactualBeliefs"),
           lhs = str_replace(lhs, fixed("w5"), "PersonalBeliefs"),
           rhs = str_replace(rhs, fixed("w1"), "MaskWearing"),
           rhs = str_replace(rhs, fixed("w2"), "InjunctiveNorms"),
           rhs = str_replace(rhs, fixed("w3"), "DescriptiveNorms"),
           rhs = str_replace(rhs, fixed("w4"), "FactualBeliefs"),
           rhs = str_replace(rhs, fixed("w5"), "PersonalBeliefs")) %>%
    transmute(
      Parameter = paste0(rhs, " (ref:rightArrow) ", lhs),
      Estimate = est,
      SE = se,
      `2.5%` = ci.lower,
      `97.5%` = ci.upper,
      p = pvalue
    )
}

# make construct validity table
makeConstructValTable <- function(constructVal) {
  as_tibble(constructVal) %>%
    transmute(
      Type = Type,
      `Pairwise contrast` = contrast,
      Estimate = estimate,
      SE = SE,
      p = p.value
    )
}