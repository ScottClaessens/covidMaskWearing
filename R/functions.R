# custom functions

loadData <- function(fileData) {
  # load data
  out <-
    read_csv(file = fileData) %>%
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
  out <- left_join(out, zip, by = c("Zip.1" = "zipcode"))
  return(out)
}

# load US covid counts from our world in data
loadOwid <- function(fileOwid) {
  read_csv(fileOwid) %>%
    # lubridate
    mutate(date = dmy(date))
}

# t-test: construct validity
testConstructValidity <- function(d, type) {
  # how much information do the descriptive items give about X type of norms?
  y1 <- (d[,paste0(type, "Learning1.7")] + d[,paste0(type, "Learning2.7")]) / 2
  # how much information do the injunctive items give about X type of norms?
  y2 <- (d[,paste0(type, "Learning3.7")] + d[,paste0(type, "Learning4.7")]) / 2
  # paired t-test
  t.test(y1[,1], y2[,1], paired = TRUE)
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

fitRICLPM <- function(d) {
  # model code for unconstrained riclpm
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
            
            # Regression of observed variables on time-invariant control 
            ContactsMask.2 + ContactsMask.5 + ContactsMask.9 + ContactsMask.11 + ContactsMask.13 + ContactsMask.14 + ContactsMask.15 + ContactsMask.16 + ContactsMask.17 + ContactsMask.18 ~ PoliticalOrientation.1
            InjNorms.2 + InjNorms.5 + InjNorms.9 + InjNorms.11 + InjNorms.13 + InjNorms.14 + InjNorms.15 + InjNorms.16 + InjNorms.17 + InjNorms.18 ~ PoliticalOrientation.1
            DesNorms.2 + DesNorms.5 + DesNorms.9 + DesNorms.11 + DesNorms.13 + DesNorms.14 + DesNorms.15 + DesNorms.16 + DesNorms.17 + DesNorms.18 ~ PoliticalOrientation.1
            FactBeliefs.2 + FactBeliefs.5 + FactBeliefs.9 + FactBeliefs.11 + FactBeliefs.13 + FactBeliefs.14 + FactBeliefs.15 + FactBeliefs.16 + FactBeliefs.17 + FactBeliefs.18 ~ PoliticalOrientation.1
            PersNorms.2 + PersNorms.5 + PersNorms.9 + PersNorms.11 + PersNorms.13 + PersNorms.14 + PersNorms.15 + PersNorms.16 + PersNorms.17 + PersNorms.18 ~ PoliticalOrientation.1
            
            # Estimate the lagged effects between the within-person centered variables (with time-variant control)
            w1_05 ~ w1_02 + w2_02 + w3_02 + w4_02 + w5_02
            w1_09 ~ w1_05 + w2_05 + w3_05 + w4_05 + w5_05
            w1_11 ~ w1_09 + w2_09 + w3_09 + w4_09 + w5_09
            w1_13 ~ w1_11 + w2_11 + w3_11 + w4_11 + w5_11
            w1_14 ~ w1_13 + w2_13 + w3_13 + w4_13 + w5_13
            w1_15 ~ w1_14 + w2_14 + w3_14 + w4_14 + w5_14
            w1_16 ~ w1_15 + w2_15 + w3_15 + w4_15 + w5_15
            w1_17 ~ w1_16 + w2_16 + w3_16 + w4_16 + w5_16
            w1_18 ~ w1_17 + w2_17 + w3_17 + w4_17 + w5_17
            
            w2_05 ~ w1_02 + w2_02 + w3_02 + w4_02 + w5_02
            w2_09 ~ w1_05 + w2_05 + w3_05 + w4_05 + w5_05
            w2_11 ~ w1_09 + w2_09 + w3_09 + w4_09 + w5_09
            w2_13 ~ w1_11 + w2_11 + w3_11 + w4_11 + w5_11
            w2_14 ~ w1_13 + w2_13 + w3_13 + w4_13 + w5_13
            w2_15 ~ w1_14 + w2_14 + w3_14 + w4_14 + w5_14
            w2_16 ~ w1_15 + w2_15 + w3_15 + w4_15 + w5_15
            w2_17 ~ w1_16 + w2_16 + w3_16 + w4_16 + w5_16
            w2_18 ~ w1_17 + w2_17 + w3_17 + w4_17 + w5_17
            
            w3_05 ~ w1_02 + w2_02 + w3_02 + w4_02 + w5_02
            w3_09 ~ w1_05 + w2_05 + w3_05 + w4_05 + w5_05
            w3_11 ~ w1_09 + w2_09 + w3_09 + w4_09 + w5_09
            w3_13 ~ w1_11 + w2_11 + w3_11 + w4_11 + w5_11
            w3_14 ~ w1_13 + w2_13 + w3_13 + w4_13 + w5_13
            w3_15 ~ w1_14 + w2_14 + w3_14 + w4_14 + w5_14
            w3_16 ~ w1_15 + w2_15 + w3_15 + w4_15 + w5_15
            w3_17 ~ w1_16 + w2_16 + w3_16 + w4_16 + w5_16
            w3_18 ~ w1_17 + w2_17 + w3_17 + w4_17 + w5_17
            
            w4_05 ~ w1_02 + w2_02 + w3_02 + w4_02 + w5_02
            w4_09 ~ w1_05 + w2_05 + w3_05 + w4_05 + w5_05
            w4_11 ~ w1_09 + w2_09 + w3_09 + w4_09 + w5_09
            w4_13 ~ w1_11 + w2_11 + w3_11 + w4_11 + w5_11
            w4_14 ~ w1_13 + w2_13 + w3_13 + w4_13 + w5_13
            w4_15 ~ w1_14 + w2_14 + w3_14 + w4_14 + w5_14
            w4_16 ~ w1_15 + w2_15 + w3_15 + w4_15 + w5_15
            w4_17 ~ w1_16 + w2_16 + w3_16 + w4_16 + w5_16
            w4_18 ~ w1_17 + w2_17 + w3_17 + w4_17 + w5_17
            
            w5_05 ~ w1_02 + w2_02 + w3_02 + w4_02 + w5_02
            w5_09 ~ w1_05 + w2_05 + w3_05 + w4_05 + w5_05
            w5_11 ~ w1_09 + w2_09 + w3_09 + w4_09 + w5_09
            w5_13 ~ w1_11 + w2_11 + w3_11 + w4_11 + w5_11
            w5_14 ~ w1_13 + w2_13 + w3_13 + w4_13 + w5_13
            w5_15 ~ w1_14 + w2_14 + w3_14 + w4_14 + w5_14
            w5_16 ~ w1_15 + w2_15 + w3_15 + w4_15 + w5_15
            w5_17 ~ w1_16 + w2_16 + w3_16 + w4_16 + w5_16
            w5_18 ~ w1_17 + w2_17 + w3_17 + w4_17 + w5_17
            
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
            w1_05 ~~ w2_05
            w1_09 ~~ w2_09
            w1_11 ~~ w2_11
            w1_13 ~~ w2_13
            w1_14 ~~ w2_14
            w1_15 ~~ w2_15
            w1_16 ~~ w2_16
            w1_17 ~~ w2_17
            w1_18 ~~ w2_18
            
            w1_05 ~~ w3_05
            w1_09 ~~ w3_09
            w1_11 ~~ w3_11
            w1_13 ~~ w3_13
            w1_14 ~~ w3_14
            w1_15 ~~ w3_15
            w1_16 ~~ w3_16
            w1_17 ~~ w3_17
            w1_18 ~~ w3_18
            
            w1_05 ~~ w4_05
            w1_09 ~~ w4_09
            w1_11 ~~ w4_11
            w1_13 ~~ w4_13
            w1_14 ~~ w4_14
            w1_15 ~~ w4_15
            w1_16 ~~ w4_16
            w1_17 ~~ w4_17
            w1_18 ~~ w4_18
            
            w1_05 ~~ w5_05
            w1_09 ~~ w5_09
            w1_11 ~~ w5_11
            w1_13 ~~ w5_13
            w1_14 ~~ w5_14
            w1_15 ~~ w5_15
            w1_16 ~~ w5_16
            w1_17 ~~ w5_17
            w1_18 ~~ w5_18
            
            w2_05 ~~ w3_05
            w2_09 ~~ w3_09
            w2_11 ~~ w3_11
            w2_13 ~~ w3_13
            w2_14 ~~ w3_14
            w2_15 ~~ w3_15
            w2_16 ~~ w3_16
            w2_17 ~~ w3_17
            w2_18 ~~ w3_18
            
            w2_05 ~~ w4_05
            w2_09 ~~ w4_09
            w2_11 ~~ w4_11
            w2_13 ~~ w4_13
            w2_14 ~~ w4_14
            w2_15 ~~ w4_15
            w2_16 ~~ w4_16
            w2_17 ~~ w4_17
            w2_18 ~~ w4_18
            
            w2_05 ~~ w5_05
            w2_09 ~~ w5_09
            w2_11 ~~ w5_11
            w2_13 ~~ w5_13
            w2_14 ~~ w5_14
            w2_15 ~~ w5_15
            w2_16 ~~ w5_16
            w2_17 ~~ w5_17
            w2_18 ~~ w5_18
            
            w3_05 ~~ w4_05
            w3_09 ~~ w4_09
            w3_11 ~~ w4_11
            w3_13 ~~ w4_13
            w3_14 ~~ w4_14
            w3_15 ~~ w4_15
            w3_16 ~~ w4_16
            w3_17 ~~ w4_17
            w3_18 ~~ w4_18
            
            w3_05 ~~ w5_05
            w3_09 ~~ w5_09
            w3_11 ~~ w5_11
            w3_13 ~~ w5_13
            w3_14 ~~ w5_14
            w3_15 ~~ w5_15
            w3_16 ~~ w5_16
            w3_17 ~~ w5_17
            w3_18 ~~ w5_18
            
            w4_05 ~~ w5_05
            w4_09 ~~ w5_09
            w4_11 ~~ w5_11
            w4_13 ~~ w5_13
            w4_14 ~~ w5_14
            w4_15 ~~ w5_15
            w4_16 ~~ w5_16
            w4_17 ~~ w5_17
            w4_18 ~~ w5_18
            
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
            w1_05 ~~ w1_05
            w1_09 ~~ w1_09
            w1_11 ~~ w1_11
            w1_13 ~~ w1_13
            w1_14 ~~ w1_14
            w1_15 ~~ w1_15
            w1_16 ~~ w1_16
            w1_17 ~~ w1_17
            w1_18 ~~ w1_18
            
            w2_02 ~~ w2_02
            w2_05 ~~ w2_05
            w2_09 ~~ w2_09
            w2_11 ~~ w2_11
            w2_13 ~~ w2_13
            w2_14 ~~ w2_14
            w2_15 ~~ w2_15
            w2_16 ~~ w2_16
            w2_17 ~~ w2_17
            w2_18 ~~ w2_18
            
            w3_02 ~~ w3_02
            w3_05 ~~ w3_05
            w3_09 ~~ w3_09
            w3_11 ~~ w3_11
            w3_13 ~~ w3_13
            w3_14 ~~ w3_14
            w3_15 ~~ w3_15
            w3_16 ~~ w3_16
            w3_17 ~~ w3_17
            w3_18 ~~ w3_18
            
            w4_02 ~~ w4_02
            w4_05 ~~ w4_05
            w4_09 ~~ w4_09
            w4_11 ~~ w4_11
            w4_13 ~~ w4_13
            w4_14 ~~ w4_14
            w4_15 ~~ w4_15
            w4_16 ~~ w4_16
            w4_17 ~~ w4_17
            w4_18 ~~ w4_18
            
            w5_02 ~~ w5_02
            w5_05 ~~ w5_05
            w5_09 ~~ w5_09
            w5_11 ~~ w5_11
            w5_13 ~~ w5_13
            w5_14 ~~ w5_14
            w5_15 ~~ w5_15
            w5_16 ~~ w5_16
            w5_17 ~~ w5_17
            w5_18 ~~ w5_18
  
            # Estimate the means
            ContactsMask.2 + ContactsMask.5 + ContactsMask.9 + ContactsMask.11 + ContactsMask.13 + ContactsMask.14 + ContactsMask.15 + ContactsMask.16 + ContactsMask.17 + ContactsMask.18 ~ 1
            InjNorms.2 + InjNorms.5 + InjNorms.9 + InjNorms.11 + InjNorms.13 + InjNorms.14 + InjNorms.15 + InjNorms.16 + InjNorms.17 + InjNorms.18 ~ 1
            DesNorms.2 + DesNorms.5 + DesNorms.9 + DesNorms.11 + DesNorms.13 + DesNorms.14 + DesNorms.15 + DesNorms.16 + DesNorms.17 + DesNorms.18 ~ 1
            FactBeliefs.2 + FactBeliefs.5 + FactBeliefs.9 + FactBeliefs.11 + FactBeliefs.13 + FactBeliefs.14 + FactBeliefs.15 + FactBeliefs.16 + FactBeliefs.17 + FactBeliefs.18 ~ 1
            PersNorms.2 + PersNorms.5 + PersNorms.9 + PersNorms.11 + PersNorms.13 + PersNorms.14 + PersNorms.15 + PersNorms.16 + PersNorms.17 + PersNorms.18 ~ 1'
  # fit model
  out <- lavaan(model, data = d, missing = "fiml", 
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
  ggsave(out, filename = "figures/map.pdf", height = 3, width = 5)
  ggsave(out, filename = "figures/map.png", height = 3, width = 5)
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
  ggsave(out, filename = "figures/attrition.pdf", width = 5.5, height = 3)
  ggsave(out, filename = "figures/attrition.png", width = 5.5, height = 3)
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
  ggsave(out, file = "figures/attritionBreakdown.pdf", height = 6, width = 7)
  ggsave(out, file = "figures/attritionBreakdown.png", height = 6, width = 7)
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
  ggsave(out, filename = "figures/timeline.pdf", width = 6, height = 6)
  ggsave(out, filename = "figures/timeline.png", width = 6, height = 6)
  return(out)
}

# plot directed acyclic graph
plotDAG <- function() {
  # coordinates for plot
  dag_coords <- tibble(
    name = c("Mask", "DesNorm", "InjNorm", "Fact", "PersNorm", "NormSens", "PolOri"),
    x    = c(0, -1, 1, -0.75, 0.75, 0, 0),
    y    = c(1, 0, 0, 0.75, 0.75, -1, 0)
  )
  # plot
  out <-
    dagify(Mask ~ DesNorm + InjNorm + Fact + PersNorm + PolOri,
           DesNorm ~ NormSens + PolOri,
           InjNorm ~ NormSens + PolOri,
           Fact ~ DesNorm + InjNorm + PolOri,
           PersNorm ~ DesNorm + InjNorm + PolOri,
           NormSens ~ PolOri,
           latent = c("NormSens"),
           coords = dag_coords
    ) %>%
    ggplot(aes(x = x, y = y, xend = xend, yend = yend)) +
    geom_dag_point(
      data = function(x) filter(x, name != "NormSens"),
      size = 30, show.legend = FALSE, colour = "black", fill = "white", pch = 21
    ) +
    geom_dag_point(
      data = function(x) filter(x, name == "NormSens"),
      alpha = 0.5, size = 30, show.legend = FALSE, colour = "lightgrey"
    ) +
    geom_dag_text(colour = "black") +
    geom_dag_edges(start_cap = circle(11.5, 'mm'), 
                   end_cap = circle(11.5, 'mm'),
                   arrow_directed = grid::arrow(length = grid::unit(8, "pt"), type = "closed")) +
    ylim(c(-1.3, 1.3)) +
    theme_void()
  # save
  ggsave(out, filename = "figures/dag.pdf", height = 8, width = 8)
  ggsave(out, filename = "figures/dag.png", height = 8, width = 8)
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
  ggsave(out, filename = "figures/corBehNorm.pdf", width = 6.5, height = 3.8)
  ggsave(out, filename = "figures/corBehNorm.png", width = 6.5, height = 3.8)
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
  ggsave(out, filename = "figures/CDCSens.pdf", height = 6, width = 4)
  ggsave(out, filename = "figures/CDCSens.png", height = 6, width = 4)
  return(out)
}

# plot model results - riclpm autoregressive and cross-lagged effects
plotRICLPM <- function(model) {
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
  # standardised coefficients and significance
  sc <- 
    standardizedSolution(model) %>%
    as_tibble() %>%
    left_join(as_tibble(parameterEstimates(model)), by = c("lhs", "op", "rhs")) %>%
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
  ggsave(p, file = "figures/riclpm.pdf", width = 7.5, height = 3.5)
  ggsave(p, file = "figures/riclpm.png", width = 7.5, height = 3.5)
  return(p)
}

# make supp item table
makeItemTable <- function() {
  tibble(
    Interpretation = c("Provides descriptive information", "", "", "",
                       "Provides injunctive information", "", "", ""),
    `Perceived norm item` = c("Descriptive", "", "Injunctive", "",
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

# make supp lavaan table
makeLavaanTable <- function(riclpm) {
  standardizedSolution(riclpm) %>%
    as_tibble() %>%
    filter(op == "~" & rhs != "PoliticalOrientation.1") %>%
    mutate_if(is.numeric, function(x) format(round(x, 2), nsmall = 2)) %>%
    mutate(lhs = str_replace(lhs, fixed("w1"), "Mask"),
           lhs = str_replace(lhs, fixed("w2"), "Inj" ),
           lhs = str_replace(lhs, fixed("w3"), "Des" ),
           lhs = str_replace(lhs, fixed("w4"), "Fact"),
           lhs = str_replace(lhs, fixed("w5"), "Pers"),
           rhs = str_replace(rhs, fixed("w1"), "Mask"),
           rhs = str_replace(rhs, fixed("w2"), "Inj" ),
           rhs = str_replace(rhs, fixed("w3"), "Des" ),
           rhs = str_replace(rhs, fixed("w4"), "Fact"),
           rhs = str_replace(rhs, fixed("w5"), "Pers")) %>%
    arrange(rhs) %>%
    transmute(
      Parameter = paste0(rhs, " (ref:rightArrow) ", lhs),
      Estimate = est.std,
      SE = se,
      `2.5%` = ci.lower,
      `97.5%` = ci.upper
    )
}