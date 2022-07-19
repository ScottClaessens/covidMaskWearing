# custom functions

loadData <- function(fileData, fileFIPS, fileElec, filePrev) {
  # load data
  out <-
    read.csv(file = fileData, sep = '\t') %>%
    as_tibble() %>%
    # create composite measures of injunctive and descriptive norms
    mutate(InjNorms.2  = (MaskRespect.2    + MaskEncouraged.2 ) / 2,
           InjNorms.3  = (MaskRespect.3    + MaskEncouraged.3 ) / 2,
           InjNorms.5  = (MaskRespect.5    + MaskEncouraged.5 ) / 2,
           InjNorms.9  = (MaskRespect.9    + MaskEncouraged.9 ) / 2,
           InjNorms.11 = (MaskRespect.11   + MaskEncouraged.11) / 2,
           InjNorms.13 = (MaskRespect.13   + MaskEncouraged.13) / 2,
           InjNorms.14 = (MaskRespect.14   + MaskEncouraged.14) / 2,
           InjNorms.15 = (MaskRespect.15   + MaskEncouraged.15) / 2,
           DesNorms.2  = (NeighborMask1.2  + NeighborMask2.2  ) / 2,
           DesNorms.3  = (NeighborMask1.3  + NeighborMask2.3  ) / 2,
           DesNorms.5  = (NeighborMask1.5  + NeighborMask2.5  ) / 2,
           DesNorms.9  = (NeighborMask1.9  + NeighborMask2.9  ) / 2,
           DesNorms.11 = (NeighborMask1.11 + NeighborMask2.11 ) / 2,
           DesNorms.13 = (NeighborMask1.13 + NeighborMask2.13 ) / 2,
           DesNorms.14 = (NeighborMask1.14 + NeighborMask2.14 ) / 2,
           DesNorms.15 = (NeighborMask1.15 + NeighborMask2.15 ) / 2
    )
  # load county-level COVID-19 prevalence data
  prev <-
    read.csv(filePrev) %>% 
    # get average log COVID-19 prevalence across all county entries
    group_by(county_fips) %>%
    summarise(logCovidCasesPer100k = log(mean(covid_cases_per_100k)))
  # load county-level political election results
  elec <-
    read.csv(fileElec) %>%
    select(county_fips, winningparty) %>%
    unique() %>%
    mutate(winningparty = ifelse(winningparty == "REPUBLICAN", 1,
                                 ifelse(winningparty == "DEMOCRAT", 0, NA)))
  # attach data to fips (county) and zip codes
  zip <-
    read.csv(fileFIPS) %>%
    select(ZIP, STCOUNTYFP) %>%
    # join county-level COVID-19 prevalence data
    left_join(prev, by = c("STCOUNTYFP" = "county_fips")) %>%
    # join county-level political election results
    left_join(elec, by = c("STCOUNTYFP" = "county_fips")) %>%
    # filter to zip codes in main data frame
    filter(ZIP %in% out$Zip.1) %>%
    # average within zip codes
    group_by(ZIP) %>%
    summarise(logCovidCasesPer100k = mean(logCovidCasesPer100k),
              winningparty = mean(winningparty)) %>%
    # get majority winning party within zip codes
    mutate(winningparty = round(ifelse(winningparty == 0.5, NA, winningparty)))
  # join to main data frame
  out <- left_join(out, zip, by = c("Zip.1" = "ZIP"))
  # add further data on zip codes
  zip <- reverse_zipcode(unique(out$Zip.1)) %>% mutate(zipcode = as.numeric(zipcode))
  out <- left_join(out, zip, by = c("Zip.1" = "zipcode"))
  return(out)
}

# load US covid counts from our world in data
loadOwid <- function(fileOwid) {
  read_csv(fileOwid) %>%
    # lubridate
    mutate(date = dmy(date))
}

fitRICLPM <- function(d, var1, var2, var3) {
  # model code for unconstrained 3-variable 8-wave riclpm with time-invariant controls
  # https://jeroendmulder.github.io/RI-CLPM/lavaan.html
  model <- '# Create between components (random intercepts)
            ri1 =~ 1*var1.2 + 1*var1.3 + 1*var1.5 + 1*var1.9 + 1*var1.11 + 1*var1.13 + 1*var1.14 + 1*var1.15
            ri2 =~ 1*var2.2 + 1*var2.3 + 1*var2.5 + 1*var2.9 + 1*var2.11 + 1*var2.13 + 1*var2.14 + 1*var2.15
            ri3 =~ 1*var3.2 + 1*var3.3 + 1*var3.5 + 1*var3.9 + 1*var3.11 + 1*var3.13 + 1*var3.14 + 1*var3.15
            
            # Create within-person centered variables
            w1_02 =~ 1*var1.2
            w1_03 =~ 1*var1.3
            w1_05 =~ 1*var1.5
            w1_09 =~ 1*var1.9
            w1_11 =~ 1*var1.11
            w1_13 =~ 1*var1.13
            w1_14 =~ 1*var1.14
            w1_15 =~ 1*var1.15
            
            w2_02 =~ 1*var2.2
            w2_03 =~ 1*var2.3
            w2_05 =~ 1*var2.5
            w2_09 =~ 1*var2.9
            w2_11 =~ 1*var2.11
            w2_13 =~ 1*var2.13
            w2_14 =~ 1*var2.14
            w2_15 =~ 1*var2.15
            
            w3_02 =~ 1*var3.2
            w3_03 =~ 1*var3.3
            w3_05 =~ 1*var3.5
            w3_09 =~ 1*var3.9
            w3_11 =~ 1*var3.11
            w3_13 =~ 1*var3.13
            w3_14 =~ 1*var3.14
            w3_15 =~ 1*var3.15
            
            # Regression of observed variables on time-invariant predictors
            var1.2 + var1.3 + var1.5 + var1.9 + var1.11 + var1.13 + var1.14 + var1.15 ~ logCovidCasesPer100k + winningparty
            var2.2 + var2.3 + var2.5 + var2.9 + var2.11 + var2.13 + var2.14 + var2.15 ~ logCovidCasesPer100k + winningparty
            var3.2 + var3.3 + var3.5 + var3.9 + var3.11 + var3.13 + var3.14 + var3.15 ~ logCovidCasesPer100k + winningparty
            
            # Estimate the lagged effects between the within-person centered variables
            w1_03 ~ w1_02 + w2_02 + w3_02
            w1_05 ~ w1_03 + w2_03 + w3_03
            w1_09 ~ w1_05 + w2_05 + w3_05
            w1_11 ~ w1_09 + w2_09 + w3_09
            w1_13 ~ w1_11 + w2_11 + w3_11
            w1_14 ~ w1_13 + w2_13 + w3_13
            w1_15 ~ w1_14 + w2_14 + w3_14
            
            w2_03 ~ w1_02 + w2_02 + w3_02
            w2_05 ~ w1_03 + w2_03 + w3_03
            w2_09 ~ w1_05 + w2_05 + w3_05
            w2_11 ~ w1_09 + w2_09 + w3_09
            w2_13 ~ w1_11 + w2_11 + w3_11
            w2_14 ~ w1_13 + w2_13 + w3_13
            w2_15 ~ w1_14 + w2_14 + w3_14
            
            w3_03 ~ w1_02 + w2_02 + w3_02
            w3_05 ~ w1_03 + w2_03 + w3_03
            w3_09 ~ w1_05 + w2_05 + w3_05
            w3_11 ~ w1_09 + w2_09 + w3_09
            w3_13 ~ w1_11 + w2_11 + w3_11
            w3_14 ~ w1_13 + w2_13 + w3_13
            w3_15 ~ w1_14 + w2_14 + w3_14
            
            # Estimate the covariance between the within-person centered variables at the first wave
            w1_02 ~~ w2_02
            w1_02 ~~ w3_02
            w2_02 ~~ w3_02
            
            # Estimate the covariances between the residuals of the within-person centered variables
            w1_03 ~~ w2_03
            w1_05 ~~ w2_05
            w1_09 ~~ w2_09
            w1_11 ~~ w2_11
            w1_13 ~~ w2_13
            w1_14 ~~ w2_14
            w1_15 ~~ w2_15
            
            w1_03 ~~ w3_03
            w1_05 ~~ w3_05
            w1_09 ~~ w3_09
            w1_11 ~~ w3_11
            w1_13 ~~ w3_13
            w1_14 ~~ w3_14
            w1_15 ~~ w3_15
            
            w2_03 ~~ w3_03
            w2_05 ~~ w3_05
            w2_09 ~~ w3_09
            w2_11 ~~ w3_11
            w2_13 ~~ w3_13
            w2_14 ~~ w3_14
            w2_15 ~~ w3_15
            
            # Estimate the variance and covariance of the random intercepts
            ri1 ~~ ri1
            ri2 ~~ ri2
            ri3 ~~ ri3
            ri1 ~~ ri2
            ri1 ~~ ri3
            ri2 ~~ ri3
            
            # Estimate the (residual) variance of the within-person centered variables
            w1_02 ~~ w1_02
            w1_03 ~~ w1_03
            w1_05 ~~ w1_05
            w1_09 ~~ w1_09
            w1_11 ~~ w1_11
            w1_13 ~~ w1_13
            w1_14 ~~ w1_14
            w1_15 ~~ w1_15
            
            w2_02 ~~ w2_02
            w2_03 ~~ w2_03
            w2_05 ~~ w2_05
            w2_09 ~~ w2_09
            w2_11 ~~ w2_11
            w2_13 ~~ w2_13
            w2_14 ~~ w2_14
            w2_15 ~~ w2_15
            
            w3_02 ~~ w3_02
            w3_03 ~~ w3_03
            w3_05 ~~ w3_05
            w3_09 ~~ w3_09
            w3_11 ~~ w3_11
            w3_13 ~~ w3_13
            w3_14 ~~ w3_14
            w3_15 ~~ w3_15
  
            # Estimate the means
            var1.2 + var1.3 + var1.5 + var1.9 + var1.11 + var1.13 + var1.14 + var1.15 ~ 1
            var2.2 + var2.3 + var2.5 + var2.9 + var2.11 + var2.13 + var2.14 + var2.15 ~ 1
            var3.2 + var3.3 + var3.5 + var3.9 + var3.11 + var3.13 + var3.14 + var3.15 ~ 1'
  # dynamically introduce variable names
  model <- str_replace_all(model, fixed("var1"), var1)
  model <- str_replace_all(model, fixed("var2"), var2)
  model <- str_replace_all(model, fixed("var3"), var3)
  # fit model
  out <- lavaan(model, data = d, missing = "fiml",
                meanstructure = T, int.ov.free = T)
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
  return(out)
}

# plot attrition across all timepoints
plotAttrition <- function(d) {
  # vector of dates for plot
  dates <- c("2020-09-27", "2020-10-27", "2020-11-28",
             "2020-12-28", "2021-01-27", "2021-02-26",
             "2021-03-28", "2021-04-27", "2021-05-27",
             "2021-06-26", "2021-07-26", "2021-08-26",
             "2021-10-25", "2021-12-16", "2022-02-25")
  # plot
  out <- 
    d %>%
    # pivot into long format
    rename(StartDate.5 = StartDate_A, StartDate.7 = StartDate) %>%
    pivot_longer((starts_with("StartDate"))) %>%
    separate(name, c("var", "time")) %>%
    # keep only relevant vars
    transmute(time = as.numeric(time), value) %>%
    # convert time to date
    mutate(time = ymd(dates[time])) %>%
    # calculate mean and standard errors
    group_by(time) %>%
    summarise(count = sum(!is.na(value)), .groups = "drop") %>%
    # plot
    ggplot(aes(x = time, y = count)) +
    geom_point() +
    geom_line() +
    scale_x_date(name = NULL, date_labels = "%b\n%Y", date_breaks = "2 month", limits = c(ymd("2020-09-20"), ymd("2022-03-05"))) +
    scale_y_continuous(name = "Number of participants in study", limits = c(0, 1000)) +
    theme_classic()
  # save
  ggsave(out, filename = "figures/attrition.pdf", width = 5, height = 3)
  return(out)
}

# plot mask wearing, injunctive and descriptive norms, and covid cases over time
plotTimeline <- function(d, owid) {
  # vector of dates for plot
  dates <- c("2020-09-27", "2020-10-27", "2020-11-28",
             "2020-12-28", "2021-01-27", "2021-02-26",
             "2021-03-28", "2021-04-27", "2021-05-27",
             "2021-06-26", "2021-07-26", "2021-08-26",
             "2021-10-25", "2021-12-16", "2022-02-25")
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
    annotate("text", x = ymd("2020-11-14"), y = 2.8, colour = "grey", size = 2.8, angle = 90, lineheight = 0.85,
             label = "FDA authorizes\nfirst COVID-19\nvaccine") +
    annotate("text", x = ymd("2021-02-09"), y = 2.5, colour = "grey", size = 2.8, angle = 90, lineheight = 0.85,
             label = "CDC: fully vaccinated\npeople can gather\nindoors without masks") +
    annotate("text", x = ymd("2021-06-13"), y = 2.0, colour = "grey", size = 2.8, angle = 90, lineheight = 0.85,
             label = "CDC: updated\nguidelines for\nindoor mask\nuse in high\nrisk areas") +
    annotate("text", x = ymd("2021-11-01"), y = 2.2, colour = "grey", size = 2.8, angle = 90, lineheight = 0.85,
             label = "CDC: recommend\nbooster shots\nfor adults") +
    # add main geoms
    geom_line(colour = "#D55E00") +
    geom_pointrange(size = 0.5, fatten = 2, colour = "#D55E00") +
    # axes and themes
    scale_x_date(name = NULL, date_labels = "%b\n%Y", date_breaks = "2 month", limits = c(ymd("2020-09-20"), ymd("2022-03-05"))) +
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
    # add main geoms
    geom_line() +
    geom_pointrange(size = 0.5, fatten = 2) +
    # axes and themes
    scale_x_date(name = NULL, date_labels = "%b\n%Y", date_breaks = "2 month", limits = c(ymd("2020-09-20"), ymd("2022-03-05"))) +
    scale_y_continuous(name = "Perceived\nstrength of\nsocial norm", limits = c(1, 7), breaks = 1:7) +
    guides(colour = guide_legend(byrow = TRUE)) +
    scale_colour_manual(values = c("#009E73", "#0072B2")) +
    theme_classic() +
    theme(legend.title = element_blank(),
          legend.position = c(0.25, 0.3),
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
    # add geom
    geom_line() +
    # axes and theme
    scale_x_date(name = NULL, date_labels = "%b\n%Y", date_breaks = "2 month", limits = c(ymd("2020-09-20"), ymd("2022-03-05"))) +
    scale_y_log10(name = "New\nCOVID-19 cases\nin United States", limits = c(1e+04, 1e+06), labels = trans_format("log10", math_format(10^.x))) +
    theme_classic()
  # put together
  out <- plot_grid(pA, pB, pC, nrow = 3, align = "v", labels = c("a","b","c"))
  # save
  ggsave(out, filename = "figures/timeline.pdf", width = 5, height = 6)
  return(out)
}

# plot directed acyclic graph
plotDAG <- function() {
  # coordinates for plot
  dag_coords <- tibble(
    name = c("Mask", "DesNorm", "InjNorm", "NormSens", "CovidPrev", "CountyPol"),
    x    = c(0, -1, 1, 0, -0.25, 0.25),
    y    = c(1, 0, 0, -1, 0.25, -0.25)
  )
  # plot
  out <-
    dagify(Mask ~ DesNorm + InjNorm + CovidPrev + CountyPol,
           DesNorm ~ NormSens + CovidPrev + CountyPol,
           InjNorm ~ NormSens + CovidPrev + CountyPol,
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
                   end_cap = circle(11.5, 'mm')) +
    ylim(c(-1.3, 1.3)) +
    theme_void()
  # save
  ggsave(out, filename = "figures/dag.pdf", height = 4.5, width = 5.6)
  return(out)
}

# plot model results - autoregressive and cross-lagged effects
plotRICLPM <- function(model) {
  # variables
  var1 <- "Mask wearing"
  var2 <- "Injunctive norm"
  var3 <- "Descriptive norm"
  col1 <- "#D55E00"
  col2 <- "#0072B2"
  col3 <- "#009E73"
  # dates vector
  dates <- ymd(c("2020-10-27", "2020-11-28", "2021-01-27", "2021-05-27",
                 "2021-07-26", "2021-10-25", "2021-12-16", "2022-02-25"))
  # main measurement occasions
  m <- 
    tibble(
      date = rep(dates, times = 3),
      var = factor(rep(c(var1, var2, var3), each = 8), levels = c(var1, var3, var2)),
      col = factor(rep(c(col1, col2, col3), each = 8), levels = c(col1, col3, col2))
    )
  # standardised coefficients and significance
  sc <- 
    standardizedSolution(model) %>%
    as_tibble() %>%
    left_join(as_tibble(parameterEstimates(model)), by = c("lhs", "op", "rhs")) %>%
    filter(op == "~" & !(rhs %in% c("logCovidCasesPer100k","winningparty"))) %>%
    select(lhs, rhs, est.std, pvalue.y)
  # loop over coefficients and get arrow data
  draw <- tibble()
  for (i in 1:63) {
    # get index for wave number
    j <- ceiling(i / 3)
    while (j > 7) j <- j - 7
    # tibble for drawing
    draw <-
      bind_rows(
        draw,
        tibble(
          x = dates[!!j], 
          y = ifelse(!!i %% 3 == 1, var1,
                     ifelse(!!i %% 3 == 2, var2, var3)), 
          xend = dates[!!j + 1], 
          yend = ifelse(!!i %in% 1:21, var1,
                        ifelse(!!i %in% 22:42, var2, var3)),
          size = abs(sc$est.std[i]),
          colour = ifelse(sc$pvalue.y[i] < 0.05, "black", "lightgrey")
        )
      )
  }
  # plot
  p <-
    # draw plot
    ggplot(m, aes(x = date, y = fct_rev(var))) + 
    geom_blank() +
    # add arrows
    geom_segment(data = draw, aes(x = x, y = y, xend = xend, yend = yend,
                                  size = size, colour = colour),
                 arrow = arrow(length = unit(0.4, "cm"), type = "closed", angle = 15), 
                 show.legend = TRUE) +
    # add measurement occasions
    geom_point(aes(colour = col), size = 5, alpha = 0.2) +
    # set colours, sizes, guides, and theme
    scale_colour_identity() +
    scale_size_continuous(range = c(0, 1), breaks = c(0.05, 0.25, 0.45)) +
    scale_x_date(date_labels = "%b\n%Y", date_breaks = "2 month", limits = c(ymd("2020-09-20"), ymd("2022-03-05"))) +
    guides(size = guide_legend(title = "Std. effect size")) +
    theme_classic() +
    theme(axis.line.y = element_blank(),
          axis.ticks.y = element_blank(),
          axis.title = element_blank(),
          legend.key.width = unit(2, "cm"))
  # save
  ggsave(p, file = "figures/resultsRICLPM.pdf", width = 7, height = 3)
  return(p)
}
