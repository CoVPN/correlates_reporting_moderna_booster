##################################################
# obligatory to append to the top of each script #
renv::activate(project = here::here(".."))
source(here::here("..", "_common.R")) #
##################################################

library(survey)
library(tidyverse)
library(dplyr, warn.conflicts = FALSE)
# Suppress summarise info
options(dplyr.summarise.inform = FALSE)
# For stratum with 1 ppt
options(survey.lonely.psu="adjust")
source(here::here("code", "make_functions.R"))


###################################################
#                  Parameters                     #
###################################################
# To select which tables are included in the report.
# Also to modify the headers and footers for each table.

if(study_name=="COVEBoost"){
  config$tpeak <- 29
  config$WtStratum <- "Wstratum"
  config$wt <- "wt.BD29"
  config$ph2 <- "ph2.BD29"
  tpeak <- timepoints <- 29
  times <- c("BD1", "BD29", "DD1", "DeltaBD29overBD1", "DeltaDD1overBD1")
  labels.time <- c(BD1="Post-booster Day 1", BD29="Post-booster Day 29", DD1="Disease Day 1",
                   DeltaBD29overBD1="BD29 fold-rise over BD1", DeltaDD1overBD1="Disease Day 1 fold-rise over BD1")
}



tlf <-
  list(
    tab_dm_ph1_all = list(
      table_header = "Demographic and Clinical Characteristics in
      the Per-Protocol Cohort (Phase 1)",
      deselect = "subgroup",
      pack_row = "subgroup",
      col1="7cm"
    ),

    tab_dm_neg_ph1 = list(
      table_header = "Demographic and Clinical Characteristics in
      the Naive Per-Protocol Cohort (Phase 1)",
      deselect = "subgroup",
      pack_row = "subgroup",
      col1="7cm"
    ),

    tab_dm_pos_ph1 = list(
      table_header = "Demographic and Clinical Characteristics in
      the Non-Naive Per-Protocol Cohort (Phase 1)",
      deselect = "subgroup",
      pack_row = "subgroup",
      col1="7cm"
    ),

    tab_dm_all = list(
      table_header = "Demographic and Clinical Characteristics in
      the Per-Protocol Cohort (Phase 2)",
      deselect = "subgroup",
      pack_row = "subgroup",
      col1="7cm"
    ),

    tab_dm_neg = list(
      table_header = "Demographic and Clinical Characteristics in
      the Naive Per-Protocol Cohort (Phase 2)",
      deselect = "subgroup",
      pack_row = "subgroup",
      col1="7cm"
    ),

    tab_dm_pos = list(
      table_header = "Demographic and Clinical Characteristics in
      the Non-Naive Per-Protocol Cohort (Phase 2)",
      deselect = "subgroup",
      pack_row = "subgroup",
      col1="7cm"
    ),

    tab_strtm1 = list(
      table_header = "",
      deselect = "Arm",
      pack_row = "Arm"
    ),

    tab_strtm2 = list(
      table_header = "",
      deselect = "Arm",
      pack_row = "Arm"
    ),

    tab_case_cnt = list(
      table_header = "Availability of immunogenicity data by case status",
      deselect = "Arm",
      pack_row = "Arm",
      table_footer = c("The $+$ (available) and $-$ (unavailable) in the column
                       labels refer to the availability of the BD1, BD29 and BD57 markers, respectively."),
      col1="7cm"),

    tab_fu = list(
      table_header = "Average duration of follow-up post BD29 for cases and non-cases, stratified by naive/non-naive status"),

    tab_days = list(
      table_header = sprintf("Duration from vaccination to BD%s visit in the per-protocol cohort", config$tpeak),
      deselect = "Arm",
      pack_row = "Arm"
    ),

    tab_case_ba = list(
      table_header = "Antibody levels to BA.1 in the per-protocol cohort",
      table_footer =c("Case = COVID-19 Omicron BA.1 endpoints occured in the interval [$\\\\geq 7$ days post BD29 AND $\\\\geq$ Dec 1 2021, May 2022 data base lock date].
                      Non-case = No acquirements of COVID-19 (of any strain) in the interval [BD1, data base lock date]",
                      "Naive = No evidence of SARS-CoV-2 infection from enrollment through to BD1.
                      Non-naive = Any evidence of SARS-CoV-2 infection in the interval [$\\\\geq 14$ days after the original 2-dose series, BD1]",
        "N is the number of cases sampled into the subcohort within baseline covariate strata.",
        "The denominator in Resp Rate is the number of participants in the whole stage 2 per-protocol cohort within baseline covariate strata, calculated using inverse probability weighting."),

      col_name = c("Status", "Marker", "Visit","N", "Resp rate", "GMT/GMC", "N",
                   "Resp rate", "GMT/GMC", "Resp Rate\nDifference", "GMTR/GMCR"),
      header_above1 = c(" "=3, "Cases*" = 3, "Non-Cases/Control" = 3,
                        "Comparison" = 2),
      col1=".8cm",
      font_size=9),

    tab_case_g = list(
      table_header = "Antibody levels to D614 in the per-protocol cohort",
      table_footer =c("Case = COVID-19 Omicron BA.1 endpoints occured in the interval [$\\\\geq 7$ days post BD29 AND $\\\\geq$ Dec 1 2021, May 2022 data base lock date].
                      Non-case = No acquirements of COVID-19 (of any strain) in the interval [BD1, data base lock date]",
                      "Naive = No evidence of SARS-CoV-2 infection from enrollment through to BD1.
                      Non-naive = Any evidence of SARS-CoV-2 infection in the interval [$\\\\geq 14$ days after the original 2-dose series, BD1]",
                      "N is the number of cases sampled into the subcohort within baseline covariate strata.",
                      "The denominator in Resp Rate is the number of participants in the whole stage 2 per-protocol cohort within baseline covariate strata, calculated using inverse probability weighting."),

      col_name = c("Status", "Marker", "Visit","N", "Resp rate", "GMT/GMC", "N",
                   "Resp rate", "GMT/GMC", "Resp Rate\nDifference", "GMTR/GMCR"),
      header_above1 = c(" "=3, "Cases*" = 3, "Non-Cases/Control" = 3,
                        "Comparison" = 2),
      col1=".8cm",
      font_size=9),

    tab_case_cnt = list(
      table_header = "Availability of immunogenicity data by case status",
      deselect = "Arm",
      pack_row = "Arm",
      table_footer = c("The $+$ (available) and $-$ (unavailable) in the column
                       labels refer to the availability of the BD1, BD29 and BD57 markers, respectively."),
      col1="7cm"),


    tab1 = list(
      table_header = "Percentage of Responders by Visits",
      col_name = c("Arm", "Case Status", "Naive/Non-Naive", "Marker", "Post-booster Day 1", "Post-booster Day 29",
                   "Disease Day 1"),
      header_above1 = c(" "=4, "Visits" = 3),
      col1="1cm"),

    tab2 = list(
      table_header = "Percentage of Responders at Post-booster Day 1 (BD1) by Boosting Period",
      col_name = c("Arm", "Case Status", "Naive/Non-Naive", "Marker", "Sep23-Oct15 2021", "Oct16-Oct31 2021",
                   "Nov 2021", "Dec 2021"),
      header_above1 = c(" "=4, "Boosted Period" = 4),
      col1="1cm"),

    tab3 = list(
      table_header = "Percentage of Responders at Post-booster Day 29 (BD29) by Boosting Period",
      col_name = c("Arm", "Case Status", "Naive/Non-Naive", "Marker", "Sep23-Oct15 2021", "Oct16-Oct31 2021",
                   "Nov 2021", "Dec 2021"),
      header_above1 = c(" "=4, "Boosted Period" = 4),
      col1="1cm"),

    tab4 = list(
      table_header = "Percentage of Responders at Disease Day 1 (DD1) by Boosting Period",
      col_name = c("Arm", "Case Status", "Naive/Non-Naive", "Marker", "Sep23-Oct15 2021", "Oct16-Oct31 2021",
                   "Nov 2021", "Dec 2021"),
      header_above1 = c(" "=4, "Boosted Period" = 4),
      col1="1cm"),

    tab5 = list(
      table_header = "Geometric mean titers (GMTs) and geometric mean concentrations (GMCs) by Visits",
      col_name = c("Arm", "Case Status", "Naive/Non-Naive", "Marker", "Post-booster Day 1", "Post-booster Day 29",
                   "Disease Day 1"),
      header_above1 = c(" "=4, "Visits" = 3),
      col1="1cm"),


    tab6 = list(
      table_header = "Geometric mean titers (GMTs) and geometric mean concentrations (GMCs) at Post-booster Day 1 (BD1) by Boosting Period",
      col_name = c("Arm", "Case Status", "Naive/Non-Naive", "Marker", "Sep23-Oct15 2021", "Oct16-Oct31 2021",
                   "Nov 2021", "Dec 2021"),
      header_above1 = c(" "=4, "Boosted Period" = 4),
      col1="1cm"),

    tab7 = list(
      table_header = "Geometric mean titers (GMTs) and geometric mean concentrations (GMCs) at Post-booster Day 29 (BD29) by Boosting Period",
      col_name = c("Arm", "Case Status", "Naive/Non-Naive", "Marker", "Sep23-Oct15 2021", "Oct16-Oct31 2021",
                   "Nov 2021", "Dec 2021"),
      header_above1 = c(" "=4, "Boosted Period" = 4),
      col1="1cm"),

    tab8 = list(
      table_header = "Geometric mean titers (GMTs) and geometric mean concentrations (GMCs) at Disease Day 1 (DD1) by Boosting Period",
      col_name = c("Arm", "Case Status", "Naive/Non-Naive", "Marker", "Sep23-Oct15 2021", "Oct16-Oct31 2021",
                   "Nov 2021", "Dec 2021"),
      header_above1 = c(" "=4, "Boosted Period" = 4),
      col1="1cm"),


    tab9 = list(
      table_header = "Geometric mean titer ratios (GMTRs) or geometric mean concentration ratios (GMCRs) between post-booster/
pre-booster",
      col_name = c("Arm", "Case Status", "Naive/Non-Naive", "Marker",  "BD29 fold-rise over BD1", "Disease Day 1 fold-rise over BD1"),
      col1="1cm"),


    tab10 = list(
      table_header = "Geometric mean titer ratios (GMTRs) or geometric mean concentration ratios (GMCRs) between BD29/BD1 by Boosting Period",
      col_name = c("Arm", "Case Status", "Naive/Non-Naive", "Marker", "Sep23-Oct15 2021", "Oct16-Oct31 2021",
                   "Nov 2021", "Dec 2021"),
      header_above1 = c(" "=4, "Boosted Period" = 4),
      col1="1cm"),

    tab11 = list(
      table_header = "Geometric mean titer ratios (GMTRs) or geometric mean concentration ratios (GMCRs) between DD1/BD1 by Boosting Period",
      col_name = c("Arm", "Case Status", "Naive/Non-Naive", "Marker", "Sep23-Oct15 2021", "Oct16-Oct31 2021",
                   "Nov 2021", "Dec 2021"),
      header_above1 = c(" "=4, "Boosted Period" = 4),
      col1="1cm"),

    tab12 = list(
      table_header = "Differences in Positive Response Rates (95% CI) between Cases and Controls by Visits",
      col_name = c("Arm", "Naive/Non-Naive", "Marker", "Post-booster Day 1", "Post-booster Day 29"),
      header_above1 = c(" "=3, "Visits" = 2),
      col1="1cm"),


    tab13 = list(
      table_header = "Differences in Positive Response Rates (95% CI) between Cases and Controls at Post-booster Day 1 (BD1) by Boosting Period",
      col_name = c("Arm", "Naive/Non-Naive", "Marker", "Sep23-Oct15 2021", "Oct16-Oct31 2021",
                   "Nov 2021", "Dec 2021"),
      header_above1 = c(" "=3, "Boosted Period" = 4),
      col1="1cm"),

    tab14 = list(
      table_header = "Differences in Positive Response Rates (95% CI) between Cases and Controls at Post-booster Day 29 (BD29) by Boosting Period",
      col_name = c("Arm", "Naive/Non-Naive", "Marker", "Sep23-Oct15 2021", "Oct16-Oct31 2021",
                   "Nov 2021", "Dec 2021"),
      header_above1 = c(" "=3, "Boosted Period" = 4),
      col1="1cm"),

    tab15 = list(
      table_header = "Geometric mean ratio (95% CI) between Cases and Controls by Visits",
      col_name = c("Arm", "Naive/Non-Naive", "Marker", "Post-booster Day 1", "Post-booster Day 29"),
      header_above1 = c(" "=3, "Visits" = 2),
      col1="1cm"),


    tab16 = list(
      table_header = "Geometric mean ratio (95% CI) between Cases and Controls at Post-booster Day 1 (BD1) by Boosting Period",
      col_name = c("Arm", "Naive/Non-Naive", "Marker", "Sep23-Oct15 2021", "Oct16-Oct31 2021",
                   "Nov 2021", "Dec 2021"),
      header_above1 = c(" "=3, "Boosted Period" = 4),
      col1="1cm"),

    tab17 = list(
      table_header = "Geometric mean ratio (95% CI) between Cases and Controls at Post-booster Day 29 (BD29) by Boosting Period",
      col_name = c("Arm", "Naive/Non-Naive", "Marker", "Sep23-Oct15 2021", "Oct16-Oct31 2021",
                   "Nov 2021", "Dec 2021"),
      header_above1 = c(" "=3, "Boosted Period" = 4),
      col1="1cm"),

    tab18 = list(
      table_header = "Geometric mean ratio (95% CI) between Naive and Non-Naive by Visits",
      col_name = c("Arm", "Case", "Marker", "Post-booster Day 1", "Post-booster Day 29"),
      header_above1 = c(" "=3, "Visits" = 2),
      col1="1cm"),

    tab19 = list(
      table_header = "Geometric mean ratio (95% CI) between Vaccine and Placebo by Visits",
      col_name = c("Naive/Non-Naive", "Case", "Marker", "Post-booster Day 1", "Post-booster Day 29"),
      header_above1 = c(" "=3, "Visits" = 2),
      col1="1cm")
    )



labels.age <- case_when(study_name %in% c("ENSEMBLE", "MockENSEMBLE") ~ c("Age 18 - 59", "Age $\\geq$ 60"),
                        TRUE~ c("Age $<$ 65", "Age $\\geq$ 65"))

labels.minor <- c("Communities of Color", "White Non-Hispanic")

labels.BMI <- c("Underweight BMI < 18.5", "Normal 18.5 $\\leq$ BMI < 25",
                "Overweight 25 $\\leq$ BMI < 30", "Obese BMI $\\geq$ 30")


assays <- assay_metadata$assay

uloqs  <-  assay_metadata$uloq; names(uloqs) <- assays

pos.cutoffs <- assay_metadata$pos.cutoff; names(pos.cutoffs) <- assays


visits <- names(labels.time)

resp.lb <- expand.grid(
  time = visits, marker = assays,
  # ind = c("Resp", "FR2", "FR4"),
  stringsAsFactors = F
) %>%
  unite("mag_cat", time, marker, sep="", remove=F) %>%
  mutate(resp_cat=paste0(mag_cat, "Resp"),
         Visit=labels.time[time],
         Marker=assay_labels[marker])


###################################################
#                Clean the Data                   #
###################################################

### Table 1. Demographics
# Output: tab_dm
# Select the covariates to be summarised.
# num_v are columns from ds_long;
# cat_v are rows of `subgroup`


# The stratified random cohort for immunogenicity
ds_s <- dat %>%
  mutate(
    raceC = as.character(race),
    ethnicityC = case_when(EthnicityHispanic==1 ~ "Hispanic or Latino",
                           EthnicityHispanic==0 & EthnicityNotreported==0 &
                           EthnicityUnknown==0 ~ "Not Hispanic or Latino",
                           EthnicityNotreported==1 |
                           EthnicityUnknown==1 ~ "Not reported and unknown "),
    RaceEthC = case_when(
      WhiteNonHispanic==1 ~ "White Non-Hispanic ",
      TRUE ~ raceC
    ),
    MinorityC = case_when(
      MinorityInd == 1 ~ "Communities of Color",
      MinorityInd == 0 ~ "White Non-Hispanic"
    ),
    HighRiskC = ifelse(HighRiskInd == 1, "At-risk", "Not at-risk"),
    AgeC = ifelse(Senior == 1, labels.age[2], labels.age[1]),
    SexC = ifelse(Sex == 1, "Female", "Male"),
    AgeRiskC = paste(AgeC, HighRiskC),
    AgeSexC = paste(AgeC, SexC),
    AgeMinorC = ifelse(is.na(MinorityC), NA, paste(AgeC, MinorityC)),
    `Baseline SARS-CoV-2` = factor(ifelse(Bserostatus == 1, "Positive", "Negative"),
                                   levels = c("Negative", "Positive")
    ),
    Arm = factor(ifelse(Trt == 1, "Vaccine", "Placebo"),
                 levels = c("Vaccine", "Placebo")),

    Status = ifelse(naive==1, "Naive", "Non-Naive"),
    AgeRisk1 = ifelse(AgeC==labels.age[1], AgeRiskC, NA),
    AgeRisk2 = ifelse(AgeC==labels.age[2], AgeRiskC, NA),
    All = "All participants"
    )

if(study_name %in% c("ENSEMBLE", "MockENSEMBLE")){
  ds_s <- ds_s %>%
    mutate(CountryC=labels.countries.ENSEMBLE[Country+1],
           RegionC=labels.regions.ENSEMBLE[Region+1],
           URMC = case_when(URMforsubcohortsampling == 1 & Country ==0 ~ "Communities of Color",
                            URMforsubcohortsampling == 0 & Country ==0 ~ "White Non-Hispanic",
                            TRUE ~ as.character(NA)),
           AgeURM = case_when(is.na(URMC) ~ as.character(NA),
                              TRUE ~ paste(AgeC, URMC)),
           # demo.stratum.ordered=demo.stratum,
           HIVC = c("Positive", "Negative")[2-HIVinfection],
           BMI = case_when(max(BMI, na.rm=T) < 5 ~ labels.BMI[BMI],
                           BMI>=30 ~ "Obese BMI $\\geq$ 30",
                           BMI>=25 ~ "Overweight 25 $\\leq$ BMI < 30",
                           BMI>=18.5 ~ "Normal 18.5 $\\leq$ BMI < 25",
                           BMI<18.5 ~ "Underweight BMI < 18.5")
           )
}

if(study_name %in% c("AZD1222")){
  ds_s <- ds_s %>%
    mutate(CountryC=c("Chile", "Peru", "United States")[Country+1])
}

# Step2: Responders
# Post baseline visits
ds <- getResponder(ds_s, times=times[!grepl("Delta", times)],
                   assays=assays, pos.cutoffs = pos.cutoffs)

subgrp <- c(
  All = "All participants",
  AgeC = "Age",
  BMI="BMI",
  HighRiskC = "Risk for Severe Covid-19",
  AgeRiskC = "Age, Risk for Severe Covid-19",
  AgeRisk1 = paste0(labels.age[1], ", Risk for Severe Covid-19"),
  AgeRisk2 = paste0(labels.age[2], ", Risk for Severe Covid-19"),
  SexC = "Sex",
  AgeSexC = "Age, sex",
  ethnicityC = "Hispanic or Latino ethnicity",
  RaceEthC = "Race",
  MinorityC = "Underrepresented minority status",
  AgeMinorC = "Age, Communities of color",
  URMC = "Underrepresented Minority Status in the U.S.",
  AgeURM = "Age, Underrepresented Minority Status in the U.S.",
  CountryC = "Country",
  HIVC = "HIV Infection"
)


###################################################
#             Generating the Tables               #
###################################################

# Setup empty tables

for (i in names(tlf)){
  assign(i, NULL)
}

if (study_name %in% c("COVE", "MockCOVE", "COVEBoost")) {
  num_v1 <- c("Age") # Summaries - Mean & Range
  num_v2 <- c("BMI") # Summaries - Mean & St.d
  cat_v <- c("AgeC", "SexC", "raceC", "ethnicityC", "HighRiskC", "AgeRiskC", "MinorityC")
} else if (study_name %in% c("ENSEMBLE", "MockENSEMBLE")) {
  num_v1 <- c("Age") # Summaries - Mean & Range
  num_v2 <- NULL # Summaries - Mean & St.d
  cat_v <- c("AgeC", "SexC", "raceC", "ethnicityC",
             "HighRiskC", "AgeRiskC", "URMC",  "CountryC", "HIVC", "BMI")
} else if (study_name %in% c("AZD1222")) {
  num_v1 <- c("Age") # Summaries - Mean &ßß Range
  num_v2 <- NULL # Summaries - Mean & St.d
  cat_v <- c("AgeC", "SexC", "CountryC", "raceC", "ethnicityC", "HighRiskC", "AgeRiskC")
} else{ # Keeping the minimal
  num_v1 <- c("Age") # Summaries - Mean & Range
  num_v2 <- NULL # Summaries - Mean & St.d
  cat_v <- c("AgeC", "SexC", "raceC", "ethnicityC", "HighRiskC", "AgeRiskC")
}

ds_long_ttl <- ds %>%
  dplyr::filter(!!as.name(paste0("ph2.BD", tpeak))) %>%
  bind_rows(mutate(., Arm="Total")) %>%
  mutate(AgeRiskC = ifelse(grepl("$\\geq$ 65", AgeRiskC, fixed=T), "Age $\\geq$ 65 ", AgeRiskC)) %>%
  mutate_all(as.character) %>%
  pivot_longer(all_of(c(num_v1, num_v2, cat_v)), names_to="subgroup", values_to="subgroup_cat")

ds_long_ttl_ph1 <- ds %>%
  dplyr::filter(!!as.name(config$ph1)) %>%
  bind_rows(mutate(., Arm="Total")) %>%
  mutate(AgeRiskC = ifelse(grepl("$\\geq$ 65", AgeRiskC, fixed=T), "Age $\\geq$ 65 ", AgeRiskC)) %>%
  mutate_all(as.character) %>%
  pivot_longer(all_of(c(num_v1, num_v2, cat_v)), names_to="subgroup", values_to="subgroup_cat")


# Calculate % for categorical covariates
dm_cat <- inner_join(
  ds_long_ttl %>%
    group_by(Status, Arm, subgroup, subgroup_cat) %>%
    summarise(n = n(), .groups = 'drop'),
  ds_long_ttl %>%
    group_by(Status, Arm, subgroup) %>%
    summarise(N = n(), .groups = 'drop'),
  by = c("Status", "Arm", "subgroup")
) %>%
  mutate(pct = n / N,
         rslt1 = sprintf("%s (%.1f%%)", n, n / N * 100),
         rslt2 = sprintf("%s/%s = %.1f%%", n, N, n / N * 100)) %>%
  dplyr::filter(subgroup %in% cat_v)


dm_cat_ph1 <- inner_join(
  ds_long_ttl_ph1 %>%
    group_by(Status, Arm, subgroup, subgroup_cat) %>%
    summarise(n = n(), .groups = 'drop'),
  ds_long_ttl_ph1 %>%
    group_by(Status, Arm, subgroup) %>%
    summarise(N = n(), .groups = 'drop'),
  by = c("Status", "Arm", "subgroup")
) %>%
  mutate(pct = n / N,
         rslt1 = sprintf("%s (%.1f%%)", n, n / N * 100),
         rslt2 = sprintf("%s/%s = %.1f%%", n, N, n / N * 100)) %>%
  dplyr::filter(subgroup %in% cat_v)

dm_cat_all <- inner_join(
  ds_long_ttl %>%
    mutate(Status=ifelse(Arm=="Total", "Total", Status)) %>%
    group_by(Status, subgroup, subgroup_cat) %>%
    summarise(n = n(), .groups = 'drop'),
  ds_long_ttl %>%
    mutate(Status=ifelse(Arm=="Total", "Total", Status)) %>%
    group_by(Status, subgroup) %>%
    summarise(N = n(), .groups = 'drop'),
  by = c("Status", "subgroup")
) %>%
  mutate(pct = n / N,
         rslt1 = sprintf("%s (%.1f%%)", n, n / N * 100),
         rslt2 = sprintf("%s/%s = %.1f%%", n, N, n / N * 100)) %>%
  dplyr::filter(subgroup %in% cat_v)

dm_cat_ph1_all <- inner_join(
  ds_long_ttl_ph1 %>%
    mutate(Status=ifelse(Arm=="Total", "Total", Status)) %>%
    group_by(Status, subgroup, subgroup_cat) %>%
    summarise(n = n(), .groups = 'drop'),
  ds_long_ttl_ph1 %>%
    mutate(Status=ifelse(Arm=="Total", "Total", Status)) %>%
    group_by(Status, subgroup) %>%
    summarise(N = n(), .groups = 'drop'),
  by = c("Status", "subgroup")
) %>%
  mutate(pct = n / N,
         rslt1 = sprintf("%s (%.1f%%)", n, n / N * 100),
         rslt2 = sprintf("%s/%s = %.1f%%", n, N, n / N * 100)) %>%
  dplyr::filter(subgroup %in% cat_v)

# Calculate mean and range for numeric covariates
dm_num <- ds_long_ttl %>%
  dplyr::filter(subgroup %in% c(num_v1, num_v2)) %>%
  mutate(subgroup_cat=as.numeric(subgroup_cat)) %>%
  group_by(Status, Arm, subgroup) %>%
  summarise(
    min = min(subgroup_cat, na.rm = T),
    max = max(subgroup_cat, na.rm = T),
    mean = mean(subgroup_cat, na.rm = T),
    sd = sd(subgroup_cat, na.rm = T),
    rslt1 = sprintf("%.1f (%.1f, %.1f)", mean, min, max),
    rslt2 = sprintf("%.1f $\\pm$ %.1f", mean, sd),
    N = n(),
    .groups = 'drop'
  ) %>%
  mutate(subgroup_cat = case_when(subgroup %in% num_v1 ~ "Mean (Range)",
                                  subgroup %in% num_v2 ~ "Mean $\\pm$ SD"),
         subgroup=ifelse(subgroup=="Age", "AgeC", subgroup))


dm_num_ph1 <- ds_long_ttl_ph1 %>%
  dplyr::filter(subgroup %in% c(num_v1, num_v2)) %>%
  mutate(subgroup_cat=as.numeric(subgroup_cat)) %>%
  group_by(Status, Arm, subgroup) %>%
  summarise(
    min = min(subgroup_cat, na.rm = T),
    max = max(subgroup_cat, na.rm = T),
    mean = mean(subgroup_cat, na.rm = T),
    sd = sd(subgroup_cat, na.rm = T),
    rslt1 = sprintf("%.1f (%.1f, %.1f)", mean, min, max),
    rslt2 = sprintf("%.1f $\\pm$ %.1f", mean, sd),
    N = n(),
    .groups = 'drop'
  ) %>%
  mutate(subgroup_cat = case_when(subgroup %in% num_v1 ~ "Mean (Range)",
                                  subgroup %in% num_v2 ~ "Mean $\\pm$ SD"),
         subgroup=ifelse(subgroup=="Age", "AgeC", subgroup))


dm_num_all <- ds_long_ttl %>%
  mutate(Status=ifelse(Arm=="Total", "Total", Status)) %>%
  dplyr::filter(subgroup %in% c(num_v1, num_v2)) %>%
  mutate(subgroup_cat=as.numeric(subgroup_cat)) %>%
  group_by(Status, subgroup) %>%
  summarise(
    min = min(subgroup_cat, na.rm = T),
    max = max(subgroup_cat, na.rm = T),
    mean = mean(subgroup_cat, na.rm = T),
    sd = sd(subgroup_cat, na.rm = T),
    rslt1 = sprintf("%.1f (%.1f, %.1f)", mean, min, max),
    rslt2 = sprintf("%.1f $\\pm$ %.1f", mean, sd),
    N = n(),
    .groups = 'drop'
  ) %>%
  mutate(subgroup_cat = case_when(subgroup %in% num_v1 ~ "Mean (Range)",
                                  subgroup %in% num_v2 ~ "Mean $\\pm$ SD"),
         subgroup=ifelse(subgroup=="Age", "AgeC", subgroup))

dm_num_ph1_all <- ds_long_ttl_ph1 %>%
  mutate(Status=ifelse(Arm=="Total", "Total", Status)) %>%
  dplyr::filter(subgroup %in% c(num_v1, num_v2)) %>%
  mutate(subgroup_cat=as.numeric(subgroup_cat)) %>%
  group_by(Status, subgroup) %>%
  summarise(
    min = min(subgroup_cat, na.rm = T),
    max = max(subgroup_cat, na.rm = T),
    mean = mean(subgroup_cat, na.rm = T),
    sd = sd(subgroup_cat, na.rm = T),
    rslt1 = sprintf("%.1f (%.1f, %.1f)", mean, min, max),
    rslt2 = sprintf("%.1f $\\pm$ %.1f", mean, sd),
    N = n(),
    .groups = 'drop'
  ) %>%
  mutate(subgroup_cat = case_when(subgroup %in% num_v1 ~ "Mean (Range)",
                                  subgroup %in% num_v2 ~ "Mean $\\pm$ SD"),
         subgroup=ifelse(subgroup=="Age", "AgeC", subgroup))

char_lev <- c(labels.age, "Mean (Range)","Mean $\\pm$ SD",
              "Female","Male", "White", "Black or African American",
              "Asian", "American Indian or Alaska Native",
              "Native Hawaiian or Other Pacific Islander", "Multiracial",
              "Other", "Not reported and unknown",
              "White Non-Hispanic", "Communities of Color",
              "Hispanic or Latino","Not Hispanic or Latino",
              "Not reported and unknown ","At-risk","Not at-risk",
              paste(labels.age[1],"At-risk"), paste(labels.age[1], "Not at-risk"),
              paste(labels.age[2],"At-risk"), paste(labels.age[2], "Not at-risk"),
              paste(labels.age[2], ""),
              # "Communities of Color", "White Non-Hispanic",
              # labels.countries.ENSEMBLE,
              "Negative", "Positive", labels.BMI)

tab_dm <- bind_rows(dm_cat, dm_num) %>%
  mutate(rslt = case_when(subgroup %in% cat_v ~ rslt1,
                          subgroup %in% num_v1 ~ rslt1,
                          subgroup %in% num_v2 ~ rslt2)) %>%
  mutate(subgroup=ifelse(subgroup %in% c("raceC"), "RaceEthC", subgroup)) %>%
  dplyr::filter(subgroup_cat %in% char_lev) %>%
  inner_join(ds_long_ttl %>%
               distinct(Status, Arm, Ptid) %>%
               group_by(Status, Arm) %>%
               summarise(tot = n()),
             by = c("Status", "Arm")) %>%
  mutate(Arm = paste0(Arm, "\n(N = ", tot, ")"), subgroup=subgrp[subgroup]) %>%
  pivot_wider(c(Status, Arm, subgroup, subgroup_cat, rslt),
              names_from = Arm,
              names_sort = T,
              values_from = c(rslt)) %>%
  mutate(Characteristics = factor(subgroup_cat, levels=char_lev),
         subgroup=factor(subgroup, levels=subgrp)) %>%
  arrange(Status, subgroup, Characteristics)

tab_dm_ph1 <- bind_rows(dm_cat_ph1, dm_num_ph1) %>%
  mutate(rslt = case_when(subgroup %in% cat_v ~ rslt1,
                          subgroup %in% num_v1 ~ rslt1,
                          subgroup %in% num_v2 ~ rslt2)) %>%
  mutate(subgroup=ifelse(subgroup %in% c("raceC"), "RaceEthC", subgroup)) %>%
  dplyr::filter(subgroup_cat %in% char_lev) %>%
  inner_join(ds_long_ttl_ph1 %>%
               distinct(Status, Arm, Ptid) %>%
               group_by(Status, Arm) %>%
               summarise(tot = n()),
             by = c("Status", "Arm")) %>%
  mutate(Arm = paste0(Arm, "\n(N = ", tot, ")"), subgroup=subgrp[subgroup]) %>%
  pivot_wider(c(Status, Arm, subgroup, subgroup_cat, rslt),
              names_from = Arm,
              names_sort = T,
              values_from = c(rslt)) %>%
  mutate(Characteristics = factor(subgroup_cat, levels=char_lev),
         subgroup=factor(subgroup, levels=subgrp)) %>%
  arrange(Status, subgroup, Characteristics)



tab_dm_all <- bind_rows(dm_cat_all, dm_num_all) %>%
  mutate(rslt = case_when(subgroup %in% cat_v ~ rslt1,
                          subgroup %in% num_v1 ~ rslt1,
                          subgroup %in% num_v2 ~ rslt2)) %>%
  mutate(subgroup=ifelse(subgroup %in% c("raceC"), "RaceEthC", subgroup)) %>%
  dplyr::filter(subgroup_cat %in% char_lev) %>%
  inner_join(ds_long_ttl %>%
               mutate(Status=ifelse(Arm=="Total", "Total", Status)) %>%
               distinct(Status, Ptid) %>%
               group_by(Status) %>%
               summarise(tot = n()),
             by = c("Status")) %>%
  mutate(Status = paste0(Status, "\n(N = ", tot, ")"), subgroup=subgrp[subgroup]) %>%
  pivot_wider(c(Status, subgroup, subgroup_cat, rslt),
              names_from = Status,
              names_sort = T,
              values_from = c(rslt)) %>%
  mutate(Characteristics = factor(subgroup_cat, levels=char_lev),
         subgroup=factor(subgroup, levels=subgrp)) %>%
  arrange(subgroup, Characteristics)

tab_dm_ph1_all <- bind_rows(dm_cat_ph1_all, dm_num_ph1_all) %>%
  mutate(rslt = case_when(subgroup %in% cat_v ~ rslt1,
                          subgroup %in% num_v1 ~ rslt1,
                          subgroup %in% num_v2 ~ rslt2)) %>%
  # mutate(subgroup=ifelse(subgroup %in% c("MinorityC", "raceC"), "RaceEthC", subgroup)) %>%
  mutate(subgroup=ifelse(subgroup %in% c("raceC"), "RaceEthC", subgroup)) %>%
  dplyr::filter(subgroup_cat %in% char_lev) %>%
  inner_join(ds_long_ttl_ph1 %>%
               mutate(Status=ifelse(Arm=="Total", "Total", Status)) %>%
               distinct(Status, Ptid) %>%
               group_by(Status) %>%
               summarise(tot = n()),
             by = c("Status")) %>%
  mutate(Status = paste0(Status, "\n(N = ", tot, ")"), subgroup=subgrp[subgroup]) %>%
  pivot_wider(c(Status, subgroup, subgroup_cat, rslt),
              names_from = Status,
              names_sort = T,
              values_from = c(rslt)) %>%
  mutate(Characteristics = factor(subgroup_cat, levels=char_lev),
         subgroup=factor(subgroup, levels=subgrp)) %>%
  arrange(subgroup, Characteristics)

if ("Naive" %in% tab_dm$Status){
  tab_dm_neg <- tab_dm %>%
    dplyr::filter(Status == "Naive") %>%
    select_if(~ !all(is.na(.))) %>%
    select_at(c("subgroup", "Characteristics",
                grep("Vaccine" ,names(.), value = T),
                grep("Placebo" ,names(.), value = T),
                grep("Total" ,names(.), value = T)))

  tab_dm_neg_ph1 <- tab_dm_ph1 %>%
    dplyr::filter(Status == "Naive") %>%
    select_if(~ !all(is.na(.))) %>%
    select_at(c("subgroup", "Characteristics",
                grep("Vaccine" ,names(.), value = T),
                grep("Placebo" ,names(.), value = T),
                grep("Total" ,names(.), value = T)))
}

if ("Non-Naive" %in% tab_dm$Status){
  tab_dm_pos <- tab_dm %>%
    dplyr::filter(Status == "Non-Naive") %>%
    select_if(~ !all(is.na(.))) %>%
    select_at(c("subgroup", "Characteristics",
                grep("Vaccine" ,names(.), value = T),
                grep("Placebo" ,names(.), value = T),
                grep("Total" ,names(.), value = T)))

  tab_dm_pos_ph1 <- tab_dm_ph1 %>%
    dplyr::filter(Status == "Non-Naive") %>%
    select_if(~ !all(is.na(.))) %>%
    select_at(c("subgroup", "Characteristics",
                grep("Vaccine" ,names(.), value = T),
                grep("Placebo" ,names(.), value = T),
                grep("Total" ,names(.), value = T)))
}

tab_dm_all <- tab_dm_all %>%
  select_if(~ !all(is.na(.))) %>%
  select_at(c("subgroup", "Characteristics",
              grep("Naive" ,names(.), value = T),
              grep("Non-Naive" ,names(.), value = T),
              grep("Total" ,names(.), value = T)))

tab_dm_ph1_all <- tab_dm_ph1_all %>%
  select_if(~ !all(is.na(.))) %>%
  select_at(c("subgroup", "Characteristics",
              grep("Naive" ,names(.), value = T),
              grep("Non-Naive" ,names(.), value = T),
              grep("Total" ,names(.), value = T)))

print("Done with Demo tables")


ds <- ds %>%
  mutate(
    Case = case_when(BDPerprotocolIncludeSeroPos==1 &
                     BD29window==1 &
                     Stage2SamplingInd==1 &
                     EventIndPrimaryOmicronBD29==1 &
                     EventTimePrimaryOmicronBD29>=7 ~ "Cases",
                     BDPerprotocolIncludeSeroPos==1 &
                     BD29window==1 &
                     Stage2SamplingInd==1 &
                     EventIndPrimaryOmicronBD1==0 ~ "Non-Cases"))


# Added table:
demo.stratum.ordered <- gsub(">=", "$\\\\geq$", demo.stratum.labels, fixed=T)

if (study_name %in% c("COVE", "MockCOVE", "COVEBoost")){
  demo.stratum.ordered <- gsub("URM", "Minority", demo.stratum.ordered)
  demo.stratum.ordered <- gsub("White non-Hisp", "Non-Minority", demo.stratum.ordered)
  demo.stratum.ordered[7:9] <- c("Age $\\\\geq$ 65, Unknown", "Age < 65, At risk, Unknown", "Age < 65, Not at risk, Unknown")
} else if (study_name %in% c("ENSEMBLE", "MockENSEMBLE")) {
  demo.stratum.ordered <- gsub("URM", "Communities of Color", demo.stratum.ordered)
  demo.stratum.ordered <- gsub("At risk", "Presence of comorbidities", demo.stratum.ordered)
  demo.stratum.ordered <- gsub("Not at risk", "Absence of comorbidities", demo.stratum.ordered)
}

strtm_cutoff <- ifelse(study_name %in% c("ENSEMBLE", "MockENSEMBLE"), length(demo.stratum.ordered)/2, length(demo.stratum.ordered))

tab_strtm <- ds %>%
  filter(!!as.name(config$ph2)) %>%
  group_by(demo.stratum, Arm, Status) %>%
  summarise("Day {tpeak} Cases":=sum(Case=="Cases", na.rm=T),
            `Non-Cases`=sum(Case=="Non-Cases", na.rm=T)) %>%
  pivot_longer(cols=c(!!as.name(paste("Day", tpeak, "Cases")), `Non-Cases`)) %>%
  arrange(Status, demo.stratum) %>%
  pivot_wider(id_cols=c(Arm, name),
              names_from = c(Status, demo.stratum),
              values_from=value)


tab_strtm1 <- tab_strtm %>% select(Arm, name, any_of(paste0("Naive_", 1:strtm_cutoff)),
                                   any_of(paste0("Non-Naive_", 1:strtm_cutoff)))
tab_strtm2 <- tab_strtm %>% select(Arm, name, any_of(paste0("Naive_", (strtm_cutoff+1):(strtm_cutoff*2))),
                                   any_of(paste0("Non-Naive_", (strtm_cutoff+1):(strtm_cutoff*2))))

ls_strtm <- list(tab_strtm1, tab_strtm2)

for (i in 1:2){
  if ((n_strtm.i <- ceiling(ncol(ls_strtm[[i]])/2-1))!=0) {
  tlf[[paste0("tab_strtm", i)]]$col_name <- colnames(ls_strtm[[i]])[-1] %>%
    gsub("name", " ", .) %>%
    gsub("Naive_", "", .) %>%
    gsub("Non-", "", .)

  ds.i <- filter(ds, demo.stratum %in% ((i-1)*strtm_cutoff+1):(i*strtm_cutoff))

  tlf[[paste0("tab_strtm", i)]]$table_header <-
    sprintf("Sample Sizes of Random Subcohort Strata (with antibody markers data at BD%s) Plus All Other Cases Outside the Random Subcohort %s",
            config$tpeak, ifelse(is.null(ds.i$RegionC), "", paste("in", paste(sort(unique(ds.i$RegionC))))))

  tlf[[paste0("tab_strtm", i)]]$header_above1 <- c(" "=1, "Naive" = sum(grepl("Naive", colnames(ls_strtm[[i]])))-sum(grepl("Non-Naive", colnames(ls_strtm[[i]]))),
                                    "Non-Naive" = sum(grepl("Non-Naive", colnames(ls_strtm[[i]]))))

  tlf[[paste0("tab_strtm", i)]]$header_above1 <- tlf[[paste0("tab_strtm", i)]]$header_above1[tlf[[paste0("tab_strtm", i)]]$header_above1!=0]

  tab_strtm_header2 <- ncol(ls_strtm[[i]])-1
  names(tab_strtm_header2) <- sprintf("%s\nSample Sizes (N=%s Participants) (%s Trial)",
                                      tlf[[paste0("tab_strtm", i)]]$table_header,
                                      sum(ds[ds$demo.stratum%in%1:strtm_cutoff, paste0("ph2.BD", tpeak)]),
                                      stringr::str_to_title(study_name))
  tlf[[paste0("tab_strtm", i)]]$header_above2 <- tab_strtm_header2
  tlf[[paste0("tab_strtm", i)]]$table_footer <- c("Demographic covariate strata:",
                                   paste(sort(unique(ds.i$demo.stratum)),
                                         demo.stratum.ordered[sort(unique(ds.i$demo.stratum))],
                                         sep=". "),
                                   " ",
                                   "Minority includes Blacks or African Americans, Hispanics or Latinos, American Indians or
                   Alaska Natives, Native Hawaiians, and other Pacific Islanders."[study_name %in% c("COVE", "MockCOVE", "COVEBoost")],
                                   "Non-Minority includes all other races with observed race (Asian, Multiracial, White, Other) and observed ethnicity Not Hispanic or Latino.
                   Participants not classifiable as Minority or Non-Minority because of unknown, unreported or missing were not included."[study_name %in% c("COVE", "MockCOVE", "COVEBoost")],
                                   " "[study_name %in% c("COVE", "MockCOVE", "COVEBoost")],
                                   "Observed = Numbers of participants sampled into the subcohort within baseline covariate strata.",
                                   "Estimated = Estimated numbers of participants in the whole per-protocol cohort within baseline
  covariate strata, calculated using inverse probability weighting.")
  }
}

if (ncol(tab_strtm1)==2) tab_strtm1 <- NULL
if (ncol(tab_strtm2)==2) tab_strtm2 <- NULL


# median (interquartile range) days from vaccination to the tpeak visit

if ((Numberdays <- paste0("NumberdaysBD1toBD", config$tpeak)) %in% names(ds)) {
  tab_days <- ds %>%
    filter(!!as.name(config$ph2), !is.na(Case)) %>%
    mutate(Visit = paste("BDay", config$tpeak)) %>%
    bind_rows(., mutate(., Case = "Total")) %>%
    group_by(Visit, Arm, Case, Status) %>%
    summarise(N=n(), dmed=median(!!as.name(Numberdays), na.rm=T), iqr=IQR(!!as.name(Numberdays))) %>%
    select(Visit, Arm, Case, `Naive/Non-Naive`=Status,
           N, `Median\n(days)`=dmed, `Interquatile Range\n(days)`=iqr)
} else {
  tab_days <- NULL
}

tab_fu <- ds %>%
  mutate(NumberdaysBD29toBD181=NumberdaysBD1toBD181-NumberdaysBD1toBD29) %>%
  mutate(FUdur=ifelse(is.na(NumberdaysBD1toBD29), NA, pmax(NumberdaysBD29toBD181, EventTimePrimaryOmicronBD29, EventTimeOmicronBD29, 0, na.rm = T))) %>%
  filter(!!as.name(config$ph1), !is.na(Case)) %>%
  group_by(Case, Status) %>%
  summarise(N=n(), `Average Follow-up (days)`=round(mean(FUdur, na.rm=T), 0))


# Case counts by availability of markers at baseline, d29, d57

if (study_name %in% c("COVE", "MockCOVE")){
  tab_case_cnt <- make.case.count.marker.availability.table(dat) %>%
    data.frame(check.names = F) %>%
    rename_all(gsub, pattern=".", replacement="_", fixed=T) %>%
    rownames_to_column("Case") %>%
    pivot_longer(cols = !Case,
                 names_to = c(".value", "Arm"),
                 names_pattern = "(.*)_(.*)") %>%
    mutate(Arm = factor(ifelse(Arm=="vacc", "Vaccine", "Placebo"), levels=c("Vaccine", "Placebo"))) %>%
    arrange(Arm, Case) %>%
    rename_at(-c(1:2), function(x)paste0("$",x,"$"))
}

# Generate a full table with all the estimates: response rates, GMT, GMTR, etc.
# (Per Peter's email Feb 5, 2021)
# Cases vs Non-cases

  sub.by <- c("CalendarBD1Interval", "Arm", "Status")
  ds.i_ <- bind_rows(filter(ds, !!as.name(config$ph1)),
                     filter(ds, !!as.name(config$ph1)) %>% mutate(CalendarBD1Interval=5))
  ds.i <- bind_rows(
    ds.i_ %>% mutate(Status="All"),
    ds.i_ %>% mutate(Arm="All"),
    ds.i_ %>% mutate(Case="All"),
    ds.i_ %>% mutate(Arm="All", Case="All"),
    ds.i_ %>% mutate(Arm="All", Status="All"),
    ds.i_ %>% mutate(Case="All", Status="All"),
    ds.i_)


  resp.v <- resp.lb %>% filter(time%in%c("BD1", "BD29", "DD1")) %>% pull(resp_cat)
  gm.v <- resp.lb %>% filter(time%in%c("BD1", "BD29", "DD1")) %>% pull(mag_cat)

  subs <- "Case"
  comp.i <- c("Cases", "Non-Cases")

  rpcnt_case <- get_rr(ds.i, resp.v, subs, sub.by, strata=config$WtStratum, weights=config$wt, subset=config$ph2)
  rgm_case <- get_gm(ds.i, resp.lb$mag_cat, subs, sub.by, strata=config$WtStratum, weights=config$wt, subset=config$ph2)
  # rgmt_case <- get_rgmt(ds.i, gm.v, subs, comp_lev=comp.i, sub.by, strata=config$WtStratum, weights=config$wt, subset=config$ph2)
  rgmt_case <- get_rgmt(ds.i, resp.lb$mag_cat, subs, comp_lev=comp.i, sub.by, strata=config$WtStratum, weights=config$wt, subset=config$ph2)

  rgmt_naive <- get_rgmt(ds.i, resp.lb$mag_cat, groups="Status", comp_lev=c("Naive", "Non-Naive"), sub.by=c("CalendarBD1Interval","Arm", "Case"), strata=config$WtStratum, weights=config$wt, subset=config$ph2)

  rgmt_arm <- get_rgmt(ds.i, resp.lb$mag_cat, groups="Arm", comp_lev=c("Vaccine", "Placebo"), sub.by=c("CalendarBD1Interval","Status", "Case"), strata=config$WtStratum, weights=config$wt, subset=config$ph2)


  print("Done with table 2b & 3b")

  rrdiff_case <- rpcnt_case %>%
    # dplyr::filter(subgroup %in% subs & grepl("Resp",resp_cat)) %>%
    mutate(groupn = 2-match(Group, comp.i)%%2) %>%
    pivot_wider(id_cols = c(subgroup, CalendarBD1Interval, Status, Arm, Visit, Marker, time),
                names_from = groupn, values_from = c(response, ci_l, ci_u), names_sep = "")

  responseNA <- setdiff(as.vector(outer(c("response", "ci_l", "ci_u"), 1:2, paste0)), names(rrdiff_case))
  rrdiff_case[, responseNA] <- NA

  rrdiff_case <- rrdiff_case %>%
    mutate(Estimate = response1-response2,
           ci_l = Estimate-sqrt((response1-ci_l1)^2+(response2-ci_u2)^2),
           ci_u = Estimate+sqrt((response1-ci_u1)^2+(response2-ci_l2)^2),
           rrdiff = ifelse(!is.na(Estimate),
                           sprintf("%s\n(%s, %s)", round(Estimate, 3), round(ci_l, 3), round(ci_u, 3)),
                           "-"))

print("Done with table6")

idVar <- c("Arm", "Group", "Status", "Marker")
visitVar <- c("Post-booster Day 1", "Post-booster Day 29", "Disease Day 1")
periodVar <- paste0("Period", 1:4)
deltaVar <- c("BD29 fold-rise over BD1", "Disease Day 1 fold-rise over BD1")

tab_case <- full_join(rpcnt_case, rgm_case %>% select(-mag:-ci_u)) %>%
  full_join(rgmt_case %>% select(-`(Intercept)`:-ci_u.Estimate)) %>%
  full_join(rrdiff_case %>% select(-response1:-ci_u)) %>%
  # filter(Arm=="All", Group!="All", Status!="All", CalendarBD1Interval==5, time%in%grep(tpeak, times, value = T)) %>%
  filter(Arm=="All", Group!="All", Status!="All", CalendarBD1Interval==5) %>%
  pivot_wider(id_cols=c(Visit, Status, Marker, comp, `Ratios of GMT/GMC`, rrdiff), names_from = Group, values_from = c(N, rslt, `GMT/GMC`))

if(length(comp_NA <- setdiff(comp.i, rpcnt_case$Group))!=0){
  tab_case <- tab_case %>%
    mutate(!!paste0("N_", comp_NA) := 0,
           !!paste0("rslt_", comp_NA) := "-",
           !!paste0("GMT/GMC_", comp_NA) :="-",
           `Ratios of GMT/GMC`=replace_na(`Ratios of GMT/GMC`, "-"))
}else{
  tab_case <- tab_case %>%
    mutate_at(vars(starts_with("N_")), replace_na, replace=0) %>%
    mutate_at(vars(starts_with("rslt_")), replace_na, replace="-") %>%
    mutate_at(vars(starts_with("GMT/GMC_")), replace_na, replace="-") %>%
    mutate(`Ratios of GMT/GMC`=replace_na(`Ratios of GMT/GMC`, "-"))
}

tab_case <- tab_case %>%
  select(Status, Marker, Visit, `N_Cases`, `rslt_Cases`,
         `GMT/GMC_Cases`, `N_Non-Cases`, `rslt_Non-Cases`, `GMT/GMC_Non-Cases`,
         rrdiff, `Ratios of GMT/GMC`)

tab_case_ba <- tab_case %>%
  filter(grepl("BA.1", Marker)) %>%
  arrange(Status, desc(Visit), desc(Marker)) %>%
  mutate(Marker=gsub(" to", "\nto", Marker),
         Visit=gsub("Day", "\nDay", Visit),
         Visit=gsub("over", "\nover", Visit),
         Status=gsub("Non-", "Non-\n", Status))

tab_case_g <- tab_case %>%
  filter(grepl("PsV Neutralization to D614G|Binding Antibody to Spike D614", Marker)) %>%
  arrange(Status, desc(Visit), desc(Marker)) %>%
  mutate(Marker=gsub(" to", "\nto", Marker),
         Visit=gsub("Day", "\nDay", Visit),
         Visit=gsub("over", "\nover", Visit),
         Status=gsub("Non-", "Non-\n", Status))

tab1 <- rpcnt_case %>%
  filter(CalendarBD1Interval==5) %>%
  select(Arm, Group, Status, rslt, Marker, Visit) %>%
  pivot_wider(names_from = Visit, values_from = rslt) %>%
  arrange_at(idVar) %>%
  select(all_of(c(idVar, visitVar)))

tab2 <- rpcnt_case %>%
  filter(time=="BD1" & CalendarBD1Interval!=5) %>%
  select(CalendarBD1Interval, Arm, Group, Status, rslt, Marker) %>%
  pivot_wider(names_from = CalendarBD1Interval, values_from = rslt, names_prefix = "Period") %>%
  arrange_at(idVar) %>%
  select(all_of(c(idVar, periodVar)))


tab3 <- rpcnt_case %>%
  filter(time=="BD29" & CalendarBD1Interval!=5) %>%
  select(CalendarBD1Interval, Arm, Group, Status, rslt, Marker) %>%
  pivot_wider(names_from = CalendarBD1Interval, values_from = rslt, names_prefix = "Period") %>%
  arrange_at(idVar) %>%
  select(all_of(c(idVar, periodVar)))


tab4 <- rpcnt_case %>%
  filter(time=="DD1" & CalendarBD1Interval!=5) %>%
  select(CalendarBD1Interval, Arm, Group, Status, rslt, Marker) %>%
  pivot_wider(names_from = CalendarBD1Interval, values_from = rslt, names_prefix = "Period") %>%
  arrange_at(idVar) %>%
  select(all_of(c(idVar, periodVar)))


tab5 <- rgm_case %>%
  filter(time %in% c("BD1", "BD29", "DD1") & CalendarBD1Interval==5) %>%
  select(Arm, Group, Status, `GMT/GMC`, Marker, Visit) %>%
  pivot_wider(names_from = Visit, values_from = `GMT/GMC`) %>%
  arrange_at(idVar) %>%
  select(all_of(c(idVar, visitVar)))

tab6 <- rgm_case %>%
  filter(time=="BD1" & CalendarBD1Interval!=5) %>%
  select(CalendarBD1Interval, Arm, Group, Status, `GMT/GMC`, Marker, Visit) %>%
  pivot_wider(names_from = CalendarBD1Interval, values_from = `GMT/GMC`, names_prefix = "Period") %>%
  arrange_at(idVar) %>%
  select(all_of(c(idVar, periodVar)))

tab7 <- rgm_case %>%
  filter(time=="BD29" & CalendarBD1Interval!=5) %>%
  select(CalendarBD1Interval, Arm, Group, Status, `GMT/GMC`, Marker, Visit) %>%
  pivot_wider(names_from = CalendarBD1Interval, values_from = `GMT/GMC`, names_prefix = "Period") %>%
  arrange_at(idVar) %>%
  select(all_of(c(idVar, periodVar)))

tab8 <- rgm_case %>%
  filter(time=="DD1" & CalendarBD1Interval!=5) %>%
  select(CalendarBD1Interval, Arm, Group, Status, `GMT/GMC`, Marker, Visit) %>%
  pivot_wider(names_from = CalendarBD1Interval, values_from = `GMT/GMC`, names_prefix = "Period") %>%
  arrange_at(idVar) %>%
  select(all_of(c(idVar, periodVar)))

tab9 <- rgm_case %>%
  filter(time %in% c("DeltaBD29overBD1", "DeltaDD1overBD1") & CalendarBD1Interval==5) %>%
  select(Arm, Group, Status, `GMT/GMC`, Marker, Visit) %>%
  pivot_wider(names_from = Visit, values_from = `GMT/GMC`) %>%
  arrange_at(idVar) %>%
  select(all_of(c(idVar, deltaVar)))

tab10 <- rgm_case %>%
  filter(time=="DeltaBD29overBD1" & CalendarBD1Interval!=5) %>%
  select(CalendarBD1Interval, Arm, Group, Status, `GMT/GMC`, Marker, Visit) %>%
  pivot_wider(names_from = CalendarBD1Interval, values_from = `GMT/GMC`, names_prefix = "Period") %>%
  arrange_at(idVar) %>%
  select(all_of(c(idVar, periodVar)))

tab11 <- rgm_case %>%
  filter(time=="DeltaDD1overBD1" & CalendarBD1Interval!=5) %>%
  select(CalendarBD1Interval, Arm, Group, Status, `GMT/GMC`, Marker, Visit) %>%
  pivot_wider(names_from = CalendarBD1Interval, values_from = `GMT/GMC`, names_prefix = "Period") %>%
  arrange_at(idVar) %>%
  select(all_of(c(idVar, periodVar)))


tab12 <- rrdiff_case %>%
  filter(time!="DD1" & CalendarBD1Interval==5) %>%
  select(Arm, Status, rrdiff, Marker, Visit) %>%
  pivot_wider(names_from = Visit, values_from = rrdiff) %>%
  arrange_at(idVar[-2]) %>%
  select(all_of(c(idVar[-2], visitVar[-3])))


tab13 <- rrdiff_case %>%
  filter(time=="BD1" & CalendarBD1Interval!=5) %>%
  select( CalendarBD1Interval, Arm, Status, rrdiff, Marker, Visit) %>%
  pivot_wider(names_from = CalendarBD1Interval, values_from = rrdiff, names_prefix = "Period") %>%
  arrange_at(idVar[-2]) %>%
  select(all_of(c(idVar[-2], periodVar)))


tab14 <- rrdiff_case %>%
  filter(time=="BD29" & CalendarBD1Interval!=5) %>%
  select(CalendarBD1Interval, Arm, Status, rrdiff, Marker, Visit) %>%
  pivot_wider(names_from = CalendarBD1Interval, values_from = rrdiff, names_prefix = "Period") %>%
  arrange_at(idVar[-2]) %>%
  select(all_of(c(idVar[-2], periodVar)))


tab15 <- rgmt_case %>%
  filter(time!="DD1" & CalendarBD1Interval==5) %>%
  select( Arm, Status, `Ratios of GMT/GMC`, Marker, Visit) %>%
  pivot_wider(names_from = Visit, values_from = `Ratios of GMT/GMC`) %>%
  arrange_at(idVar[-2]) %>%
  select(all_of(c(idVar[-2], visitVar[-3])))


tab16 <- rgmt_case %>%
  filter(time=="BD1" & CalendarBD1Interval!=5) %>%
  select( CalendarBD1Interval, Arm, Status, `Ratios of GMT/GMC`, Marker, Visit) %>%
  pivot_wider(names_from = CalendarBD1Interval, values_from = `Ratios of GMT/GMC`, names_prefix = "Period") %>%
  arrange_at(idVar[-2]) %>%
  select(all_of(c(idVar[-2], periodVar)))


tab17 <- rgmt_case %>%
  filter(time=="BD29" & CalendarBD1Interval!=5) %>%
  select(CalendarBD1Interval, Arm, Status, `Ratios of GMT/GMC`, Marker, Visit) %>%
  pivot_wider(names_from = CalendarBD1Interval, values_from = `Ratios of GMT/GMC`, names_prefix = "Period") %>%
  arrange_at(idVar[-2]) %>%
  select(all_of(c(idVar[-2], periodVar)))


tab18 <- rgmt_naive %>%
  filter(time!="DD1" & CalendarBD1Interval==5) %>%
  select(Arm, Case, `Ratios of GMT/GMC`, Marker, Visit) %>%
  pivot_wider(names_from = Visit, values_from = `Ratios of GMT/GMC`) %>%
  arrange(Arm, Case, Marker) %>%
  select(all_of(c("Arm", "Case", "Marker", visitVar[-3])))

tab19 <- rgmt_arm %>%
  filter(time!="DD1" & CalendarBD1Interval==5) %>%
  select( Status, Case, `Ratios of GMT/GMC`, Marker, Visit) %>%
  pivot_wider(names_from = Visit, values_from = `Ratios of GMT/GMC`) %>%
  arrange(Status, Case, Marker) %>%
  select(all_of(c("Status", "Case", "Marker", visitVar[-3])))





if(study_name %in% c("PREVENT19") & all(ds$Country==0)){
  for (i in 1:length(tlf)){
    if(!is.null(tlf[[i]]$table_header)){
      tlf[[i]]$table_header <- paste0(tlf[[i]]$table_header, " in U.S. only")
    }
  }
}

# path for tables
save.results.to <- here::here("output")
if (!dir.exists(save.results.to))  dir.create(save.results.to)
save.results.to <- paste0(here::here("output"), "/", attr(config,"config"))

if (!dir.exists(save.results.to))  dir.create(save.results.to)
print(paste0("save.results.to equals ", save.results.to))

save(list = c("tlf", names(tlf)), file = file.path(save.results.to, sprintf("Tables%s.Rdata", ifelse(exists("COR"), COR, ""))))
