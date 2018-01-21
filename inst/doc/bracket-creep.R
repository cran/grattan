## ----setup, include = FALSE----------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----loadPackages--------------------------------------------------------
library(scales)
library(knitr)
library(ggplot2)
library(hutils)
library(magrittr)
library(data.table)
if (requireNamespace("taxstats", quietly = TRUE)){
  library(taxstats)
} else {
  templib <- tempfile()
  hutils::provide.dir(templib)
  install.packages("taxstats",
                   lib = templib,
                   repos = "https://hughparsonage.github.io/drat/",
                   type = "source")
  library("taxstats", lib.loc = templib)
}
library(grattan)

## ----sample_file_1617----------------------------------------------------
sample_file_1617 <-
  project(sample_file_1213,
          h = 4L,
          fy.year.of.sample.file = "2012-13")

## ----sample_file_2021----------------------------------------------------
sample_file_2021 <-
  project(sample_file_1213,
          h = 8L,
          fy.year.of.sample.file = "2012-13")

## ----add-tax-paid-avg-tax------------------------------------------------
sample_file_1617[, tax_paid := income_tax(Taxable_Income,
                                          .dots.ATO = copy(sample_file_1617),
                                          fy.year = "2016-17")]
sample_file_1617[, avg_tax := tax_paid / Taxable_Income]
sample_file_2021[, tax_paid := income_tax(Taxable_Income,
                                          .dots.ATO = copy(sample_file_2021),
                                          fy.year = "2019-20")]
sample_file_2021[, avg_tax := tax_paid / Taxable_Income]

## ----avg_tax_by_decile---------------------------------------------------
avg_tax_by_decile_1617 <- 
  sample_file_1617 %>%
  .[, .(avg_tax = mean(avg_tax)),
    keyby = .(decile = weighted_ntile(Taxable_Income, n = 10))]

avg_tax_by_decile_2021 <- 
  sample_file_2021 %>%
  .[, .(avg_tax = mean(avg_tax)),
    keyby = .(decile = weighted_ntile(Taxable_Income, n = 10))]

## ----tax-changes-grattan-forecast----------------------------------------
avg_tax_by_decile_1617[avg_tax_by_decile_2021] %>%
  .[decile > 1] %>%
  .[, ppt_increase := 100*(i.avg_tax - avg_tax)] %>%
  .[, decile := factor(decile)] %>%
  ggplot(aes(x = decile, y = ppt_increase)) + 
  geom_col()

## ----Budget_wage_series--------------------------------------------------
Budget_wage_series <-
  data.table(fy_year = c("2017-18", "2018-19", "2019-20", "2020-21"),
             r = c(0.025, 0.03, 0.035, 0.0375))

kable(Budget_wage_series)

## ----project-with-respect-to-budget--------------------------------------
sample_file_1617 <- project(sample_file_1213,
                            h = 4L,
                            fy.year.of.sample.file = "2012-13")

sample_file_2021 <- project(sample_file_1213,
                            fy.year.of.sample.file = "2012-13",
                            h = 8L,
                            wage.series = Budget_wage_series)

sample_file_1617[, tax_paid := income_tax(Taxable_Income,
                                          .dots.ATO = copy(sample_file_1617),
                                          fy.year = "2016-17")]
sample_file_1617[, avg_tax := tax_paid / Taxable_Income]
sample_file_2021[, tax_paid := income_tax(Taxable_Income,
                                          .dots.ATO = copy(sample_file_2021),
                                          fy.year = "2019-20")]
sample_file_2021[, avg_tax := tax_paid / Taxable_Income]

avg_tax_by_decile_1617 <- 
  sample_file_1617 %>%
  .[, .(avg_tax = mean(avg_tax)),
    keyby = .(decile = weighted_ntile(Taxable_Income, n = 10))]

avg_tax_by_decile_2021 <- 
  sample_file_2021 %>%
  .[, .(avg_tax = mean(avg_tax)),
    keyby = .(decile = weighted_ntile(Taxable_Income, n = 10))]

difference_2021_Budget <-
  avg_tax_by_decile_1617[avg_tax_by_decile_2021] %>%
  .[decile > 1] %>%
  .[, ppt_increase := 100*(i.avg_tax - avg_tax)]

difference_2021_Budget %>%
  copy %>%
  .[, decile := factor(decile)] %>%
  ggplot(aes(x = decile, y = ppt_increase)) + 
  geom_col()

## ----middle_income_avg_inc-----------------------------------------------
middle_income_avg_inc <-
  difference_2021_Budget %>%
  .[decile %between% c(3, 7)] %$%
  range(round(ppt_increase, 1))

## ----percentile_50000----------------------------------------------------
sample_file_1617[, percentile := weighted_ntile(Taxable_Income, n = 100)]
stopifnot(56 %in% sample_file_1617[Taxable_Income %between% c(49500, 50500)][["percentile"]])

avg_tax_rate_2017_50k <- 
  sample_file_1617[percentile == 56] %$% 
  mean(avg_tax) %>%
  round(3)

avg_tax_rate_2021_50k <- 
  sample_file_2021 %>%
  .[, percentile := weighted_ntile(Taxable_Income, n = 100)] %>%
  .[percentile == 56] %$% 
  mean(avg_tax) %>%
  round(3)

## ----Change_1ppt_2021----------------------------------------------------
tax_delta <- function(bracket_number, rate_increase = -0.01) {
  current_tax <-
    sample_file_2021[, .(tax = sum(tax_paid), 
                         WEIGHT = WEIGHT[1])] %$% 
    sum(tax * WEIGHT)
  
  orig_rates <- c(0, 0.19, 0.325, 0.37, 0.45)
  new_rates <- orig_rates
  new_rates[bracket_number] <- new_rates[bracket_number] + rate_increase
    
  # rebate_income is an internal function
  .ri <- grattan:::rebate_income
  
  new_tax <- 
    sample_file_2021 %>%
    copy %>%
    .[, base_tax. := IncomeTax(Taxable_Income,
                              thresholds = c(0, 18200, 37000, 87000, 180e3),
                              rates = new_rates)] %>%
    .[, medicare_levy. := medicare_levy(income = Taxable_Income, fy.year = "2019-20",
                                       Spouse_income = Spouse_adjusted_taxable_inc,
                                       sapto.eligible = (age_range <= 1),
                                       family_status = if_else(Spouse_adjusted_taxable_inc > 0, "family", "individual"))] %>%
    .[, lito. := lito(Taxable_Income, max_lito = 445, lito_taper = 0.015, min_bracket = 37000)] %>%
    .[, rebate_income := .ri(Taxable_Income,
                             Rptbl_Empr_spr_cont_amt = Rptbl_Empr_spr_cont_amt,
                             Net_fincl_invstmt_lss_amt = Net_fincl_invstmt_lss_amt,
                             Net_rent_amt = Net_rent_amt,
                             Rep_frng_ben_amt = Rep_frng_ben_amt)] %>%
    .[, sapto. := sapto(rebate_income, fy.year = "2019-20", sapto.eligible = (age_range <= 1))] %>%
    .[, tax_payable := pmaxC(base_tax. - lito. - sapto., 0) + medicare_levy.] %>%
    .[, .(tax = sum(tax_payable), 
          WEIGHT = WEIGHT[1])] %$% 
    sum(tax * WEIGHT)
  
  current_tax - new_tax
}

## ----table_1pt-----------------------------------------------------------
data.table(tax_bracket = c("<18,200",
                           "18,200-37,000",
                           "37,000-87,000",
                           "87,000-180,000",
                           "180,000+"),
           budget_impact = c(NA, round(vapply(2:5, tax_delta, FUN.VALUE = double(1)) / 1e9, 2))) %>%
  kable

