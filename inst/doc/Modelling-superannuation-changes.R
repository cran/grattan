## ---- echo=FALSE---------------------------------------------------------
knitr::opts_chunk$set(message = FALSE)

## ------------------------------------------------------------------------
library(grattan)
library(data.table)
if (requireNamespace("taxstats", quietly = TRUE)){
  library(taxstats)
  sample_files_all <- get_sample_files_all()
} else {
  install.packages("taxstats", repos = "https://hughparsonage.github.io/drat/", type = "source")
  library(taxstats)
  sample_files_all <- get_sample_files_all()
}
library(magrittr)
#' dollar scales
#' 
#' @name grattan_dollar
#' @param x A numeric vector
#' @param digits Minimum number of digits after the decimal point. (\code{nsmall} in \code{base::format}).
#' @details Makes negative numbers appear as \eqn{-\$10,000} instead of \eqn{\$-10,000} in \code{scales::dollar}.
#' @export
# from scales

grattan_dollar <- function (x, digits = 0) {
  #
  nsmall <- digits
  commaz <- format(abs(x), nsmall = nsmall, trim = TRUE, big.mark = ",", 
                   scientific = FALSE, digits = 1L)
  
  dplyr::if_else(x < 0, 
          paste0("\U2212","$", commaz),
          paste0("$", commaz))
}

(new_revenue <- 
  sample_file_1314 %>%
  project_to(to_fy = "2017-18") %>%
  as.data.table %>%
  revenue_from_new_cap_and_div293(new_cap = 25e3, fy.year = "2016-17", new_age_based_cap = FALSE, new_div293_threshold = 250e3))

paste(grattan_dollar(new_revenue / 1e9), "bn")

(n_affected <-
  sample_file_1314 %>%
  project_to(to_fy = "2017-18") %>%
  as.data.table %>%
  n_affected_from_new_cap_and_div293(new_cap = 25e3, fy.year = "2016-17", new_age_based_cap = FALSE, new_div293_threshold = 250e3))

prettyNum(round(n_affected), big.mark = ",")

## ------------------------------------------------------------------------
sample_file_1718 <-
  sample_file_1314 %>%
  project_to(to_fy = "2017-18") %>%
  as.data.table

## ------------------------------------------------------------------------
new_sample_file_1718 <- 
  sample_file_1718 %>%
  model_new_caps_and_div293(new_cap = 25e3, fy.year = "2016-17", new_age_based_cap = FALSE, new_div293_threshold = 250e3)


## ------------------------------------------------------------------------
library(knitr)
library(dplyr)
library(dtplyr)  # for data.table

new_sample_file_1718 %>%
  mutate(Taxable_Income_decile = ntile(Taxable_Income, 10)) %>%
  group_by(Taxable_Income_decile) %>%
  summarise(`Average increase in tax` = round(mean(new_revenue - prv_revenue), 2)) %>%
  arrange(Taxable_Income_decile) %>%
  kable

## ------------------------------------------------------------------------
library(ggplot2)
new_sample_file_1718 %>%
  mutate(Taxable_Income_decile = ntile(Taxable_Income, 10)) %>%
  group_by(Taxable_Income_decile) %>%
  summarise(`Average increase in tax` = mean(new_revenue - prv_revenue)) %>%
  arrange(Taxable_Income_decile) %>%
  #
  mutate(`Taxable income decile` = factor(Taxable_Income_decile)) %>%
  ggplot(aes(x = `Taxable income decile`, y = `Average increase in tax`)) + 
  geom_bar(stat = "identity") + 
  
  # cosmetic:
  scale_y_continuous(label = grattan_dollar) + 
  theme(axis.title.y = element_text(face = "bold", angle = 90, margin = margin(1, 1, 1, 1, "lines")))

