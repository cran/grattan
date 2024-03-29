#' Modelling superannuation changes
#' 
#' @description Model changes to the contributions cap, Division 293 threshold 
#' and related modelling. Note: defaults are relevant to pre-2017 for 
#' compatibility.


#' @param .sample.file A \code{data.table} whose variables include those in \code{taxstats::sample_file_1314}.
#' @param fy.year The financial year tax scales.
#' @param new_cap The \strong{proposed} cap on concessional contributions for all taxpayers if \code{age_based_cap} is FALSE, or for those below the age threshold otherwise.
#' @param new_cap2 The \strong{proposed} cap on concessional contributions for those above the age threshold. No effect if \code{age_based_cap} is FALSE.
#' @param new_age_based_cap Is the \strong{proposed} cap on concessional contributions age-based? 
#' @param new_cap2_age The age above which \code{new_cap2} applies.
#' @param new_ecc (logical) Should an excess concessional contributions charge be calculated? (Not implemented.)
#' @param new_contr_tax A string to determine the contributions tax.
#' @param new_div293_threshold The \strong{proposed} Division 293 threshold. 
#' @param use_other_contr Should \code{MCS_Othr_Contr} be used to calculate Division 293 liabilities?
#' @param scale_contr_match_ato (logical) Should concessional contributions be inflated to match aggregates in 2013-14? That is, should the concessional contributions by multiplied by the internal constant \code{grattan:::super_contribution_inflator_1314}, which was defined to be: \deqn{\frac{\textrm{Total assessable contributions in SMSF and funds}}{\textrm{Total contributions in 2013-14 sample file}}}{Total assessable contributions in SMSF and funds / Total contributions in 2013-14 sample file.}. 
#' @param .lambda Scalar weight applied to \code{concessional contributions}. \eqn{\lambda = 0} means no (extra) weight. \eqn{\lambda = 1} means contributions are inflated by the ratio of aggregates to the sample file's total. For \eqn{R = \textrm{actual} / \textrm{apparent}} then the contributions are scaled by \eqn{1 + \lambda(R - 1)}.
#' @param reweight_late_lodgers (logical) Should WEIGHT be inflated to account for late lodgers?
#' @param .mu Scalar weight for WEIGHT. (\eqn{w' = \mu w}) No effect if \code{reweight_late_lodgers} is \code{FALSE}.
#' @param impute_zero_concess_contr Should zero concessional contributions be imputed using salary?
#' @param .min.Sw.for.SG The minimum salary required for super guarantee to be imputed.
#' @param .SG_rate The super guarantee rate for imputation.
#' @param prv_cap The \strong{comparator} cap on concessional contributions for all taxpayers if \code{age_based_cap} is FALSE, or for those below the age threshold otherwise.
#' @param prv_cap2 The \strong{comparator} cap on concessional contributions for those above the age threshold. No effect if \code{age_based_cap} is FALSE.
#' @param prv_age_based_cap Is the \strong{comparator} cap on concessional contributions age-based? 
#' @param prv_cap2_age The age above which \code{new_cap2} applies.
#' @param prv_ecc (logical) Should an excess concessional contributions charge be calculated? (Not implemented.)
#' @param prv_div293_threshold The \strong{comparator} Division 293 threshold. 
#' @param ... Passed to \code{model_new_caps_and_div293}.
#' @param adverse_only Count only individuals who are adversely affected by the change.
#' @return For \code{model_new_caps_and_div293}, a \code{data.frame}, comprising 
#' the variables in \code{.sample.file}, the superannuation variables generated by 
#' \code{apply_super_caps_and_div293}, and two variables, \code{prv_revenue} and \code{new_revenue},
#'  which give the tax (income tax, super tax, and division 293 tax) payable by 
#'  that taxpayer in the comparator scenario and the proposed scenario, respectively.
#' 
#' @examples
#' 
#' # if (requireNamespace("taxstats", quietly = TRUE)) {
#' #   library(data.table)
#' #   s1314 <- taxstats::sample_file_1314
#' #   s1314[, WEIGHT := 50L]
#' #   revenue_from_new_cap_and_div293(s1314, new_cap = 12e3, "2016-17")
#' #   revenue_from_new_cap_and_div293(s1314, new_contr_tax = "mr - 15%", "2016-17")
#' # }
#' 
#' 
#' @export

model_new_caps_and_div293 <- function(.sample.file, 
                                      fy.year,
                                      new_cap = 30e3, 
                                      new_cap2 = 35e3, 
                                      new_age_based_cap = TRUE, 
                                      new_cap2_age = 49, 
                                      new_ecc = FALSE,
                                      new_contr_tax = "15%",
                                      new_div293_threshold = 300e3,
                                      use_other_contr = FALSE, 
                                      scale_contr_match_ato = FALSE, 
                                      .lambda = 0, 
                                      reweight_late_lodgers = TRUE,
                                      .mu = 1.05,
                                      impute_zero_concess_contr = TRUE,
                                      .min.Sw.for.SG = 450 * 12,
                                      .SG_rate = 0.0925, 
                                      
                                      prv_cap = 30e3, 
                                      prv_cap2 = 35e3, 
                                      prv_age_based_cap = TRUE, 
                                      prv_cap2_age = 49, 
                                      prv_ecc = FALSE,
                                      prv_div293_threshold = 300e3) {
  prv_revenue <- new_revenue <- NULL
  if (!any("WEIGHT" == names(.sample.file))){
    warning("WEIGHT not specified. Using WEIGHT=50 (assuming a 2% sample file).")
    WEIGHT <- 50
    .sample.file[, WEIGHT := WEIGHT]
  } 
  
  
  
  sample_file <- apply_super_caps_and_div293(.sample.file, 
                                             colname_concessional = "old_concessional_contributions",
                                             colname_div293_tax = "old_div293_tax", 
                                             colname_new_Taxable_Income = "old_Taxable_Income",
                                             div293_threshold = prv_div293_threshold, 
                                             
                                             use_other_contr = use_other_contr, 
                                             scale_contr_match_ato = scale_contr_match_ato, 
                                             .lambda = .lambda, 
                                             reweight_late_lodgers = reweight_late_lodgers,
                                             .mu = .mu,
                                             impute_zero_concess_contr = impute_zero_concess_contr,
                                             .min.Sw.for.SG = .min.Sw.for.SG,
                                             .SG_rate = .SG_rate, 
                                             
                                             cap = prv_cap, 
                                             cap2 = prv_cap2, 
                                             age_based_cap = prv_age_based_cap, 
                                             cap2_age = prv_cap2_age, 
                                             ecc = prv_ecc,
                                             warn_if_colnames_overwritten = FALSE, 
                                             drop_helpers = FALSE, 
                                             copyDT = TRUE)
  
  new_Taxable_Income <- NULL
  old_Taxable_Income <- NULL
  old_div293_tax <- NULL
  new_div293_tax <- NULL
  
  sample_file <- 
    sample_file[, c("Ind", "old_concessional_contributions", "old_div293_tax", 
                    "div293_income", "old_Taxable_Income"),
                with = FALSE]
  sample_file %>%
    setnames("div293_income", "old_div293_income") %>%
    setkeyv("Ind")
  
  
  
  
  new_sample_file <- apply_super_caps_and_div293(.sample.file, 
                                                 colname_concessional = "new_concessional_contributions",
                                                 colname_div293_tax = "new_div293_tax", 
                                                 colname_new_Taxable_Income = "new_Taxable_Income",
                                                 div293_threshold = new_div293_threshold, 
                                                 
                                                 use_other_contr = use_other_contr, 
                                                 scale_contr_match_ato = scale_contr_match_ato, 
                                                 .lambda = .lambda, 
                                                 reweight_late_lodgers = reweight_late_lodgers,
                                                 .mu = .mu,
                                                 impute_zero_concess_contr = impute_zero_concess_contr,
                                                 .min.Sw.for.SG = .min.Sw.for.SG,
                                                 .SG_rate = .SG_rate, 
                                                 
                                                 cap = new_cap, 
                                                 cap2 = new_cap2, 
                                                 age_based_cap = new_age_based_cap, 
                                                 cap2_age = new_cap2_age, 
                                                 ecc = new_ecc,
                                                 warn_if_colnames_overwritten = FALSE, 
                                                 drop_helpers = FALSE, 
                                                 copyDT = TRUE)
  
  
  sexp_contributions_tax <- substitute(new_contr_tax)
  use_marginal_rate <- 
    !missing(new_contr_tax) &&
    length(new_contr_tax) == 1L &&
    is.character(new_contr_tax) &&
    startsWith(new_contr_tax, "mr")
  
  rel_marginal_rate <- 0
  if (use_marginal_rate) {
    if (startsWith(new_contr_tax, "mr - ")) {
      rel_marginal_rate <- -as.double(sub("^mr - ([0-9]+)[%]$", "\\1", new_contr_tax)) / 100
    } else if (startsWith(new_contr_tax, "mr + ")) {
      rel_marginal_rate <- +as.double(sub("^mr . ([0-9]+)[%]$", "\\1", new_contr_tax)) / 100
    } else {
      stop("`contributions_tax = ", new_contr_tax, "` ", 
           "not of the form mr - <double> or mr + <double>")
    }
  }
  
  
  
  
  setkeyv(new_sample_file, "Ind")
  ans <- sample_file[new_sample_file]
  
  ans[, OldContributionsTax := 0.15 * old_concessional_contributions]
  prv_ordinary_tax <- NULL
  ans[, prv_ordinary_tax := income_tax(old_Taxable_Income, fy.year, .dots.ATO = .SD)]
  ans[, prv_revenue := income_tax(old_Taxable_Income, fy.year, .dots.ATO = .SD) + old_div293_tax + OldContributionsTax]
  if (use_marginal_rate) {
    NewMarginalRate <- OldContributionsTax <- NewContributionsTax <-
      old_concessional_contributions <- new_concessional_contributions <- NULL
    
    ans[, NewMarginalRate := marginal_rate(new_sample_file, fy.year)]
    ans[, NewContributionsTax := (NewMarginalRate + rel_marginal_rate) * new_concessional_contributions]
   
  } else {
    ans[, NewContributionsTax := 0.15 * new_concessional_contributions]
  }
  ans[, new_ordinary_tax := income_tax(new_Taxable_Income, fy.year, .dots.ATO = .SD)]
  new_ordinary_tax <- NULL
  ans[, new_revenue := income_tax(new_Taxable_Income, fy.year, .dots.ATO = .SD) + NewContributionsTax + new_div293_tax]
  ans
}

#' @rdname model_new_caps_and_div293
#' @return For \code{n_affected_from_new_cap_and_div293}, the number of individuals affected by the proposed changes.
#' @export

n_affected_from_new_cap_and_div293 <- function(..., adverse_only = TRUE){
  # Less than a dollar change in revenue is no change
  if (adverse_only){
    model_new_caps_and_div293(...) %>%
    {sum(((.[["new_revenue"]] - .[["prv_revenue"]]) > 1) * .[["WEIGHT"]][1])}
  } else {
    model_new_caps_and_div293(...) %>%
      # abs() means a decrease in tax will count
    {sum((abs(.[["new_revenue"]] - .[["prv_revenue"]]) > 1) * .[["WEIGHT"]][1])}
  }
}

#' @rdname model_new_caps_and_div293
#' @return For \code{revenue_from_new_cap_and_div293}, the extra revenue expected from the proposed changes. 
#' @export

revenue_from_new_cap_and_div293 <- function(...){
  model_new_caps_and_div293(...) %>%
  {sum((.[["new_revenue"]] - .[["prv_revenue"]]) * .[["WEIGHT"]][1])}
}



