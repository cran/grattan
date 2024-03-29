
context("Individual income tax")

test_that("income tax checks", {
  skip_on_cran()
  # no fy.year
  skip_if(date2fy(Sys.Date()) > "2020-21")
  
  expect_warning(income_tax(30e3), regexp = "fy\\.year is missing, using current financial year")
  
  expect_equal(suppressWarnings(income_tax(30e3)), income_tax(30e3, date2fy(Sys.Date())))
  
  # not fy
  expect_error(income_tax(1, "2015-17"), regexp = "financial year")
  
  # not implemented
  expect_error(income_tax(1, "2030-31", allow.forecasts = TRUE))
  
  # NA
  expect_warning(income_tax(-1, "2014-15"), regexp = "Negative", ignore.case = TRUE)
  
  # not a data frame
  expect_error(income_tax(1, "2013-14", .dots.ATO = "foo"))
  
  # multiple fy.years
  expect_error(income_tax(1:6, 
                          fy.year = letters[1:6]))
  
  expect_error(income_tax(50e3, "2013-14", .dots.ATO = data.frame(x = 1:5)), 
               regexp = "Number of rows")
})

test_that("income_tax returns known results",{
  
  # All numbers are from the ATO comprehensive tax calculator. 
  expect_equal(income_tax(30e3, fy.year = "2011-12"), 2550)
  expect_equal(income_tax(20e3, fy.year = "2011-12"), 659.6)
  expect_equal(income_tax(20e3, fy.year = "2011-12", return.mode = "integer"), 659L)
  
  expect_equal(income_tax(50e3, fy.year = "2012-13"), 8297)
  expect_equal(income_tax(60e3, fy.year = "2012-13"), 11847)
  expect_equal(income_tax(70e3, fy.year = "2012-13"), 15347)
  expect_equal(income_tax(200e3, fy.year = "2012-13"), 66547)
  
  expect_equal(income_tax(30e3, fy.year = "2013-14"), 2247)
  expect_equal(income_tax(40e3, fy.year = "2013-14"), 4747)
  # expect_equal(income_tax(40e3, "2013-14", family_status = "family", n_dependants = 1L), 4394.70)
  expect_equal(income_tax(40e3, "2013-14", 
                          .dots.ATO = data.table(sp_flag = TRUE,
                                                 c_depend_child = 1L,
                                                 spc_rebate_income = 0L,
                                                 c_age_30_june = 42L)),
               4394.70)
  # different rounding treatment.
  # Test dropped
  # expect_equal(round(income_tax(40e3,
  #                               "2013-14",
  #                               family_status = "family",
  #                               n_dependants = 0L,
  #                               age = 66)),
  #              2882)
  # expect_equal(income_tax(40e3, 
  #                         "2013-14",
  #                         family_status = "family",
  #                         n_dependants = 2L,
  #                         .dots.ATO = data.frame(Spouse_adjusted_taxable_inc = 30e3)),
  #              4747)
  expect_equal(income_tax(40e3, 
                          "2013-14", 
                          .dots.ATO = data.table(sp_flag = TRUE,
                                                 c_depend_child = 2L,
                                                 spc_rebate_income = 30e3L,
                                                 c_age_30_june = 42L)),
               4747)
  
  expect_equal(income_tax(31993, fy.year = "2014-15"), 2815.53)
  expect_equal(income_tax(31993, fy.year = "2014-15", age = 70), 0)
  expect_equal(income_tax(135288, fy.year = "2014-15"), 40709, tol = 1, scale = 1)
  expect_equal(income_tax(41636, fy.year = "2014-15"), 5535.95, tol = 1, scale = 1)
  expect_equal(income_tax(26010, fy.year = "2014-15"), 1550.30, tol = 1, scale = 1)
  
  # Test rolling now no longer the default
  # expect_equal(rolling_income_tax(31993, fy.year = "2014-15"), 2815.53)
  # expect_equal(rolling_income_tax(31993, fy.year = "2014-15", age = 70), 0)
  # expect_equal(rolling_income_tax(135288, fy.year = "2014-15"), 40709, tol = 1, scale = 1)
  # expect_equal(rolling_income_tax(41636, fy.year = "2014-15"), 5535.95, tol = 1, scale = 1)
  # expect_equal(rolling_income_tax(26010, fy.year = "2014-15"), 1550.30, tol = 1, scale = 1)
  
  
})

test_that("income_tax is not NA for any years)", {
  # i.e. works for all the years we guarantee
  expect_false(any(is.na(income_tax(50e3, fy.year = yr2fy(2001:2020)))))
})

test_that("income_tax is not NA for 2003-04", {
  expect_false(any(is.na(income_tax(30e3, fy.year = yr2fy(2004), age = 66))))
})

test_that("income_tax always returns the length of its arguments", {
  LEN <- max(ceiling(abs(rcauchy(10, scale = 3))))
  expect_equal(length(income_tax(runif(LEN, 0, 2e6),
                                 fy.year = sample(yr2fy(2004:2014),
                                                  size = LEN,
                                                  replace = TRUE))),
               LEN)
})

test_that("Budget Speech 2003-04", {
  "A taxpayer on $45,000 will receive a tax cut of $208, a taxpayer on $55,000 will receive 
  a tax cut of $448 and a taxpayer on $65,000 will receive a tax cut of $573"
  tax_cut <- function(x) {
    x <- as.integer(x)
    income_tax(x, "2002-03") - income_tax(x, "2003-04")
  }
  expect_equal(tax_cut(45000), 208)
  expect_equal(tax_cut(55000), 448)
  expect_equal(tax_cut(65000), 573)
})

test_that("Previous years (intel compiler trigger)", {
  expect_true(TRUE)
  expect_true(hutilscpp::is_sorted(income_tax(1e3:151e3, fy::yr2fy(1984))))
  expect_true(hutilscpp::is_sorted(income_tax(1e3:151e3, fy::yr2fy(1985))))
  expect_true(hutilscpp::is_sorted(income_tax(1e3:151e3, fy::yr2fy(1986))))
  expect_true(hutilscpp::is_sorted(income_tax(1e3:151e3, fy::yr2fy(1987))))
  expect_true(hutilscpp::is_sorted(income_tax(1e3:151e3, fy::yr2fy(1988))))
  expect_true(hutilscpp::is_sorted(income_tax(1e3:151e3, fy::yr2fy(1989))))
  expect_true(hutilscpp::is_sorted(income_tax(1e3:151e3, fy::yr2fy(1990))))
  expect_true(hutilscpp::is_sorted(income_tax(1e3:151e3, fy::yr2fy(1991))))
  expect_true(hutilscpp::is_sorted(income_tax(1e3:151e3, fy::yr2fy(1992))))
  expect_true(hutilscpp::is_sorted(income_tax(1e3:151e3, fy::yr2fy(1993))))
  expect_true(hutilscpp::is_sorted(income_tax(1e3:151e3, fy::yr2fy(1994))))
  expect_true(hutilscpp::is_sorted(income_tax(1e3:151e3, fy::yr2fy(1995))))
  expect_true(hutilscpp::is_sorted(income_tax(1e3:151e3, fy::yr2fy(1996))))
  expect_true(hutilscpp::is_sorted(income_tax(1e3:151e3, fy::yr2fy(1997))))
  expect_true(hutilscpp::is_sorted(income_tax(1e3:151e3, fy::yr2fy(1998))))
  expect_true(hutilscpp::is_sorted(income_tax(1e3:151e3, fy::yr2fy(1999))))
  expect_true(hutilscpp::is_sorted(income_tax(1e3:151e3, fy::yr2fy(2000))))
  expect_true(hutilscpp::is_sorted(income_tax(1e3:151e3, fy::yr2fy(2001))))
  expect_true(hutilscpp::is_sorted(income_tax(1e3:151e3, fy::yr2fy(2002))))
  expect_true(hutilscpp::is_sorted(income_tax(1e3:151e3, fy::yr2fy(2003))))
  expect_true(hutilscpp::is_sorted(income_tax(1e3:151e3, fy::yr2fy(2004))))
  expect_true(hutilscpp::is_sorted(income_tax(1e3:151e3, fy::yr2fy(2005))))
  expect_true(hutilscpp::is_sorted(income_tax(1e3:151e3, fy::yr2fy(2006))))
  expect_true(hutilscpp::is_sorted(income_tax(1e3:151e3, fy::yr2fy(2007))))
  expect_true(hutilscpp::is_sorted(income_tax(1e3:151e3, fy::yr2fy(2008))))
  expect_true(hutilscpp::is_sorted(income_tax(1e3:151e3, fy::yr2fy(2009))))
  expect_true(hutilscpp::is_sorted(income_tax(1e3:151e3, fy::yr2fy(2010))))
  expect_true(hutilscpp::is_sorted(income_tax(1e3:151e3, fy::yr2fy(2011))))
  expect_true(hutilscpp::is_sorted(income_tax(1e3:151e3, fy::yr2fy(2012))))
  expect_true(hutilscpp::is_sorted(income_tax(1e3:151e3, fy::yr2fy(2013))))
  expect_true(hutilscpp::is_sorted(income_tax(1e3:151e3, fy::yr2fy(2014))))
  expect_true(hutilscpp::is_sorted(income_tax(1e3:151e3, fy::yr2fy(2015))))
  expect_true(hutilscpp::is_sorted(income_tax(1e3:151e3, fy::yr2fy(2016))))
  expect_true(hutilscpp::is_sorted(income_tax(1e3:151e3, fy::yr2fy(2017))))
  expect_true(hutilscpp::is_sorted(income_tax(1e3:151e3, fy::yr2fy(2018))))
  expect_true(hutilscpp::is_sorted(income_tax(1e3:151e3, fy::yr2fy(2019))))
  expect_true(hutilscpp::is_sorted(income_tax(1e3:151e3, fy::yr2fy(2020))))
  expect_true(hutilscpp::is_sorted(income_tax(1e3:151e3, fy::yr2fy(2021))))
  expect_true(hutilscpp::is_sorted(income_tax(1e3:151e3, fy::yr2fy(2022))))
  expect_true(hutilscpp::is_sorted(income_tax(1e3:151e3, fy::yr2fy(2023))))
  expect_true(hutilscpp::is_sorted(income_tax(1e3:151e3, fy::yr2fy(2024))))
  expect_true(hutilscpp::is_sorted(income_tax(1e3:151e3, fy::yr2fy(2025))))
  expect_true(hutilscpp::is_sorted(income_tax(1e3:151e3, fy::yr2fy(2026))))
  expect_true(hutilscpp::is_sorted(income_tax(1e3:151e3, fy::yr2fy(2027))))
  expect_true(hutilscpp::is_sorted(income_tax(1e3:151e3, fy::yr2fy(2028))))
  expect_true(hutilscpp::is_sorted(income_tax(1e3:151e3, fy::yr2fy(2029))))
  expect_true(hutilscpp::is_sorted(income_tax(1e3:151e3, fy::yr2fy(2030))))
  
  expect_true(hutilscpp::is_sorted(income_tax(1e3:151e3, fy::yr2fy(1984))))
  expect_true(hutilscpp::is_sorted(income_tax(1e3:151e3, fy::yr2fy(1985))))
  expect_true(hutilscpp::is_sorted(income_tax(1e3:151e3, fy::yr2fy(1986))))
  expect_true(hutilscpp::is_sorted(income_tax(1e3:151e3, fy::yr2fy(1987))))
  expect_true(hutilscpp::is_sorted(income_tax(1e3:151e3, fy::yr2fy(1988))))
  expect_true(hutilscpp::is_sorted(income_tax(1e3:151e3, fy::yr2fy(1989))))
  expect_true(hutilscpp::is_sorted(income_tax(1e3:151e3, fy::yr2fy(1990))))
  expect_true(hutilscpp::is_sorted(income_tax(1e3:151e3, fy::yr2fy(1991))))
  expect_true(hutilscpp::is_sorted(income_tax(1e3:151e3, fy::yr2fy(1992))))
  expect_true(hutilscpp::is_sorted(income_tax(1e3:151e3, fy::yr2fy(1993))))
  expect_true(hutilscpp::is_sorted(income_tax(1e3:151e3, fy::yr2fy(1994))))
  expect_true(hutilscpp::is_sorted(income_tax(1e3:151e3, fy::yr2fy(1995))))
  expect_true(hutilscpp::is_sorted(income_tax(1e3:151e3, fy::yr2fy(1996))))
  expect_true(hutilscpp::is_sorted(income_tax(1e3:151e3, fy::yr2fy(1997))))
  expect_true(hutilscpp::is_sorted(income_tax(1e3:151e3, fy::yr2fy(1998))))
  expect_true(hutilscpp::is_sorted(income_tax(1e3:151e3, fy::yr2fy(1999))))
  expect_true(hutilscpp::is_sorted(income_tax(1e3:151e3, fy::yr2fy(2000))))
  expect_true(hutilscpp::is_sorted(income_tax(1e3:151e3, fy::yr2fy(2001))))
  expect_true(hutilscpp::is_sorted(income_tax(1e3:151e3, fy::yr2fy(2002))))
  expect_true(hutilscpp::is_sorted(income_tax(1e3:151e3, fy::yr2fy(2003))))
  expect_true(hutilscpp::is_sorted(income_tax(1e3:151e3, fy::yr2fy(2004))))
  expect_true(hutilscpp::is_sorted(income_tax(1e3:151e3, fy::yr2fy(2005))))
  expect_true(hutilscpp::is_sorted(income_tax(1e3:151e3, fy::yr2fy(2006))))
  expect_true(hutilscpp::is_sorted(income_tax(1e3:151e3, fy::yr2fy(2007))))
  expect_true(hutilscpp::is_sorted(income_tax(1e3:151e3, fy::yr2fy(2008))))
  expect_true(hutilscpp::is_sorted(income_tax(1e3:151e3, fy::yr2fy(2009))))
  expect_true(hutilscpp::is_sorted(income_tax(1e3:151e3, fy::yr2fy(2010))))
  expect_true(hutilscpp::is_sorted(income_tax(1e3:151e3, fy::yr2fy(2011))))
  expect_true(hutilscpp::is_sorted(income_tax(1e3:151e3, fy::yr2fy(2012))))
  expect_true(hutilscpp::is_sorted(income_tax(1e3:151e3, fy::yr2fy(2013))))
  expect_true(hutilscpp::is_sorted(income_tax(1e3:151e3, fy::yr2fy(2014))))
  expect_true(hutilscpp::is_sorted(income_tax(1e3:151e3, fy::yr2fy(2015))))
  expect_true(hutilscpp::is_sorted(income_tax(1e3:151e3, fy::yr2fy(2016))))
  expect_true(hutilscpp::is_sorted(income_tax(1e3:151e3, fy::yr2fy(2017))))
  expect_true(hutilscpp::is_sorted(income_tax(1e3:151e3, fy::yr2fy(2018))))
  expect_true(hutilscpp::is_sorted(income_tax(1e3:151e3, fy::yr2fy(2019))))
  expect_true(hutilscpp::is_sorted(income_tax(1e3:151e3, fy::yr2fy(2020))))
  expect_true(hutilscpp::is_sorted(income_tax(1e3:151e3, fy::yr2fy(2021))))
  expect_true(hutilscpp::is_sorted(income_tax(1e3:151e3, fy::yr2fy(2022))))
  expect_true(hutilscpp::is_sorted(income_tax(1e3:151e3, fy::yr2fy(2023))))
  expect_true(hutilscpp::is_sorted(income_tax(1e3:151e3, fy::yr2fy(2024))))
  expect_true(hutilscpp::is_sorted(income_tax(1e3:151e3, fy::yr2fy(2025))))
  expect_true(hutilscpp::is_sorted(income_tax(1e3:151e3, fy::yr2fy(2026))))
  expect_true(hutilscpp::is_sorted(income_tax(1e3:151e3, fy::yr2fy(2027))))
  expect_true(hutilscpp::is_sorted(income_tax(1e3:151e3, fy::yr2fy(2028))))
  expect_true(hutilscpp::is_sorted(income_tax(1e3:151e3, fy::yr2fy(2029))))
  expect_true(hutilscpp::is_sorted(income_tax(1e3:151e3, fy::yr2fy(2030))))
  List <-
    lapply(1984:2000, function(yr) {
      income_tax(1e3:151e3, fy::yr2fy(yr))
    })
  expect_true(all(vapply(List, hutilscpp::is_sorted, FUN.VALUE = NA)))
  expect_true(TRUE)
  List <-
    lapply(2000:2030, function(yr) {
      income_tax(1e3:151e3, fy::yr2fy(yr))
    })
  expect_true(all(vapply(List, hutilscpp::is_sorted, FUN.VALUE = NA)))
})

test_that("Previous years", {
  skip_if_not_installed("hutilscpp")
  List <-
    lapply(1984:2030, function(yr) {
      income_tax(1e3:151e3, fy::yr2fy(yr))
    })
  expect_true(all(vapply(List, hutilscpp::is_sorted, FUN.VALUE = NA)))
  
  List67 <-
    lapply(1984:2030, function(yr) {
      income_tax(1e3:151e3, 
                 fy::yr2fy(yr),
                 .dots.ATO = data.table(ic_taxable_income_loss = 1e3:151e3,
                                        c_age_30_june = 67L))
    })
  expect_true(all(vapply(List, hutilscpp::is_sorted, FUN.VALUE = NA)))
})

