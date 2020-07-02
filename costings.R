

# Only real question:
#   - Do we need to modify our projection?




library(ggplot2)
library(scales)
library(data.table)
library(magrittr)
library(hutils)
library(hutilscpp)
library(grattan)

tax201718 <- fread("../taxstats1718/2018_sample_file.csv")
tax202122 <- project(tax201718,
                     h = 4L,
                     differentially_uprate_Sw = FALSE,
                     fy.year.of.sample.file = "2017-18",
                     lf.series = 0,
                     .recalculate.inflators = TRUE,
                     check_fy_sample_file = FALSE)
tax202122A <-
  model_new_caps_and_div293(tax202122,
                            fy.year = "2019-20",
                            new_cap = 20e3,
                            prv_cap = 25e3,
                            new_contr_tax = "mr - 15%",
                            new_age_based_cap = FALSE,
                            prv_age_based_cap = FALSE)

s2021 <-
  fread("../taxstats1718/2018_sample_file.csv") %>%
  project(h = 3L,
          differentially_uprate_Sw = FALSE,
          fy.year.of.sample.file = "2017-18",
          lf.series = 0.01,
          .recalculate.inflators = TRUE,
          check_fy_sample_file = FALSE)



# Reduce pre-tax contribution cap to 11k
# Reduce pre-tax contribution cap to 15k
# Reduce pre-tax contribution cap to 20k
# Tax contributions at marginal rate less 15ppts
# Tax contributions at marginal rate less 15ppts; cap contributions at 15k
# Tax contributions at marginal rate less 15ppts; cap contributions at 20k
# Tax contributions at marginal rate less 20ppts
# Tax contributions at marginal rate less 20ppts; cap contributions at 15k
# Tax contributions at marginal rate less 20ppts; cap contributions at 20k


for (newContrTax in c("15%", "mr - 15%", "mr - 20%")) {
  for (newCap in c(11e3, 15e3, 20e3)) {
    cat(formatC(newContrTax, width = nchar("mr - 20%")), "\t", scales::dollar(newCap), "\t")
    s2021 %>%
      revenue_from_new_cap_and_div293(fy.year = "2019-20",
                                      prv_cap = 25e3,
                                      new_contr_tax = newContrTax,
                                      new_cap = newCap,
                                      prv_age_based_cap = FALSE) %>%
      divide_by(1e9) %>%
      scales::dollar(.) %>%
      cat("bn\n")
  }
}


# Tax all super earnings in retirement phase at 15%
with(s2021, {
  # Assume 7% returns in accumulation, 5% in retiremtn
  earnings <- (age_range <= 1L) * 0.05 * MCS_Ttl_Acnt_Bal
  earnings * 0.15
})


revenue_from_bal_cap <- function(balance_cap) {
  with(s2021, {
    old_tax <- income_tax(Taxable_Income, "2020-21", .dots.ATO = s2021)
    # Assume 7% returns in accumulation, 5% in retiremtn
    # (7.3% five years to June 2019)
    wt <- first(WEIGHT)
    old_earnings <- if_else(age_range <= 1L, 0.05, 0.07) * MCS_Ttl_Acnt_Bal
    old_earnings_tax <- (age_range > 1) * 0.15 * old_earnings
    new_earnings <- if_else(age_range <= 1L, 0.05, 0.07) * pminC(MCS_Ttl_Acnt_Bal, balance_cap)
    new_earnings_tax <- (age_range > 1) * 0.15 * new_earnings
    extra_taxable_income <- if_else(age_range <= 1L, 0.05, 0.07) * pmax0(MCS_Ttl_Acnt_Bal - balance_cap)

    # 121G in 2018-19 (after tax)
    stopifnot(((sum(old_earnings) / 1.07)  * 0.85 * wt) %between% c(115e9, 125e9))

    NewTaxableIncome <- extra_taxable_income + Taxable_Income
    new_tax <- income_tax(NewTaxableIncome, "2020-21", .dots.ATO = s2021)

    delta <- new_tax - old_tax + new_earnings_tax - old_earnings_tax

    sum(delta) * wt
  })
}

# Lower transfer balance cap to 500k
# Lower transfer balance cap to 750k
# Lower transfer balance cap to 1m

