

# Only real question:
#   - Do we need to modify our projection?




library(ggplot2)
library(scales)
library(data.table)
library(magrittr)
library(hutils)
library(hutilscpp)
library(grattan)
packageVersion("grattan") >= "1.9.0.0"


s1718 <- fread("../taxstats1718/2018_sample_file.csv")
s1819 <- fread("../SampleFile1819/sample_file_1819.tsv")
tax202122 <- project(s1718,
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

s2021_via_1718 <-
  s1718 %>%
  project(h = 3L,
          differentially_uprate_Sw = FALSE,
          fy.year.of.sample.file = "2017-18",
          lf.series = 0,
          .recalculate.inflators = TRUE,
          check_fy_sample_file = FALSE)
s2021_via_1819 <-
  s1819 %>%
  .[, Ind := seq_len(.N)] %>%
  project(h = 2L,
          differentially_uprate_Sw = FALSE,
          fy.year.of.sample.file = "2018-19",
          lf.series = 0,
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

cat("Contr. tax\tConc. Cap\tCosting/$bn (1718 file)\tCosting/$bn (1819 file)\n")
for (newContrTax in c("15%", "mr - 15%", "mr - 20%")) {
  for (newCap in c(0, 11e3, 15e3, 20e3)) {
    cat(formatC(newContrTax, width = nchar("Contr. tax")), "\t $",
        formatC(as.integer(newCap), width = nchar("$35,000"), big.mark = ","), "\t",
        sep = "")
    for (orig_sample_file in list(s2021_via_1718, s2021_via_1819)) {
      orig_sample_file %>%
        revenue_from_new_cap_and_div293(fy.year = "2019-20",
                                        prv_div293_threshold = 250e3,
                                        new_div293_threshold = 250e3,
                                        new_contr_tax = newContrTax,
                                        new_cap = newCap,
                                        prv_cap = 25e3,
                                        new_age_based_cap = FALSE,
                                        prv_age_based_cap = FALSE) %>%
        divide_by(1e9) %>%
        formatC(format = "f", flag = "#", digits = 1, width = nchar("Costing/$bn (1718 file)")) %>%
        cat
      cat("\t")
    }
    cat("\n")
  }
}

# Verify or modify balances to line up with APRA
TOTAL_AUM_APRA_1718 <- (1914087 + 2129 + 735400) * 1e6  # >4 members + <=4 members + SMSF
TOTAL_AUM_APRA_1819 <- (2071149 + 2098 + 747600) * 1e6  # >4 members + <=4 members + SMSF
TOTAL_AUM_APRA_1920 <- TOTAL_AUM_APRA_1819 * 1.07
# https://www.apra.gov.au/sites/default/files/2020-01/Annual%20Superannuation%20Bulletin%20June%202019.pdf
# (Also colocated PDF)
APRA_NET_INVESTMENT_INCOME_1819 <- (128954 + 145 + 32623) * 1e6
APRA_P_GEQ_5_MEMBERS <- 2071149 * 1e6 / TOTAL_AUM_APRA_1819
APRA_NET_EARNINGS_POST_TAX_1819 <- 120986 * 1e6 / APRA_P_GEQ_5_MEMBERS ## >4 members


r_APRA_over_ATO <-
  with(s1718, {
    TOTAL_AUM_APRA_1718 / sum(MCS_Ttl_Acnt_Bal * 50)
  })


revenue_from_bal_cap <- function(balance_cap,
                                 sample_file = c("s2021_via_1718", "s2021_via_1819"),
                                 apra_concord = c("none", "balance", "weight"),
                                 r_earnings_retirement = 0.05,
                                 r_earnings_accumulation = 0.07) {
  apra_concord <- match.arg(apra_concord)
  sample_file <- match.arg(sample_file)
  s2021 <- get(sample_file, mode = "list")
  with(s2021, {

    # Assume 7% returns in accumulation, 5% in retiremtn
    # (7.3% five years to June 2019)

    wt <- first(WEIGHT)
    if (apra_concord == "weight") {
      wt <- r_APRA_over_ATO * wt
    }
    if (apra_concord == "balance") {
      MCS_Ttl_Acnt_Bal <- r_APRA_over_ATO * MCS_Ttl_Acnt_Bal
    }
    r_earnings <- if_else(age_range <= 1L, r_earnings_retirement, r_earnings_accumulation)

    old_earnings <- r_earnings * MCS_Ttl_Acnt_Bal
    old_earnings_tax <- (age_range > 1) * 0.15 * old_earnings
    old_net_earnings_post_tax <- old_earnings - old_earnings_tax
    new_earnings <- r_earnings * pminC(MCS_Ttl_Acnt_Bal, balance_cap)
    new_earnings_tax <- (age_range > 1) * 0.15 * new_earnings
    extra_taxable_income <- r_earnings * pmax0(MCS_Ttl_Acnt_Bal - balance_cap)
    extra_taxable_income <- extra_taxable_income * 0.5 # CGT
    #
    old_earnings2021 <- sum(old_net_earnings_post_tax) * wt
    avg_growth <- mean(r_earnings)
    old_earnings1819 <- (old_earnings2021) / (avg_growth ^ 2)
    net_old_earnings1819 <- sum(old_net_earnings_post_tax) * wt


    NewTaxableIncome <- extra_taxable_income + Taxable_Income

    new_tax <- income_tax(NewTaxableIncome, "2020-21", .dots.ATO = s2021)
    old_tax <- income_tax(Taxable_Income, "2020-21", .dots.ATO = s2021)
    new_earnings_tax <- (age_range > 1) * 0.15 * new_earnings
    old_earnings_tax <- (age_range > 1) * 0.15 * old_earnings

    delta <- new_tax - old_tax + new_earnings_tax - old_earnings_tax

    sum(delta) * wt
  })
}

cat("\n")
# Lower transfer balance cap to 500k
# Lower transfer balance cap to 750k
# Lower transfer balance cap to 1m
cat(formatC("Uprator", width = nchar("mr - 20%")), "\t", formatC("Balance cap", width = nchar(" $1,000,000")), "\t")
cat(" Costing/$bn\n")
for (balC in c(100e3, 500e3, 750e3, 1e6)) {
  for (apraC in c("none", "balance", "weight")) {
    for (sa_f in c("s2021_via_1718", "s2021_via_1819")) {
      cat(formatC(apraC, width = nchar("mr - 20%")),
          "\t",
          formatC(scales::dollar(balC), width = nchar(" $1,000,000")),
          "\t")
      cat(formatC(revenue_from_bal_cap(balC, sa_f, apraC) / 1e9,
                  format = "f",
                  flag = "#",
                  digits = 1,
                  width = nchar(" Costing/$bn")), "\n")
    }
  }
}
