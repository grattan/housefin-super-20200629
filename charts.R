options("dplyr.summarise.inform" = FALSE)
options(digits = 3)
options(scipen = 99)
options(encoding = "UTF-8")
stopifnot("charts.R" %in% dir())
library(data.table)
library(hutilscpp)
library(taxstats)
library(grattanCharts)
library(tidyr)
library(dplyr)
library(ggplot2)
library(magrittr)
library(grattantheme)
library(grattandata)
library(hutils)
library(grattan)

"%between%" <- data.table::`%between%`
comma <- function(x) prettyNum(x, big.mark = ",")
p0 <- paste0

s1718 <-
  tryCatch(read_microdata("ato_2018_sample_file.csv", fast = TRUE),
           error = function(e) {
             readr::read_csv("~/taxstats1718/2018_sample_file.csv")
           })

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



earnings_tax_concession <- function(.sample_file,
                                    apra_concord = c("none", "balance", "weight"),
                                    r_earnings_retirement = 0.05,
                                    r_earnings_accumulation = 0.07,
                                    effective_tax_on_earnings = 0.10,
                                    p_excess_earnings_cgt = 0.8) {
  apra_concord <- match.arg(apra_concord)
  s2021 <- copy(.sample_file)
  balance_cap <- 0
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
    old_earnings_tax <- (age_range > 1) * effective_tax_on_earnings * old_earnings
    old_net_earnings_post_tax <- old_earnings - old_earnings_tax
    new_earnings <- r_earnings * pminC(MCS_Ttl_Acnt_Bal, balance_cap)
    new_earnings_tax <- (age_range > 1) * effective_tax_on_earnings * new_earnings
    extra_taxable_income <- r_earnings * pmax0(MCS_Ttl_Acnt_Bal - balance_cap)
    extra_taxable_income <-
      p_excess_earnings_cgt * extra_taxable_income * 0.5 +  # CGT
      (1 - p_excess_earnings_cgt) * extra_taxable_income
    #
    old_earnings2021 <- sum(old_net_earnings_post_tax) * wt
    avg_growth <- mean(r_earnings)
    old_earnings1819 <- (old_earnings2021) / (avg_growth ^ 2)
    net_old_earnings1819 <- sum(old_net_earnings_post_tax) * wt


    NewTaxableIncome <- extra_taxable_income + Taxable_Income

    new_tax <- income_tax(NewTaxableIncome, "2020-21", .dots.ATO = s2021)
    old_tax <- income_tax(Taxable_Income, "2020-21", .dots.ATO = s2021)
    new_earnings_tax <- (age_range > 1) * effective_tax_on_earnings * new_earnings
    old_earnings_tax <- (age_range > 1) * effective_tax_on_earnings * old_earnings

    delta <- new_tax - old_tax + new_earnings_tax - old_earnings_tax

    s2021[, earnings_delta := delta]
  })
}

s2021 <-
  s1718 %>%
  project_to(to_fy = "2020-21",
             fy.year.of.sample.file = "2017-18",
             lf.series = 0,
             r_super_balance = 1.07) %>%
  apply_super_caps_and_div293(cap = 25e3,
                              age_based_cap = FALSE,
                              incl_listo = TRUE,
                              div293_threshold = 250e3)

TaxExpenditure2021 <-
  s1718 %>%
  project_to(to_fy = "2020-21",
             fy.year.of.sample.file = "2017-18",
             lf.series = 0,
             r_super_balance = 1.07) %>%
  model_new_caps_and_div293(fy.year = "2020-21",
                            prv_cap = 25e3,
                            new_cap = 0,
                            new_age_based_cap = FALSE,
                            prv_age_based_cap = FALSE,
                            prv_listo_rate = 0.15,
                            new_listo_rate = 0,
                            new_contr_tax = "mr - 0%",
                            prv_div293_threshold = 250e3,
                            new_div293_threshold = Inf)

TaxExpenditure2021_earnings <-
  s1718 %>%
  project_to(to_fy = "2020-21",
             fy.year.of.sample.file = "2017-18",
             lf.series = 0,
             r_super_balance = 1.07) %>%
  earnings_tax_concession(apra_concord = "weight")


grattan_save_all <- function(filename, object) {
  grattantheme::grattan_save(filename = filename,
                             object = object,
                             type = "all",
                             save_pptx = TRUE,
                             save_data = TRUE)
}

# Figure 3.
grattan_save_all(provide.file("Figure-3-1/Figure31.pdf"), {
  merge(TaxExpenditure2021,
        TaxExpenditure2021_earnings,
        by = "Ind") %>%
    as_tibble %>%
    filter(old_concessional_contributions > 0) %>%
    mutate_ntile("old_Taxable_Income", n = 10L) %>%


    group_by(old_Taxable_IncomeDecile) %>%
    summarise(TotalBenefits = sum(earnings_delta + new_revenue - prv_revenue)) %>%
    mutate(Decile = factor(old_Taxable_IncomeDecile)) %>%
    mutate(Percentage = 100 * TotalBenefits / sum(TotalBenefits)) %>%
    ggplot(aes(x = Decile, y = Percentage)) +
    geom_col() +
    scale_y_continuous_grattan() +
    theme_grattan() +
    labs(title = "Superannuation tax breaks primarily benefit high-income earners",
         subtitle = "Percentage of total tax breaks",
         caption = paste0("Notes: Value of tax breaks calculated against a comprehensive ",
                          "income tax benchmark. Deciles sorted by taxable income.",
                          "Superannuation tax breaks includes concessional taxes for ",
                          "contributions and earnings, taking into account LISTO. ",
                          "Assumes 5% earnings in retirement and 7% earnings in accumulation; ",
                          "assumes that the effective tax on earnings is 10% ",
                          "assumes, if earnings taxes were abolished, ",
                          "taxfilers would put 80% of earnings income into assets that ",
                          "would enjoy the capital gains tax discount; ",
                          "Projections to 2020-21 assume 2% wage growth and 0% ",
                          "growth in the number of taxfilers from 2019-20 to 2020-21 ",
                          "Only includes taxpayers that made a pre-tax contribution ",
                          "in that year.",
                          "\nSource: ATO 2017-18 2% sample file"))
})

# Figure 4.3
grattan_save_all(provide.file("Figure-4-3/Figure43.pdf"), {
  s2021 %>%
    as_tibble %>%
    mutate(TaxBracket = case_when(Taxable_Income <= 18200 ~ "Less than\n$18,200",
                                  Taxable_Income <= 37000 ~ "$18,200 - $37,000",
                                  Taxable_Income <= 90000 ~ "$37,000 - $90,000",
                                  Taxable_Income <= 180e3 ~ "$90,000 - $180,000",
                                  TRUE ~ "180,000+")) %>%
    mutate(TaxBracket = factor(TaxBracket,
                               levels = c("Less than\n$18,200",
                                          "$18,200 - $37,000",
                                          "$37,000 - $90,000",
                                          "$90,000 - $180,000",
                                          "180,000+"))) %>%
    group_by(TaxBracket) %>%
    summarise(minIncome = min(Taxable_Income),
              avg_SG_contributions = mean(SG_contributions),
              avg_salary_sacrifice_contributions = mean(salary_sacrifice_contributions),
              avg_personal_deductible_contributions = mean(personal_deductible_contributions),
              avg_non_concessional_contributions = mean(non_concessional_contributions)) %>%
    pivot_longer(cols = grep("^avg", names(.), value = TRUE)) %>%
    mutate(name = trim_common_affixes(name),
           name = sub("_", "-", name)) %>%
    ggplot(aes(x = TaxBracket, y = value)) +
    geom_col(aes(fill = name)) +
    theme_grattan() +
    grattan_fill_manual() +
    scale_y_continuous_grattan(labels = grattanCharts::grattan_dollar) +
#
#     scale_x_continuous(breaks = c(0.5, 1.5, 2.5, 3.5, 4.5),
#                        labels = letters[1:4]) +
    theme(legend.position = c(0, 1),
          legend.justification = c(0, 1),
          legend.direction = "vertical") +
    labs(x = "Taxable income bracket",
         title = "Those on high incomes make larger voluntary contributions, increasing the value of contributions tax breaks",
         subtitle = "Average superannuation contributions (2020-21)",
         caption = paste0("Notes: ",
                          "Projections to 2020-21 assume 2% wage growth and 0% ",
                          "growth in the number of taxfilers from 2019-20 to 2020-21. ",
                          "SG = super guarantee contributions, assumed to be ",
                          "employer contributions less reportable employer super ",
                          "contributions; ",
                          "salary-sacrifice = reportable employer super contributions; ",
                          "personal-deductible = non-employer superannuation contributions; ",
                          "non-concessional = nonnegative component of personal contributions ",
                          "less non-employer super contributions.",
                          "\n",
                          "Source: ATO 2017-18 2% sample file"))
})

# Figure 4.4
# Average concessional contributions for people over 50
grattan_save_all(provide.file("Figure-4-4/Figure44.pdf"), {
  s2021 %>%
    as_tibble %>%
    filter(age_range <= 4) %>%
    mutate_ntile("Taxable_Income", n = 10L) %>%
    group_by(Taxable_IncomeDecile) %>%
    summarise(avg_SG_contributions = mean(SG_contributions),
              avg_salary_sacrifice_contributions = mean(salary_sacrifice_contributions),
              avg_personal_deductible_contributions = mean(personal_deductible_contributions)) %>%
    pivot_longer(cols = grep("^avg", names(.), value = TRUE)) %>%
    mutate(fill = trim_common_affixes(name)) %>%
    mutate(fill = Switch(trim_common_affixes(fill),
                         "SG" = "Compulsory\ncontributions",
                          "salary_sacrifice" = "Salary-\nsacrificed\nvoluntary\ncontributions",
                         "personal_deductible" = "Personal\nvoluntary\ncontributions",
                         DEFAULT = ""),
           fill = factor(fill,
                         levels = rev(c("Compulsory\ncontributions",
                                        "Salary-\nsacrificed\nvoluntary\ncontributions",
                                        "Personal\nvoluntary\ncontributions")),
                         ordered = TRUE),
           x = factor(Taxable_IncomeDecile, levels = 1:10, ordered = TRUE),
           y = value) %>%
    arrange(desc(fill)) %>%
  # stacked_bar_with_right_labels(scale_fill_manual_args = list(values = grattantheme::grattan_pal(n = 3)),
  #                               scale_y_args = list(expand = c(0, 0), labels = grattanCharts::grattan_dollar),
  #                               scale_x_args = list(name = "Taxable income decile"))
    group_by(Taxable_IncomeDecile) %>%
    mutate(y2 = cumsum(dplyr::lag(y, default = 0)) + y / 2) %>%
    mutate(label = if_else(Taxable_IncomeDecile == 10, as.character(fill), NA_character_),
           x_text = 10.5) %>%
    ggplot(aes(x = x, y = y, fill = fill)) +
    geom_col() +
    theme_grattan() +
    grattan_fill_manual() +
    grattan_colour_manual() +
    geom_text(aes(x = x_text, y = y2, color = fill, label = label),
              hjust = 0,
              lineheight = 0.8,
              fontface = "bold",
              na.rm = TRUE) +
    scale_x_discrete(name = "Taxable income decile", expand = expansion(add = c(0.5, 2.5))) +
    scale_y_continuous_grattan(labels = scales::dollar) +
    labs(title = "Contributions among over 50s are heavily skewed towards high-income earners",
         subtitle = "Average concessional contribution for taxfilers over 50 (2020-21)",
         caption = p0("Notes: ",
                      "Projections to 2020-21 assume 2% wage growth and 0% ",
                      "growth in the number of taxfilers from 2019-20 to 2020-21. ",
                      "Taxable income deciles based on 50+ taxfilers only. ", "\n",
                      "Source: ATO 2017-18 2% sample file"))
})
# Figure 4.5
grattan_save_all(provide.file("Figure-4-5/Figure45.pdf"), {
  s2021 %>%
    as_tibble %>%
    mutate(Age = if_else(age_range <= 3, "55+", "<55")) %>%
    mutate_ntile("Taxable_Income", n = 10L) %>%
    group_by(Age, Taxable_IncomeDecile) %>%
    summarise(tot = sum(concessional_contributions - SG_contributions)) %>%
    ungroup %>%
    mutate(p = tot / sum(tot),
           ordering = if_else(Age != "55+", Taxable_IncomeDecile, 21L - Taxable_IncomeDecile)) %>%

    arrange(desc(ordering)) %>%
    mutate(text_y = cumsum(dplyr::lag(p, default = 0)) + p / 2,
           text_label = if_else(Taxable_IncomeDecile <= 5, "", as.character(Taxable_IncomeDecile))) %>%

    ggplot(aes(x = 1,
               y = p,
               fill = Age,
               group = interaction(ordering, Age))) +
    geom_col() +
    coord_polar(theta = "y", direction = 1) +
    scale_y_continuous_grattan(expand_top = 0,
                               breaks = c(0, 10, 16, 22, 29, 38, 48, 60, 72, 82, 88) / 100,
                               labels = function(x) paste0(as.integer(x * 100), "%")) +
    geom_text(aes(label = text_label,
                  y = text_y,
                  group = interaction(ordering, Age))) +
    theme_grattan() +
    theme(axis.text.y = element_blank(),
          axis.title = element_blank(),
          panel.grid = element_blank(),
          axis.line = element_blank()) +
    grattan_fill_manual(n = 2) +
    labs(title = "Voluntary pre-tax contributions are mostly made by those who are older and on high incomes",
         subtitle = "Percentage of voluntary pre-tax contributions, (2020-21)",
         caption = p0("Notes: ",
                      "Projections to 2020-21 assume 2% wage growth and 0% ",
                      "growth in the number of taxfilers from 2019-20 to 2020-21. ",
                      "Voluntary pre-tax contributions means concessional contributions ",
                      "less SG contributions. ",
                      "\n",
                      "Source: ATO 2017-18 2% sample file"))

})
# Figure 4.6: but for 11k (rather than 10k)
grattan_save_all(provide.file("Figure-4-6/Figure46.pdf"), {
  s2021 %>%
    mutate_ntile("Taxable_Income", n = 10L) %>%
    filter(concessional_contributions > 11e3) %>%
    mutate(Sex = if_else(Gender == 1, "Female", "Male")) %>%
    group_by(Taxable_IncomeDecile, Sex) %>%
    summarise(n_taxfilers = sum(WEIGHT),
              n_taxfilers_SG = sum(WEIGHT * (SG_contributions > 11e3))) %>%
    mutate(n_taxfilers_nonSG = n_taxfilers - n_taxfilers_SG) %>%
    select(-n_taxfilers) %>%
    pivot_longer(cols = c(grep("^n_", names(.), value = TRUE))) %>%
    mutate(name = trim_common_affixes(name),
           fill = if_else(name != "non", "All", "Voluntary only"),
           Taxable_IncomeDecile = factor(Taxable_IncomeDecile),
           Sex = factor(Sex, levels = c("Male", "Female"))) %>%
    ggplot(aes(x = Taxable_IncomeDecile, y = value, fill = fill)) +
    geom_col() +
    facet_wrap(~Sex) +
    theme_grattan() +
    labs(x = "Taxable income decile") +
    scale_y_continuous_grattan(labels = scales::comma) +
    theme(legend.position = c(1, 1),
          legend.justification = c(1, 1)) +
    grattan_fill_manual(n = 2) +
    labs(title = "Few people other than high-income earners contribute over $11,000 a year",
         subtitle = paste("Number of taxfilers with more than $11,000 in concessional contributions, 2020-21"),
         caption = p0("Notes: ",
                      "Projections to 2020-21 assume 2% wage growth and 0% ",
                      "growth in the number of taxfilers from 2019-20 to 2020-21. ",
                      "Non-voluntary component means employer contributions less ",
                      "reportable employer super contributions. ",
                      "\n",
                      "Source: ATO 2017-18 2% sample file"))

})
# Figure 4.7
# Value of pre-tax voluntary contributions to superannuation in excess of
# $10,000 in a year, 2012-13, $2015-16 (by age)
grattan_save_all(provide.file("Figure-4-7/Figure47.pdf"), {
  s2021 %>%
    merge(age_range_decoder, by = "age_range") %>%
    as_tibble %>%
    mutate(voluntary_concessional_contributions = concessional_contributions - SG_contributions) %>%
    filter(voluntary_concessional_contributions > 11e3) %>%
    arrange(age_range_description) %>%
    mutate(Age = if_else(age_range <= 3, "55+", "<55"),
           age_range_description = if_else(age_range >= 7, "Under\n40", as.character(age_range_description)),
           age_range_description = if_else(age_range == 0, "70+", age_range_description),
           age_range_description = forcats::fct_inorder(age_range_description)) %>%
    group_by(Age, age_range_description) %>%
    summarise(tot = sum(voluntary_concessional_contributions)) %>%
    ungroup %>%
    mutate(p = tot / sum(tot)) %>%
    arrange(desc(age_range_description)) %>%
    mutate(text_y = cumsum(dplyr::lag(p, default = 0)) + p / 2,
           text_label =  as.character(age_range_description),
           text_x = seq(0.8, 1.15, length.out = n())) %>%

    ggplot(aes(x = 1,
               y = p,
               fill = Age,
               group = interaction(age_range_description, Age))) +
    geom_col() +
    coord_polar(theta = "y", direction = 1) +
    scale_y_continuous_grattan(expand_top = 0,
                               breaks = c(0, 10, 16, 22, 29, 38, 48, 60, 72, 82, 88) / 100,
                               labels = function(x) paste0(as.integer(x * 100), "%")) +
    geom_text(aes(x = text_x,
                  label = text_label,
                  y = text_y,
                  group = interaction(age_range_description, Age)),
              lineheight = 0.85) +
    theme_grattan() +
    theme(axis.text.y = element_blank(),
          axis.title = element_blank(),
          panel.grid = element_blank(),
          axis.line = element_blank()) +
    grattan_fill_manual(n = 2) +
    labs(title = "",
         subtitle = "Value of pre-tax voluntary contributions to superannuation in excess of
$11,000 in a year, 2020-21",
         caption = "Source: ATO 2017-18 2% sample file")
})

# Figure 5.1
grattan_save_all(provide.file("Figure-5-1/Figure51.pdf"), {
  s2021 %>%
    as_tibble %>%
    mutate(Age = if_else(age_range <= 3, "55+", "<55")) %>%
    mutate(TaxBracket = case_when(Taxable_Income <= 18200 ~ "Less than\n$18,200",
                                  Taxable_Income <= 37000 ~ "$18,200 - $37,000",
                                  Taxable_Income <= 90000 ~ "$37,000 - $90,000",
                                  Taxable_Income <= 180e3 ~ "$90,000 - $180,000",
                                  TRUE ~ "180,000+")) %>%
    mutate(TaxBracket = factor(TaxBracket,
                               levels = c("Less than\n$18,200",
                                          "$18,200 - $37,000",
                                          "$37,000 - $90,000",
                                          "$90,000 - $180,000",
                                          "180,000+"))) %>%
    group_by(Age, TaxBracket) %>%
    summarise(tot = sum(concessional_contributions - SG_contributions)) %>%
    ungroup %>%
    mutate(p = tot / sum(tot),
           ordering = if_else(Age != "55+", as.integer(TaxBracket), 21L - as.integer(TaxBracket))) %>%

    arrange(desc(ordering)) %>%
    mutate(text_y = cumsum(dplyr::lag(p, default = 0)) + p / 2,
           text_label = if_else(TaxBracket %in% levels(TaxBracket)[1:2], "", as.character(TaxBracket)),
           text_label = gsub(",000", "k", text_label)) %>%

    ggplot(aes(x = 1,
               y = p,
               fill = Age,
               group = interaction(ordering, Age))) +
    geom_col() +
    coord_polar(theta = "y", direction = 1) +
    scale_y_continuous_grattan(expand_top = 0,
                               breaks = c(0, 10, 37, 55, 60, 65, 82, 97) / 100,
                               labels = function(x) paste0(as.integer(x * 100), "%")) +
    geom_text(aes(x = 1.15,
                  label = text_label,
                  y = text_y,
                  group = interaction(ordering, Age))) +
    theme_grattan() +
    theme(axis.text.y = element_blank(),
          axis.title = element_blank(),
          panel.grid = element_blank(),
          axis.line = element_blank(),
          legend.title = element_text(hjust = 0),
          legend.position = "right",
          legend.direction = "vertical") +
    guides(fill = guide_legend("Age")) +
    grattan_fill_manual(n = 2) +
    labs(title = "Voluntary post-tax contributions are mostly made by those who are older and on high incomes",
         subtitle = "Percentage of voluntary post-tax contributions, 2020-21",
         caption = "Source: ATO 2017-18 2% sample file")

})
# Figure 5.2
grattan_save_all(provide.file("Figure-5-2/Figure52.pdf"), {
  s2021 %>%
    mutate(Balance = cut(MCS_Ttl_Acnt_Bal,
                         breaks = c(-Inf, 100e3, 250e3, 500e3, 750e3, 1e6, 2e6, Inf),
                         labels = c("Less than $100,000",
                                    "$100,000 to $250,000",
                                    "$250,000 to $500,000",
                                    "$500,000 to $750,000",
                                    "$750,000 to $1 million",
                                    "$1 million to $2 million",
                                    "More than $2 million"),
                         ordered_result = TRUE)) %>%
    group_by(Balance) %>%
    summarise(n_taxfilers = n(),
              v_posttax_contributions = sum(non_concessional_contributions)) %>%
    mutate(p_taxfilers = n_taxfilers / sum(n_taxfilers),
           p_posttax_contributions = v_posttax_contributions / sum(v_posttax_contributions)) %>%
    pivot_longer(grep("^p_", names(.), value = TRUE)) %>%
    mutate(name = trim_common_affixes(name),
           name = factor(if_else(name %ein% "taxfiler", "Taxfilers", "Post-tax contributions"),
                         levels = c("Taxfilers", "Post-tax contributions"),
                         ordered = TRUE)) %>%
    arrange(name, Balance) %>%
    group_by(name) %>%
    mutate(text_y = cumsum(dplyr::lag(value, default = 0)) + value / 2,
           text_color = if_else(Balance %ein% c("More than $2 million",
                                                "$1 million to $2 million"),
                                "white",
                                "black"),
           text_color = if_else(name == "Taxfilers", NA_character_, text_color)) %>%
    ggplot(aes(x = name, y = value, fill = Balance)) +
    geom_col(position = position_stack(reverse = TRUE)) +
    geom_text(aes(x = name, y = text_y, label = Balance,
                  color = text_color),
              position = position_identity(),
              na.rm = TRUE) +
    scale_color_identity() +
    scale_fill_manual(values = c("white", grattantheme::grattan_yellow,
                                 grattantheme::grattan_lightorange, grattantheme::grattan_darkorange,
                                 grattantheme::grattan_red, grattantheme::grattan_darkred,
                                 "black")) +
    theme_grattan() +
    theme(axis.title.x = element_blank()) +
    scale_y_continuous_grattan(labels = function(x) paste0(as.integer(x * 100), "%")) +
    labs(title = "Voluntary post-tax contributions are made to high-balance accounts",
         subtitle = "Share of taxpayers and post-tax contributions, by existing superannuation balance",
         caption = p0("Notes: ",
                      "Projections to 2020-21 assume 2% wage growth and 0% ",
                      "growth in the number of taxfilers from 2019-20 to 2020-21. ",
                      "Post-tax contributions equals personal contributions less ",
                      "non-employer super contributions.",
                      "",
                      "\n",
                      "Source: ATO 2017-18 2% sample file"))
})

# Figure 6.1
# Superannuation earnings by 60+ year old, 2015-16
grattan_save_all(provide.file("Figure-6-1/Figure61.pdf"), {
  s2021 %>%
    filter(age_range <= 2) %>%
    mutate(earnings_if_taxed = MCS_Ttl_Acnt_Bal * 0.05 * (1 - 0.09),
           earnings_tax_concession = MCS_Ttl_Acnt_Bal * 0.05 * 0.09) %>%
    mutate(Total_Income = Tot_inc_amt + MCS_Ttl_Acnt_Bal * 0.05,
           Total_Income_Decile = factor(weighted_ntile(Total_Income, n = 10L))) %>%
    group_by(Total_Income_Decile) %>%
    summarise(avgSuperEarnings_if_taxed = mean(earnings_if_taxed),
              avgEarningsConcession = mean(earnings_tax_concession)) %>%
    pivot_longer(grep("^avg", names(.), value = TRUE)) %>%
    mutate(name = if_else(name %ein% "avgEarningsConcession",
                          "Earnings concession",
                          "Earnings if taxed")) %>%
    ggplot(aes(x = Total_Income_Decile,
               y = value,
               fill = name)) +
    geom_col() +
    grattan_fill_manual(n = 2, palette = "dark") +
    theme_grattan() +
    theme(legend.position = c(0, 1),
          legend.justification = c(0, 1),
          legend.direction = "horizontal") +
    grattan_y_continuous(label = grattan_dollar) +
    labs(subtitle = "Superannuation earnings per 60+ year old, 2020-21",
         x = "Total income decile",
         caption = p0("Notes: ",
                      "Projections to 2020-21 assume 2% wage growth and 0% ",
                      "growth in the number of taxfilers from 2019-20 to 2020-21.",
                      "Earnings if taxed equals 5% of balance multiplied by 91% ",
                      "(i.e. assuming a 5% rate of return and a 9% average effective ",
                      "tax rate on earnings) ",
                      "Earnings concession equals 5% of 9% of the balance",
                      "for symmetric reasons.",
                      "\n",
                      "Source: ATO 2017-18 2% sample file."))

})


# Figure 6.2
# Average additional tax paid by 60+ year olds under reform proposals, by
# total income decile (including super earnings), $2015-16
grattan_save_all(provide.file("Figure-6-2/Figure62.pdf"), {
  s2021 %>%
    .[, MarginalRate := grattan:::marginal_rate(.SD, fy.year = "2020-21")] %>%
    mutate(tax = income_tax(Taxable_Income, "2020-21", .dots.ATO = .)) %>%
    as_tibble %>%
    # select()s hereinafter are just for debugging (to view files)
    select(age_range, Partner_status,
           MCS_Ttl_Acnt_Bal,
           MarginalRate,
           Taxable_Income, Tot_inc_amt, WEIGHT, tax) %>%
    filter(age_range <= 2) %>%

    # tax free threshold assumed to depend only on the following:
    group_by(age_range, Partner_status) %>%

    # Use 'most common taxable income of people paying small but nozero tax
    # as proxy for true taxfree threshold
    mutate(taxfree_threshold = Mode(Taxable_Income[tax %between% c(1, 100)]),
           # This is the amount available if earnings were taxed at 15%
           minTaxable_Income_blw_15pc = min(Taxable_Income[MarginalRate > 0.125])) %>%
    ungroup %>%
    mutate(unused_taxfree_threshold = pmax0(taxfree_threshold - Taxable_Income),
           unused_income_blw_15pc = pmax0(minTaxable_Income_blw_15pc - Taxable_Income)) %>%
    mutate(earnings = 0.05 * MCS_Ttl_Acnt_Bal,
           earnings_tax = 0.125 * earnings,

           # Allow earnings to be transferred to Taxable Income only to the point
           # minTaxable_Income_blw_15pc -- any more and the personal income tax
           # paid would exceed the benefit of behaviour change
           # 'transfer from super earnings to personal income tax'

           earnings_w_behaviour_change = pmax0(earnings - unused_income_blw_15pc),
           earnings_tax_w_behaviour_change = 0.125 * earnings_w_behaviour_change,
           Taxable_Income_w_behaviour_change = Taxable_Income + pminV(earnings, unused_income_blw_15pc),

           # Same thing except using earnings - 20k as the earnings at risk
           earnings_abv_20k = pmax0(earnings - 20e3),
           earnings_tax_abv_20k = 0.125 * earnings_abv_20k,
           earnings_w_behaviour_change_20k_threshold = pmax0(earnings_abv_20k - unused_income_blw_15pc),
           earnings_tax_w_behaviour_change_20k_threshold = 0.125 * earnings_w_behaviour_change_20k_threshold,
           Taxable_Income_w_behaviour_change_20k_threshold = Taxable_Income + pminV(earnings_abv_20k, unused_income_blw_15pc),


           TotalIncomePlusEarnings = Tot_inc_amt + earnings,
           TotalIncomePlusEarningsDecile = weighted_ntile(TotalIncomePlusEarnings, n = 10L)) %>%
    mutate(tax_behaviour_change = income_tax(Taxable_Income_w_behaviour_change,
                                             "2020-21",
                                             .dots.ATO = s2021[age_range <= 2]),
           tax_behaviour_change_20k_threshold = income_tax(Taxable_Income_w_behaviour_change_20k_threshold,
                                                           "2020-21",
                                                           .dots.ATO = s2021[age_range <= 2])) %>%
    group_by(TotalIncomePlusEarningsDecile) %>%
    summarise(avg_extra_tax_ante_behaviour_change = mean(earnings_tax),
              avg_extra_tax_post_behaviour_change = mean(tax_behaviour_change - tax + earnings_tax_w_behaviour_change),
              avg_extra_tax_over20k = mean(earnings_tax_abv_20k),
              avg_extra_tax_over20k_post_behaviour_change = mean(tax_behaviour_change_20k_threshold - tax + earnings_tax_w_behaviour_change_20k_threshold)) %>%
    pivot_longer(grep("^avg_", names(.), value = TRUE)) %>%
    mutate(Decile = factor(TotalIncomePlusEarningsDecile),
           name = trim_common_affixes(name),
           name = Switch(name,
                         "ante_behaviour_change" = "15% tax on super earnings",
                         "post_behaviour_change" = "15% tax on super earnings after behaviour change",
                         "over20k" = "15% tax on super earnings over $20,000",
                         "over20k_post_behaviour_change" = "15% tax on super earnings over $20,000 after behaviour change",
                         DEFAULT = ""),
           name = forcats::fct_inorder(name)) %>%
    ggplot(aes(x = Decile, y = value, fill = name)) +
    geom_col(position = position_dodge()) +
    scale_y_continuous_grattan(labels = grattan_dollar, breaks = c(0, 5e3, 10e3)) +
    theme_grattan() +
    theme(legend.position = c(0, 1),
          legend.justification = c(0, 1),
          legend.direction = "vertical") +
    grattan_fill_manual(n = 4, palette = "dark") +
    labs(title = "A tax on earnings in retirement would mostly affect those with higher incomes",
         subtitle = "Average additional tax paid by 60+ year olds under reform proposals, by total income decile (including super earnings), 2020-21",
         caption = p0("Note: Earnings estimated as 5% of super balances. ",
                      "Effective earnings tax assumed to be 12.5%. ",
                      "Behavioural response assumed to be that individuals whose ",
                      "taxable income is below either the tax-free threshold or ",
                      "the taxable income at which most people's marginal tax rate ",
                      "exceeds 12.5% would transfer as much super earnings into ",
                      "their taxable income as would reduce their tax. ",
                      "The effective tax-free threshold is the most common taxable income ",
                      "in which people of that age and partner status pay between $1 and $100 ",
                      "income tax. The threshold for the 12.5% marginal rate is the minimum ",
                      "taxable income at which the marginal tax rate is greater than 12.5% for ",
                      "that age and partner status. ",
                      "\n",
                      "Source: ATO 2017-18 2% sample file."))
})


# Figure BSS-4
# Projected number of individuals in 2019-20 making pre-tax contributions of more
# than $25,000
grattan_save_all("Figure-BSS-4/Figure-BSS-4.pdf", {
  s2021 %>%
    select(Gender, Taxable_Income, concessional_contributions, WEIGHT) %>%
    mutate_ntile(Taxable_Income, n = 10L) %>%
    as_tibble %>%
    filter(concessional_contributions > 11e3) %>%
    group_by(Gender, Taxable_IncomeDecile) %>%
    summarise(nTaxfilers = sum(WEIGHT)) %>%
    mutate(Sex = if_else(Gender == 0, "Male", "Female"),
           Taxable_IncomeDecile = factor(Taxable_IncomeDecile))  %>%
    ggplot(aes(x = Taxable_IncomeDecile, y = nTaxfilers, fill = Sex)) +
    geom_col() +
    theme_grattan() +
    grattan_fill_manual(n = 2, palette = "dark") +
    grattan_y_continuous(labels = comma) +
    facet_wrap(~Sex)
})


# Breakdown of CIT v pre-2015 v proposed
grattan_save_all("Figure-concessional-breakdown/Figure-concessional-breakdown.pdf", {
  list("2015-16 reforms" = {
    s1718 %>%
      project(h = 3L,
              lf.series = 0,
              wage.series = 0.02) %>%
      model_new_caps_and_div293(fy.year = "2020-21",
                                prv_cap = 30e3,
                                prv_cap2 = 35e3,
                                prv_age_based_cap = TRUE,
                                prv_cap2_age = 49,
                                prv_div293_threshold = 300e3,

                                new_cap = 25e3,
                                new_age_based_cap = FALSE,
                                new_div293_threshold = 250e3) %>%
      mutate_ntile("Taxable_Income", n = 10L) %>%
      as_tibble %>%
      mutate(tax_breaks_value = new_revenue - prv_revenue) %>%
      group_by(Taxable_IncomeDecile) %>%
      summarise(ContributionsTaxBreak_bn = sum(tax_breaks_value * WEIGHT) / 1e9)
  },
  "Proposed" = {
    s1718 %>%
      project(h = 3L,
              lf.series = 0,
              wage.series = 0.02) %>%
      model_new_caps_and_div293(fy.year = "2020-21",
                                prv_cap = 25e3,
                                prv_age_based_cap = FALSE,
                                prv_div293_threshold = 250e3,

                                new_cap = 11e3,
                                new_age_based_cap = FALSE,
                                new_div293_threshold = 200e3) %>%
      mutate_ntile("Taxable_Income", n = 10L) %>%
      as_tibble %>%
      mutate(tax_breaks_value = new_revenue - prv_revenue) %>%
      group_by(Taxable_IncomeDecile) %>%
      summarise(ContributionsTaxBreak_bn = sum(tax_breaks_value * WEIGHT) / 1e9)
  },
  "Remaining concessions" = {
    s1718 %>%
      project(h = 3L,
              lf.series = 0,
              wage.series = 0.02) %>%
      model_new_caps_and_div293(fy.year = "2020-21",
                                prv_cap = 11e3,
                                prv_age_based_cap = FALSE,
                                prv_div293_threshold = 200e3,

                                new_cap = 0,
                                new_age_based_cap = FALSE,
                                new_contr_tax = "mr - 0%",
                                new_div293_threshold = Inf) %>%
      mutate_ntile("Taxable_Income", n = 10L) %>%
      as_tibble %>%
      mutate(tax_breaks_value = new_revenue - prv_revenue) %>%
      group_by(Taxable_IncomeDecile) %>%
      summarise(ContributionsTaxBreak_bn = sum(tax_breaks_value * WEIGHT) / 1e9)
  }) %>%
    rbindlist(idcol = "Policy") %>%
    mutate(Taxable_IncomeDecile = factor(Taxable_IncomeDecile),
           Policy = forcats::fct_inorder(Policy)) %>%
    ggplot(aes(x = Taxable_IncomeDecile,
               y = ContributionsTaxBreak_bn,
               fill = Policy)) +
    geom_col() +
    theme_grattan() +
    grattan_fill_manual(n = 5, palette = "dark") +
    theme(legend.position = c(0, 1),
          legend.justification = c(0, 1),
          legend.direction = "vertical") +
    labs(x = "Taxable income decile",
         subtitle = "Value of tax concessions ($bn, 2020-21)",
         caption = "Note: Comprehensive income tax benchmark. \nSource: ATO 2017-18 2% sample file.")
})


revenue_from_bal_cap <- function(balance_cap,
                                 sample_file = c("s2021_via_1718", "s2021_via_1819"),
                                 apra_concord = c("none", "balance", "weight"),
                                 r_earnings_retirement = 0.05,
                                 r_earnings_accumulation = 0.07,
                                 effective_tax_on_earnings = 0.10,
                                 p_excess_earnings_cgt = 0.8) {
  apra_concord <- match.arg(apra_concord)
  sample_file <- match.arg(sample_file)
  with(copy(s2021), {

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
    old_earnings_tax <- (age_range > 1) * effective_tax_on_earnings * old_earnings
    old_net_earnings_post_tax <- old_earnings - old_earnings_tax
    new_earnings <- r_earnings * pminC(MCS_Ttl_Acnt_Bal, balance_cap)
    new_earnings_tax <- (age_range > 1) * effective_tax_on_earnings * new_earnings
    extra_taxable_income <- r_earnings * pmax0(MCS_Ttl_Acnt_Bal - balance_cap)
    extra_taxable_income <-
      p_excess_earnings_cgt * extra_taxable_income * 0.5 +  # CGT
      (1 - p_excess_earnings_cgt) * extra_taxable_income
    #
    old_earnings2021 <- sum(old_net_earnings_post_tax) * wt
    avg_growth <- mean(r_earnings)
    old_earnings1819 <- (old_earnings2021) / (avg_growth ^ 2)
    net_old_earnings1819 <- sum(old_net_earnings_post_tax) * wt


    NewTaxableIncome <- extra_taxable_income + Taxable_Income

    new_tax <- income_tax(NewTaxableIncome, "2020-21", .dots.ATO = s2021)
    old_tax <- income_tax(Taxable_Income, "2020-21", .dots.ATO = s2021)
    new_earnings_tax <- (age_range > 1) * effective_tax_on_earnings * new_earnings
    old_earnings_tax <- (age_range > 1) * effective_tax_on_earnings * old_earnings

    delta <- new_tax - old_tax + new_earnings_tax - old_earnings_tax

    delta
  })
}

revenue_from_xfer_bal_cap <- function(new_balance_cap,
                                      prv_balance_cap = 1.6e6,
                                      sample_file = c("s2021_via_1718", "s2021_via_1819"),
                                      apra_concord = c("none", "balance", "weight"),
                                      r_earnings_retirement = 0.05,
                                      r_earnings_accumulation = 0.07,
                                      effective_tax_on_earnings = 0.1) {
  apra_concord <- match.arg(apra_concord)
  sample_file <- match.arg(sample_file)
  s2021 <- get(sample_file, mode = "list")
  # Only care about over 60s for the difference
  s2021 <- s2021[age_range <= 2]

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
    earnings <- MCS_Ttl_Acnt_Bal * r_earnings


    # Calculate earnings from excess amount
    prv_abv_cap <- pmax0(MCS_Ttl_Acnt_Bal - prv_balance_cap) * r_earnings
    new_abv_cap <- pmax0(MCS_Ttl_Acnt_Bal - new_balance_cap) * r_earnings

    # Excess earnings tax
    prv_earnings_tax <- effective_tax_on_earnings * prv_abv_cap
    new_earnings_tax <- effective_tax_on_earnings * new_abv_cap


    new_earnings_tax - prv_earnings_tax
  })
}









