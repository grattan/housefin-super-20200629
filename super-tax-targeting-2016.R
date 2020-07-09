
# ======================================================================================================== #
# Super model for Budget Repair report ============================================================================ #
# ======================================================================================================== #

# This script is for analyzing current policy settings and reform options for the tax treatment of super earnings. We gather super balances, incomes etc. from the SIH 11-12 person-level dataset, inflate everything forward to 2015-16, and use these to project super earnings on those balances. We then apply current policy setting for super earnings, generate benchmark tax treatments where: (a) all taxpayers pay 15% tax on super earnings regardless of age; and (b) super earnings are incorporated into taxable income and taxed at personal marginal tax rates (this is how Treasury's tax expenditures are done). We then consider the various reform options, generate costings and charts that are used in the super chapter of the Budget Repair report

# However, as always more can be done to refine this analysis. The below to do list reports various refinements and whether they have been done.

# NEXT STEPS -

# (1) We should inflate all our variables forward to 2015-16, including super balances- DONE for inflation, but we could grow income forward by nominal wages (eg. use the ABS wage price index for this)
# (2) Check our estimate for super earnings - standard benchmark used in Treasury and the OECD appears to be 7 per cent after fees (but before tax)
# (3) Adjust tax rates for super earnings to account for (a) 10% CGT for super; and (b) dividend imputation. Estimates from Mercer suggest an effective tax rate of around 8-9 per cent is reasonable
# (4) Generate a more nuanced taxable income variable (i.e. better than SIH total income) to use when estimating value of super earnings tax concession against personal income tax benchmark
# (5) Should we be substracting any super income streams from our total income (including super earnings)? If we're including super earnings in income, then we shouldnt be including drawdowns on super as income at the same time to avoid double counting
# (6) Better define those in the drawdown phase as those aged over 60 drawing an income stream from a super fund - DONE
# (7) Introducing a different earnings rate in the accumulation and drawdown phases - DONE
# (8) Introducing a different effective tax rate on earnings between the accumulation and drawdown phases - DONE
# (9) Write in Medicare Levy function, differentiated by Age (DONE) and family status (DONE)
# (10) Write in SAPTO for singles and couples (DONE) and allow SAPTO entitlements to be transferred among couples (NOT DONE)
# (11) Write in LITO function (DONE)
# (12) Account for behavioural change as retirees shift funds out of super to minimise tax by making the most of their TF threshold, SAPTO and LITO entitlements (DONE)
# (12) Better map ABS total incomes to ATO taxable incomes. One approach would be to estimate our estimated tax on individuals, and compare this to the ABS' imputed estimated. If all else fails we might have to use HILDA instead if the data there is more reliable / detailed, especially on the tax estimates.

# ======================================================================================================== #
# Loading required packages ============================================================================ #
# ======================================================================================================== #

# The survey of income and housing variable list can be found at the ABS website
# http://www.abs.gov.au/AUSSTATS/abs@.nsf/DetailsPage/6541.0.30.0012011-12?OpenDocument
# Basic cross tabs are also here and can be used for QC

rm(list=ls())

# Load packages

# install.packages("foreign")
# install.packages("dplyr")
# install.packages("survey")
# install.packages("Hmisc")
# install.packages("car")
# install.packages("gam")
# install.packages("lattice")
# install.packages("ggplot2")
# install.packages("magrittr")

library(foreign) # for reading .dta files
library(dplyr) # for data manipulation
library(survey) # For survey weighted cross tabs
library(Hmisc) # for variable manipulation
library(car)
library(gam)
library(lattice)
library(ggplot2)
library(magrittr)

# ========================================================================================================
# Pulling in the data
# ======================================================================================================== #

# I know we should we using relative file paths but this will work for now
setwd("~/Desktop/Budget Policy/Basic exploration and setup")

person.df <- read.dta("sih11bp.dta")

# List of variables we need for the analysis
key.vars <- c("HHPOS", # Position in the household (publication definition)
              "AGEBC", # Age of person
              "SEXP", # Sex
              "LFSCP", # LFS
              "FAMTYPE",
              "ABSHID",
              "VSUPGCP",
              "VSUPNCP",
              "SIHPSWT",
              "ISSSCP",
              "IWSUCP",
              "IAGECP",
              "INCTSCP8",
              "ITAXCP8",
              "ITGCBCP8",
              "ISUPERCP",
              "INSSCP", # Current weekly benefit from employer provided superannuation (non salary sacrifice)
              "ISSSCP", # Current weekly employee income salary sacrificed for superannuation
              "I40SUP") # Personal irregular receipts from superannuation payments over last 2 years

# Renaming the variables for ease of use
names(key.vars) <- c("HH.pos",
                     "Age",
                     "Sex",
                     "LFS",
                     "Fam.type",
                     "HHID",
                     "Gov.super",
                     "Non.gov.super",
                     "Weights",
                     "Sacrified.income.super",
                     "Employee.income.weekly",
                     "Age.pension",
                     "Total.income",
                     "Tax",
                     "Tot.income.gov",
                     "Super.income",
                     "Super.cont.non.ss",
                     "Super.cont.ss",
                     "Super.lump.sum")


person.dfkv %>% filter(Age>=27) %>%
group_by(Age.pension = 0)) %>%
  summarise(count = sum(Weights))


# Subset the data frame keeping on the key variables
person.dfkv <- person.df[,key.vars]
colnames(person.dfkv) <- names(key.vars)

# ======================================================================================================== #
# Create new variables         ===========
# ======================================================================================================== #


# Create personID
person.dfkv$PID <- 1:nrow(person.dfkv)

# Create total super balance variable
person.dfkv$Total.super <- person.dfkv$Gov.super + person.dfkv$Non.gov.super

# Create own disposable income series (total less estimated taxes)
person.dfkv$Income.disp <- person.dfkv$Total.income - person.dfkv$Tax

# We also create our own definition of taxable income, which is total income less super income streams which are not taxed under the PIT
# We take off super income streams which are tax free, since they are not really income but rather a drawdown of existing assets.

person.dfkv$Taxable.income <- person.dfkv$Total.income - person.dfkv$Super.income
person.dfkv$Total.income <- person.dfkv$Total.income - person.dfkv$Super.income

# We also create a new income variable that incomes super withdrawals (but excludes super earnings)
person.dfkv$Total.income.inc.wdls <- person.dfkv$Total.income + person.dfkv$Super.income

person.dfkv$Taxable.income <- ifelse(person.dfkv$Taxable.income < 0, 0, person.dfkv$Taxable.income)

# We also need to annualise total income, so that we can pass a PIT function over it to work out how much tax they would pay

person.dfkv$Total.income.annual <- person.dfkv$Total.income * 52
person.dfkv$Taxable.income.annual <- person.dfkv$Taxable.income * 52
person.dfkv$Total.income.inc.wdls.annual <- person.dfkv$Total.income.inc.wdls * 52

# We create a new variable that adds in an estimate of the earnings from superannuation fund balances, which are not captured in the SIH 11-12. What the SIH includes on super is the value of any benefits paid  from a superannuation fund, such as a pension or a lump sum withdrawals.

# We've also assumed that all withdrawals from super (pension or lump sum) are not included in estimates of total income. This isa comprehensive income tax base so withdrawals are not included - same as withdrawing $$$ from a bank account isnt counted as income

# ======================================================================================================== #
# Set up price changes and tax rates         ===========
# ======================================================================================================== #

# Some basic calculations to adjust for inflation

# We want to cost this for the 2015-16 budget
# For now I'll just inflate at the inflation rate - in the future it may be better to inflate  variables by 10 year averages of inflation and wages, depending on the variable

# We dont inflate super balances at this stage since we adjust them separately below. This is because (a) balances compound over time at a rate faster than inflation - ore $$$ are being added to the system each year in contributions and investment earnings, than are being taken out via drawdowns; and (b) the survey data is likely to underreport aggregate super balances

# Assume 2.5 per cent inflation rate

# Prices are now in 2015-16 terms since that's the budget we're looking at
years = 4
inflation.f <- 1.025^years

# List of variables that do not need to be adjusted
no.inflation <- c("HH.pos","Age","Sex","LFS","HHID","Weights", "PID", "Fam.type", "Total.super", "Gov.super", "Non.gov.super")

# Inflation adjusting the data frame (inflating all not selected as no inflation variables)
person.dfkvi <- cbind(person.dfkv[,no.inflation], (person.dfkv[,!names(person.dfkv) %in% no.inflation])*inflation.f)

#===================================================================================
# Grow forward super account balances


# We have total super balances, before inflating, of $1.14 trillion

sum(person.dfkvi$Total.super * person.dfkvi$Weights) / 10^9

sum(person.dfkvi$Gov.super * person.dfkvi$Weights) / 10^9 # $300 billion
sum(person.dfkvi$Non.gov.super * person.dfkvi$Weights) / 10^9 # $840 billion

# By comparison, APRA reports total funds under management of $2 trillion as of end June 2015
# http://www.apra.gov.au/Super/Publications/Documents/1508-QSP-June2015.pdf

# So we inflate forward our super account balances by a factor of 2/1.14. This way our weighted survey sample has total account balances equal to the aggregate account balances nationally, and we assume that the distribution of super balances across our survey sample holds across the whole population

# Let x be our growth factor
x <- 2/1.14

# Inflating super balances by x
person.dfkvi$Gov.super <- person.dfkvi$Gov.super * x
person.dfkvi$Non.gov.super <- person.dfkvi$Non.gov.super * x
person.dfkvi$Total.super <- person.dfkvi$Total.super * x

sum(person.dfkvi$Total.super * person.dfkvi$Weights) / 10^9 # $2 trillion in total super balances


#===================================================================================
# Investigating actual distribution of super balances - using uninflated dataset - No need to QC
#===================================================================================#

# This recreates some of the ASFA work on those with very high account balances. We use some of these numbers in the chapter, and in various op eds and such

# Number of people with balances of over $1 million in 2011-12 is 110,000, or 0.6% of individuals, with total funds of $181 billion

person.dfkv %>% group_by(Total.super<10^6) %>%
  summarise(No.individuals = sum(Weights),
            Total.funds = sum(Total.super * Weights) / 10^9) %>%
  ungroup %>%
  mutate(Prop.individuals = No.individuals / sum(No.individuals) * 100)

# Inflating forward, we estimate that 130,000 would now have balances of more than $1 million, or 0.7% of individuals, with total funds of $220 billion

person.dfkvi %>% group_by(Total.super<10^6) %>%
  summarise(No.individuals = sum(Weights),
            Total.funds = sum(Total.super * Weights) / 10^9)  %>%
  ungroup %>%
  mutate(Prop.individuals = No.individuals / sum(No.individuals) * 100)

# Number of people with balances over $2.5 million is 13,480, with total funds of $50 billion

person.dfkv %>% group_by(Total.super < 2500000) %>%
  summarise(No.individuals = sum(Weights),
            Total.funds = sum(Total.super * Weights) / 10^9) %>%
  ungroup %>%
  mutate(Prop.individuals = No.individuals / sum(No.individuals) * 100)

# Inflating forward, we estimate that around 16,000 would now have balances of more than $2.5 million, with total balances of $61.6 billion

person.dfkvi %>% group_by(Total.super < 2500000) %>%
  summarise(No.individuals = sum(Weights),
            Total.funds = sum(Total.super * Weights) / 10^9) %>%
  ungroup %>%
  mutate(Prop.individuals = No.individuals / sum(No.individuals) * 100)

# Number of people with a super account of some value is 12.5 million

person.dfkv %>% group_by(Total.super>0) %>%
  summarise(No.individuals = sum(Weights),
            Total.funds = sum(Total.super * Weights) / 10^9) %>%
  ungroup %>%
  mutate(Prop.individuals = No.individuals / sum(No.individuals) * 100)

# ===================================================================================
# Adding in extra tax policy variables
# ==================================================================================#

# We add these in afterwards as they are from 2014-15, so we dont want to inflate them up from 2012-13

# We map a new variable for individuals based on whether they are (a) indiv; or (b) couple

person.dfkvi$Fam.type.numeric <- as.numeric(person.dfkvi$Fam.type)
person.dfkvi$Single <- ifelse(person.dfkvi$Fam.type.numeric > 9 | person.dfkvi$Fam.type.numeric < 2, 1, 0)

# ======
# Medicare Levy

# We set the Medicare Levy phase-in thresholds for individuals, retirees and families (we prob wont use this last one as it's more complicated to implement, but feasible in SIH as we know how many kids they have)

# Need to check what year this are from, to make sure we're indexing them in the right way. At present we're indexing them as if they are the thresholds in 2012-13.

# Individuals
person.dfkvi$ML.lower <- rep(20896*1.025,length(person.dfkvi$Age))
person.dfkvi$ML.upper <- rep(26120*1.025,length(person.dfkvi$Age))
person.dfkvi$ML.lower.senior <- rep(33044*1.025,length(person.dfkvi$Age))
person.dfkvi$ML.upper.senior <- rep(41305*1.025,length(person.dfkvi$Age))

# Families - these thresholds are for family taxable income for the couple
# person.dfkvi$ML.lower.cpl <- rep(20896,length(person.dfkvi$Age))
# person.dfkvi$ML.upper.cpl <- rep(44076,length(person.dfkvi$Age))
# person.dfkvi$ML.lower.cpl.senior <- rep(33044,length(person.dfkvi$Age))
# person.dfkvi$ML.upper.cpl.senior <- rep(57500,length(person.dfkvi$Age))
# person.dfkvi$ML.per.child <- rep(3238,length(person.dfkvi$Age))

# ======
# SAPTO

# We add in the income thresholds and maximum offsets for SAPTO - we only use the individual thresholds for now. These are the figures for 2014-15. I dont think these thresholds are indexed - they've stayed the same over 2012-13, 2013-14 and 2014-15 - so we use the same ones in 2015-16

person.dfkvi$SAPTO.lower.indiv <- rep(32279, length(person.dfkvi$Age))
person.dfkvi$SAPTO.upper.indiv <- rep(50119, length(person.dfkvi$Age))
person.dfkvi$SAPTO.max.indiv <- rep(2230, length(person.dfkvi$Age))
person.dfkvi$SAPTO.lower.cpl <- rep(57948, length(person.dfkvi$Age))
person.dfkvi$SAPTO.upper.cpl <- rep(83580, length(person.dfkvi$Age))
person.dfkvi$SAPTO.max.cpl <- rep(1602, length(person.dfkvi$Age))

# ======
# LITO - income thresholds are unindexed

person.dfkvi$LITO.lower <- rep(37000, length(person.dfkvi$Age))
person.dfkvi$LITO.upper <- rep(66666, length(person.dfkvi$Age))
person.dfkvi$LITO.max <- rep(445, length(person.dfkvi$Age))


# ==================================================================================
# Subsetting for those aged over 60 in the drawdown phase
# =================================================================================#

# Where Age >= 22 we know they are over 60
# Where Super.income > 0, we know they are in the drawdown phase

# We start by defining a 'drawdown' variable, where we need Age as a numeric variable to filter on it
person.dfkvi$Age.numeric <- as.numeric(person.dfkvi$Age)
person.dfkvi$Super.ddown <- ifelse(person.dfkvi$Age.numeric < 22, 0, ifelse(person.dfkvi$Super.income > 0, 1, 0))

# Now we see how many of those that are over 60, and have a super balance, are in the drawdown phase

person.dfkvi %>% filter(Age.numeric>21) %>%
  group_by(Super.ddown) %>%
  summarise(total.super = sum((Total.super * Weights) / 10^9),
            count.persons = sum(Weights))

# So roughly 70 per cent of the super assets of over 60s are in the drawdown phase. So it's important that we distinguish between super assets of over 60s that are in the drawdown phase, and those that are not.

# Now we generate out drawdown subset which we use for our earnings analysis

person.dfkvi.ddown <- person.dfkvi %>% filter(Age.numeric>21, Super.ddown == 1)

#===================================================================================
# Investigating those drawing down on super - using inflated dataset - No need to QC
#===================================================================================#

# Number of people with balances of over $1 million in 2011-12 is 67,000, or 6.8% of those in the drawdown phase, with total funds of $113 billion

person.dfkvi.ddown %>% group_by(Total.super<1000000) %>%
  summarise(No.individuals = sum(Weights),
            Total.funds = sum(Total.super * Weights) / 10^9)  %>%
  ungroup %>%
  mutate(Prop.individuals = No.individuals / sum(No.individuals) * 100)

# Number of people with balances over $2.5 million is 7,125, or 0.7 per cent of those in the drawdown phase, with total funds of $28 billion

person.dfkvi.ddown %>% group_by(Total.super < 2500000) %>%
  summarise(No.individuals = sum(Weights),
            Total.funds = sum(Total.super * Weights) / 10^9) %>%
  ungroup %>%
  mutate(Prop.individuals = No.individuals / sum(No.individuals) * 100)

# =======================================================================================================
# Estimate super earnings
# ======================================================================================================== #

# Set super rate of return

# The rate of return on super account balances will vary between the accumulation and drawdown phases. Retirees in the drawdown phase tend to be more conservative in their investment decisions, with a greater weighting towards safer, lower yielding assets such as bank deposits

# The 10-year average post-tax return for all super funds in the APRA Superannuation Bulletin is 6 per cent. This figure is net of fees and taxes on earnings, and therefore also includes any franking credits on investments in Australian equities or trusts. (http://www.apra.gov.au/Super/Publications/Documents/Revised%202013%20Annual%20Superannuation%20Bulletin%2005-02-14.pdf)

# With an effective tax rate on super earnings bounded by the tax rate on capital gains (10%) and the tax rate on other investment income for the super fund (15%), it's reasonable to suggest a pre-tax rate of return on super fund assets of 7% in the accumulation phase

# In the drawdown phase we use a pre-tax rate of return of 5%. The 5% return in the drawdown phase comes from PBO advice to Senator David L on a similar costing that has been published onthr DLP's facebook page (no kidding...) - https://www.facebook.com/LDP.australia/posts/10152975190247672

Super.return.rate.acc <- 0.07 # 7 per cent return in the accumulation phase
Super.return.rate.ddown <- 0.05 # 5 per cent return in the drawdown phase

# So super earnings will depend upon whether super account holders are in the accumulation or drawdown phases

person.dfkvi$Super.earnings <- ifelse(person.dfkvi$Super.ddown == 1, Super.return.rate.ddown*person.dfkvi$Total.super, Super.return.rate.acc*person.dfkvi$Total.super)
summary(person.dfkvi$Super.earnings)

sum(person.dfkvi$Total.super * person.dfkvi$Weights) / 10^9 # We estimate total super balances of $2 trillion
sum(person.dfkvi$Super.earnings * person.dfkvi$Weights) / 10^9 # With annual super earnings of $130 billion

# Funds reported investment earnings of $137 billion in 2013-14, and $117 billion in 2014-15. So our numbers probably stack up okay.

# Finally, we write two new identities for taxable and total income that account for super earnings in the member's super fund. We use these when we include each members super fund earnings in taxable income to do the costing

person.dfkvi$Total.income.annual.s <- person.dfkvi$Total.income.annual + person.dfkvi$Super.earnings
person.dfkvi$Taxable.income.annual.s <- person.dfkvi$Taxable.income.annual + person.dfkvi$Super.earnings

# =======================================================================================================
# Effective tax rate on super earnings
# ======================================================================================================#

# Here we apply an effective tax rate on super earnings, where those earnings are net of fees. Since franking credits are included in super fund income, these franking credits are taxed in the fund. Therefore the effective tax rate on super earnings should be bounded by the tax rate on capital gains (10%) and the tax rate on other investment income for the super fund (15%).

# For now, we assume a 50:50 split in super fund earnings between capital gains and other investment earnings. This leads to an effective tax rate on super earnings of 12.5%. We will structure the analysis to allow different effective tax rates on super earnings between the accumulation and drawdown phases, as those in the drawdown phase tend to invest in less risky, income earning assets with fewer or no capital gains (i.e. bank deposits)

# Set assumed tax rate - we use 8% currently but depends on the answers to the above
tax.rate.acc <- 0.125 # For the accumulation phase
tax.rate.ddown <- 0.14 # For the drawdown phase

person.dfkvi$super.earnings.tax <- ifelse(person.dfkvi$Super.ddown == 1, person.dfkvi$Super.earnings*tax.rate.ddown, person.dfkvi$Super.earnings*tax.rate.acc)

person.dfkvi %>% group_by(Super.ddown == 0) %>%
  summarise(total.assets = weighted.mean(x = Total.super, w = Weights))

# ==================================================================================
# Setting up our personal income tax functions
# =================================================================================#

# Now we write a quick tax function - we include the Medicare Levy, SAPTO and LITO

tax.function <- function(income){

  tax <- ifelse(income<18200, 0,
                ifelse(income<37000, (income-18200)*0.19,
                       ifelse(income<80000, 3572 + (income-37000)*0.325,
                              ifelse(income<180000, 17547 + (income-80000)*0.37, 54547 + 0.45*(income-180000)))))
  tax
}




# We also write a function for the Medicare Levy

ML.function <- function(income,
                        ML.lower,
                        ML.upper,
                        ML.lower.senior,
                        ML.upper.senior,
                        Age.numeric){

  Medicare.levy <- ifelse(Age.numeric < 27,ifelse(income < ML.lower, 0,
                                                  ifelse(income < ML.upper, (income - ML.lower) * 0.1, 0.02 * income)),
                          ifelse(income<ML.lower.senior, 0,
                                 ifelse(income < ML.upper.senior, (income - ML.lower.senior) * 0.1, 0.02 * income)))

  Medicare.levy

}

# And a function for SAPTO entitlement
# Here we distingush between the entitlements for individuals and couples, as well as by age. We don't however, currently allow couples to share their SAPTO entitlements between them (as can happen) and so we will underestimate the degree to which SAPTO reduces personal income tax paid at the aggregate. If we have time we'll add this in.

SAPTO.function <- function(income,
                           Age.numeric,
                           Single,
                           SAPTO.lower.indiv,
                           SAPTO.upper.indiv,
                           SAPTO.max.indiv,
                           SAPTO.lower.cpl,
                           SAPTO.upper.cpl,
                           SAPTO.max.cpl) {

  SAPTO <- ifelse(Age.numeric < 27,
                  0,
                  ifelse(Single == 1,
                         ifelse(income < SAPTO.lower.indiv,
                                SAPTO.max.indiv,
                                ifelse(income < SAPTO.upper.indiv,
                                       (SAPTO.upper.indiv - income) * 0.125,
                                       0)),
                         ifelse(income < SAPTO.lower.cpl / 2 ,
                                SAPTO.max.cpl,
                                ifelse(income < SAPTO.upper.cpl / 2,
                                       (SAPTO.upper.cpl / 2 - income) * 0.125,
                                       0))))

  SAPTO
}

# And a function for LITO entitlement

LITO.function <- function(income, LITO.lower, LITO.upper, LITO.max) {

  LITO <- ifelse(income < LITO.lower, LITO.max,
                 ifelse(income < LITO.upper, (LITO.upper - income) * 0.015, 0))
}

# We also define an 'unused' tax free threshold function, which we will pick up later when we consider behavioural change in response to the reintroduction of taxes on super earnings in the drawdown phase

TF.unused.function <- function(income) {

  TF.unused <- ifelse(income < 0, 18200,
                      ifelse(income<18200, (18200 - income), 0))
}

# ==================================================================================
# Checking our tax functions using a simulated dataset - no need to QC this
# =================================================================================#

# Before we write our various tax functions, we first set up a test module so that we can run a range of taxable incomes over them to make sure that they work property

Simulation.df <- data_frame("Taxable.income.annual" = seq(10000,100000, by = 5000),
                            "Age.numeric" = 28,
                            "Single" = 1, # Change this to simulate TF threshold for couples (0) or singles (1)
                            "SAPTO.lower.indiv" = 32279,
                            "SAPTO.upper.indiv" = 50119,
                            "SAPTO.max.indiv" = 2230,
                            "SAPTO.lower.cpl" = 57948,
                            "SAPTO.upper.cpl" = 83580,
                            "SAPTO.max.cpl" = 1602,
                            "ML.lower" = 21418.4,
                            "ML.upper" = 26773 ,
                            "ML.lower.senior" = 33870.1 ,
                            "ML.upper.senior" = 42337.62,
                            "LITO.lower" = 37000,
                            "LITO.upper" = 66666,
                            "LITO.max" = 445)

# Testing tax function

Simulation.df$Tax.liability <- tax.function(Simulation.df$Taxable.income.annual)

# Testing Medicare Levy

Simulation.df$Medicare.levy <- ML.function(Simulation.df$Taxable.income.annual,
                                           Simulation.df$ML.lower,
                                           Simulation.df$ML.upper,
                                           Simulation.df$ML.lower.senior,
                                           Simulation.df$ML.upper.senior,
                                           Simulation.df$Age.numeric)

# Summing tax liabilities (before offsets)

Simulation.df$Tax.liability.tot <- Simulation.df$Tax.liability + Simulation.df$Medicare.levy

# Testing SAPTO function

Simulation.df$SAPTO <- SAPTO.function(Simulation.df$Taxable.income.annual,
                                      Simulation.df$Age.numeric,
                                      Simulation.df$Single,
                                      Simulation.df$SAPTO.lower.indiv,
                                      Simulation.df$SAPTO.upper.indiv,
                                      Simulation.df$SAPTO.max.indiv,
                                      Simulation.df$SAPTO.lower.cpl,
                                      Simulation.df$SAPTO.upper.cpl,
                                      Simulation.df$SAPTO.max.cpl)

# Summing tax liabilities (including SAPTO offset)

Simulation.df$Tax.liability.tot <- ifelse(Simulation.df$Tax.liability.tot < Simulation.df$SAPTO, 0,
                                          Simulation.df$Tax.liability.tot - Simulation.df$SAPTO)

# Testing LITO function

Simulation.df$LITO <- LITO.function(Simulation.df$Taxable.income.annual,
                                    Simulation.df$LITO.lower,
                                    Simulation.df$LITO.upper,
                                    Simulation.df$LITO.max)

# Summing tax liabilities (including SAPTO and LITO offsets)

Simulation.df$Tax.liability.tot <- ifelse(Simulation.df$Tax.liability.tot < Simulation.df$LITO, 0,
                                          Simulation.df$Tax.liability.tot - Simulation.df$LITO)

# Testing unused TF threshold function

Simulation.df$Unused.TF <- TF.unused.function(Simulation.df$Taxable.income.annual)

# View(Simulation.df)

# We export the Simulation dataset to Excel and inspect visually to make sure it's all hunky dory

# The Excel file is:

# Macintosh HD:Users:bcoates:Dropbox:Grattan Dropbox:Budget Repair Report:Data and analysis:Superannuation:Spreadsheets:[Cross checking behavioural change module.xlsx]Sheet1

# Now that our simulation function works we can move onto using it for the whole ATO sample file

# ==================================================================================
# Establishing a no tax concession counterfactual for super earnings - PIT collected excluding super income
# =================================================================================#

# We estimate each person's tax liability

person.dfkvi$Tax.estimate <- tax.function(person.dfkvi$Taxable.income.annual)

sum(person.dfkvi$Tax.estimate * person.dfkvi$Weights)/10^9 # $187 billion

# We also estimate their Medicare Levy liability - this uses taxable income for the income test

person.dfkvi$Medicare.levy <- ML.function(person.dfkvi$Taxable.income.annual,
                                          person.dfkvi$ML.lower,
                                          person.dfkvi$ML.upper,
                                          person.dfkvi$ML.lower.senior,
                                          person.dfkvi$ML.upper.senior,
                                          person.dfkvi$Age.numeric)

person.dfkvi$Tax.estimate <- person.dfkvi$Tax.estimate + person.dfkvi$Medicare.levy

# Neither SAPTO or LITO are refundable tax offsets, so we only apply them to reduce tax if the individual actually has a tax liability up to this point

# LITO

person.dfkvi$LITO.entitlement <- LITO.function(person.dfkvi$Taxable.income.annual,
                                               person.dfkvi$LITO.lower,
                                               person.dfkvi$LITO.upper,
                                               person.dfkvi$LITO.max)

person.dfkvi$Tax.estimate <- ifelse(person.dfkvi$Tax.estimate < person.dfkvi$LITO.entitlement, 0,
                                    person.dfkvi$Tax.estimate - person.dfkvi$LITO.entitlement)


# SAPTO

# We need to distinguish for individuals based on age here, and family status (single vs couple) - DONE
# SAPTO eligibility uses an income definition known as 'rebate income' which is the sum of (a) Taxable Income; (b) Adjusted fringe Benefits; (c) Total net investment loss (financial net investment loss and rental property loss); and (d) reportable super contributions
# For now, we're just going to use taxable income since we dont know most of these things. As a result, we're going to be more generous in providing SAPTO entitlements to individuals than the ATO will be

person.dfkvi$SAPTO.entitlement <- SAPTO.function(person.dfkvi$Taxable.income.annual,
                                                 person.dfkvi$Age.numeric,
                                                 person.dfkvi$Single,
                                                 person.dfkvi$SAPTO.lower.indiv,
                                                 person.dfkvi$SAPTO.upper.indiv,
                                                 person.dfkvi$SAPTO.max.indiv,
                                                 person.dfkvi$SAPTO.lower.cpl,
                                                 person.dfkvi$SAPTO.upper.cpl,
                                                 person.dfkvi$SAPTO.max.cpl)

person.dfkvi$Tax.estimate <- ifelse(person.dfkvi$Tax.estimate < person.dfkvi$SAPTO.entitlement, 0,
                                    person.dfkvi$Tax.estimate - person.dfkvi$SAPTO.entitlement)


# Now we have our estimated tax entitlement, accounting for the Medicare Levy (individuals only), and SAPTO and LITO entitlements
# We compare this against the ABS-derived tax estimate in the SIH 11-12, which used the PIT tax scales for that year, including a 1.5% Medicare Levy

person.dfkvi$Tax.annual <- person.dfkvi$Tax * 52
(sum(person.dfkvi$Tax.estimate * person.dfkvi$Weights) - sum(person.dfkvi$Tax.annual * person.dfkvi$Weights)) / 10 ^ 9 # 55 billion difference
sum(person.dfkvi$Tax.annual * person.dfkvi$Weights) / 10^9 # ABS estimates PIT of $147 billion for 2011-12
sum(person.dfkvi$Tax.estimate * person.dfkvi$Weights) / 10^9 # We estimate PIT of $199 billion for 2015-16

# 2012-13 Taxation Statistics (inflated to 2015-16) has taxable income of $760 billion
# We have taxable income here of $887.7 billion

sum(person.dfkvi$Taxable.income.annual * person.dfkvi$Weights) / 10^9

# ==================================================================================
# Establishing a no tax concession counterfactual for super earnings - PIT collected including super earnings in taxable income
# =================================================================================#

# We also generate an estimate of the tax collected if super earnings were taxed as part of regular income

person.dfkvi$Tax.estimate.s <- tax.function(person.dfkvi$Taxable.income.annual.s)

# We alo estimate their Medicare Levy liability

person.dfkvi$Medicare.levy <- ML.function(person.dfkvi$Taxable.income.annual.s,
                                          person.dfkvi$ML.lower,
                                          person.dfkvi$ML.upper,
                                          person.dfkvi$ML.lower.senior,
                                          person.dfkvi$ML.upper.senior,
                                          person.dfkvi$Age.numeric)

person.dfkvi$Tax.estimate.s <- person.dfkvi$Tax.estimate.s + person.dfkvi$Medicare.levy

# Neither SAPTO or LITO are refundable tax offsets, so we only apply them to reduce tax if the individual actually has a tax liability up to this point

# LITO

person.dfkvi$LITO.entitlement <- LITO.function(person.dfkvi$Taxable.income.annual.s,
                                               person.dfkvi$LITO.lower,
                                               person.dfkvi$LITO.upper,
                                               person.dfkvi$LITO.max)

person.dfkvi$Tax.estimate.s <- ifelse(person.dfkvi$Tax.estimate.s < person.dfkvi$LITO.entitlement, 0,
                                      person.dfkvi$Tax.estimate.s - person.dfkvi$LITO.entitlement)

# SAPTO

person.dfkvi$SAPTO.entitlement <- SAPTO.function(person.dfkvi$Taxable.income.annual.s,
                                                 person.dfkvi$Age.numeric,
                                                 person.dfkvi$Single,
                                                 person.dfkvi$SAPTO.lower.indiv,
                                                 person.dfkvi$SAPTO.upper.indiv,
                                                 person.dfkvi$SAPTO.max.indiv,
                                                 person.dfkvi$SAPTO.lower.cpl,
                                                 person.dfkvi$SAPTO.upper.cpl,
                                                 person.dfkvi$SAPTO.max.cpl)

person.dfkvi$Tax.estimate.s <- ifelse(person.dfkvi$Tax.estimate.s < person.dfkvi$SAPTO.entitlement, 0,
                                      person.dfkvi$Tax.estimate.s - person.dfkvi$SAPTO.entitlement)

# ==================================================================================
# Establishing a base case for current taxation arrangements for super
# =================================================================================#

# Currently earnings on super balances for those aged under 60 are taxed at 15 per cent (or 10 per cent in the case of capital gains). We specified the effective tax rates on super earnings separately for those in the accumulation and the drawdown phases.

# For those aged over 60, and drawing the minimum (4% annually) from their super account, super earnings are tax free

# So we establish a base case treatment of super earnings taxation under the current arrangements

# First we convert Age into a numeric varible upon which we can filter for different tax treatments of super by Age
person.dfkvi$Age.numeric <- as.factor(person.dfkvi$Age)
person.dfkvi$Age.numeric <- as.numeric(person.dfkvi$Age.numeric)

# We now assign super earnings tax liability, excluding those that are aged over 60 and in the drawdown phase.

person.dfkvi$super.earnings.tax.current <- ifelse(person.dfkvi$Super.ddown == 1, 0, person.dfkvi$Super.earnings * tax.rate.acc)

# So the current value of the tax concession for each taxpayer, relative to a personal income tax benchmark, is:

person.dfkvi$Super.concession <- person.dfkvi$Tax.estimate.s - person.dfkvi$Tax.estimate - person.dfkvi$super.earnings.tax.current

# ==================================================================================
# Establishing the value of tax free super for over 60s, compared to a 15% super earnings tax on everyone
# =================================================================================#

# We do this compared to a benchmark of 15% tax on super earnings (for everyone, including those aged 60+)

person.dfkvi$Super.earnings.tax.everyone <- ifelse(person.dfkvi$Super.ddown == 1,
                                                   person.dfkvi$Super.earnings * tax.rate.ddown,
                                                   person.dfkvi$Super.earnings * tax.rate.acc)

person.dfkvi$Super.concession.over60s <- person.dfkvi$Super.earnings.tax.everyone - person.dfkvi$super.earnings.tax.current

sum(person.dfkvi$Super.concession.over60s * person.dfkvi$Weights) / 10^9  # Tax concession is worth $3.7 billion - this should align with the costing for abolishing tax-free super earnings in the ddown phase (without behaviuour change)

# ==================================================================================
# Cross checking our estimates super balances in drawdown against APRA - No need to QC
# =================================================================================#

# What assets are in the drawdown phase by this definition?

super.bal.ddown.df <- person.dfkvi %>% group_by(Super.ddown == 1) %>%
  summarise(total.assets = sum(Total.super * Weights) / 10^9,
            total.people = sum(Weights))

# So we find that $533 billion in assets are in the drawdown phase

# What super assets are held by over 60s in general?

super.bal.over60.df <- person.dfkvi %>% group_by(Age.numeric > 21) %>%
  summarise(total.assets = sum(Total.super * Weights) / 10^9,
            total.people = sum(Weights))

# $752 billion in assets are held by over 60s

# What proportion of the super balances of over 60s are in drawdown phase?

share.over60s.bal.ddown <- person.dfkvi %>% filter(Age.numeric > 21) %>%
  group_by((Super.ddown == 1)) %>%
  summarise(total.assets = sum(Total.super * Weights) / 10^9) %>% ungroup %>%
  mutate(prop.assets = total.assets / sum(total.assets))

# So 70 per cent of the assets of over 60s are in the drawdown phase

# ==================================================================================
# Modelling reform options - no behaviour change
# ==================================================================================

# For now we investigate these reform options without behavioural change. We'll come back to the question of behavioural change later, as it's important

# In this section we consider 3 reform options:

# (a) Reintroduce a 15% tax rate on super for over 60s (Grattan Proposal)
# (b) Reintroduce a 15% tax rate on super for over 60s - with an $18200 tax free threshold
# (c) Apply a 30% super earning tax rate for under 60s - with a $75000 tax free threshold (ALP policy)

# ==================================================================================
# (a) Reintroduce a 15% tax rate on super for over 60s

person.dfkvi$super.earnings.tax.opt1 <- ifelse(person.dfkvi$Age.numeric < 27,
                                               person.dfkvi$Super.earnings * tax.rate.acc,
                                               person.dfkvi$Super.earnings * tax.rate.ddown)


person.dfkvi$Super.concession.opt1 <- person.dfkvi$Tax.estimate.s - person.dfkvi$Tax.estimate - person.dfkvi$super.earnings.tax.opt1

sum((person.dfkvi$super.earnings.tax.opt1 - person.dfkvi$super.earnings.tax.current) * person.dfkvi$Weights) / 10^9


# ==================================================================================
# (b) 20000  - tax free threshold for over 60s in super drawdown phase

person.dfkvi$super.earnings.tax.20000tf <- ifelse(person.dfkvi$Super.ddown == 1,
                                                  ifelse(person.dfkvi$Super.earnings - 20000 >= 0,
                                                         (person.dfkvi$Super.earnings-20000) *tax.rate.ddown, 0),
                                                  person.dfkvi$Super.earnings * tax.rate.acc)

# Value of the tax concession, relative to a personal income tax benchmark, becomes:

person.dfkvi$Super.concession.20000tf <- person.dfkvi$Tax.estimate.s - person.dfkvi$Tax.estimate - person.dfkvi$super.earnings.tax.20000tf

sum(person.dfkvi$Super.concession.20000tf * person.dfkvi$Weights) / 10^9 # Total concession is $17.2 billion

# The value of the tax concession, compared to a world where everyone pays 15% tax on super earniings, is:

person.dfkvi$Super.concession.20000tf.over60s <- person.dfkvi$super.earnings.tax.opt1 - person.dfkvi$super.earnings.tax.20000tf

sum(person.dfkvi$Super.concession.20000tf.over60s * person.dfkvi$Weights) / 10^9

# How much does it save  - $2 billion

sum((person.dfkvi$super.earnings.tax.20000tf - person.dfkvi$super.earnings.tax.current) * person.dfkvi$Weights) / 10^9

# How much do we sacrifice by including a tax free threshold for over 60s of 20000, compared to no TF threshold - $2.9 billion.

sum((person.dfkvi$super.earnings.tax.opt1 - person.dfkvi$super.earnings.tax.20000tf) * person.dfkvi$Weights) / 10^9

# So we lose well more than half of the potential revenue gain by including a TF thresholf of 20000

# We'll just check this against the share of total earnings that would be captured by the TF threshold

person.dfkvi %>% filter(Super.ddown == 1) %>%
  group_by(Super.earnings<20000) %>%
  summarise(earnings.total = sum(Super.earnings * Weights)) %>%
  ungroup %>% mutate(earnings.prop = earnings.total/sum(earnings.total))

# So 17 per cent of super earnings come from ppl that have less than 20k in earnings.

# However with a TF threshold we also lose a whole bunch of tax $$ from those earning more than 20k would have tax free earnings on the first 20k they earn also

person.dfkvi$Super.earnings.under.20kTF <- ifelse(person.dfkvi$Super.ddown == 1,
                                                  ifelse(person.dfkvi$Super.earnings < 20000,person.dfkvi$Super.earnings, 20000),
                                                  0)

person.dfkvi %>% filter(Super.ddown == 1) %>%
  group_by(Super.earnings < 20000) %>%
  summarise(no.individuals = sum(Weights),
            earnings.total.under20kTF = sum(Super.earnings.under.20kTF * Weights),
            earnings.total = sum(Super.earnings * Weights)) %>%
  ungroup %>% mutate(earnings.prop = earnings.total.under20kTF / sum(earnings.total))

# So 47% of earnings in the draw down phase would fall under the TF threshold, 17% from those earning less than 20k from super in ddown phase, and 30% from those earning more than 20k annually from super in the ddown phase.

# An alternative policy would be to put together a rebate on super earnings, like LITO or SAPTO, that is withdrawn for higher super earnings. However the taper rate could have the effect of worsening EMTRs to long run savings via super for middle-upper income earners, and would add more complexity.

# ==================================================================================
# (c) a tax free threshold of 75,000 for under 60s, tax free super earnings for over 60s in ddown phase, consistent with proposed ALP policy

person.dfkvi$super.earnings.tax.ALP <- ifelse(person.dfkvi$Super.ddown == 1, ifelse(person.dfkvi$Super.earnings >= 75000,((person.dfkvi$Super.earnings-75000) * tax.rate.ddown), 0), person.dfkvi$Super.earnings * tax.rate.acc)

# Value of the tax concession becomes:

person.dfkvi$Super.concession.ALP <- person.dfkvi$Tax.estimate.s - person.dfkvi$Tax.estimate - person.dfkvi$super.earnings.tax.ALP

# How much does it save compared to the current tax concessions on super earnings - $0.6 billion

sum((person.dfkvi$super.earnings.tax.ALP - person.dfkvi$super.earnings.tax.current) * person.dfkvi$Weights) / 10^9

# This estimate is much lower than the PBO's costing of a policy for David L, in a request to the PBO which sought to replicate the ALP's proposed (but unpublished) policy. That costing estimated a revenue impact of $600 million in 2018-19.





# ==================================================================================
# Modelling reform options - with behavioural change
# =================================================================================#

# Now we take a look at the reform options accounying for behavioural change with respect to whether ppl choose to withdraw their super savings to make use of Tax free thresholds outside of the super system.

# In this section we consider 3 reform options:

# (a) Reintroduce a 15% tax rate on super for over 60s (Grattan Proposal)
# (b) Reintroduce a 15% tax rate on super for over 60s - with an $18200 tax free threshold
# (c) Apply a 30% super earning tax rate for under 60s - with a $75000 tax free threshold (ALP policy)

# ==================================================================================
# (a) Reintroduce a 15% tax rate on super for over 60s

# We already have our estimates of the value of the savings from the three reform options assuming no behaviour change.

# However, those in the drawdown phase that now face tax on super earnings from the first $ have a strong incentive to withdraw funds from super in order to make the most of the tax free threshold that exists outside of super. We assume that individuals respond to the tax change by minimising their tax by withdrawing super earnings up to the point that they maximise their tax-free threshold and LITO and SAPTO entitlements outside of super.

# So we write in our behavioural change - ppl affected withdraw what super earnings they can from super to make use of TF thresholds outside of super

# We need to write a function that accounts for the tax-free threshold, plus an SAPTO and LITO entitlement WHEN they are actually unused since they taper away.

person.dfkvi %<>%
  mutate(Excess.TF.income.threshold = ifelse(Super.ddown == 1,
                                             ifelse(Taxable.income.annual < 18200,
                                                    TF.unused.function(Taxable.income.annual) *
                                                      tax.rate.ddown + SAPTO.entitlement + LITO.entitlement,
                                                    ifelse(Tax.estimate + Medicare.levy -
                                                             SAPTO.entitlement - LITO.entitlement < 0,
                                                           SAPTO.entitlement + LITO.entitlement -
                                                             Tax.estimate - Medicare.levy,
                                                           0)),
                                             0))

# We check what's the max income of someone that has tax credits left. It should only be circa $33k

ddown.max.income.df <- person.dfkvi %>% filter(Excess.TF.income.threshold > 0)

summary(ddown.max.income.df$Taxable.income.annual)

# Ok so the max income of someone with  is $33,160, which is pretty spot on with the maximum effective tax free threshold for an individual of Age Pension age

# So now we know how much tax ppl can avoid by shifting their earnings out of super to make the most of any remaining TF threshold and SAPTO / LITO entitlements outside of the super system

# Super earnings tax collected after behaviour change

person.dfkvi$super.earnings.tax.opt1.behav <- ifelse(person.dfkvi$super.earnings.tax.opt1 < person.dfkvi$Excess.TF.income.threshold,
                                                     0,
                                                     person.dfkvi$super.earnings.tax.opt1 - person.dfkvi$Excess.TF.income.threshold)

# We write a variable of the tax foregone by behavioural change

person.dfkvi$super.earnings.tax.opt1.foregone <- person.dfkvi$super.earnings.tax.opt1 - person.dfkvi$super.earnings.tax.opt1.behav

sum(person.dfkvi$super.earnings.tax.opt1.foregone * person.dfkvi$Weights) / 10^9

# So only lose $0.9 billion via behavioural change

# What proportion of those in the drawdown phase reduce their tax liability through behavioural change?

# No need to QC

# Behav.effect.opt1.df <- person.dfkvi %>% filter(Super.ddown ==1) %>%
#   group_by(super.earnings.tax.opt1 == super.earnings.tax.opt1.behav) %>%
#   summarise (No.individuals = sum(Weights),
#              Total.earnings = sum(Super.earnings * Weights),
#              Total.extra.tax.no.behav = sum((super.earnings.tax.opt1 - super.earnings.tax.current) * Weights),
#              Total.super.tax.fgone = sum(super.earnings.tax.opt1.foregone * Weights)) %>%
#   ungroup %>%
#   mutate(Prop.individuals = No.individuals / sum(No.individuals),
#          Prop.earnings = Total.earnings / sum(Total.earnings),
#          Prop.earnings.tax.foregone = Total.super.tax.fgone / sum(Total.extra.tax.no.behav))

# # View(Behav.effect.opt1.df)

# So 70% of those affected could reduce their tax liability by shifting $$ from super, and we lose 30% of the potential extra super earnings tax

# Budget saving from reintroducing the 15% tax on super earnings for over 60s - $3.9 billion

sum((person.dfkvi$super.earnings.tax.opt1.behav - person.dfkvi$super.earnings.tax.current) * person.dfkvi$Weights) / 10^9

# What does behavioural change cost us? $0.9 billion

sum((person.dfkvi$super.earnings.tax.opt1 - person.dfkvi$super.earnings.tax.opt1.behav) * person.dfkvi$Weights) / 10^9


# ==================================================================================
# (b) 20000  - tax free threshold for over 60s in super drawdown phase - with behavioural change
# =================================================================================#

person.dfkvi$super.earnings.tax.20000tf <- ifelse(person.dfkvi$Super.ddown == 1,
                                                  ifelse(person.dfkvi$Super.earnings - 20000 >= 0,
                                                         (person.dfkvi$Super.earnings-20000) *tax.rate.ddown, 0),
                                                  person.dfkvi$Super.earnings * tax.rate.acc)

# Now with behavioural change, we only collect the tax on those that can't withdraw some of their asets from super to make the most of SAPTO, LITO and unused TF thresholds outside of the super system

person.dfkvi$super.earnings.tax.20000tf.behav <- ifelse(person.dfkvi$super.earnings.tax.20000tf < person.dfkvi$Excess.TF.income.threshold, 0,
                                                        person.dfkvi$super.earnings.tax.20000tf - person.dfkvi$Excess.TF.income.threshold)


# We write a variable of the tax foregone by behavioural change

person.dfkvi$super.earnings.tax.20000tf.foregone <- person.dfkvi$super.earnings.tax.20000tf - person.dfkvi$super.earnings.tax.20000tf.behav

sum(person.dfkvi$super.earnings.tax.20000tf.foregone * person.dfkvi$Weights)/10^9

# So only lose $0.3 billion via behavioural change. Most of those that would choose to shift money out of super are those that would benefit from TF earnings inside of super with a 20k super earnings TF threshold, or an equivalent rebate

# Value of the tax concession, relative to a personal income tax benchmark, becomes:

person.dfkvi$Super.concession.20000tf.behav <- person.dfkvi$Tax.estimate.s - person.dfkvi$Tax.estimate - person.dfkvi$super.earnings.tax.20000tf.behav

# How much does it save - $1.6 billion

sum((person.dfkvi$super.earnings.tax.20000tf.behav - person.dfkvi$super.earnings.tax.current) * person.dfkvi$Weights) / 10^9

# How much do we sacrifice by including a tax free threshold for over 60s of 20000, compared to no TF threshold - $2.3 billion.

sum((person.dfkvi$super.earnings.tax.opt1.behav - person.dfkvi$super.earnings.tax.20000tf.behav) * person.dfkvi$Weights) / 10^9

# So we lose roughly half the revenue with a TF thresold, but less than before we account for behavioural change. This is because most of those that can avoid the tax via by taking $$ out of super and benefit from TF thresholds under PIT are all helped by making the first 20k of super earnings tax free.

# Incidentally the revenue leakage due to behavioural change under this proposal is $340 million

sum((person.dfkvi$super.earnings.tax.20000tf.foregone * person.dfkvi$Weights)) / 10^9


# ====================================================================================
# (c) a tax free threshold of 75,000 for over 60s, consistent with proposed ALP policy
# ===================================================================================#

person.dfkvi$super.earnings.tax.ALP <- ifelse(person.dfkvi$Super.ddown == 1, ifelse(person.dfkvi$Super.earnings >= 75000,((person.dfkvi$Super.earnings-75000) * tax.rate.ddown), 0), person.dfkvi$Super.earnings * tax.rate.acc)

# Now with behavioural change, we only collect the tax on those that can't withdraw some of their asets from super to make the most of SAPTO, LITO and unused TF thresholds outside of the super system

person.dfkvi$super.earnings.tax.ALP.behav <- ifelse(person.dfkvi$super.earnings.tax.ALP < person.dfkvi$Excess.TF.income.threshold, 0,
                                                    person.dfkvi$super.earnings.tax.ALP - person.dfkvi$Excess.TF.income.threshold)

# We write a variable of the tax foregone by behavioural change

person.dfkvi$super.earnings.tax.ALP.foregone <- person.dfkvi$super.earnings.tax.ALP - person.dfkvi$super.earnings.tax.ALP.behav

sum(person.dfkvi$super.earnings.tax.ALP.foregone * person.dfkvi$Weights)/10^9

#  There is no revenue leakage via behavioural change. Basically no one with super earnings of more than 75k has unused TF threshold, SAPTO or LITO entitlements in the PIT system

# Value of the tax concession becomes:

person.dfkvi$Super.concession.ALP <- person.dfkvi$Tax.estimate.s - person.dfkvi$Tax.estimate - person.dfkvi$super.earnings.tax.ALP.behav

# How much does it save compared to the current tax concessions on super earnings - $0.513 billion

sum((person.dfkvi$super.earnings.tax.ALP.behav - person.dfkvi$super.earnings.tax.current) * person.dfkvi$Weights) / 10^9

# This estimate is marginally lower than the PBO's costing of a policy for David L, in a request to the PBO which sought to replicate the ALP's proposed (but unpublished) policy. That costing estimated a revenue impact of $600 million in 2018-19.


# ==================================================================================
# Final outputs for super chapter in budget repair report - figures, charts and tables
# ==================================================================================

# ==================================================================================
# Our final costings for the reform options, without behavioural change, which are cited in the chapter, are:

# (a) Budget saving from reintroducing the 15% tax on super earnings for over 60s - $4.87 billion
sum((person.dfkvi$super.earnings.tax.opt1 - person.dfkvi$super.earnings.tax.current) * person.dfkvi$Weights) / 10^9

# (b) 20000  - tax free threshold for over 60s in super drawdown phase - $1.97 billion
sum((person.dfkvi$super.earnings.tax.20000tf - person.dfkvi$super.earnings.tax.current) * person.dfkvi$Weights) / 10^9

# (c) a tax free threshold of 75,000 for under 60s, tax free super earnings for over 60s, consistent with proposed ALP policy - $0.56 billion
sum((person.dfkvi$super.earnings.tax.ALP - person.dfkvi$super.earnings.tax.current) * person.dfkvi$Weights) / 10^9
# Note: we might not end up using this last one in the chapter

# ==================================================================================
# Our final costings for the reform options, with behavioural change, which are cited in the chapter, are:

# (a) Budget saving from reintroducing the 15% tax on super earnings for over 60s - $3.94 billion
sum((person.dfkvi$super.earnings.tax.opt1.behav - person.dfkvi$super.earnings.tax.current) * person.dfkvi$Weights) / 10^9

# (b) 20000  - tax free threshold for over 60s in super drawdown phase - $1.6 billion
sum((person.dfkvi$super.earnings.tax.20000tf.behav - person.dfkvi$super.earnings.tax.current) * person.dfkvi$Weights) / 10^9

# (c) a tax free threshold of 75,000 for under 60s, tax free super earnings for over 60s, consistent with proposed ALP policy - $0.56 billion
sum((person.dfkvi$super.earnings.tax.ALP - person.dfkvi$super.earnings.tax.current) * person.dfkvi$Weights) / 10^9
# Note: we might not end up using this last one in the chapter


# ==================================================================================
# Cost BT / ASFA proposal a tax free threshold of 150,000 for over 60s in the drawdown phase
# ================================================================================== #

# We dont bother with behavioural change when ww're talking about a TF threshold of 150k

person.dfkvi$super.earnings.tax.BT <- ifelse(person.dfkvi$Super.ddown == 1, ifelse(person.dfkvi$Super.earnings >= 150000,((person.dfkvi$Super.earnings-150000) * tax.rate.ddown), 0), person.dfkvi$Super.earnings * tax.rate.acc)

# Value of the tax concession becomes:

person.dfkvi$Super.concession.BT <- person.dfkvi$Tax.estimate.s - person.dfkvi$Tax.estimate - person.dfkvi$super.earnings.tax.BT

# How much does it save compared to the current tax concessions on super earnings - $186 million in 2015-16

sum((person.dfkvi$super.earnings.tax.BT - person.dfkvi$super.earnings.tax.current) * person.dfkvi$Weights) / 10^9

# How many people are affected?

BT.affected.df <- person.dfkvi %>% filter(Super.ddown == 1) %>%
  group_by(super.earnings.tax.BT == super.earnings.tax.current) %>%
  summarise(total.affected = sum(Weights)) %>% ungroup %>%
  mutate(prop.affected = total.affected / sum(total.affected))

# So 10,947 people are affected by the ASFA proposal (i.e. they have super incomes of at least $150,000, or total balances of more than $2.5 million)

#
# # ==================================================================================
# # Costing alternative proposal for a $1 million cap on super balances - or $50,000 tax free threshold
# # ================================================================================== #
#
# # We dont bother with behavioural change when we're talking about a TF threshold of 150k
#
# person.dfkvi$super.earnings.tax.1mil.cap <- ifelse(person.dfkvi$Super.ddown == 1, ifelse(person.dfkvi$Super.earnings >= 50000,((person.dfkvi$Super.earnings-50000) * tax.rate.ddown), 0), person.dfkvi$Super.earnings * tax.rate.acc)
#
# # Value of the tax concession becomes:
#
# person.dfkvi$Super.concession.1mil.cap <- person.dfkvi$Tax.estimate.s - person.dfkvi$Tax.estimate - person.dfkvi$super.earnings.tax.BT
#
# # How much does it save compared to the current tax concessions on super earnings - $930 million in 2015-16
#
# sum((person.dfkvi$super.earnings.tax.1mil.cap - person.dfkvi$super.earnings.tax.current) * person.dfkvi$Weights) / 10^9
#
# # How many people are affected?
#
# Onemilcap.affected.df <- person.dfkvi %>% group_by(super.earnings.tax.1mil.cap == super.earnings.tax.current) %>%
#   summarise(total.affected = sum(Weights))
#
# # So 147,211 people are affected by the $1 million cap proposal (i.e. they have super incomes of at least $50,000, or total balances of more than $1 million)

# # ==================================================================================
# # Costing alternative proposal for a $400,000 cap on super balances - or $20,000 tax free threshold
# # ================================================================================== #
#
# # How many would be affected by a cap on tax-free balabces in the drawdown phase of $400,000?
#
# person.dfkvi$super.earnings.tax.Cap.400k <- ifelse(person.dfkvi$Super.ddown == 1, ifelse(person.dfkvi$Super.earnings >= 20000,((person.dfkvi$Super.earnings-20000) * tax.rate.ddown), 0), person.dfkvi$Super.earnings * tax.rate.acc)
#
# Cap.400k.affected.df <- person.dfkvi %>% filter(Super.ddown == 1) %>%
#   group_by(super.earnings.tax.Cap.400k == super.earnings.tax.current) %>%
#   summarise(total.affected = sum(Weights)) %>% ungroup %>%
#   mutate(prop.affected = total.affected / sum(total.affected))
#
# sum((person.dfkvi$super.earnings.tax.Cap.400k - person.dfkvi$super.earnings.tax.current) * person.dfkvi$Weights) / 10^9

# ==================================================================================
# Distributional analysis: super earnings and tax concessions by total income (both with and without super earnings)
# ================================================================================== #

# Here we establish weighted quintiles for annual total income (with and without super income)

# First lets make sure our income variables are spot on

summary(person.dfkvi$Total.income) # There are people with large negative incomes

# The 6523.0 - Household Income and Income Distribution, Australia, 2011-12  has the following to say on the matter:

# Some households report extremely low and even negative income in the survey, which places them well below the safety net of income support provided by government pensions and allowances. Households may under report their incomes in the survey at all income levels, including low income households. However, households can correctly report low levels of income if they incur losses in their unincorporated business or have negative returns from their other investments.

# When creating equivalised income deciles the ABS converts negative disposable incomes to zero

# http://www.abs.gov.au/AUSSTATS/abs@.nsf/Lookup/6523.0Explanatory%20Notes12011-12?OpenDocument

# Therefore we first re-write total income such that it cant be negative for each of our three income variables:

# Total annual income, excluding super fund earnings
# Total annual income, including super fund earnings
# Total annual income, including super withdrawals in income, but including super earnings (to avoid double counting)

person.dfkvi$Total.income.annual <- ifelse(person.dfkvi$Total.income.annual < 0,
                                           0,
                                           person.dfkvi$Total.income.annual)

person.dfkvi$Total.income.annual.s <- ifelse(person.dfkvi$Total.income.annual.s < 0,
                                             0,
                                             person.dfkvi$Total.income.annual.s)

person.dfkvi$Total.income.inc.wdls.annual <- ifelse(person.dfkvi$Total.income.inc.wdls.annual < 0,
                                                    0,
                                                    person.dfkvi$Total.income.inc.wdls.annual)

# Then we define a survey object

SIHP.svy <- svydesign(id=~PID, weights= ~Weights, fpc=NULL, data = person.dfkvi)

# Then we use this to define survey weighted quintiles and deciles

# Deciles for total income (excluding super) and total income (including super)
SIHP.tincome.10 <- svyquantile(~Total.income.annual, SIHP.svy,c(0, .1, .2, .3, .4, .5, .6, .7, .8, .9, 1), ci = FALSE)
SIHP.tincome.s.10 <- svyquantile(~Total.income.annual.s, SIHP.svy,c(0, .1, .2, .3, .4, .5, .6, .7, .8, .9, 1), ci = FALSE)

# So now we use cut to break up the dataset along the deciles specified in the survey object

# Total income (excluding super)
person.dfkvi$tincome.decile <- cut(person.dfkvi$Total.income.annual,
                                   breaks = SIHP.tincome.10,
                                   include.lowest = TRUE) %>% as.numeric()

person.dfkvi$tincome.decile.range <- cut(person.dfkvi$Total.income.annual,
                                         breaks = SIHP.tincome.10,
                                         include.lowest = TRUE)

# Total income (including super)
person.dfkvi$tincome.s.decile <- cut(person.dfkvi$Total.income.annual.s,
                                     breaks = SIHP.tincome.s.10,
                                     include.lowest = TRUE) %>% as.numeric()

person.dfkvi$tincome.s.decile.range <- cut(person.dfkvi$Total.income.annual.s,
                                           breaks = SIHP.tincome.s.10,
                                           include.lowest = TRUE)

# ==================================================================================
# Income deciles for over 60s only
# ================================================================================== #

# Then we define a survey object

person.dfkvi.over60 <- person.dfkvi %>% filter(Age.numeric > 21)

SIHP.svy.over60 <- svydesign(id=~PID, weights= ~Weights,
                             fpc=NULL,
                             data = person.dfkvi.over60)

# Then we use this to define survey weighted quintiles and deciles

# Deciles for total income (excluding super) and total income (including super) and total income including super withdrawals but not super earnings

SIHP.tincome.10.over60 <- svyquantile(~Total.income.annual,
                                      SIHP.svy.over60,
                                      c(0, .1, .2, .3, .4, .5, .6, .7, .8, .9, 1),
                                      ci = FALSE,
                                      include.lowest = TRUE)

SIHP.tincome.s.10.over60 <- svyquantile(~Total.income.annual.s,
                                        SIHP.svy.over60,
                                        c(0, .1, .2, .3, .4, .5, .6, .7, .8, .9, 1),
                                        ci = FALSE,
                                        include.lowest = TRUE)

SIHP.tincome.w.10.over60 <- svyquantile(~Total.income.inc.wdls.annual,
                                        SIHP.svy.over60,
                                        c(0, .1, .2, .3, .4, .5, .6, .7, .8, .9, 1),
                                        ci = FALSE,
                                        include.lowest = TRUE)

# So now we use cut to break up the dataset along the deciles specified in the survey object

# Total income (excluding super)
person.dfkvi$tincome.decile.over60 <- cut(person.dfkvi$Total.income.annual,
                                          breaks = SIHP.tincome.10.over60,
                                          include.lowest = TRUE) %>% as.numeric()

person.dfkvi$tincome.decile.over60.range <- cut(person.dfkvi$Total.income.annual,
                                                breaks = SIHP.tincome.10.over60,
                                                include.lowest = TRUE)

# Total income (including super)
person.dfkvi$tincome.s.decile.over60 <- cut(person.dfkvi$Total.income.annual.s,
                                            breaks = SIHP.tincome.s.10.over60,
                                            include.lowest = TRUE) %>% as.numeric()

person.dfkvi$tincome.s.decile.over60.range <- cut(person.dfkvi$Total.income.annual.s,
                                                  breaks = SIHP.tincome.s.10.over60,
                                                  include.lowest = TRUE)

# Total income (including super withdrawals, but not earnings)

person.dfkvi$tincome.w.decile.over60 <- cut(person.dfkvi$Total.income.inc.wdls.annual,
                                            breaks = SIHP.tincome.w.10.over60,
                                            include.lowest = TRUE) %>% as.numeric()

person.dfkvi$tincome.w.decile.over60.range <- cut(person.dfkvi$Total.income.inc.wdls.annual,
                                                  breaks = SIHP.tincome.w.10.over60,
                                                  include.lowest = TRUE)

# ==================================================================================
# Income deciles for over 60s only in the drawdown phase
# ================================================================================== #

# Then we define a survey object (you need the survey pkg for this)

person.dfkvi.over60.ddown <- person.dfkvi %>% filter(Age.numeric > 21) %>% filter(Super.ddown == 1)

SIHP.svy.over60.ddown <- svydesign(id=~PID, weights= ~Weights, fpc=NULL, data = person.dfkvi.over60.ddown)

# Then we use this to define survey weighted quintiles and deciles

# Deciles for total income (excluding super) and total income (including super)
SIHP.tincome.10.over60.ddown <- svyquantile(~Total.income.annual,
                                            SIHP.svy.over60.ddown,
                                            c(0, .1, .2, .3, .4, .5, .6, .7, .8, .9, 1),
                                            ci = FALSE)

SIHP.tincome.s.10.over60.ddown <- svyquantile(~Total.income.annual.s,
                                              SIHP.svy.over60.ddown,
                                              c(0, .1, .2, .3, .4, .5, .6, .7, .8, .9, 1),
                                              ci = FALSE)

# So now we use cut to break up the dataset along the deciles specified in the survey object

# Total income (excluding super)
person.dfkvi$tincome.decile.over60.ddown <- cut(person.dfkvi$Total.income.annual,
                                                breaks = SIHP.tincome.10.over60.ddown,
                                                include.lowest = TRUE) %>% as.numeric()

person.dfkvi$tincome.decile.over60.ddown.range <- cut(person.dfkvi$Total.income.annual,
                                                      breaks = SIHP.tincome.10.over60.ddown,
                                                      include.lowest = TRUE)

# Total income (including super)
person.dfkvi$tincome.s.decile.over60.ddown <- cut(person.dfkvi$Total.income.annual.s,
                                                  breaks = SIHP.tincome.s.10.over60.ddown,
                                                  include.lowest = TRUE) %>% as.numeric()

person.dfkvi$tincome.s.decile.over60.ddown.range <- cut(person.dfkvi$Total.income.annual.s,
                                                        breaks = SIHP.tincome.s.10.over60.ddown,
                                                        include.lowest = TRUE)

#==================================================================================
# Our final charts for the chapter are:
#================================================================================== #

# Chart 2 - Total income (including super earnings) of over 60s
Chart2.df <- person.dfkvi %>% filter(Age.numeric >=22) %>%
  group_by(tincome.s.decile.range) %>%
  summarise(Mean.super.earnings =  weighted.mean(x = Super.earnings, w =  Weights),
            Mean.tax.concession = weighted.mean(x = Super.concession.over60s, w = Weights))

Chart2a.df <- person.dfkvi %>% filter(Age.numeric >=22) %>%
  group_by(tincome.s.decile.over60.range) %>%
  summarise(Mean.super.earnings =  weighted.mean(x = Super.earnings, w =  Weights),
            Mean.tax.concession = weighted.mean(x = Super.concession.over60s, w = Weights))

View(Chart2a.df)

write.table(Chart2a.df , sep = "\t", file = clip <- pipe("pbcopy", "w"))
close(clip)

# Chart 3c: Additional tax paid on super earning - Comparing behavioural change and not - including super in income deciles

# Using ddown over 60 income deciles

Chart3c.df <- person.dfkvi %>% filter(Super.ddown == 1) %>%
  group_by(tincome.s.decile.over60.ddown.range) %>%
  summarise(Mean.super.earnings =  sum(Super.earnings * Weights) / sum(Weights),
            Extra.tax.opt1.nobh = sum((super.earnings.tax.opt1 -
                                         super.earnings.tax.current) * Weights) / sum(Weights),
            Extra.tax.opt1.wbh = sum((super.earnings.tax.opt1.behav -
                                        super.earnings.tax.current) * Weights) / sum(Weights),
            Extra.tax.tf20000.no.bh = sum((super.earnings.tax.20000tf -
                                             super.earnings.tax.current) * Weights) / sum(Weights),
            Extra.tax.20000tf.wbh = sum((super.earnings.tax.20000tf.behav -
                                           super.earnings.tax.current) * Weights) / sum(Weights),
            no.individuals = sum(Weights),
            No.datapoints = length(Total.super),
            Ave.account.balance = weighted.mean(x = Total.super, w = Weights))

# View(Chart3c.df)

# Chart 3d: Additional tax paid on super earning - Comparing behavioural change and not - including super in income deciles

# Using all over 60 income deciles

Chart3d.df <- person.dfkvi %>% filter(Super.ddown == 1) %>%
  group_by(tincome.s.decile.over60.range) %>%
  summarise(Mean.super.earnings =  sum(Super.earnings * Weights) / sum(Weights),
            Extra.tax.opt1.nobh = sum((super.earnings.tax.opt1 -
                                         super.earnings.tax.current) * Weights) / sum(Weights),
            Extra.tax.opt1.wbh = sum((super.earnings.tax.opt1.behav -
                                        super.earnings.tax.current) * Weights) / sum(Weights),
            Extra.tax.tf20000.no.bh = sum((super.earnings.tax.20000tf -
                                             super.earnings.tax.current) * Weights) / sum(Weights),
            Extra.tax.20000tf.wbh = sum((super.earnings.tax.20000tf.behav -
                                           super.earnings.tax.current) * Weights) / sum(Weights),
            no.individuals = sum(Weights),
            No.datapoints = length(Total.super),
            Ave.account.balance = weighted.mean(x = Total.super, w = Weights),
            Average.income.s.annual = weighted.mean(x = Total.income.annual.s, w = Weights))

View(Chart3d.df)

write.table(Chart3d.df , sep = "\t", file = clip <- pipe("pbcopy", "w"))
close(clip)


# Chart 33: Additional tax paid on super earning - Comparing behavioural change and not - including super in income deciles

# Whole population and deciles of over 60s

Chart3e.df <- person.dfkvi %>% filter(Age.numeric > 21) %>%
  group_by(tincome.s.decile.over60.range) %>%
  summarise(Mean.super.earnings =  sum(Super.earnings * Weights) / sum(Weights),
            Extra.tax.opt1.nobh = sum((super.earnings.tax.opt1 -
                                         super.earnings.tax.current) * Weights) / sum(Weights),
            Extra.tax.opt1.wbh = sum((super.earnings.tax.opt1.behav -
                                        super.earnings.tax.current) * Weights) / sum(Weights),
            Extra.tax.tf20000.no.bh = sum((super.earnings.tax.20000tf -
                                             super.earnings.tax.current) * Weights) / sum(Weights),
            Extra.tax.20000tf.wbh = sum((super.earnings.tax.20000tf.behav -
                                           super.earnings.tax.current) * Weights) / sum(Weights),
            no.individuals = sum(Weights),
            No.datapoints = length(Total.super),
            Ave.account.balance = weighted.mean(x = Total.super, w = Weights),
            ddown = sum(Weights * Super.ddown),
            Average.income.s.annual = weighted.mean(x = Total.income.annual.s, w = Weights))

View(Chart3e.df)

write.table(Chart3c.df , sep = "\t", file = clip <- pipe("pbcopy", "w"))
close(clip)


# Chart 6 - Summary costing of earnings tax concession reforms - comparing pre- and post behavioural change

Chart6.df <- data_frame(Reform.15percent.tax.on.everyone = sum((person.dfkvi$super.earnings.tax.opt1 - person.dfkvi$super.earnings.tax.current) * person.dfkvi$Weights) / 10^9,
                        Reform.15percent.tax.20000tf = sum((person.dfkvi$super.earnings.tax.20000tf - person.dfkvi$super.earnings.tax.current) * person.dfkvi$Weights) / 10^9,
                        Reform.15percent.tax.75000tf = sum((person.dfkvi$super.earnings.tax.ALP - person.dfkvi$super.earnings.tax.current) * person.dfkvi$Weights) / 10^9,
                        Reform.15percent.tax.on.everyone.bchange = sum((person.dfkvi$super.earnings.tax.opt1.behav - person.dfkvi$super.earnings.tax.current) * person.dfkvi$Weights) / 10^9,
                        Reform.15percent.tax.20000tf.bchange = sum((person.dfkvi$super.earnings.tax.20000tf.behav - person.dfkvi$super.earnings.tax.current) * person.dfkvi$Weights) / 10^9,
                        Reform.15percent.tax.75000tf.bchange = sum((person.dfkvi$super.earnings.tax.ALP.behav - person.dfkvi$super.earnings.tax.current) * person.dfkvi$Weights) / 10^9)

write.table(Chart6.df, sep = "\t", file = clip <- pipe("pbcopy", "w"))
close(clip)

# # View(Chart6.df)

Ddown.stats <- person.dfkvi %>% filter(Super.ddown == 1) %>%
  group_by(tincome.s.decile.over60.ddown.range) %>%
  summarise(Mean.super.income = weighted.mean(x = Super.income * 52, w = Weights),
            Mean.super.earnings = weighted.mean(x = Super.earnings, w = Weights),
            no.individuals = sum(Weights),
            No.datapoints = length(Total.super),
            Ave.account.balance = weighted.mean(x = Total.super, w = Weights))

View(Ddown.stats)

write.table(Chart3c.df , sep = "\t", file = clip <- pipe("pbcopy", "w"))
close(clip)

# Chart 7: Mean super earnings, super balance and total income (including super earnings) of all over 60s

Chart7.df <- person.dfkvi %>% filter(Age.numeric > 21) %>%
  group_by(tincome.s.decile.over60.range) %>%
  summarise(Mean.super.earnings =  weighted.mean(x = Super.earnings, w = Weights),
            no.individuals = sum(Weights),
            No.datapoints = length(Total.super),
            Ave.account.balance = weighted.mean(x = Total.super, w = Weights),
            Average.income.s.annual = weighted.mean(x = Total.income.annual.s, w = Weights))

View(Chart7.df)

write.table(Chart7.df , sep = "\t", file = clip <- pipe("pbcopy", "w"))
close(clip)
