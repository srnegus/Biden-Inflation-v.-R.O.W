install.packages(c("tidyverse","plm","readr","lmtest", "sandwich","CausalImpact", "Synth"))

library(tidyverse)
library(plm)
library(readr)
library(lmtest)
library(sandwich)
library(CausalImpact)
library(Synth)

# Loading in the data
df_raw = read.csv("imf PCPIPCH.csv")

# Renaming column
df_raw = df_raw %>% 
  rename(country = Inflation.rate..average.consumer.prices..Annual.percent.change.)

# Specifying group of 7 countries and treated country
g7list = c("United States","Canada", "United Kingdom", "Germany", "Italy", "France", "Japan")
treated_country = "United States"
control = setdiff(g7list, treated_country)

# Filtering and reshaping data for G7
df_g7 = df_raw %>%
  filter(country %in% g7list) %>%
  pivot_longer(-country, names_to = 'year', values_to = "inflation_rate") %>%
  mutate(across(c('year'), substr, 2, nchar(year))) %>%
  mutate(year = as.integer(year), inflation_rate = as.numeric(inflation_rate))

# Getting desired years
df_g7 = df_g7 %>%
  filter(year >= 2011 & year < 2025)

###### Difference-in-Difference #####
#####################################

# Data for diff-in-diff
df = df_g7 %>% 
  mutate(treated = ifelse(country == treated_country, 1, 0)) %>%
  mutate(post = ifelse(year >= 2021, 1, 0),
         treated_post = treated*post)


# Fitting OLS model with no FE
formula = as.formula('inflation_rate ~ treated + post + treated_post')
model = lm(formula, data = df)
summary(model)

# Fitting model with FE and clustered SE
df = pdata.frame(df, index = c("country","year"))
fe_formula = as.formula("inflation_rate ~ treated + post + treated_post + factor(country)-1")
fe_model = plm(fe_formula, data = df, model = "within", effect = "individual", index = c("country","year"))

summary(fe_model)
coeftest(fe_model, vcov = vcovHC(fe_model, type = "HC1", cluster = "group"))

##### Causal Impact #####
#########################

# Reshaping data to wide
df_wide = df_g7 %>%
  pivot_wider(names_from = country, values_from = inflation_rate) %>%
  arrange(year)

# Moving U.S. to 1st column
df_wide = df_wide %>%
  select(year, "United States", everything())

df_wide = df_wide %>%
  rename(USA = `United States`, UK = `United Kingdom`)

z = zoo(df_wide[-1], order.by = df_wide$year)

# Defining periods 
pre_period = c(2011,2020)
post_period = c(2021,2023)

# Causal Impact Analysis
impact = CausalImpact(z, pre_period, post_period)
summary(impact)
plot(impact)

##### Synthetic Control #####
#############################

df_g7$country = recode(df_g7$country, "United States" = "USA", "United Kingdom" = "UK")
df = df_g7

# Unique countries
unique_countries = unique(df$country)

# Removing USA
unique_countries = unique_countries[unique_countries != "USA"]

# Assigning integers
group_id = seq_along(unique_countries)

# Making a mapping dataframe
mapping_df = data.frame(country = c("USA", unique_countries),
                        group_id = c(0,group_id))

df = merge(df, mapping_df, by = "country", all.x = TRUE)
