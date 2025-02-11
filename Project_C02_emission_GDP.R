rm(list=ls())

library(tidyverse)
library(readxl)
library(plyr)
library(haven)
library(fixest)
library(ggplot2) 
library(dplyr)     
library(lubridate)  
library(skimr)
library(stargazer)
library(kableExtra)
library(corrplot)
library(xtable)

data0 <- read_delim("clean_data.csv",
                 delim = ",",
                 quote = '"',)
data0$...1 <- NULL

colnames(data0) <- c("country", "code", "year", "C02_capita", "GDP_capita", "population", "urban_pop", "coal_capita", "oil_capita", "gaz_capita")
summary(data0)

data0 <- data0 %>%
  group_by(country) %>%  
  filter(min(year) <= 1992) %>%  
  ungroup()
str(data0)

ggplot(data0, aes(x = C02_capita)) +
  geom_histogram(bins = 30, fill = "blue", color = "black", alpha = 0.7) +
  labs(title = "Annual CO2 emission per capita in tonnes", x = "CO2 emissions", y = "Frequency") +
  theme_minimal()

#cor_data <- subset(data0, select = c(3:10))
#cor_data <- drop_na(cor_data)
#corr_matrix <- cor(cor_data, use = "complete.obs", method = "pearson")
#corrplot(corr_matrix, method = "number")
#print(xtable(corr_matrix), type = "latex")


#skim_table <- skimr::skim(data0)
#skim_selected <- skim_table %>%
#  select(skim_variable, n_missing, complete_rate, numeric.mean, numeric.sd)
#skim_selected2 <- skim_table %>%
#  select(skim_variable, numeric.p0, numeric.p25, numeric.p50, numeric.p75, numeric.p100)

#latex_table <- skim_selected %>%
#  kable("latex", booktabs = TRUE, longtable = TRUE, caption = "Descriptive Statistics") %>%
#  kable_styling(latex_options = c("repeat_header"))

#latex_table2 <- skim_selected2 %>%
#  kable("latex", booktabs = TRUE, longtable = TRUE, caption = "Descriptive Statistics") %>%
#  kable_styling(latex_options = c("repeat_header"))

#writeLines(latex_table, "descriptive_statistics.tex")
#writeLines(latex_table2, "descriptive_statistics2.tex")

data0$year <- as.numeric(data0$year)
data0$year <- as.Date(paste0(data0$year, "-01-01")) %>% format("%Y")

data <- subset(data0, select = -c(6:10))

str(data)



#Cross-section OLS for the year 2005
data <- mutate(data, 
               C02_capita_adj = C02_capita + 1)

subset2005 <- subset(data, year == '2005')

ggplot(subset2005, aes(x = GDP_capita)) +
  geom_histogram(bins = 30, fill = "blue", color = "black", alpha = 0.7) +
  labs(title = "Histogram of Variable", x = "Variable", y = "Frequency") +
  theme_minimal()

ggplot(subset2005, aes(x = C02_capita_adj)) +
  geom_histogram(bins = 30, fill = "blue", color = "black", alpha = 0.7) +
  labs(title = "Histogram of Variable", x = "Variable", y = "Frequency") +
  theme_minimal()

ggplot(subset2005, aes(x = log(GDP_capita), y = log(C02_capita_adj))) +
  geom_point() +
  geom_smooth(method = "lm", color = "red", se = FALSE)
  labs(title = "Scatter Plot", x = "Independent Variable", y = "Dependent Variable") +
    theme_minimal()

sum(is.na(data))  
  
reg <- lm(log(C02_capita_adj) ~ log(GDP_capita), data = subset2005)
summary(reg)



#Cross-section OLS for the last year
subset2022 <- subset(data, year == '2022')

ggplot(subset2022, aes(x = GDP_capita)) +
  geom_histogram(bins = 30, fill = "blue", color = "black", alpha = 0.7) +
  labs(title = "Histogram of Variable", x = "Variable", y = "Frequency") +
  theme_minimal()

ggplot(subset2022, aes(x = C02_capita_adj)) +
  geom_histogram(bins = 30, fill = "blue", color = "black", alpha = 0.7) +
  labs(title = "Histogram of Variable", x = "Variable", y = "Frequency") +
  theme_minimal()

ggplot(subset2022, aes(x = log(GDP_capita), y = log(C02_capita))) +
  geom_point() +
  geom_smooth(method = "lm", color = "red", se = FALSE)
labs(title = "Scatter Plot", x = "Independent Variable", y = "Dependent Variable") +
  theme_minimal()


reg2 <- lm(log(C02_capita) ~ log(GDP_capita), data = subset2022)
summary(reg2)

library(modelsummary)

models <- list("2005" = reg, "2022" = reg2)

coef_labels <- c(
  "(Intercept)" = "Constant",       
  "log(GDP_capita)" = "Log of GDP per capita"
)

modelsummary(models, 
             stars = TRUE, 
             title = "Regression Models", 
             coef_map = coef_labels)



#First difference model, with time trend, no lags
work <- data %>%
  arrange(country, year)  %>%
  group_by(country) %>%
  mutate(
    ln_C02_capita_adj = log(C02_capita_adj),
    ln_GDP_capita = log(GDP_capita)
  ) %>%
  ungroup()

work <- work %>%
  group_by(country) %>%  
  filter(min(ln_C02_capita_adj) >= 0) %>%  
  ungroup() 

work <- work %>%
  arrange(country, year)  %>%
  group_by(country) %>%
  mutate(
    dln_C02_capita_adj = ln_C02_capita_adj - lag(ln_C02_capita_adj),
    dln_GDP_capita = ln_GDP_capita - lag(ln_GDP_capita)
  ) %>%
  ungroup()

setFixest_estimation(panel.id = ~country + year)

fe_reg <- feols(dln_C02_capita_adj ~ dln_GDP_capita | year,
                  data = work,
                  vcov = NW(24))

summary(fe_reg)



#First difference model, with time trend, 2 year lags
fe_reg2 <- feols(dln_C02_capita_adj ~ l(dln_GDP_capita,0:2) | year,
                data = work,
                vcov = NW(24))
summary(fe_reg2)



#First difference model, with time trend, 6 year lags
fe_reg6 <- feols(dln_C02_capita_adj ~ l(dln_GDP_capita,0:6) | year,
                 data = work,
                 vcov = NW(24))
summary(fe_reg6)



#Fixed effects model with time and country fixed effects
fe_reg_all <- feols(ln_C02_capita_adj ~ ln_GDP_capita | year + country,
                data = work,
                vcov = NW(24))
summary(fe_reg_all)


etable(fe_reg, fe_reg2, fe_reg6, 
       headers = c("no lags + time&country FE", "2 lags + time&country FE", "6 lags + time&country FE"),
       dict = c(
         "(Intercept)" = "Constant") )

models2 <- list("no lags + time&country FE" = fe_reg,
                "2 lags + time&country FE" = fe_reg2,
                "6 lags + time&country FE" = fe_reg6)

coef_labels <- c(
  "(Intercept)" = "Constant",       
  "dln_GDP_capita" = "FD log of GDP in t",
  "l(dln_GDP_capita, 1)" = "FD log of GDP in t+1",
  "l(dln_GDP_capita, 2)" = "FD log of GDP in t+2",
  "l(dln_GDP_capita, 3)" = "FD log of GDP in t+3",
  "l(dln_GDP_capita, 4)" = "FD log of GDP in t+4",
  "l(dln_GDP_capita, 5)" = "FD log of GDP in t+5",
  "l(dln_GDP_capita, 6)" = "FD log of GDP in t+6"
)

modelsummary(models2, stars = TRUE,
             title = "regression models",
             coef_map = coef_labels)

#-------------------------------------------------------------------------------
#Adding confounders
str(data0)
dropna_cols <- c("gaz_capita","oil_capita","coal_capita","urban_pop")

data0 <- filter_at(data0, vars(dropna_cols), all_vars(!is.na(.)))

data0 <- data0 %>%
  group_by(country) %>%
  filter(oil_capita != 0) %>%
  filter(gaz_capita != 0) %>%
  filter(coal_capita != 0) %>%
 ungroup()

data0 <- mutate(data0, 
                C02_capita_adj = C02_capita + 1)

#graphs
ggplot(data0, aes(x = oil_capita)) +
  geom_histogram(bins = 30, fill = "blue", color = "black", alpha = 0.7) +
  labs(title = "Annual oil consumption per capita in kWh", x = "Oil", y = "Frequency") +
  theme_minimal()

ggplot(data0, aes(x = gaz_capita)) +
  geom_histogram(bins = 30, fill = "blue", color = "black", alpha = 0.7) +
  labs(title = "Annual gas consumption per capita in kWh", x = "Gas", y = "Frequency") +
  theme_minimal()

ggplot(data0, aes(x = coal_capita)) +
  geom_histogram(bins = 30, fill = "blue", color = "black", alpha = 0.7) +
  labs(title = "Annual coal consumption per capita in kWh", x = "Coal", y = "Frequency") +
  theme_minimal()

data0 <- mutate(data0,
                share_urban = urban_pop / population)

ggplot(data0, aes(x = share_urban)) +
  geom_histogram(bins = 30, fill = "blue", color = "black", alpha = 0.7) +
  labs(title = "Share of urban citizens", x = "share", y = "Frequency") +
  theme_minimal()

work0 <- data0 %>%
  arrange(country, year)  %>%
  group_by(country) %>%
  mutate(
    ln_C02_capita_adj = log(C02_capita_adj),
    ln_GDP_capita = log(GDP_capita),
    ln_oil_capita = log(oil_capita),
    ln_gaz_capita = log(gaz_capita),
    ln_coal_capita = log(coal_capita)
  ) %>%
  ungroup()

work0 <- work0 %>%
  arrange(country, year)  %>%
  group_by(country) %>%
  mutate(
    dln_C02_capita_adj = ln_C02_capita_adj - lag(ln_C02_capita_adj),
    dln_GDP_capita = ln_GDP_capita - lag(ln_GDP_capita),
    dshare_urban = share_urban - lag(share_urban),
    dln_oil_capita = ln_oil_capita - lag(ln_oil_capita),
    dln_gaz_capita = ln_gaz_capita - lag(ln_gaz_capita),
    dln_coal_capita = ln_coal_capita - lag(ln_coal_capita)
  ) %>%
  ungroup()

fe_reg_all_w <- feols(ln_C02_capita_adj ~ ln_GDP_capita + share_urban + ln_coal_capita +
                        ln_oil_capita + ln_gaz_capita| year + country,
                      data = work0,
                      vcov = NW(24))
summary(fe_reg_all_w)

models2 <- list("time&country FE" = fe_reg_all,
                "time&country FE + confounders" = fe_reg_all_w)

coef_labels <- c(
  "(Intercept)" = "Constant",       
  "ln_GDP_capita" = "log of GDP per capita",
  "share_urban" = "share of urban citizens",
  "ln_coal_capita" = "log of coal consumption per capita",
  "ln_oil_capita" = "log of oil consumption per capita",
  "ln_gaz_capita" = "log of gas consumption per capita"
)

modelsummary(models2, stars = TRUE,
             title = "regression models",
             coef_map = coef_labels)

#Cross-section OLS for the year 2005
subset_data0_2005 <- subset(work0, year == '2005')

reg_2005 <- lm(ln_C02_capita_adj ~ ln_GDP_capita + share_urban + ln_gaz_capita, data = subset_data0_2005)
summary(reg_2005)

#First difference model, with time trend, 2 year lags
fe_reg_c2 <- feols(dln_C02_capita_adj ~ l(dln_GDP_capita,0:2) + dshare_urban + dln_gaz_capita + dln_coal_capita + dln_oil_capita| year,
                 data = work0,
                 vcov = NW(24))
summary(fe_reg_c2)

#Fixed effects model with time and country fixed effects
fe_reg_c_all <- feols(dln_C02_capita_adj ~ dln_GDP_capita + dshare_urban + dln_gaz_capita + dln_coal_capita + dln_oil_capita| year + country,
                 data = work0,
                 vcov = NW(24))
summary(fe_reg_c_all)



models3 <- list("2 lags with time FE" = fe_reg_c2)

coef_labels <- c(
  "(Intercept)" = "Constant",       
  "ln_GDP_capita" = "Log of GDP per capita",
  "dln_GDP_capita" = "FD log of GDP in t",
  "l(dln_GDP_capita, 1)" = "FD log of GDP in t+1",
  "l(dln_GDP_capita, 2)" = "FD log of GDP in t+2",
  "share_urban" = "Share of urban population",
  "dshare_urban" = "FD Share of urban population in t",
  "l(dshare_urban, 1)" = "FD Share of urban population in t+1",
  "l(dshare_urban, 2)" = "FD Share of urban population in t+2",
  "ln_gaz_capita" = "Log of gas consumption per capita",
  "dln_gaz_capita" = "FD gas consumption per capita in t",
  "l(dln_gaz_capita, 1)" = "FD gas consumption per capita in t+1",
  "l(dln_gaz_capita, 2)" = "FD gas consumption per capita in t+2",
  "dln_coal_capita" = "FD coal consumption per capita in t",
  "l(dln_coal_capita, 1)" = "FD coal consumption per capita in t+1",
  "l(dln_coal_capita, 2)" = "FD coal consumption per capita in t+2",
  "dln_oil_capita" = "FD oil consumption per capita in t",
  "l(dln_oil_capita, 1)" = "FD oil consumption per capita in t+1",
  "l(dln_oil_capita, 2)" = "FD oil consumption per capita in t+2"
)

modelsummary(models3, stars = TRUE,
             title = "regression models with confounders",
             coef_map = coef_labels)


#-------------------------------------------------------------------------------
color <- c("blue", "red", "black")

work0$year <- as.numeric(work0$year)

lnip_THA <- ggplot(data=work0, aes(x = year)) +
  geom_line(data = subset(work0, country == 'Austria'), aes(y = ln_GDP_capita), 
            color = "blue", size = 0.6) +
  geom_line(data = subset(work0, country == 'Austria'), aes(y = C02_capita_adj), 
            color = "red", size = 0.6) +
  theme_classic() +
  xlab("Year") +
  ylab("Log values") +
  scale_x_continuous(breaks = seq(1992, 2022, by = 3)) +
  coord_cartesian(ylim = c(8, 10.7))

lnip_THA

