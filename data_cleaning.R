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

data0 <- read_delim("co2_gdp_raw.csv",
                    delim = ",",
                    quote = '"',)

data1 <- read_delim("energy_raw.csv",
                    delim = ",",
                    quote = '"',)

data2 <- read_delim("population_raw.csv",
                    delim = ",",
                    quote = '"',)

data0 <- data0 %>% filter(Year >= 1992 & Year <= 2022)
data1 <- data1 %>% filter(Year >= 1992 & Year <= 2022)
data2 <- data2 %>% filter(Year >= 1992 & Year <= 2022)

data0 <- data0[0:7]
colnames(data0)
data0$`900793-annotations` <- NULL
data1 <- data1[0:6]
data2$`Rural population` <- NULL


data0 <- data0 %>%
  group_by(Entity) %>%
  filter(!any(is.na(Code) | is.na(`Annual CO₂ emissions (per capita)`) | is.na(`GDP per capita`))) %>%
  ungroup()  

data0$bind <- paste(data0$Code, data0$Year)
data1$bind <- paste(data1$Code, data1$Year)
data2$bind <- paste(data2$Code, data2$Year)

data0 <- data0 %>%
  left_join(select(data1, bind, `Coal per capita (kWh)`, `Oil per capita (kWh)`,
                   `Gas per capita (kWh)`), by = "bind") %>%
  left_join(select(data2, bind, `Urban population`), by = "bind")

data0$bind <-  NULL

ready_data <- data0
colnames(ready_data)
ready_data <- ready_data %>% select(Entity, Code, Year, `Annual CO₂ emissions (per capita)`,
                        `GDP per capita`, `Population (historical)`, `Urban population`,
                        `Coal per capita (kWh)`, `Oil per capita (kWh)`, 
                        `Gas per capita (kWh)`)
colnames(ready_data) <- c("country", "code", "year", "C02_capita", "GDP_capita", "population", "urban_pop", "coal_capita", "oil_capita", "gaz_capita")

data0 <- read_delim("co2-emissions-vs-gdp.csv",
                    delim = ",",
                    quote = '"',)
colnames(data0) <- c("country", "code", "year", "C02_capita", "GDP_capita", "population", "urban_pop", "coal_capita", "oil_capita", "gaz_capita")

str(ready_data)

ready_data$coal_capita <- ifelse(is.na(ready_data$coal_capita), NA, round(ready_data$coal_capita, 0))
ready_data$oil_capita <- ifelse(is.na(ready_data$oil_capita), NA, round(ready_data$oil_capita, 0))
ready_data$gaz_capita <- ifelse(is.na(ready_data$gaz_capita), NA, round(ready_data$gaz_capita, 0))

write.csv(ready_data, "clean_data.csv")

