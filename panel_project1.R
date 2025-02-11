rm(list=ls())

library(tidyverse)
library(readxl)
library(plyr)
library(haven)
library(fixest)
library(ggplot2) 
library(dplyr)     
library(lubridate) 


getwd()
setwd("C:/Users/Пользователь/Desktop/MA1y/Data_Analysis_4/Project_panel_reg")

df0 <- read_excel("usa-imports.xls", sheet = "FRED Graph", skip = 10)

colnames(df0) <- c("date", "usa_imp_sa")

df0 <- df0 %>% mutate(year = as.Date(as.character(date),"%Y-%m-%d") %>% format("%Y"),
                    month = as.Date(as.character(date),"%Y-%m-%d") %>% format("%m"),
                    time = as.Date(as.character(date), "%Y-%m-%d") %>% format("%Y-%m")) %>% 
  select(c("time", "year", "month", "usa_imp_sa"))

df0 %>% write.csv("usa-imports.csv", row.names=FALSE)

#-------------------------------------------------------------------------------
#read_delim affords to run big data files
df <- read_delim("worldbank-monthly-asia-2019_long.csv",
                 delim = ",",
                 quote = '"',)
df <- df %>% select(c("Series", "Country", "Country Code", "Time", "Value"))

#making lower letters
colnames(df) <- tolower(names(df))

#replace of a space in column names for ""
colnames(df) <- str_replace_all(colnames(df), " ","")

# create time
df <- separate(df, time, c("year", "month"), sep = "M", remove = F)
df <- drop_na(df, month)
df <- mutate(df, year = as.integer(year), month = as.integer(month))

str(df)

df <- filter(df, year > 1990) 

# rename variables
#count(df, series, sort = T)

series_recode <- c("Industrial Production, constant US$,,," = "ind_prod_const",
                   "CPI Price, seas. adj.,,," = "cpi_sa",
                   "Exchange rate, new LCU per USD extended backward, period average,," = "exchnage_rate_vs_usd",
                   "Industrial Production, constant US$, seas. adj.,," = "ind_prod_const_sa",
                   "Nominal Effecive Exchange Rate,,,," = "exchange_rate_neer",
                   "Real Effective Exchange Rate,,,," = "exchange_rate_reer",
                   "CPI Price, % y-o-y, not seas. adj.,," = "cpi_yoy_nsa")

df <- mutate(df, series = recode(series, !!!series_recode))

# tidy data: have variables as columns (pivot_wider transforms long dataset to wide dataset)
#names_from becomes new columns
#values_from becomes new values
df <- pivot_wider(df, names_from = series, values_from=value)

#delete time column
df <- select(df, -c(time))

dropna_cols <- c("ind_prod_const","exchnage_rate_vs_usd","cpi_sa","cpi_yoy_nsa",
                 "ind_prod_const_sa","exchange_rate_neer","exchange_rate_reer")

#vars() captures columns from the list dropna_cols
#any_vars keeps the non-na values in all columns
#if there is at least one na in a raw, filter_at() removes the raw
df <- filter_at(df, vars(dropna_cols), any_vars(!is.na(.)))

df <- mutate(df, time = paste(as.character(year),"-",month, "-01",sep=""))

df <- mutate(df, time = format(as.Date(time, "%Y-%m-%d"), "%Y-%m"))

df <- cbind(select(df, c("time", "year", "month", "country", "countrycode")),
            select(df, -c("time", "year", "month", "country", "countrycode")))

write.csv(df, "asia-indprod_tidy.csv", row.names=FALSE)


#Merging two data sets

df_right <- read_csv("asia-indprod_tidy.csv")

df_left <- read_csv("usa-imports.csv")

merged <- merge(df_right,df_left,by=c("time","year","month"),all=FALSE)

merged <- merged %>% mutate(month = as.integer(month))

merged %>% write.csv("asia-industry_tidy.csv", row.names=FALSE)



#---------------------------------Analysis--------------------------------------
raw <- read_csv('asia-industry_tidy.csv')
data <- filter(raw, year >= 1998 & !(year==1998 & month==1) & !(year==2018 & month > 4))


data %>%
  group_by(countrycode) %>%
  dplyr::mutate(mean=mean(ind_prod_const), 
                std.dev=sd(ind_prod_const),
                freq = n()) %>%
  drop_na() %>%
  mutate(mean=formatC(mean, format = 'e', digits=3),
         std.dev=formatC(std.dev, format = 'e', digits=3)) %>% # scientific format
  arrange(countrycode) %>%
  distinct(countrycode, mean, std.dev, freq) %>%
  dplyr::select(countrycode, mean, std.dev, freq)


# feature engineering
# create two time vars temp1 and temp2. If countrycode is USA or CHN, the index
# production should be embedded, otherwise 0. Then create two other vars:
# usa_ip_sa and chn_ip_sa, they take max values of temp1/temp2 vars grouped 
# by 'time' variable 
data <- data %>%
  group_by(time) %>%
  mutate(temp1 = ifelse(countrycode=='USA',ind_prod_const_sa, 0), 
         usa_ip_sa = max(temp1), 
         temp2 = ifelse(countrycode == 'CHN', ind_prod_const_sa, 0), 
         chn_ip_sa = max(temp2)) %>%
  dplyr::select(-temp1, -temp2)

#creating log values
data <- data %>%
  mutate(ln_ip = log(ind_prod_const_sa),
         ln_usa_ip=log( usa_ip_sa),
         ln_chn_ip= log( chn_ip_sa),
         ln_usa_imports= log (usa_imp_sa),
         ln_er_usd= log(exchnage_rate_vs_usd)) %>%
  filter(!is.na(ln_ip))

# keep countries of choice
data <-  data %>%
  filter(countrycode == 'MYS' | countrycode == 'PHL' | countrycode == 'SGP' | countrycode == 'THA')

data %>%
  group_by(countrycode) %>%
  count()

# panel setup
# new factor variables cc with values in labels
data <- data %>%
  mutate(
    cc = factor(countrycode, levels = c("THA", "MYS", "PHL", "SGP"), labels = c("Thailand" ,"Malaysia", "Philippines", "Singapore")),
    date = lubridate::make_date(year, month, 1)
  )

# lagged variables
work <- data %>%
  arrange(cc, date)  %>%
  group_by(cc) %>%
  mutate(
    dln_ip = ln_ip - lag(ln_ip),
    dln_usa_ip = ln_usa_ip - lag(ln_usa_ip),
    dln_chn_ip = ln_chn_ip - lag(ln_chn_ip),
    dln_usa_imports = ln_usa_imports - lag(ln_usa_imports),
    ddln_usa_imports = dln_usa_imports - lag(dln_usa_imports),
    dln_er_usd = ln_er_usd - lag(ln_er_usd)
  ) %>%
  ungroup()

work %>%
  group_by(countrycode) %>%
  dplyr::summarise(mean=mean(dln_ip, na.rm = TRUE))


work %>%
  filter(countrycode == 'THA') %>%
  group_by(year) %>%
  count()  %>%
  ungroup()

work %>%
  ungroup() %>%
  filter(countrycode == 'THA') %>%
  dplyr::summarise(mean = mean(dln_ip, na.rm =TRUE))

# create TS graphs
color <- c("blue", "red", "black")

lnip_THA <- ggplot(data=work, aes(x=as.Date(date), y=ln_ip))+
  geom_line(data=subset(work[work$country=='Thailand', ]), color=color[1], size=0.6) +
  theme_classic() +
  xlab("Date (month)") +
  ylab("ln(Thai industrial production, bn US dollars)") +
  coord_cartesian(ylim=c(22.4, 23.4), xlim=as.Date(c('1998-01-01 UTC', '2018-01-01 UTC'))) +
  scale_x_date(date_labels = '%b%Y', 
               breaks = as.Date(c("1998-01-01 UTC","2002-01-01 UTC",
                                  "2006-01-01 UTC", "2010-01-01 UTC",
                                  "2014-01-01 UTC","2018-01-01 UTC"))) +
  geom_vline(xintercept = as.Date('2008-11-01 UTC'), linetype="dashed",  color=color[3], size=0.5)+
  annotate('text', size=2, color=color[3], label='2008-09 crisis', y=22.7, x=as.Date('2008-07-01 UTC'), angle=90) +
  geom_vline(xintercept = as.Date('2011-10-01 UTC'), linetype="dashed",  color=color[3], size=0.5)+
  annotate('text', size=2, color=color[3], label='2011-12 flood', y=22.68, x=as.Date('2011-06-01 UTC'), angle=90)
lnip_THA
#save_fig("ch23_lnip_THA", output, "small")
#save_fig("ch23-figure-1a-thai-lnip", output, "small")



dlnip_THA <- ggplot(data=work, aes(x=as.Date(date), y=dln_ip))+
  geom_line(data=subset(work[work$country=='Thailand', ]), color=color[1], size=0.6) +
  theme_classic() +
  xlab("Date (month)") +
  ylab('Change ln(Thai industrial prod., bn US dollars)') +
  coord_cartesian(ylim=c(-0.3, 0.2), xlim=as.Date(c('1998-01-01 UTC', '2018-01-01 UTC'))) +
  scale_x_date(date_labels = '%b%Y', 
               breaks = as.Date(c("1998-01-01 UTC","2002-01-01 UTC",
                                  "2006-01-01 UTC", "2010-01-01 UTC",
                                  "2014-01-01 UTC","2018-01-01 UTC"))) +
  geom_vline(xintercept = as.Date('2008-11-01 UTC'), linetype="dashed",  color=color[3], size=0.5)+
  annotate('text', size=2, color=color[3],  label='2008-09 crisis', y=-0.2, x=as.Date('2008-12-01 UTC'), angle=90) +
  geom_vline(xintercept = as.Date('2011-10-01 UTC'), linetype="dashed",  color=color[3], size=0.5)+
  annotate('text', size=2, color=color[3],  label='2011-12 flood', y=-0.2, x=as.Date('2011-05-01 UTC'), angle=90)
dlnip_THA
#save_fig("ch23_dlnip_THA", output, "small")


lnusaimp <- ggplot(data=work, aes(x=as.Date(date), y=ln_usa_imports))+
  geom_line() +
  theme_classic() +
  xlab("Date (month)") +
  ylab("ln(USA imports, bn US dollars)") +
  scale_y_continuous(breaks=c(seq(11.2, 12.2, 0.2))) +
  coord_cartesian(ylim=c(11.2, 12.25), xlim=as.Date(c('1998-01-01 UTC', '2018-01-01 UTC'))) +
  scale_x_date(date_labels = '%b%Y', 
               breaks = as.Date(c("1998-01-01 UTC","2002-01-01 UTC",
                                  "2006-01-01 UTC", "2010-01-01 UTC",
                                  "2014-01-01 UTC","2018-01-01 UTC"))) +
  geom_vline(xintercept = as.Date('2008-11-01 UTC'), linetype="dashed",  color=color[3], size=0.5)+
  annotate('text',size=2, color=color[3], label='2008-09 crisis', y=11.5, x=as.Date('2008-07-01 UTC'), angle=90)
lnusaimp
#save_fig("ch23_lnusaimp", output, "small")
#save_fig("ch23-figure-1b-usa-lnimp", output, "small")


dlnusaimp <- ggplot(data=work, aes(x=as.Date(date), y=dln_usa_imports))+
  geom_line() +
  theme_classic() +
  xlab("Date (month)") +
  ylab('Change in ln(USA imports, bn US dollars)') +
  coord_cartesian(ylim=c(-0.15, 0.1), xlim=as.Date(c('1998-01-01 UTC', '2018-01-01 UTC'))) +
  scale_x_date(date_labels = '%b%Y', 
               breaks = as.Date(c("1998-01-01 UTC","2002-01-01 UTC",
                                  "2006-01-01 UTC", "2010-01-01 UTC",
                                  "2014-01-01 UTC","2018-01-01 UTC"))) +
  geom_vline(xintercept = as.Date('2008-11-01 UTC'), linetype="dashed",  color=color[3], size=0.5)+
  annotate('text', size=2, color=color[3], label='2008-09 crisis', y=-0.10, x=as.Date('2008-06-01 UTC'), angle=90)
dlnusaimp


# REGRESSIONS

# Serial correlation matters because it may lead to biased standard error estimates.
# We recommended two ways to address this problem: estimate Newey–West standard errors
# or include the lag of the dependent variable in the regression. (Used here.)

# Set panel properties:
setFixest_estimation(panel.id = ~country + time)

# Thailand (using Newey-West SE)
thai_reg <- feols(dln_ip ~ l(dln_usa_imports,0:4)+l(dln_ip,1:2),
                  data = filter(work, countrycode=="THA"),
                  vcov = NW(24))
summary(thai_reg)


# long-term coeff, lagged dy, countries separately
lt_formula <- formula(dln_ip ~ l(dln_usa_imports,4)+l(ddln_usa_imports,0:3)+l(dln_ip,1))

tha_reg_lt <- feols(lt_formula, data = filter(work, countrycode=="THA"), vcov = 'iid' )
mys_reg_lt <- feols(lt_formula, data = filter(work, countrycode=="MYS"), vcov = 'iid' )
phl_reg_lt <- feols(lt_formula, data = filter(work, countrycode=="PHL"), vcov = 'iid' )
sgp_reg_lt <- feols(lt_formula, data = filter(work, countrycode=="SGP"), vcov = 'iid' )

# long-term coeff, lagged dy, countries pooled
pooled_reg_lt <- feols(dln_ip ~ l(dln_usa_imports,4) + l(ddln_usa_imports,0:3) 
                       + l(dln_ip,1) + cc , data = work, vcov = 'iid' )

summary(pooled_reg_lt)
#ch23-table-1-asia-reg



etable(tha_reg_lt,mys_reg_lt,phl_reg_lt,sgp_reg_lt,pooled_reg_lt,
       headers = c("Thailand","Malaysia","Philippines","Singapore","Pooled"),
       drop = 'ddln_usa', digits = 3, fitstat = c("n","r2"),
       dict = c(
         "l(dln_usa_imports,4)" = "USA imports log change, cumulative coeff.",
         "l(dln_ip,1)" = "Industrial production log change, lag",
         "ccMalaysia" = "Malaysia", 
         "ccPhilippines" = "Philippines", 
         "ccSingapore"="Singapore", 
         "(Intercept)" = "Constant") )




