## Get packages ####
library(tidyverse)
library(lubridate)
library(zoo)

## get data ####
ecb_speech_corpus_clean <- readRDS("~/GitHub/PhDEcon107/codes/ecb_speech_corpus_clean.rds")
sent_all <- readRDS("~/GitHub/PhDEcon107/backup/sent_all.rds")

# get ICC ####
icc <- read_delim("GitHub/PhDEcon107/data_new/icc.csv", 
                  delim = ";", escape_double = FALSE, trim_ws = TRUE)
# wrangling ####
names(icc)<- c('DATE', 'icc')

icc$DATE <- as.Date(paste0(icc$DATE, "-01"))
icc$icc <- as.numeric(gsub(",", ".", icc$icc))

# get stamp ====
icc <- icc %>% 
  filter(DATE >= as.Date('2005-06-01'))

sent_all$icc <- icc$icc
sent_all$icc_diff <- c(NA, diff(icc$icc))
