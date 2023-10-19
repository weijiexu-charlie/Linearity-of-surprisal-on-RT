library(Rmisc)
library(tidyverse)
library(stringr)
library(scales)
library(grid)
library(ggpubr)
library(MASS)
library(lme4)
library(stats)
library(modelr)
library(plotrix)
library(mgcv)
library(hexbin)
library(formattable)

rm(list=ls())

russian.rt <- read.csv("Russian/data.csv", sep = "\t", encoding = "UTF-8", 
                       na.strings = c("NA"), header = TRUE)

scaling_var <- function(data){
  # The input data is a vector
  data <- as.numeric(data)
  (data - mean(data,na.rm=TRUE))/sd(data,na.rm=TRUE)
}

russian.meta <- read.csv('Russian/russian_meta_logp_logfreq_mono.csv') %>%
  mutate(logp = as.numeric(logp),
         wlen = nchar(word.id))

# Get LM Perplexity
num_lm_tokens <- russian.meta %>%
  dplyr::select(c('item.id','lm_tokens')) %>%
  distinct() %>%
  dplyr::select(c('lm_tokens')) %>%
  sum()
exp(-sum(russian.meta$logp, na.rm=TRUE) / num_lm_tokens)

russian.meta <- russian.meta %>%
  mutate(logp.s=scaling_var(russian.meta$logp),
         logfreq.s=scaling_var(russian.meta$logfreq),
         wlen.s=scaling_var(russian.meta$wlen))

russian.meta <- russian.meta[
  order(russian.meta[,1], russian.meta[,2]),
]

russian.meta <- russian.meta %>%
  group_by(item.id) %>%
  mutate(logp1=lag(logp, 1),
         logp2=lag(logp, 2),
         logp3=lag(logp, 3)) %>%
  mutate(logfreq1=lag(logfreq, 1),
         logfreq2=lag(logfreq, 2),
         logfreq3=lag(logfreq, 3)) %>%
  mutate(wlen1=lag(wlen, 1),
         wlen2=lag(wlen, 2),
         wlen3=lag(wlen, 3)) %>%
  mutate(logp1.s=lag(logp.s, 1),
         logp2.s=lag(logp.s, 2),
         logp3.s=lag(logp.s, 3)) %>%
  mutate(logfreq1.s=lag(logfreq.s, 1),
         logfreq2.s=lag(logfreq.s, 2),
         logfreq3.s=lag(logfreq.s, 3)) %>%
  mutate(wlen1.s=lag(wlen.s, 1),
         wlen2.s=lag(wlen.s, 2),
         wlen3.s=lag(wlen.s, 3)) %>%
  ungroup()

russian <- russian.rt %>%
  dplyr::select(c('item.id','word.serial.no','DATA_FILE','IA_FIRST_RUN_DWELL_TIME','word.id')) %>%
  inner_join(russian.meta) %>%
  mutate(RT = as.numeric(IA_FIRST_RUN_DWELL_TIME),
         sentence.id = as.numeric(item.id),
         wnum = as.numeric(word.serial.no),
         subj = DATA_FILE) %>%
  filter(logp>-20) %>%
  filter(RT<2000) %>%
  filter(!str_detect(word.id, "\\p{P}"))    # Remove punctuation

russian <- russian[
  order(russian[,3], russian[,1], russian[,2]),
]

# Get token number
russian %>%
  dplyr::select(c('item.id', 'word.serial.no', 'word.id')) %>%
  distinct() %>%
  nrow()  # 892 tokens

write.csv(russian, "preproc_data/russian_mono.csv", row.names=FALSE)

