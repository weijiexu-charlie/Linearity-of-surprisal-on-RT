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

japanese.rt <- read.csv("Japanese/main_data/fpt.csv")

japanese.meta <- read.csv('Japanese/main_data/japanese_meta_logp_logfreq_mono.csv') 

# Get LM Perplexity
num_lm_tokens <- japanese.meta %>%
  dplyr::select(c('article','lm_tokens')) %>%
  distinct() %>%
  dplyr::select(c('lm_tokens')) %>%
  sum()
exp(-sum(japanese.meta$logp, na.rm=TRUE) / num_lm_tokens)

scaling_var <- function(data){
  # The input data is a vector
  data <- as.numeric(data)
  (data - mean(data,na.rm=TRUE))/sd(data,na.rm=TRUE)
}

japanese.meta <- japanese.meta %>%
  mutate(logp = as.numeric(logp),
         wlen = as.numeric(nchar(surface)))
japanese.meta <- japanese.meta %>%
  mutate(logp.s=scaling_var(japanese.meta$logp),
         logfreq.s=scaling_var(japanese.meta$logfreq),
         wlen.s=scaling_var(japanese.meta$wlen))

japanese.meta <- japanese.meta[
  order(japanese.meta[,1], japanese.meta[,2]),
]

japanese.meta <- japanese.meta %>%
  group_by(article) %>%
  mutate(logp1=lag(logp, 1),
         logp2=lag(logp, 2),
         logp3=lag(logp, 3)) %>%
  mutate(wlen1=lag(wlen, 1),
         wlen2=lag(wlen, 2),
         wlen3=lag(wlen, 3)) %>%
  mutate(logfreq1=lag(logfreq, 1),
         logfreq2=lag(logfreq, 2),
         logfreq3=lag(logfreq, 3)) %>%
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

japanese <- japanese.rt %>%
  dplyr::select(c('subj','article','time','surface','sample','length')) %>%
  mutate(RT = as.numeric(time)) %>%
  group_by(subj, article) %>%
  mutate(tokenN = row_number()) %>%
  ungroup() %>%
  inner_join(japanese.meta) 

japanese <- japanese %>%
  filter(logp>-20) %>%
  filter(RT<2000) %>%
  filter(!str_detect(surface, "\\p{P}"))    # Remove punctuations

# Get token number
japanese %>%
  dplyr::select(c('article', 'tokenN', 'surface')) %>%
  distinct() %>%
  nrow()  # 970 tokens

write.csv(japanese, "preproc_data/japanese_mono.csv", row.names=FALSE)

