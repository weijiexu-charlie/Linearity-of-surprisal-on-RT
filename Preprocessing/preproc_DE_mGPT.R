library(Rmisc)
library(tidyverse)
library(stringr)
library(scales)
library(grid)
library(ggpubr)
library(MASS)
library(lmerTest)
library(lme4)
library(stats)
library(modelr)
library(plotrix)
library(mgcv)
library(hexbin)
library(formattable)

rm(list=ls())

german.rt <- read.csv('German/PSC_EM.fp.sf.VWR.csv')

scaling_var <- function(data){
  # The input data is a vector
  data <- as.numeric(data)
  (data - mean(data,na.rm=TRUE))/sd(data,na.rm=TRUE)
}

german.meta <- read.csv('German/german_meta_logp_logfreq_mGPT.csv') %>%
  mutate(logp = as.numeric(logp),
         wlen = as.numeric(nchar(word)))


# Get LM Perplexity
num_lm_tokens <- german.meta %>%
  dplyr::select(c('sn','lm_tokens')) %>%
  distinct() %>%
  dplyr::select(c('lm_tokens')) %>%
  sum()
exp(-sum(german.meta$logp, na.rm=TRUE) / num_lm_tokens)


german.meta <- german.meta %>%
  mutate(logp.s=scaling_var(german.meta$logp),
         logfreq.s=scaling_var(german.meta$logfreq),
         wlen.s=scaling_var(german.meta$wlen))

german.meta <- german.meta[
  order(german.meta[,1], german.meta[,2]),
]

german.meta <- german.meta %>%
  group_by(sn) %>%
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

german <- german.rt %>%
  dplyr::select(c('id','sn','nw','wn','word','dur')) %>%
  inner_join(german.meta) %>%
  mutate(RT = as.numeric(dur),
         subj = id) %>%
  filter(logp>-20) %>%
  filter(RT<2000) %>%
  filter(!str_detect(word, "\\p{P}"))    # Remove punctuation

german <- german[
  order(german[,1], german[,2], german[,4]),
]

# Get token number
german %>%
  dplyr::select(c('sn', 'wn', 'word')) %>%
  distinct() %>%
  nrow()  # 557 tokens

write.csv(german, "preproc_data/german_mGPT.csv", row.names=FALSE)

