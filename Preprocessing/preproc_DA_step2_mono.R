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

danish.rt <- read.csv('Danish/danish_rt.csv')

danish.meta <- read.csv('Danish/danish_meta_logp_logfreq_mono.csv')

# Get LM Perplexity
num_lm_tokens <- danish.meta %>%
  dplyr::select(c('speechId','subtextId','lm_tokens')) %>%
  distinct() %>%
  dplyr::select(c('lm_tokens')) %>%
  sum()
exp(-sum(danish.meta$logp, na.rm=TRUE) / num_lm_tokens)


scaling_var <- function(data){
  # The input data is a vector
  data <- as.numeric(data)
  (data - mean(data,na.rm=TRUE))/sd(data,na.rm=TRUE)
}

danish.meta <- danish.meta %>%
  mutate(logp = as.numeric(logp),
         wlen = as.numeric(nchar(word)))
danish.meta <- danish.meta %>%
  mutate(logp.s=scaling_var(danish.meta$logp),
         logfreq.s=scaling_var(danish.meta$logfreq),
         wlen.s=scaling_var(danish.meta$wlen))

danish.meta <- danish.meta[
  order(danish.meta[,1], danish.meta[,2], danish.meta[,3], danish.meta[,4]),
]

danish.meta <- danish.meta %>%
  group_by(speechId) %>%
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

danish <- danish.rt %>%
  dplyr::select(c('subj','part','trialId','speechId','paragraphId','sentenceId','wordId', 'word_first_pass_dur')) %>%
  inner_join(danish.meta) %>%
  mutate(RT = as.numeric(word_first_pass_dur)) %>%
  filter(logp>-20) %>%
  filter(RT<2000) %>%
  filter(!str_detect(word, "\\p{P}"))    # Remove punctuation

danish <- danish[
  order(danish[,1], danish[,4], danish[,5], danish[,6], danish[,7]),
]

# Get token number
danish %>%
  dplyr::select(c('speechId','paragraphId','sentenceId','wordId','word')) %>%
  distinct() %>%
  nrow()  # 26454 tokens

# write.csv(danish, "preproc_data/danish.csv", row.names=FALSE)
write.csv(danish, "preproc_data/danish_mono.csv", row.names=FALSE)



