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

dutch.rt <- read.csv("Dutch/L1ReadingData.csv")

dutch.meta <- read.csv('Dutch/dutch_meta_logp_logfreq_mGPT.csv') %>%
  mutate(WORD_ID = IA_ID)

# Get LM Perplexity
num_lm_tokens <- dutch.meta %>%
  dplyr::select(c('SUBTEXT_ID','lm_tokens')) %>%
  distinct() %>%
  dplyr::select(c('lm_tokens')) %>%
  sum()
exp(-sum(dutch.meta$logp, na.rm=TRUE) / num_lm_tokens)


scaling_var <- function(data){
  # The input data is a vector
  data <- as.numeric(data)
  (data - mean(data,na.rm=TRUE))/sd(data,na.rm=TRUE)
}

dutch <- dutch.rt %>%
  inner_join(dutch.meta, by="WORD_ID") %>%
  mutate(WORD = WORD.y)
# WORD.x all end with a space. Use the word in WORD.y as reference instead.

dutch <- dutch %>% 
  mutate(RT = as.numeric(WORD_FIRST_FIXATION_DURATION),
         part = as.numeric(PART),
         trial = as.numeric(TRIAL),
         word.id.within.trial = as.numeric(WORD_ID_WITHIN_TRIAL),
         logp = as.numeric(logp),
         wlen = nchar(WORD),
         subj = PP_NR) %>%
  dplyr::select(c('subj','part','trial','word.id.within.trial','WORD','wlen','RT','logp','logfreq'))

dutch <- dutch[
  order(dutch[,1], dutch[,2], dutch[,3], dutch[,4]),
]

# Not grouped since the whole text is from a book.
dutch <- dutch %>%
  mutate(logRT = log(RT),
         logp.s=scaling_var(dutch$logp),
         logfreq.s=scaling_var(dutch$logfreq),
         wlen.s=scaling_var(dutch$wlen)) %>%
  group_by(subj) %>%
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
         logp3.s=lag(logp.s, 3),
         logp4.s=lag(logp.s, 4)) %>%
  mutate(logfreq1.s=lag(logfreq.s, 1),
         logfreq2.s=lag(logfreq.s, 2),
         logfreq3.s=lag(logfreq.s, 3)) %>%
  mutate(wlen1.s=lag(wlen.s, 1),
         wlen2.s=lag(wlen.s, 2),
         wlen3.s=lag(wlen.s, 3),
         wlen4.s=lag(wlen.s, 4)) %>%
  ungroup()

dutch <- dutch %>%
  filter(logp>-20) %>%
  filter(RT<2000) %>%
  filter(!str_detect(WORD, "\\p{P}"))    # Remove punctuation

# Get token number
dutch %>%
  dplyr::select(c('part', 'trial', 'word.id.within.trial','WORD')) %>%
  distinct() %>%
  nrow()  # 58302 tokens

write.csv(dutch, "preproc_data/dutch_mGPT.csv", row.names=FALSE)



