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

chinese.rt <- read.csv('Chinese/data.csv') %>%
  rename(sentence_id = sentence)

chinese.meta <- read.csv('Chinese/chinese_meta_logp_logfreq_mGPT.csv') %>%
  mutate(word_fixated = word_id)

# Get LM Perplexity
num_lm_tokens <- chinese.meta %>%
  dplyr::select(c('sentence_id','lm_tokens')) %>%
  distinct() %>%
  dplyr::select(c('lm_tokens')) %>%
  sum()
exp(-sum(chinese.meta$logp, na.rm=TRUE) / num_lm_tokens)


scaling_var <- function(data){
  # The input data is a vector
  data <- as.numeric(data)
  (data - mean(data,na.rm=TRUE))/sd(data,na.rm=TRUE)
}

chinese.meta <- chinese.meta %>%
  mutate(logp = as.numeric(logp),
         wlen = as.numeric(nchar(word)))
chinese.meta <- chinese.meta %>%
  mutate(logp.s=scaling_var(chinese.meta$logp),
         logfreq.s=scaling_var(chinese.meta$logfreq),
         wlen.s=scaling_var(chinese.meta$wlen))

chinese.meta <- chinese.meta[
  order(chinese.meta[,1], chinese.meta[,2]),
]

chinese.meta <- chinese.meta %>%
  group_by(sentence_id) %>%
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


chinese.rt <- chinese.rt %>%
  group_by(subject, sentence_id, word_fixated) %>%
  mutate(fix_count=n()) %>%
  mutate(pass = ifelse(fix_count==1, 1, 99)) %>%
  ungroup() %>%
  group_by(subject, sentence_id) %>%
  mutate(fix_order=row_number()) %>%
  ungroup()

chinese.rt2 <- chinese.rt %>%
  filter(fix_count>1) %>%
  group_by(subject, sentence_id, word_fixated) %>%
  mutate(fix_order_wi_multi=row_number(),   # number of time the current word occurred so far
         pass = replace(pass, 1, 1),
         pass1.idx = fix_order[1]) %>%   
  mutate(pass1.dist = fix_order - pass1.idx) %>%   # distance between current pos and the first-pass pos
  mutate(if_pass1 = fix_order_wi_multi-pass1.dist) %>%
  # if if_pass1 is 1, there is no intervening words between current pos and first-pass pos,
  # otherwise, if_pass should be less than 1, since pass1.dist would increase faster than fix_order_wi_multi
  mutate(pass = ifelse(if_pass1==1, 1, 99)) %>%
  filter(pass==1) %>%
  ungroup() %>%
  dplyr::select(-c('fix_order_wi_multi', 'pass1.idx', 'pass1.dist', 'if_pass1'))

chinese.rt <- chinese.rt %>%
  filter(fix_count==1) %>%
  rbind(chinese.rt2) %>%
  group_by(subject, sentence_id, word_fixated) %>%
  summarise(RT = sum(fix_duration),
            pass = pass,
            subj = subject) %>%
  ungroup() %>%
  distinct()

chinese <- chinese.rt %>%
  inner_join(chinese.meta) %>%  # data points in chinese.rt where word_fixated==0 are dropped
  filter(logp>-20) %>%
  filter(RT<2000) %>%
  filter(!str_detect(word, "\\p{P}"))    # Remove punctuations

chinese <- chinese[
  order(chinese[,1], chinese[,2], chinese[,3]),
]

# Get token number
# chinese %>%
#   dplyr::select(c('sentence_id', 'word_fixated', 'word')) %>%
#   distinct() %>%
#   nrow()  # 1521 tokens

write.csv(chinese, "preproc_data/chinese_mGPT.csv", row.names=FALSE)







