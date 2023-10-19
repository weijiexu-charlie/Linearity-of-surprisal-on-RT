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

big.dundee <- read.csv('English/big_dundee_et.csv')
raw.big.dundee <- read.csv('English/raw_dundee_eyemovements.csv')
dundee.meta <- read.csv('English/dundee_meta_logp_logfreq_mGPT.csv') %>%
  dplyr::select(c('TEXTID','WNUM','WORD','WLEN','logp','logfreq','SUBTEXTID','SUBTEXT_WNUM','lm_tokens'))



# Get LM Perplexity
num_lm_tokens <- dundee.meta %>%
  dplyr::select(c('TEXTID','SUBTEXTID','lm_tokens')) %>%
  distinct() %>%
  dplyr::select(c('lm_tokens')) %>%
  sum()
exp(-sum(dundee.meta$logp, na.rm=TRUE) / num_lm_tokens)

scaling_var <- function(data){
  # The input data is a vector
  data <- as.numeric(data)
  (data - mean(data,na.rm=TRUE))/sd(data,na.rm=TRUE)
}

big.dundee2 <- big.dundee %>%
  filter(PASS==1) %>%
  group_by(TEXTID, WNUM, SUBJECT) %>%
  summarise(FDUR = sum(FDUR),
            WORD = WORD,
            PASS = PASS) %>%
  ungroup() %>%
  distinct()

# big.dundee does not have RT for WNUM=1
dundee <- big.dundee2 %>%
  dplyr::select(c('TEXTID','WNUM','SUBJECT','FDUR','PASS')) %>%
  inner_join(dundee.meta) %>%
  mutate(TEXTID = as.numeric(TEXTID),
         WNUM = as.numeric(WNUM),
         RT = as.numeric(FDUR),
         logp = as.numeric(logp),
         wlen = as.numeric(WLEN),
         subj=as.factor(SUBJECT))

dundee <- dundee[
  order(dundee[,3], dundee[,1], dundee[,2] ),
]

dundee <- dundee %>%
  mutate(logRT = log(RT),
         logp.s=scaling_var(dundee$logp),
         logfreq.s=scaling_var(dundee$logfreq),
         wlen.s=scaling_var(dundee$wlen)) %>%
  group_by(subj, TEXTID) %>%
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

dundee <- dundee %>%
  filter(logp>-20) %>%
  filter(RT<2000) %>%
  filter(!str_detect(WORD, "\\p{P}"))    # Remove punctuation

# Get token number
dundee %>%
  dplyr::select(c('TEXTID', 'WNUM', 'WORD')) %>%
  distinct() %>%
  nrow()  # 24679 tokens


# write.csv(dundee, "preproc_data/english.csv", row.names=FALSE)
write.csv(dundee, "preproc_data/english_mGPT.csv", row.names=FALSE)



