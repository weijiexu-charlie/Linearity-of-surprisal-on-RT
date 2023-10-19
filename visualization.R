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

english <- read.csv("preproc_data/english_mGPT.csv") %>% 
  mutate(language = 'English')
english.mono <- read.csv("preproc_data/english_mono.csv") %>% 
  mutate(language = 'English')
dutch <- read.csv("preproc_data/dutch_mGPT.csv") %>% 
  mutate(language = 'Dutch')
dutch.mono <- read.csv("preproc_data/dutch_mono.csv") %>% 
  mutate(language = 'Dutch')
chinese <- read.csv("preproc_data/chinese_mGPT.csv") %>%
  mutate(language = 'Mandarin')
chinese.mono <- read.csv("preproc_data/chinese_mono.csv") %>%
  mutate(language = 'Mandarin')
german <- read.csv("preproc_data/german_mGPT.csv") %>%
  mutate(language = 'German')
german.mono <- read.csv("preproc_data/german_mono.csv") %>%
  mutate(language = 'German')
japanese <- read.csv("preproc_data/japanese_mGPT.csv") %>%
  mutate(language = 'Japanese')
japanese.mono <- read.csv("preproc_data/japanese_mono.csv") %>%
  mutate(language = 'Japanese')
danish <- read.csv("preproc_data/danish_mGPT.csv") %>% 
  mutate(language = 'Danish')
danish.mono <- read.csv("preproc_data/danish_mono.csv") %>% 
  mutate(language = 'Danish')
russian <- read.csv("preproc_data/russian_mGPT.csv") %>%
  mutate(language = 'Russian')
russian.mono <- read.csv("preproc_data/russian_mono.csv") %>%
  mutate(language = 'Russian')

control_english = lmer(RT ~ logfreq.s + logfreq1.s + logfreq2.s + wlen.s + wlen1.s + wlen2.s +
                         (logfreq.s + logfreq1.s + logfreq2.s + wlen.s + wlen1.s + wlen2.s|subj), data=na.omit(english), REML=FALSE)
control_dutch = lmer(RT ~ logfreq.s + logfreq1.s + logfreq2.s + logfreq3.s + wlen.s + wlen1.s + wlen2.s + wlen3.s +
                       (1|subj), data=na.omit(dutch), REML=FALSE)
control_chinese = lmer(RT ~ logfreq.s + wlen.s +
                         (logfreq.s + wlen.s|subj), data=chinese)
control_german = lmer(RT ~ logfreq.s + logfreq1.s + logfreq2.s + logfreq3.s + wlen.s + wlen1.s + wlen2.s + wlen3.s +
                        (logfreq.s + logfreq1.s + logfreq2.s + logfreq3.s + wlen.s + wlen1.s + wlen2.s + wlen3.s|subj), data=german)
control_japanese = lmer(RT ~ logfreq.s + wlen.s +
                          (wlen.s|subj), data=japanese)
control_danish = lmer(RT ~ logfreq.s + logfreq1.s + logfreq2.s + logfreq3.s + wlen.s + wlen1.s + wlen2.s + wlen3.s +
                        (logfreq.s + logfreq1.s + logfreq2.s + logfreq3.s + wlen.s + wlen1.s + wlen2.s + wlen3.s|subj), data=na.omit(danish), REML=FALSE)
control_russian = lmer(RT ~ logfreq.s + logfreq1.s + logfreq2.s + logfreq3.s + wlen.s + wlen1.s + wlen2.s + wlen3.s +
                         (logfreq.s + logfreq1.s + logfreq2.s + logfreq3.s + wlen.s + wlen1.s + wlen2.s + wlen3.s|subj), data=russian)

english2 <- english %>% add_residuals(control_english) %>%
  dplyr::select(c('language', 'logp', 'RT', 'resid'))
english2.mono <- english.mono %>% add_residuals(control_english) %>%
  dplyr::select(c('language', 'logp', 'RT', 'resid'))
dutch2 <- dutch %>% add_residuals(control_dutch) %>%
  dplyr::select(c('language', 'logp', 'RT', 'resid'))
dutch2.mono <- dutch.mono %>% add_residuals(control_dutch) %>%
  dplyr::select(c('language', 'logp', 'RT', 'resid'))
chinese2 <- chinese %>% add_residuals(control_chinese) %>%
  dplyr::select(c('language', 'logp', 'RT', 'resid'))
chinese2.mono <- chinese.mono %>% add_residuals(control_chinese) %>%
  dplyr::select(c('language', 'logp', 'RT', 'resid'))
german2 <- german %>% add_residuals(control_german) %>%
  dplyr::select(c('language', 'logp', 'RT', 'resid'))
german2.mono <- german.mono %>% add_residuals(control_german) %>%
  dplyr::select(c('language', 'logp', 'RT', 'resid'))
japanese2 <- japanese %>% add_residuals(control_japanese) %>%
  dplyr::select(c('language', 'logp', 'RT', 'resid'))
japanese2.mono <- japanese.mono %>% add_residuals(control_japanese) %>%
  dplyr::select(c('language', 'logp', 'RT', 'resid'))
danish2 <- danish %>% add_residuals(control_danish) %>%
  dplyr::select(c('language', 'logp', 'RT', 'resid'))
danish2.mono <- danish.mono %>% add_residuals(control_danish) %>%
  dplyr::select(c('language', 'logp', 'RT', 'resid'))
russian2 <- russian %>% add_residuals(control_russian) %>%
  dplyr::select(c('language', 'logp', 'RT', 'resid'))
russian2.mono <- russian.mono %>% add_residuals(control_russian) %>%
  dplyr::select(c('language', 'logp', 'RT', 'resid'))


data.mGPT <- english2 %>%
  rbind(dutch2) %>%
  rbind(chinese2) %>%
  rbind(german2) %>%
  rbind(japanese2) %>%
  rbind(danish2) %>%
  rbind(russian2)

data.mono <- english2.mono %>%
  rbind(dutch2.mono) %>%
  rbind(chinese2.mono) %>%
  rbind(german2.mono) %>%
  rbind(japanese2.mono) %>%
  rbind(danish2.mono) %>%
  rbind(russian2.mono)

meanplot = function(d, num_bins, alpha=.02, stat_method='lm') {
  d %>%
    mutate(logp_bin=cut_interval(logp, num_bins)) %>%
    # group_by(language, language_extra, model, logp_bin) %>%
    group_by(language, logp_bin) %>%
    summarise(logp=mean(logp, na.rm=TRUE),
              m=mean(RT, na.rm=TRUE),
              se=std.error(RT, na.rm=TRUE),
              upper=m+1.96*se,
              lower=m-1.96*se) %>%
    ungroup() %>%
    ggplot(aes(x=-logp, y=m, ymin=lower, ymax=upper)) + 
    #geom_point(data=d, aes(x=-logp/log(2), y=rt, ymin=0, ymax=0), alpha=alpha, color="darkblue") +
    stat_smooth(color="red", method=stat_method) +
    geom_errorbar(color="blue") + 
    geom_point(color="black") + 
    theme_bw() + 
    ylab("Reading Time (ms)") +
    xlab("Surprisal (bits)") 
}

meanplot_resid = function(d, num_bins, alpha=.02, stat_method='lm') {
  d %>%
    mutate(logp_bin=cut_interval(logp, num_bins)) %>%
    # group_by(language, language_extra, model, logp_bin) %>%
    group_by(language, logp_bin) %>%
    summarise(logp=mean(logp, na.rm=TRUE),
              m=mean(resid, na.rm=TRUE),
              se=std.error(RT, na.rm=TRUE),
              upper=m+1.96*se,
              lower=m-1.96*se) %>%
    ungroup() %>%
    ggplot(aes(x=-logp, y=m, ymin=lower, ymax=upper)) + 
    #geom_point(data=d, aes(x=-logp/log(2), y=rt, ymin=0, ymax=0), alpha=alpha, color="darkblue") +
    stat_smooth(color="red", method=stat_method) +
    geom_errorbar(color="blue") + 
    geom_point(color="black") + 
    theme_bw() + 
    ylab("Residual RT (ms)") +
    xlab("Surprisal (bits)") 
}

plot.RT.mGPT <- data.mGPT %>%
  meanplot(25) +
  facet_wrap(~language, scale="free_y", ncol=4) 
plot.RT.mGPT

plot.resid.mGPT <- data.mGPT %>%
  meanplot_resid(25) +
  facet_wrap(~language, scale="free_y", ncol=4) 
plot.resid.mGPT

plot.RT.mono <- data.mono %>%
  meanplot(25) +
  facet_wrap(~language, scale="free_y", ncol=4) 
plot.RT.mono

plot.resid.mono <- data.mono %>%
  meanplot_resid(25) +
  facet_wrap(~language, scale="free_y", ncol=4) 
plot.resid.mono


pdf("plots/RT_meanplot_mGPT.pdf", width = 10, height = 5.2)
plot.RT.mGPT
dev.off()

pdf("plots/resid_meanplot_mGPT.pdf", width = 10, height = 5.2)
plot.resid.mGPT
dev.off()

pdf("plots/RT_meanplot_mono.pdf", width = 10, height = 5.2)
plot.RT.mono
dev.off()

pdf("plots/resid_meanplot_mono.pdf", width = 10, height = 5.2)
plot.resid.mono
dev.off()



