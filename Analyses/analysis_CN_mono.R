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

chinese <- read.csv("preproc_data/chinese_mono.csv")

#########################################################
################### Adapted Spillover ###################
#########################################################

### Examine Linearity
### 0 spillover word (see in the analysis with mGPT)
### Result: non-significant quadratic effect, even non-significant linear effect!!!

# Null
poly0 <- lmer(RT ~ logfreq.s + wlen.s +
                (logp.s + logfreq.s + wlen.s + I(logp.s^2)|subj), 
              data=na.omit(chinese),
              REML=FALSE)
summary(poly0)

# Linear
poly1 <- lmer(RT ~ logp.s + logfreq.s + wlen.s +
                (logp.s + logfreq.s + wlen.s + I(logp.s^2)|subj), 
              data=na.omit(chinese),
              REML=FALSE)
summary(poly1)
anova(poly0, poly1)   # ns, p=0.284

# Nonlinear
poly2 <- lmer(RT ~ logp.s + logfreq.s + wlen.s + I(logp.s^2) +
                (logp.s + logfreq.s + wlen.s + I(logp.s^2)|subj), 
              data=na.omit(chinese),
              REML=FALSE)
summary(poly2)   
anova(poly1, poly2)  # ns, p=0.08147


###########################################################
################ Fixed Spillover (2 words) ################
###########################################################

### Result: non-significant quadratic effect, even non-significant linear effect!!!

# Null
poly0.spill2 <- lmer(RT ~ logfreq.s + logfreq1.s + logfreq2.s + wlen.s + wlen1.s + wlen2.s +
                       (logp.s + logp1.s + logp2.s + logfreq.s + logfreq1.s + logfreq2.s + wlen.s + wlen1.s + wlen2.s + I(logp.s^2) + I(logp1.s^2) + I(logp2.s^2)|subj), 
                     data=na.omit(chinese),
                     REML=FALSE)
summary(poly0.spill2)

# Linear
poly1.spill2 <- lmer(RT ~ logp.s + logp1.s + logp2.s + logfreq.s + logfreq1.s + logfreq2.s + wlen.s + wlen1.s + wlen2.s +
                       (logp.s + logp1.s + logp2.s + logfreq.s + logfreq1.s + logfreq2.s + wlen.s + wlen1.s + wlen2.s + I(logp.s^2) + I(logp1.s^2) + I(logp2.s^2)|subj), 
                     data=na.omit(chinese),
                     REML=FALSE)
summary(poly1.spill2)
anova(poly0.spill2, poly1.spill2)    # ns, p=0.05828

# Nonlinear:
poly2.spill2 <- lmer(RT ~ logp.s + logp1.s + logp2.s + logfreq.s + logfreq1.s + logfreq2.s + wlen.s + wlen1.s + wlen2.s + I(logp.s^2) + I(logp1.s^2) + I(logp2.s^2) +
                       (logp.s + logp1.s + logp2.s + logfreq.s + logfreq1.s + logfreq2.s + wlen.s + wlen1.s + wlen2.s + I(logp.s^2) + I(logp1.s^2) + I(logp2.s^2)|subj), 
                     data=na.omit(chinese),
                     REML=FALSE)
summary(poly2.spill2)  
anova(poly1.spill2, poly2.spill2)    # ns, p=0.4237




