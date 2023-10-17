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

danish <- read.csv("preproc_data/danish_mono.csv")


#########################################################
################### Adapted Spillover ###################
#########################################################

### Examine Linearity
### 3 spillover words (see in the analysis with mGPT)
### Result: Significant quadratic effect

# Null
poly0 <- lmer(RT ~ logfreq.s + logfreq1.s + logfreq2.s + logfreq3.s + wlen.s + wlen1.s + wlen2.s + wlen3.s +
                (logp.s + logp1.s + logp2.s + logp3.s + logfreq.s + logfreq1.s + logfreq2.s + logfreq3.s + wlen.s + wlen1.s + wlen2.s + wlen3.s + I(logp.s^2) + I(logp1.s^2) + I(logp2.s^2) + I(logp3.s^2)|subj), 
              data=na.omit(danish),
              REML=FALSE)
summary(poly0)

# Linear
poly1 <- lmer(RT ~ logp.s + logp1.s + logp2.s + logp3.s + logfreq.s + logfreq1.s + logfreq2.s + logfreq3.s + wlen.s + wlen1.s + wlen2.s + wlen3.s +
                (logp.s + logp1.s + logp2.s + logp3.s + logfreq.s + logfreq1.s + logfreq2.s + logfreq3.s + wlen.s + wlen1.s + wlen2.s + wlen3.s + I(logp.s^2) + I(logp1.s^2) + I(logp2.s^2) + I(logp3.s^2)|subj), 
              data=na.omit(danish),
              REML=FALSE)
summary(poly1)
anova(poly0, poly1)  # Significant, p=0.005593

# Nonlinear
poly2 <- lmer(RT ~ logp.s + logp1.s + logp2.s + logp3.s + logfreq.s + logfreq1.s + logfreq2.s + logfreq3.s + wlen.s + wlen1.s + wlen2.s + wlen3.s + I(logp.s^2) + I(logp1.s^2) + I(logp2.s^2) + I(logp3.s^2) + 
                (logp.s + logp1.s + logp2.s + logp3.s + logfreq.s + logfreq1.s + logfreq2.s + logfreq3.s + wlen.s + wlen1.s + wlen2.s + wlen3.s + I(logp.s^2) + I(logp1.s^2) + I(logp2.s^2) + I(logp3.s^2)|subj), 
              data=na.omit(danish),
              REML=FALSE)
summary(poly2)
anova(poly1, poly2)  # significant, p<0.0001


###########################################################
################ Fixed Spillover (2 words) ################
###########################################################

### Result: Significant quadratic effect

# Null
poly0.spill2 <- lmer(RT ~ logfreq.s + logfreq1.s + logfreq2.s + wlen.s + wlen1.s + wlen2.s +
                       (logp.s + logp1.s + logp2.s + logfreq.s + logfreq1.s + logfreq2.s + wlen.s + wlen1.s + wlen2.s + I(logp.s^2) + I(logp1.s^2) + I(logp2.s^2)|subj), 
                     data=na.omit(danish),
                     REML=FALSE)
summary(poly0.spill2)

# Linear
poly1.spill2 <- lmer(RT ~ logp.s + logp1.s + logp2.s + logfreq.s + logfreq1.s + logfreq2.s + wlen.s + wlen1.s + wlen2.s +
                       (logp.s + logp1.s + logp2.s + logfreq.s + logfreq1.s + logfreq2.s + wlen.s + wlen1.s + wlen2.s + I(logp.s^2) + I(logp1.s^2) + I(logp2.s^2)|subj), 
                     data=na.omit(danish),
                     REML=FALSE)
summary(poly1.spill2)
anova(poly0.spill2, poly1.spill2)    # Significant, p=0.003632

# Nonlinear:
poly2.spill2 <- lmer(RT ~ logp.s + logp1.s + logp2.s + logfreq.s + logfreq1.s + logfreq2.s + wlen.s + wlen1.s + wlen2.s + I(logp.s^2) + I(logp1.s^2) + I(logp2.s^2) +
                       (logp.s + logp1.s + logp2.s + logfreq.s + logfreq1.s + logfreq2.s + wlen.s + wlen1.s + wlen2.s + I(logp.s^2) + I(logp1.s^2) + I(logp2.s^2)|subj), 
                     data=na.omit(danish),
                     REML=FALSE)
summary(poly2.spill2)  
anova(poly1.spill2, poly2.spill2)   # Significant, p<0.001



