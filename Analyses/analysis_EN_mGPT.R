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

dundee <- read.csv("preproc_data/english_mGPT.csv")


#########################################################
################### Adapted Spillover ###################
#########################################################

# ---------------------- Step 1 ----------------------
### Model selection; Determine spillover using length and frequency as controls
### Maximal converging random effects for both models to be compared
### Result: Justified up to spillover 2

## Spillover 1: 
en.s0 = lmer(RT ~ logfreq.s + wlen.s + 
               (wlen.s|subj),
             data=na.omit(dundee),
             REML=FALSE) 
summary(en.s0)
en.s1 = lmer(RT ~ logfreq.s + logfreq1.s + wlen.s + wlen1.s + 
               (wlen.s|subj), 
             data=na.omit(dundee),
             REML=FALSE) 
summary(en.s1)
anova(en.s0, en.s1)  # Significant (Spillover 1 is needed)

## Spillover 2:
en.s11 = lmer(RT ~ logfreq.s + logfreq1.s + wlen.s + wlen1.s + 
               (logfreq.s + logfreq1.s + logfreq2.s + wlen.s + wlen1.s + wlen2.s|subj), 
             data=na.omit(dundee),
             REML=FALSE) 
summary(en.s11)
en.s2 = lmer(RT ~ logfreq.s + logfreq1.s + logfreq2.s + wlen.s + wlen1.s + wlen2.s + 
               (logfreq.s + logfreq1.s + logfreq2.s + wlen.s + wlen1.s + wlen2.s|subj), 
             data=na.omit(dundee),
             REML=FALSE)
summary(en.s2)
anova(en.s11, en.s2)   # Significant (Spillover 2 is needed)

# Spillover 3:
en.s22 = lmer(RT ~ logfreq.s + logfreq1.s + logfreq2.s + wlen.s + wlen1.s + wlen2.s + 
                (logfreq.s + logfreq1.s + logfreq2.s + logfreq3.s + wlen.s + wlen1.s + wlen2.s + wlen3.s|subj), 
              data=na.omit(dundee),
              REML=FALSE) 
summary(en.s22)
en.s3 = lmer(RT ~ logfreq.s + logfreq1.s + logfreq2.s + logfreq3.s + wlen.s + wlen1.s + wlen2.s + wlen3.s + 
               (logfreq.s + logfreq1.s + logfreq2.s + logfreq3.s + wlen.s + wlen1.s + wlen2.s + wlen3.s|subj), 
             data=na.omit(dundee),
             REML=FALSE)
summary(en.s3)
anova(en.s22, en.s3)   # ns, spillover 2 is not needed 


# ---------------------- Step 2 ----------------------
### Examine Linearity
### Result: non-significant quadratic effect

# Null
poly0 <- lmer(RT ~ logfreq.s + logfreq1.s + logfreq2.s + wlen.s + wlen1.s + wlen2.s +
                (logp.s + logp1.s + logp2.s + logfreq.s + logfreq1.s + logfreq2.s + wlen.s + wlen1.s + wlen2.s + I(logp.s^2) + I(logp1.s^2) + I(logp2.s^2)|subj), 
              data=na.omit(dundee),
              REML=FALSE)
summary(poly0)

# Linear
poly1 <- lmer(RT ~ logp.s + logp1.s + logp2.s + logfreq.s + logfreq1.s + logfreq2.s + wlen.s + wlen1.s + wlen2.s +
                (logp.s + logp1.s + logp2.s + logfreq.s + logfreq1.s + logfreq2.s + wlen.s + wlen1.s + wlen2.s + I(logp.s^2) + I(logp1.s^2) + I(logp2.s^2)|subj), 
              data=na.omit(dundee),
              REML=FALSE)
summary(poly1)
anova(poly0, poly1)   # Significant

# Nonlinear:
poly2 <- lmer(RT ~ logp.s + logp1.s + logp2.s + logfreq.s + logfreq1.s + logfreq2.s + wlen.s + wlen1.s + wlen2.s + I(logp.s^2) + I(logp1.s^2) + I(logp2.s^2) +
                (logp.s + logp1.s + logp2.s + logfreq.s + logfreq1.s + logfreq2.s + wlen.s + wlen1.s + wlen2.s + I(logp.s^2) + I(logp1.s^2) + I(logp2.s^2)|subj), 
              data=na.omit(dundee),
              REML=FALSE)
summary(poly2)  
anova(poly1, poly2)   # ns, p = 0.9354



