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

japanese <- read.csv("preproc_data/japanese_mGPT.csv")


#########################################################
################### Adapted Spillover ###################
#########################################################

# ---------------------- Step 1 ----------------------
### Model selection; Determine spillover using length and frequency as controls
### Maximal converging random effects for both models to be compared
### Result: Justified up to Spillover 0

# Spillover 1
ja.s0 = lmer(RT ~ logfreq.s + wlen.s + 
               (logfreq.s + logfreq1.s + wlen.s + wlen1.s|subj),
             data=na.omit(japanese),
             REML=FALSE) 
summary(ja.s0)    
ja.s1 = lmer(RT ~ logfreq.s + logfreq1.s + wlen.s + wlen1.s + 
               (logfreq.s + logfreq1.s + wlen.s + wlen1.s|subj), 
             data=na.omit(japanese),
             REML=FALSE) 
summary(ja.s1)
anova(ja.s0, ja.s1)   # ns, p=0.7972 (Spillover 1 is not needed)


# ---------------------- Step 2 ----------------------
### Examine Linearity
### Result: Non-significant quadratic effect

# Null
poly0 <- lmer(RT ~ logfreq.s + wlen.s +
                (logp.s + logfreq.s + wlen.s + I(logp.s^2)|subj), 
              data=na.omit(japanese),
              REML=FALSE)
summary(poly0)

# Linear
poly1 <- lmer(RT ~ logp.s + logfreq.s + wlen.s +
                (logp.s + logfreq.s + wlen.s + I(logp.s^2)|subj), 
              data=na.omit(japanese),
              REML=FALSE)
summary(poly1)
anova(poly0, poly1)   # Significant

# Nonlinear
poly2 <- lmer(RT ~ logp.s + logfreq.s + wlen.s + I(logp.s^2) +
                (logp.s + logfreq.s + wlen.s + I(logp.s^2)|subj), 
              data=na.omit(japanese),
              REML=FALSE)
summary(poly2)   
anova(poly1, poly2)  # ns, p=0.3535


###########################################################
################ Fixed Spillover (2 words) ################
###########################################################
### Result: Significant quadratic effect

# Null
poly0.spill2 <- lmer(RT ~ logfreq.s + logfreq1.s + logfreq2.s + wlen.s + wlen1.s + wlen2.s +
                       (logp.s + logp1.s + logp2.s + logfreq.s + logfreq1.s + logfreq2.s + wlen.s + wlen1.s + wlen2.s + I(logp.s^2) + I(logp1.s^2) + I(logp2.s^2)|subj), 
                     data=na.omit(japanese),
                     REML=FALSE)
summary(poly0.spill2)

# Linear
poly1.spill2 <- lmer(RT ~ logp.s + logp1.s + logp2.s + logfreq.s + logfreq1.s + logfreq2.s + wlen.s + wlen1.s + wlen2.s +
                       (logp.s + logp1.s + logp2.s + logfreq.s + logfreq1.s + logfreq2.s + wlen.s + wlen1.s + wlen2.s + I(logp.s^2) + I(logp1.s^2) + I(logp2.s^2)|subj), 
                     data=na.omit(japanese),
                     REML=FALSE)
summary(poly1.spill2)
anova(poly0.spill2, poly1.spill2)   # Significant

# Nonlinear:
poly2.spill2 <- lmer(RT ~ logp.s + logp1.s + logp2.s + logfreq.s + logfreq1.s + logfreq2.s + wlen.s + wlen1.s + wlen2.s + I(logp.s^2) + I(logp1.s^2) + I(logp2.s^2) +
                       (logp.s + logp1.s + logp2.s + logfreq.s + logfreq1.s + logfreq2.s + wlen.s + wlen1.s + wlen2.s + I(logp.s^2) + I(logp1.s^2) + I(logp2.s^2)|subj), 
                     data=na.omit(japanese),
                     REML=FALSE)
summary(poly2.spill2)  
anova(poly1.spill2, poly2.spill2)  # Significant


