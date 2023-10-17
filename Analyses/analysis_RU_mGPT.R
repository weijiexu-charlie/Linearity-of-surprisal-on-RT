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

russian <- read.csv("preproc_data/russian_mGPT.csv")

#########################################################
################### Adapted Spillover ###################
#########################################################

# ---------------------- Step 1 ----------------------
### Model selection; Determine spillover using length and frequency as controls
### Maximal converging random effects for both models to be compared
### Result: Justified up to Spillover 3

# Spillover 1
ru.s0 = lmer(RT ~ logfreq.s + wlen.s + 
               (logfreq.s + wlen.s|subj),
             data=na.omit(russian),
             REML=FALSE) 
summary(ru.s0)    
ru.s1 = lmer(RT ~ logfreq.s + logfreq1.s + wlen.s + wlen1.s + 
               (logfreq.s + wlen.s|subj), 
             data=na.omit(russian),
             REML=FALSE) 
summary(ru.s1)
anova(ru.s0, ru.s1)  # Significant (Spillover 1 is needed)

# Spillover 2
ru.s11 = lmer(RT ~ logfreq.s + logfreq1.s + wlen.s + wlen1.s + 
                (1|subj), 
              data=na.omit(russian),
              REML=FALSE) 
summary(ru.s11)
ru.s2 = lmer(RT ~ logfreq.s + logfreq1.s + logfreq2.s + wlen.s + wlen1.s + wlen2.s + 
               (1|subj), 
             data=na.omit(russian),
             REML=FALSE)
summary(ru.s2)
anova(ru.s11, ru.s2)  # Significant (Spillover 2 is needed)

# Spillover 3
ru.s22 = lmer(RT ~ logfreq.s + logfreq1.s + logfreq2.s + wlen.s + wlen1.s + wlen2.s +
                (logfreq.s + logfreq1.s + logfreq2.s + logfreq3.s + wlen.s + wlen1.s + wlen2.s + wlen3.s|subj), 
              data=na.omit(russian),
              REML=FALSE) 
summary(ru.s22)
ru.s3 = lmer(RT ~ logfreq.s + logfreq1.s + logfreq2.s + logfreq3.s + wlen.s + wlen1.s + wlen2.s + wlen3.s + 
               (logfreq.s + logfreq1.s + logfreq2.s + logfreq3.s + wlen.s + wlen1.s + wlen2.s + wlen3.s|subj), 
             data=na.omit(russian),
             REML=FALSE)
summary(ru.s3)
anova(ru.s22, ru.s3)   # Significant (Spillover 3 is needed)


# ---------------------- Step 2 ----------------------
### Examine Linearity
### Result: Significant quadratic term

# Null
poly0 <- lmer(RT ~ logfreq.s + logfreq1.s + logfreq2.s + logfreq3.s + wlen.s + wlen1.s + wlen2.s + wlen3.s +
                (logp.s + logp1.s + logp2.s + logp3.s + logfreq.s + logfreq1.s + logfreq2.s + logfreq3.s + wlen.s + wlen1.s + wlen2.s + wlen3.s + I(logp.s^2) + I(logp1.s^2) + I(logp2.s^2) + I(logp3.s^2)|subj), 
              data=na.omit(russian),
              REML=FALSE)
summary(poly0)

# Linear
poly1 <- lmer(RT ~ logp.s + logp1.s + logp2.s + logp3.s + logfreq.s + logfreq1.s + logfreq2.s + logfreq3.s + wlen.s + wlen1.s + wlen2.s + wlen3.s +
                (logp.s + logp1.s + logp2.s + logp3.s + logfreq.s + logfreq1.s + logfreq2.s + logfreq3.s + wlen.s + wlen1.s + wlen2.s + wlen3.s + I(logp.s^2) + I(logp1.s^2) + I(logp2.s^2) + I(logp3.s^2)|subj), 
              data=na.omit(russian),
              REML=FALSE)
summary(poly1)
anova(poly0, poly1)   # Significant

# Nonlinear
poly2 <- lmer(RT ~ logp.s + logp1.s + logp2.s + logp3.s + logfreq.s + logfreq1.s + logfreq2.s + logfreq3.s + wlen.s + wlen1.s + wlen2.s + wlen3.s + I(logp.s^2) + I(logp1.s^2) + I(logp2.s^2) + I(logp3.s^2) +
                (logp.s + logp1.s + logp2.s + logp3.s + logfreq.s + logfreq1.s + logfreq2.s + logfreq3.s + wlen.s + wlen1.s + wlen2.s + wlen3.s + I(logp.s^2) + I(logp1.s^2) + I(logp2.s^2) + I(logp3.s^2)|subj), 
              data=na.omit(russian),
              REML=FALSE)
summary(poly2)   
anova(poly1, poly2)   # Significant


###########################################################
################ Fixed Spillover (2 words) ################
###########################################################
### Examine Linearity
### Result: Significant quadratic term

# Null
poly0.spill2 <- lmer(RT ~ logfreq.s + logfreq1.s + logfreq2.s + wlen.s + wlen1.s + wlen2.s +
                (logp.s + logp1.s + logp2.s + logfreq.s + logfreq1.s + logfreq2.s + wlen.s + wlen1.s + wlen2.s + I(logp.s^2) + I(logp1.s^2) + I(logp2.s^2)|subj), 
              data=na.omit(russian),
              REML=FALSE)
summary(poly0.spill2)

# Linear
poly1.spill2 <- lmer(RT ~ logp.s + logp1.s + logp2.s + logfreq.s + logfreq1.s + logfreq2.s + wlen.s + wlen1.s + wlen2.s +
                (logp.s + logp1.s + logp2.s + logfreq.s + logfreq1.s + logfreq2.s + wlen.s + wlen1.s + wlen2.s + I(logp.s^2) + I(logp1.s^2) + I(logp2.s^2)|subj), 
              data=na.omit(russian),
              REML=FALSE)
summary(poly1.spill2)
anova(poly0.spill2, poly1.spill2)   # Significant

# Nonlinear:
poly2.spill2 <- lmer(RT ~ logp.s + logp1.s + logp2.s + logfreq.s + logfreq1.s + logfreq2.s + wlen.s + wlen1.s + wlen2.s + I(logp.s^2) + I(logp1.s^2) + I(logp2.s^2) +
                (logp.s + logp1.s + logp2.s + logfreq.s + logfreq1.s + logfreq2.s + wlen.s + wlen1.s + wlen2.s + I(logp.s^2) + I(logp1.s^2) + I(logp2.s^2)|subj), 
              data=na.omit(russian),
              REML=FALSE)
summary(poly2.spill2)  
anova(poly1.spill2, poly2.spill2)  # Significant





