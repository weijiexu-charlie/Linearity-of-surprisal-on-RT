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

danish <- read.csv("preproc_data/danish_mGPT.csv")


#########################################################
################### Adapted Spillover ###################
#########################################################

# ---------------------- Step 1 ----------------------
### Model selection; Determine spillover using length and frequency as controls
### Maximal converging random effects for both models to be compared
### Result: Justified up to Spillover 3

# Spillover 1:
da.s0 = lmer(RT ~ logfreq.s + wlen.s + 
               (wlen.s|subj),
             data=na.omit(danish),
             REML=FALSE) 
summary(da.s0)
da.s1 = lmer(RT ~ logfreq.s + logfreq1.s + wlen.s + wlen1.s + 
               (wlen.s|subj), 
             data=na.omit(danish),
             REML=FALSE) 
summary(da.s1)
anova(da.s0, da.s1)  # Significant (Spillover 1 is needed)

# Spillover 2
da.s11 = lmer(RT ~ logfreq.s + logfreq1.s + wlen.s + wlen1.s + 
                (logfreq.s + logfreq1.s + logfreq2.s + wlen.s + wlen1.s + wlen2.s|subj), 
              data=na.omit(danish),
              REML=FALSE) 
summary(da.s11)
da.s2 = lmer(RT ~ logfreq.s + logfreq1.s + logfreq2.s + wlen.s + wlen1.s + wlen2.s +
               (logfreq.s + logfreq1.s + logfreq2.s + wlen.s + wlen1.s + wlen2.s|subj), 
             data=na.omit(danish),
             REML=FALSE)
summary(da.s2)
anova(da.s11, da.s2)   # Significant (Spillover 2 is needed)

# Spillover 3:
da.s22 = lmer(RT ~ logfreq.s + logfreq1.s + logfreq2.s + wlen.s + wlen1.s + wlen2.s + 
                (logfreq.s + logfreq1.s + logfreq2.s + wlen.s + wlen1.s + wlen2.s|subj), 
              data=na.omit(danish),
              REML=FALSE) 
summary(da.s22)
da.s3 = lmer(RT ~ logfreq.s + logfreq1.s + logfreq2.s + logfreq3.s + wlen.s + wlen1.s + wlen2.s + wlen3.s +
               (logfreq.s + logfreq1.s + logfreq2.s + wlen.s + wlen1.s + wlen2.s|subj), 
             data=na.omit(danish),
             REML=FALSE)
summary(da.s3)
anova(da.s22, da.s3)  # Significant (Spillover 3 is needed)


# ---------------------- Step 2 ----------------------
### Examine Linearity
### Result: non-significant quadratic effect

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
anova(poly0, poly1)  # Significant

# Nonlinear
poly2 <- lmer(RT ~ logp.s + logp1.s + logp2.s + logp3.s + logfreq.s + logfreq1.s + logfreq2.s + logfreq3.s + wlen.s + wlen1.s + wlen2.s + wlen3.s + I(logp.s^2) + I(logp1.s^2) + I(logp2.s^2) + I(logp3.s^2) + 
                (logp.s + logp1.s + logp2.s + logp3.s + logfreq.s + logfreq1.s + logfreq2.s + logfreq3.s + wlen.s + wlen1.s + wlen2.s + wlen3.s + I(logp.s^2) + I(logp1.s^2) + I(logp2.s^2) + I(logp3.s^2)|subj), 
              data=na.omit(danish),
              REML=FALSE)
summary(poly2)
anova(poly1, poly2)  # ns, p=0.4271 (non-significant quadratic effect)


###########################################################
################ Fixed Spillover (2 words) ################
###########################################################

# Result: non-significant quadratic effect

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
anova(poly0.spill2, poly1.spill2)    # p=0.01117, significant linear

# Nonlinear:
poly2.spill2 <- lmer(RT ~ logp.s + logp1.s + logp2.s + logfreq.s + logfreq1.s + logfreq2.s + wlen.s + wlen1.s + wlen2.s + I(logp.s^2) + I(logp1.s^2) + I(logp2.s^2) +
                       (logp.s + logp1.s + logp2.s + logfreq.s + logfreq1.s + logfreq2.s + wlen.s + wlen1.s + wlen2.s + I(logp.s^2) + I(logp1.s^2) + I(logp2.s^2)|subj), 
                     data=na.omit(danish),
                     REML=FALSE)
summary(poly2.spill2)  
anova(poly1.spill2, poly2.spill2)   # ns, p=0.2887



