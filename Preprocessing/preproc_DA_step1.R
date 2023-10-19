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

file_list <- list.files(path="/Users/weijiexu/Desktop/Surprisal/Danish/ExtractedFeatures/")

danish.rt <- data.frame()
for (i in 1:length(file_list)){
  filename <- paste("/Users/weijiexu/Desktop/Surprisal/Danish/ExtractedFeatures", file_list[i], sep="/")
  temp_data <- read.csv(filename) %>%
    mutate(subj = i) %>%
    unique()
  danish.rt <- rbind(danish.rt, temp_data)
}

danish.meta <- danish.rt %>%
  dplyr::select(c('speechId', 'paragraphId', 'sentenceId', 'wordId', 'word')) %>%
  distinct()

danish.meta <- danish.meta[
  order(danish.meta[,1], danish.meta[,2], danish.meta[,3], danish.meta[,4] ),
]

write.csv(danish.rt, "Danish/danish_rt.csv", row.names=FALSE)

write.csv(danish.meta, "Danish/danish_meta.csv", row.names=FALSE)
