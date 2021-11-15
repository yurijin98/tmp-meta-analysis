library(tidyverse)
library(meta)
library(metafor)
library(openxlsx)
library(stringr)

data<-read.xlsx("Final Studies_Meta Analysis_1.3.xlsx",startRow = 2)
meta <- data[,c(1,6:8,10:12)]
names(meta) <- c("StudyID", "Size", "MeanDiff_SBP", "MeanDiff_DBP", "SE_SBP", "SE_DBP", "Duration_M")

# drop na columns
meta<-meta[c(!is.na(meta$StudyID)),]

# I just extract the first number and omit the content after \n
meta$MeanDiff_SBP<-str_extract(meta$MeanDiff_SBP, "-*\\d+.\\d+") %>% as.numeric()
meta$MeanDiff_DBP<-str_extract(meta$MeanDiff_DBP, "-*\\d+.\\d+") %>% as.numeric()
meta$SE_SBP<-str_extract(meta$SE_SBP, "-*\\d+.\\d+") %>% as.numeric()
meta$SE_DBP<-str_extract(meta$SE_DBP, "-*\\d+.\\d+") %>% as.numeric()

meta <- meta[order(meta$StudyID),]
meta$StudyID <- as.character(meta$StudyID)
#meta_SBP
meta_SBP <- metagen(TE = MeanDiff_SBP, seTE = SE_SBP, studlab = StudyID, data = meta, sm = "MD", comb.fixed = F, comb.random = T, method.tau = "SJ", title = "SBP", prediction = TRUE)
forest_SBP <- forest.meta(meta_SBP, layout = "RevMan5")
funnel.meta(meta_SBP, studlab = TRUE)
#meta_DBP
meta_DBP <- metagen(TE = MeanDiff_DBP, seTE = SE_DBP, studlab = StudyID, data = meta, sm = "MD", comb.fixed = F, comb.random = T, method.tau = "SJ", title = "DBP", prediction = TRUE)
forest_DBP <- forest.meta(meta_DBP, layout = "RevMan5")
funnel.meta(meta_DBP, studlab = TRUE)

M<-str_extract(meta$Duration_M, "\\d+") %>% as.numeric()
duration <- M*30
duration_m <- M - 1
duration_m2 <- M - 2
duration_lg <- log(duration_m)
size <- meta$Size

# SBP
meta_SBP_reg <- metareg(meta_SBP, ~duration_m2)
meta_SBP_reg

bubble(meta_SBP_reg, studlab = TRUE)

# DBP
meta_DBP_reg <- metareg(meta_DBP, ~duration_m2)
meta_DBP_reg

bubble(meta_DBP_reg, studlab = TRUE)

