library(tidyverse)
library(meta)
library(metafor)
library(openxlsx)
library(stringr)

data<-read.xlsx("Final Studies_Meta Analysis_1.3.xlsx",startRow = 2)
meta <- data[,c(1,3,6:8,10:12)]
names(meta) <- c("StudyID","Author" ,"Size", "MeanDiff_SBP", "MeanDiff_DBP", "SE_SBP", "SE_DBP", "Duration_M")

# add study ID and Author to 2nd 3rd row
meta[2:3,1:2]=meta[1,1:2]
# add duration
meta[c(2,3,6),'Duration_M']=c('2','2','6')
# input nan in SE_SBP and SE_DBP
tmp_sbp<-str_extract_all(meta$SE_SBP,"-*\\d+.\\d+")%>% map(`[`,2) %>% unlist() %>%  na.omit() 
meta$SE_SBP[c(2,7)]=tmp_sbp
tmp_dbp<-str_extract_all(meta$SE_DBP,"-*\\d+.\\d+")%>% map(`[`,2) %>% unlist() %>%  na.omit() 
meta$SE_DBP[c(2,7)]=tmp_dbp
# input SE study ID 1(placebo)
meta[3,c('SE_SBP','SE_DBP')]=c('3.004','2.593')

# manually change Mean_Diff_SBP& DBP of study ID 5:
meta[7,c('MeanDiff_SBP','MeanDiff_DBP')]=c('-2.54','-3.95')
# extract the first number and omit the content after \n
meta<-meta %>% mutate_at(c('MeanDiff_SBP','MeanDiff_DBP','SE_SBP','SE_DBP'),~str_extract(.,"-*\\d+.\\d+") %>% as.numeric(.))
# drop na columns
meta<-meta[c(!is.na(meta$SE_DBP)),]
# delete et al
meta$Author<-str_extract(meta$Author,'\\w+')
# adjust the mean of study ID 6 into -4.8 and -3.2
meta[8,c('MeanDiff_SBP','MeanDiff_DBP')]=-1*meta[8,c('MeanDiff_SBP','MeanDiff_DBP')]
meta


rownames(meta)<-seq(1:dim(meta)[1])
meta$Author=paste(rownames(meta),meta$Author,sep='.')
meta <- meta[order(meta$StudyID),]
# ?metagen
# ?funnel.meta
meta_SBP <- metagen(TE = MeanDiff_SBP, seTE = SE_SBP, studlab = Author,data = meta, sm = "MD", comb.fixed = F, comb.random = T, method.tau = "SJ", title = "SBP", prediction = TRUE)
forest_SBP <- forest.meta(meta_SBP, layout = "RevMan5")
grid.text("SBP Forest Plot", .5, .9, gp=gpar(cex=1.5))
funnel.meta(meta_SBP,studlab = TRUE,cex.studlab = 1,cex = 1,pch=16,pos.studlab=c(4,1,2,2,1,1,1,1),xlim=c(-15,15))
title(main = "SBP Funnel Plot",  cex.main = 1.5,font.main = 1)

# title(main = "Main title", sub = "Sub-title",
#       xlab = "X axis", ylab = "Y axis",
#       cex.main = 2,   font.main= 4, col.main= "red",
#       cex.sub = 0.75, font.sub = 3, col.sub = "green",
#       col.lab ="darkblue")

#meta_DBP
meta_DBP <- metagen(TE = MeanDiff_DBP, seTE = SE_DBP, studlab = Author, data = meta, sm = "MD", comb.fixed = F, comb.random = T, method.tau = "SJ", title = "DBP", prediction = TRUE)
forest_DBP <- forest.meta(meta_DBP, layout = "RevMan5")
grid.text("DBP Forest Plot", .5, .9, gp=gpar(cex=1.5))
funnel.meta(meta_DBP,  studlab = TRUE,cex.studlab = 1,cex = 1,pch=16,pos.studlab=c(4,1,2,2,1,1,1,1),xlim=c(-6,6))
title(main = "DBP Funnel Plot",  cex.main = 1.5,font.main = 1)




M<-meta$Duration_M%>% as.numeric()
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




















