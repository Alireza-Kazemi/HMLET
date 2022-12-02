rm(list=ls(all=TRUE))
setwd("/Users/grishabandodkar/Documents/GitHub/HMLET/RPackage/HMLET/ForGrisha")

library(pacman)
p_load(reshape2,
       ez,
       lme4,
       lmerTest,
       ggplot2,
       grid,
       tidyr,
       plyr,
       dplyr,
       effects,
       gridExtra,
       DescTools,
       Cairo, #alternate image writing package with superior performance.
       corrplot,
       knitr,
       PerformanceAnalytics,
       afex,
       ggpubr,
       readxl,
       officer,
       psych,
       rstatix,
       emmeans)
library(HMLET)


datRet = read.csv("ForGrisha.csv")

#----------------------------------------------------------------
dat = datRet[datRet$PostDec==0,c("SID","TrialNum","Retrieval","ConditionName","timeStamp","Target2","RespType2","FixatedOn")]
dat = dat[dat$RespType2 %in% c("Corr Loc","Incorr Loc"),]
dat = dat[dat$timeStamp<=2000,]

dat$TrialNum = paste(dat$Retrieval, dat$TrialNum, sep = "_")

dat = PermutationTestDataPrep_HMLET(data = dat, ID = "SID", trial = "TrialNum", timepoint = "timeStamp",
                                    condition = "RespType2", conditionLevels = c("Corr Loc","Incorr Loc"),
                                    gazeInAOI = "FixatedOn", testName = "ConditionName", targetAOI = "Target")

PlotTemporalGazeTrends_HMLET(dat, showDataPointNumbers = T)

Res = ClusterStats_HMLET(dat, paired = T, detailed = F)

PlotTemporalGazeTrends_HMLET(dat, clusterData = Res)

set.seed(5)
samples = 10
res = PermutationTest_HMLET(dat, samples = samples, paired = T, permuteTrialsWithinSubject = F)
res[[1]]
PlotNullDistribution_HMLET(res)
PlotTemporalGazeTrends_HMLET(res)
PlotTemporalGazeTrends_HMLET(res, onlySignificantClusters = F)


#--------------------------------------------------- Forward Target or else 250ms----
dat = datRet[datRet$PostDec==0,c("SID","TrialNum","Retrieval","ConditionName","timeStamp","Target2","RespType2","FixatedOn")]
dat = dat[dat$RespType2 %in% c("Corr Loc","Incorr Loc"),]
groupingColumns = c("SID","TrialNum","Retrieval","ConditionName","Target2","RespType2")
dat = CreateTimeBinData_HMLET(data = dat,  timeBinWidth =  250, timeMax = 2000, timepoint = "timeStamp",
                              FixatedOn = "FixatedOn",groupingColumns = groupingColumns)

dat$Gazeprop = case_when(dat$RespType2 == "Corr Loc" ~  dat$AOI_Target/
                           (dat$AOI_Target+dat$AOI_Lure+dat$AOI_Content),
                         dat$RespType2 == "Incorr Loc" ~  dat$AOI_Lure/
                           (dat$AOI_Target+dat$AOI_Lure+dat$AOI_Content))

dat$TrialNum = paste(dat$Retrieval, dat$TrialNum, sep = "_")


dat = PermutationTestDataPrep_HMLET(data = dat, ID = "SID", trial = "TrialNum",
                                    timepoint = "timeBinIndex", timeBinName = "timeBin",
                                    condition = "RespType2", conditionLevels = c("Corr Loc","Incorr Loc"),
                                    gazeInAOI = "Gazeprop", testName = "ConditionName")

PlotTemporalGazeTrends_HMLET(dat)

Res = ClusterStats_HMLET(dat, paired = T, detailed = F)

PlotTemporalGazeTrends_HMLET(dat, clusterData = Res)
