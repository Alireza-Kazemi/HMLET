library(HMLET)
library(ggplot2)
library(reshape2)


# RD = "C:\\Users\\kazemi\\Documents\\GitHub\\HMLET\\RPackage\\HMLET\\tests\\testthat\\"
# WR = "/Users/grishabandodkar/Documents/GitHub/HMLET/RPackage/HMLET/tests/testthat/"
dat = read.table("HMLET_Testing_Data.csv", header=TRUE, sep=",", strip.white = TRUE)

d1= CreateTimeBinData_HMLET(data=dat, timePoint = "timePoint",
                              FixatedOn = "AOI")


datP = PermutationTestDataPrep_HMLET(data = dat, ID = "ID", trial = "trial", timePoint = "timePoint",
                                    condition = "condition", conditionLevels = c("C1","C2"),
                                    gazeMeasure = "AOI", targetAOI = 1)

write.csv(datP,paste(RD,"Output_PermutationTestDataPrep.csv",sep = ""),row.names = F)

P = PlotTemporalGazeTrends_HMLET(datP)
plot(P)

Res = ClusterStats_HMLET(datP, paired = T, detailed = F)
write.csv(Res,paste(RD,"Output_ClusterStats.csv",sep = ""),row.names = F)

# P = PlotTemporalGazeTrends_HMLET(datP, clusterData = Res)
# plot(P)

set.seed(5)
res = PermutationTest_HMLET(datP, samples = 1000, paired = T, permuteTrialsWithinSubject = F)

write.csv(res[[1]],paste(RD,"ClusterInfos.csv",sep = ""),row.names = F)
write.csv(res[[2]],paste(RD,"NullDistribution.csv",sep = ""),row.names = F)
write.csv(res[[3]],paste(RD,"Output_PermutationTest.csv",sep = ""),row.names = F)
write.table(res[[4]], paste(RD,"PermuationSampleNumber.txt",sep = ""), append = FALSE, sep = " ", dec = ".",
            row.names = F, col.names = F)
write.csv(res[[5]],paste(RD,"Data.csv",sep = ""),row.names = F)


PlotNullDistribution_HMLET(res)
PlotTemporalGazeTrends_HMLET(res)
PlotTemporalGazeTrends_HMLET(res, onlySignificantClusters = F)
#
#
#
