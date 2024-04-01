library(HMLET)

RD = "D:\\GithubRep\\HMLET\\Demo\\"
WR = "D:\\GithubRep\\HMLET\\Demo\\"
dat = read.table(paste(RD,"HMLET_Testing_Data.csv",sep = ""), header=TRUE, sep=",", strip.white = TRUE)


d_timeBin = CreateTimeBinData_HMLET(data=dat, timePoint = "timePoint",
                                    groupingColumns = c("ID","trial","condition"),
                                    FixatedOn = "AOI")

write.csv(d_timeBin,paste(RD,"Output_CreateTimeBinData.csv",sep=""),row.names = F)

datP = PermutationTestDataPrep_HMLET(data = dat, ID = "ID", trial = "trial", timePoint = "timePoint",
                                    condition = "condition", conditionLevels = c("C1","C2"),
                                    gazeMeasure = "AOI", targetAOI = 1)

write.csv(datP,paste(RD,"Output_PermutationTestDataPrep.csv",sep=""),row.names = F)

P = PlotTimeSeries_HMLET(datP)
plot(P)

Res = ClusterStats_HMLET(datP, paired = T, detailed = F)

P = PlotTimeSeries_HMLET(datP, clusterData = Res)
plot(P)

set.seed(5)
res = PermutationTest_HMLET(datP, samples = 500, paired = T, permuteTrialsWithinSubject = F)

write.csv(res[[1]],paste(RD,"Output_ClusterInfos.csv",sep = ""),row.names = F)
write.csv(res[[2]],paste(RD,"Output_NullDistribution.csv",sep = ""),row.names = F)
write.csv(res[[3]],paste(RD,"Output_tValueDistribution.csv",sep = ""),row.names = F)
write.table(res[[4]], paste(RD,"PermuationSampleNumber.txt",sep = ""), append = FALSE, sep = " ", dec = ".",
            row.names = F, col.names = F)
write.csv(res[[5]],paste(RD,"Data.csv",sep = ""),row.names = F)


PlotNullDistribution_HMLET(res)
PlotTimeSeries_HMLET(res)
PlotTimeSeries_HMLET(res, onlySignificantClusters = F)



