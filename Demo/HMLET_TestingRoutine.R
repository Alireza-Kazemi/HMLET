library(HMLET)

dat = read.table("HMLET_Testing_Data.csv", header=TRUE, sep=",", strip.white = TRUE)


d_timeBin = CreateTimeBinData_HMLET(data=dat, timePoint = "timePoint",
                                    groupingColumns = c("ID","trial","condition"),
                                    FixatedOn = "AOI")

write.csv(d_timeBin,"Output_CreateTimeBinData.csv",row.names = F)

datP = PermutationTestDataPrep_HMLET(data = dat, ID = "ID", trial = "trial", timePoint = "timePoint",
                                    condition = "condition", conditionLevels = c("C1","C2"),
                                    gazeMeasure = "AOI", targetAOI = 1)

write.csv(datP,"Output_PermutationTestDataPrep.csv",row.names = F)

P = PlotTimeSeries_HMLET(datP)
plot(P)

Res = ClusterStats_HMLET(datP, paired = T, detailed = F)

P = PlotTimeSeries_HMLET(datP, clusterData = Res)
plot(P)

set.seed(5)
res = PermutationTest_HMLET(datP, samples = 500, paired = T, permuteTrialsWithinSubject = F)

write.csv(res[[1]],"Output_ClusterInfos.csv",row.names = F)
write.csv(res[[2]],"Output_NullDistribution.csv",row.names = F)
write.csv(res[[3]],"Output_tValueDistribution.csv",row.names = F)
write.table(res[[4]], "PermuationSampleNumber.txt", append = FALSE, sep = " ", dec = ".",
            row.names = F, col.names = F)
write.csv(res[[5]],"Data.csv",row.names = F)


PlotNullDistribution_HMLET(res)
PlotTimeSeries_HMLET(res)
PlotTimeSeries_HMLET(res, onlySignificantClusters = F)



