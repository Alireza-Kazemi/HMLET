library(HMLET)


RD = "C:\\Users\\kazemi\\Documents\\GitHub\\HMLET\\Demo\\"
WR = "C:\\Users\\kazemi\\Documents\\GitHub\\HMLET\\Demo\\"
dat = read.table(paste(RD,"HMLET_Testing_Data.csv",sep = ""), header=TRUE, sep=",", strip.white = TRUE)




datP = PermutationTestDataPrep_HMLET(data = dat, ID = "ID", trial = "trial", timepoint = "timepoint", 
                                    condition = "condition", conditionLevels = c("C1","C2"),
                                    gazeInAOI = "AOI", targetAOI = 1)

write.csv(datP,"Output.csv",row.names = F)

P = PlotTemporalGazeTrends_HMLET(datP)
plot(P)

Res = ClusterStats_HMLET(datP, paired = T, detailed = F)

P = PlotTemporalGazeTrends_HMLET(datP, clusterData = Res)
plot(P)

set.seed(5)
res = PermutationTest_HMLET(datP, samples = 1000, paired = T, permuteTrialsWithinSubject = F)

write.csv(res[[1]],paste(RD,"ClusterInfos.csv",sep = ""),row.names = F)
write.csv(res[[2]],paste(RD,"NullDistribution.csv",sep = ""),row.names = F)
write.csv(res[[3]],paste(RD,"tValueDistribution.csv",sep = ""),row.names = F)
write.table(res[[4]], paste(RD,"PermuationSampleNumber.txt",sep = ""), append = FALSE, sep = " ", dec = ".",
            row.names = F, col.names = F)
write.csv(res[[5]],paste(RD,"Data.csv",sep = ""),row.names = F)


PlotNullDistribution_HMLET(res)
PlotTemporalGazeTrends_HMLET(res)
PlotTemporalGazeTrends_HMLET(res, onlySignificantClusters = F)



