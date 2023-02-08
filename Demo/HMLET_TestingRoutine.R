library(HMLET)


RD = "C:\\Users\\kazemi\\Documents\\GitHub\\HMLET\\Demo\\"
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
res = PermutationTest_HMLET(datP, samples = 100, paired = T, permuteTrialsWithinSubject = F)
res[[1]]
PlotNullDistribution_HMLET(res)
PlotTemporalGazeTrends_HMLET(res)
PlotTemporalGazeTrends_HMLET(res, onlySignificantClusters = F)



