
data = dat
samples = 200
paired = T 
permuteTrialsWithinSubject = T   
threshold_t = NA

set.seed(5)

################################################### Compute subject-level unique labels
datSave = data

clusterInfAll = NULL
tValueDistAll = NULL
tValuesAll    = NULL
dataAll       = NULL

testName = unique(datSave$testName)[1]
# for(testName in unique(datSave$testName)){
print(paste("          -------->","Running permutation test for:",testName,"<--------"))
data = datSave[datSave$testName == testName,]

if(is.na(threshold_t)){
  # Treshold should be based on the permutation sampling
  num_sub = length(unique(data$ID))
  threshold_t = stats::qt(p=1-.05/2, df=num_sub-1)
}
data = RemoveIncompleteTimePoints_HMLET(data)
  
  
res = ClusterStats_HMLET(data, paired = paired, detailed = T, threshold_t = threshold_t)
clusterInf = res[[1]]
tValues = res[[2]]


print("Creating unique permutation labels:")
tValueDist = TrialLevelPermutationTestWithin_HMLET(data, samples = samples, paired = paired, threshold_t = threshold_t)
    

    
    
    
    
    
  tValueDist$testName = unique(testName)
  tValueDist = tValueDist[,c("testName",names(tValueDist)[names(tValueDist)!="testName"])]
  
  clusterInf$pValue = unique(NA)
  clusterInf$significant = unique("")
  for(i in 1:nrow(clusterInf)){
    clusterInf$pValue[i]=mean(as.numeric(abs(tValueDist$NullDist)>abs(clusterInf$tStatistic[i])))
    clusterInf$significant[i] = ifelse(clusterInf$pValue[i]<0.05,"*",ifelse(clusterInf$pValue[i]<0.1,".",""))
  }


clusterInfAll = rbind(clusterInfAll, clusterInf)
tValueDistAll = rbind(tValueDistAll, tValueDist)
tValuesAll    = rbind(tValuesAll, tValues)
dataAll       = rbind(dataAll, data)
# }