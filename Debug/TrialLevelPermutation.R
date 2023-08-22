
data = dat
samples = 500
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
tValueDist = TrialLevelPermutationTestWithin_HMLET(data, samples = samples, paired = paired, threshold_t = threshold_t) #----
    
print("Compute unique permutation labels per subject:")
labels = unique(data[,c("ID","trial","condition")])
labelsPerm = NULL
for (sID in unique(labels$ID)){
  labelTemp = labels[labels$ID==sID,]
  L = UniquePermutations_HMLET(labelTemp[,"condition"], n = samples)
  L = data.frame(L)
  names(L) = 1:ncol(L)
  labelTemp = cbind(labelTemp,L)
  names(labelTemp) = c(names(labelTemp)[1:3],paste("perm",names(labelTemp)[-(1:3)],sep = ""))
  labelsPerm = rbind(labelsPerm,labelTemp)
}
labels = merge(labels,labelsPerm, by = c("ID","trial","condition"))

print("Estimate tStatistic distribution:")
pb = txtProgressBar(min = 0, max = 1 , initial = 0, style = 3)

tValueDist = NULL
for (itt in 1:(samples)){
  datItt = merge(data,labels[,c(1,2,3,3+itt)],by = c("ID","trial","condition"),all.x=T)
  if(nrow(data)!=nrow(datItt)){
    print(paste("Error in itt =", itt))
  }
  datItt$condition = datItt[, paste("perm",itt,sep = "")]
  resp_time = as.data.frame(dplyr::summarise(dplyr::group_by(datItt,ID,timepoint,condition),prop = mean(AOI, na.rm=T)))
  tValues = ComputeTValues_HMLET(resp_time, paired = paired)
  tValues = FindClusters_HMLET(tValues, threshold_t = threshold_t)
  sdat = melt(tValues,id.vars = c("timepoint","value"),variable.name = "Direction", value.name = "index")
  sdat = sdat[sdat$index!=0,]
  if(nrow(sdat)!=0){
    sdat = as.data.frame(dplyr::summarise(dplyr::group_by(sdat,Direction,index),tStatistic = sum(value, na.rm=T)))
    tValueTemp = data.frame(Positive =  max(sdat$tStatistic), Negative = min(sdat$tStatistic))
  }else{
    tValueTemp = data.frame(Positive =  0, Negative =0)
  }
  tValueDist = rbind(tValueDist,tValueTemp)
  setTxtProgressBar(pb,itt/samples)
}
tValueDist$Positive = ifelse(tValueDist$Positive>=0,tValueDist$Positive,0)
tValueDist$Negative = ifelse(tValueDist$Negative<=0,tValueDist$Negative,0)
tValueDist$NullDist = ifelse(abs(tValueDist$Positive)>abs(tValueDist$Negative),tValueDist$Positive,tValueDist$Negative)
close(pb)

# Rest ----
    
    
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