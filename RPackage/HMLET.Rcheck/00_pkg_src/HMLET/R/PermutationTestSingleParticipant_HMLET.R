#' Conduct trial level permutation test for a single participant.
#'
#' @param data data frame with temporal data for permutation tests routine, output from PermutationTestDataPrep_HMLET.R.
#' @param samples optional integer for number of samples, defaults to 2000.
#' @param threshold_t optional probability threshold for statistical comparison, defaults to NA and will be computed based on number of trials when "between trials
#'                    permutation" is called or will be computed based on number of subjects when "between subjects permutation" is called.
#'                    Alpha = 0.025.
#'
#' @return a list of permutation tests in which distribution of desired statistic under the null hypothesis is estimated in a large number of permutations of the original data
#' @export
#'
PermutationTestSingleParticipant_HMLET <- function(data, samples = 2000, threshold_t = NA){
  paired = F  # this test cannot be paired (Maris & Oostenveld, 2007)
  if(length(unique(data$ID))>1){
    stop("\nThis routine is for permutation test at trial level for one participant!
You may want to code your participant IDs within testName column.")
  }
  if(is.factor(data$timePoint)){
    data$timeBin = data$timePoint
    data$timePoint = as.numeric(factor(data$timeBin, levels = levels(data$timeBin), labels = 1:length(levels(data$timeBin))))
    warning(paste("\n    >> timePoints are converted to numeric indexes: ",
                  "\n    >> ",c(paste(levels(data$timeBin),1:length(levels(data$timeBin)), sep = " -> ", collapse = "   ")), sep = ""))
  }
  datSave = data

  clusterInfAll = NULL
  statValueDistAll = NULL
  statValuesAll    = NULL
  dataAll       = NULL

  for(testName in unique(datSave$testName)){
    print(paste("          -------->","Running permutation test for:",testName,"<--------"))
    data = datSave[datSave$testName == testName,]

    if(is.na(threshold_t)){
      # Threshold should be based on the number of trials
      num_trials = length(unique(data$trial))
      threshold_t = stats::qt(p=1-.05/2, df=num_trials-2)
    }
    #---> to safely use ClusterStats_HMLET as it computes number of observations from ID
    saveIDs = data$ID
    data$ID = data$trial
    res = ClusterStats_HMLET(data, paired = paired, detailed = T, threshold_t = threshold_t)
    clusterInf = res[[1]]
    statValues = res[[2]]

    if(nrow(clusterInf)>0){
      print("Creating unique permutation labels:")

      ###---------------------------
      print("Compute unique permutation labels for trials:")
      labels = unique(data[,c("trial","condition")])
      condLevels = levels(factor(labels$condition))
      labelsPerm = NULL
      L = UniquePermutations_HMLET(labels[,"condition"], uniqueLabels = condLevels, n = samples)
      L = as.data.frame(L)
      names(L) = gsub("V","perm",names(L))
      labels = cbind(labels,L)

      #----------------------------- Perform Permutation tests
      print("Estimate tStatistic distribution:")
      pb = utils::txtProgressBar(min = 0, max = 1 , initial = 0, style = 3)

      tValueDist = NULL
      for (itt in 1:(samples)){
        datItt = merge(data,labels[,c(1,2,2+itt)],by = c("trial","condition"),all.x=T)
        datItt$condition = datItt[, paste("perm",itt,sep = "")]
        resp_time = as.data.frame(dplyr::summarise(dplyr::group_by(datItt,ID,timePoint,condition),prop = mean(AOI, na.rm=T)))
        tValues = ComputeTValues_HMLET(resp_time, paired = paired)
        tValues = FindClusters_HMLET(tValues, threshold_t = threshold_t)
        sdat = melt(tValues,id.vars = c("timePoint","value"),variable.name = "Direction", value.name = "index")
        sdat = sdat[sdat$index!=0,]
        if(nrow(sdat)!=0){
          sdat = as.data.frame(dplyr::summarise(dplyr::group_by(sdat,Direction,index),tStatistic = sum(value, na.rm=T)))
          tValueTemp = data.frame(Positive =  max(sdat$tStatistic), Negative = min(sdat$tStatistic))
        }else{
          tValueTemp = data.frame(Positive =  0, Negative =0)
        }
        tValueDist = rbind(tValueDist,tValueTemp)
        utils::setTxtProgressBar(pb,itt/samples)
      }
      tValueDist$Positive = ifelse(tValueDist$Positive>=0,tValueDist$Positive,0)
      tValueDist$Negative = ifelse(tValueDist$Negative<=0,tValueDist$Negative,0)
      tValueDist$NullDist = ifelse(abs(tValueDist$Positive)>abs(tValueDist$Negative),tValueDist$Positive,tValueDist$Negative)
      statValueDist = tValueDist
      statValueDist$testName = unique(testName)
      statValueDist = statValueDist[,c("testName",names(statValueDist)[names(statValueDist)!="testName"])]

      clusterInf$pValue = unique(NA)
      clusterInf$significant = unique("")
      for(i in 1:nrow(clusterInf)){
        clusterInf$pValue[i]=mean(as.numeric(abs(statValueDist$NullDist)>abs(clusterInf$tStatistic[i])))
        clusterInf$significant[i] = ifelse(clusterInf$pValue[i]<0.05,"*",ifelse(clusterInf$pValue[i]<0.1,".",""))
      }
    }else{
      clusterInf = NULL
      statValueDist = NULL
    }

    clusterInfAll = rbind(clusterInfAll, clusterInf)
    statValueDistAll = rbind(statValueDistAll, statValueDist)
    statValuesAll    = rbind(statValuesAll, statValues)
    data$ID         = saveIDs
    dataAll       = rbind(dataAll, data)
  }
  return(list(clusterStat = clusterInfAll,
              statNULL = statValueDistAll,
              stastatValues = statValuesAll,
              samples = samples,
              filteredData = dataAll))
}
