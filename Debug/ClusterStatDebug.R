

data = dat
paired = T
detailed = F
threshold_t = NULL

  
if(is.factor(data$timepoint)){
    data$timeBin = data$timepoint
    data$timepoint = as.numeric(factor(data$timeBin, levels = levels(data$timeBin), labels = 1:length(levels(data$timeBin))))
    warning(paste("\n    >> Timepoints are converted to numeric indexes: ",
                  "\n    >> ",c(paste(levels(data$timeBin),1:length(levels(data$timeBin)), sep = " -> ", collapse = "   ")), sep = ""))
  }
  
  
  datSave = data
  
  clusterInfAll = NULL
  tValuesAll    = NULL
  
  for(testName in unique(datSave$testName)){
    data = datSave[datSave$testName == testName,]
    
    A = unique(data[,c("ID","trial","condition")])
    # as.data.frame(summarise(group_by(unique(data[,c("ID","trial","condition")]), ID, condition),N=n()))
    
    
    rmIdx = as.data.frame(dplyr::summarise(dplyr::group_by(data,ID,timepoint,condition,testName), prop = mean(AOI, na.rm=T)))
    rmIdx = reshape2::dcast(rmIdx, ID+timepoint+testName~condition, value.var = "prop")
    rmIdx = as.data.frame(mutate(group_by(rmIdx, ID),N=n()))
    rmIdx = rmIdx[!complete.cases(rmIdx),]
    # as.data.frame(summarise(group_by(rmIdx, ID , N), Nrm=n(), nProp = round(n()/mean(N)*100,1)))

    data = RemoveIncompleteTimePoints_HMLET(data)
    
    # why we have difference here:
    A = as.data.frame(summarise(group_by(data, timepoint,condition), N=n()))
    
    
    if(is.null(threshold_t)){
      num_sub = length(unique(data$ID))
      threshold_t = qt(p=1-.05/2, df=num_sub-1)
    }
    resp_time = as.data.frame(dplyr::summarise(dplyr::group_by(data, ID, timepoint, condition), prop = mean(AOI, na.rm = T)))
    tValues = ComputeTValues_HMLET(resp_time, paired = paired)
    tValues = FindClusters_HMLET(tValues, threshold_t = threshold_t)
    
    sdat = reshape2::melt(tValues,id.vars = c("timepoint","value"),variable.name = "Direction", value.name = "index")
    sdat = sdat[sdat$index!=0,]
    if(nrow(sdat)!=0){
      sdat = as.data.frame(dplyr::summarise(dplyr::group_by(sdat,Direction,index),tStatistic = sum(value, na.rm=T)))
      tValueTemp = data.frame(Positive =  max(sdat$tStatistic), Negative = min(sdat$tStatistic))
    }else{
      tValueTemp = data.frame(Positive =  0, Negative =0)
    }
    
    tValues$testName = unique(data$testName)
    tValues = tValues[,c("testName",names(tValues)[names(tValues)!="testName"])]
    
    clusterInf = melt(tValues,id.vars = c("testName","timepoint","value"),variable.name = "Direction", value.name = "index")
    clusterInf = clusterInf[clusterInf$index!=0,]
    if(nrow(clusterInf)==0){
      warning(paste("\n    >> In test data: ",unique(data$testName),
                    "\n    >> No significant cluster found based on threshold Alpha = ",
                    threshold_t, sep = ""))
    }
    else{
      clusterInf = as.data.frame(dplyr::summarise(dplyr::group_by(clusterInf, testName, Direction, index),
                                                  tStatistic = sum(value), timeStart = min(timepoint), timeEnd = max(timepoint)))
    }
    
    clusterInfAll = rbind(clusterInfAll, clusterInf)
    tValuesAll    = rbind(tValuesAll, tValues)
  }
  


  
  ####################################
  data = dat
  timeBinWidth =  16.67
  timeMax = 2000
  timepoint = "timeStamp"
  FixatedOn = "FixatedOn"
  groupingColumns = c("SID","TrialNum","Retrieval","timeStamp","ConditionName","Target2","RespType2")
  AOIs = NULL
  timeForward = T
  aggregateFun = mean
  
  sort(unique(data$timeStamp))
  
  
  # This is just a check to make sure time bin size is not out of order
  minTimeStep = round(abs(diff(data[, timepoint])),2)
  minTimeStep = minTimeStep[minTimeStep>0]
  minTimeStep = min(minTimeStep, na.rm = T)
  if(timeBinWidth<minTimeStep){
    warning(paste("\n    >> Time bin width (",timeBinWidth,") is smaller than timepoint steps",
                  "\n    >> Time bin width is set to smallest non-zero timepoint step  =",
                  minTimeStep, sep = ""))
    timeBinWidth = minTimeStep
    
  }
  
  # applies aggregating function on binary AOIs within time bins per each level of grouping variables in specified order
  data = data[data[, timepoint]<=timeMax, ]
  data$timeBin = ceiling(data[, timepoint]/timeBinWidth-1)*timeBinWidth+timeBinWidth/2
  timeBinsOrder = unique(data$timeBin)
  timeBinsOrder = timeBinsOrder[order(timeBinsOrder)]
  if(timeForward){
    data$timeBin = factor(data$timeBin,levels = timeBinsOrder, labels = paste(round(timeBinsOrder-timeBinWidth/2,2),
                                                                              round(timeBinsOrder+timeBinWidth/2,2),sep = "->"))
  }else{
    timeBinsOrder = timeBinsOrder[length(timeBinsOrder):1]
    data$timeBin = factor(data$timeBin,levels = timeBinsOrder, labels = paste(round(timeBinsOrder-timeBinWidth/2,2),
                                                                              round(timeBinsOrder+timeBinWidth/2,2),sep = "->"))
  }
  
  data$timeBinIndex = as.numeric(factor(data$timeBin, levels = levels(data$timeBin), labels = 1:length(levels(data$timeBin))))
  
  # if AOIs are not specified then create an array of AOIs based on unique values from FixatedOn column
  if(is.null(AOIs)){
    AOIs = unique(data[,c(FixatedOn)])
    AOIs = AOIs[!is.na(AOIs)]
  }
  
  for (AOIName in AOIs){
    data[,paste("AOI", AOIName, sep = "_")] = unique(0)
    data[, paste("AOI", AOIName, sep = "_")][data[, FixatedOn]==AOIName] = unique(1)
  }
  
  # I moved this comment to here because the aggregation is taking place in the following lines
  # applies the aggregating function on binary AOIs within time bins per each level of the grouping variables defined by the user.
  if(!is.null(groupingColumns)){
    groupingColumns = c(groupingColumns, "timeBin", "timeBinIndex")
    data = data %>%
      dplyr::group_by_at(groupingColumns) %>%
      dplyr::summarise_at(c(paste("AOI", AOIs, sep="_")), aggregateFun, na.rm = TRUE) %>%
      as.data.frame()
  }