########################### Initialization ##########################
rm(list=ls(all=TRUE))


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
       emmeans,
       eyetrackingR,miceadds)

path = "C:/Users/kazemi/Documents/GitHub/MLET/Functions"
p_load(combinat,simctest,rray,purrr,utils)

# remotes::install_github("r-lib/rray")
options(dplyr.summarise.inform = FALSE)

source.all(path = path)
############################## test my dataset -----
d = read.csv("sampleData_0.2.csv")

sdat = ClusterStats_MLET(d, paired = T, detailed = F)


num_sub = length(unique(d$ID))  
threshold_t = qt(p=1-.05/2, df=num_sub-1)
set.seed(5)
samples = 2000
Res = PermutationTest_MLET(d, samples = samples, paired = T, permuteTrialsWithinSubject = F, threshold_t = threshold_t)
Res[[1]]
ggplot(Res[[2]], aes(x=NullDist)) +
  geom_histogram( color="#e9ecef", position = 'identity', bins =50)

clust_analysis = RunEyeTrackingRRoutine(d,samples)

summary(clust_analysis)
Res[[1]]

A = Res[[2]]
A$cond = "MLETSL"
A = A[,c("cond","NullDist")]
B = A
B$cond = "EyeTR"
B$NullDist = clust_analysis$null_distribution
datCompare = rbind(A,B)
ggplot(datCompare, aes(x=NullDist, fill = cond)) +
  geom_histogram( color="#e9ecef", position = 'identity', bins =100)+
  facet_wrap(~cond, nrow = 2)

#---------------------------------------   TrialLevel permutation

num_sub = length(unique(data$timepoint))
threshold_t = qt(p=1-.05/2, df=num_sub-1)
set.seed(5)
samples2 = 2000
Res2 = PermutationTest_MLET(d, samples = samples2, paired = T, permuteTrialsWithinSubject = T, threshold_t = threshold_t)
Res2[[1]]
ggplot(Res2[[2]], aes(x=NullDist)) +
  geom_histogram( color="#e9ecef", position = 'identity', bins =50)

Btemp = Res2[[2]]
Btemp$cond = "MLETTL"
datCompare = rbind(datCompare,Btemp[,c("cond","NullDist")])
ggplot(datCompare, aes(x=NullDist, fill = cond)) +
  geom_histogram( color="#e9ecef", position = 'identity', bins =100)+
  facet_wrap(~cond, nrow = 3)



###############################################################################
Cdat = read.csv("TdistributionUnderNull.csv")

Cdat = melt(Cdat,id.vars = "X", variable.name = "Dist" )

ggplot(Cdat, aes(x=value, fill=Dist)) +
  geom_histogram( color="#e9ecef", position = 'identity', bins =50)+
  facet_wrap(~Dist, nrow = 3)

tdist = reshape2::dcast(Cdat, X~Dist, value.var = "value" )
tdist$Positive = ifelse(tdist$MyVersionPositiveT<0,NA,tdist$MyVersionPositiveT)
tdist$Negative = ifelse(tdist$MyVersionNegativeT>=0,NA,tdist$MyVersionNegativeT)
tdist$Positive[tdist$Positive==0 & tdist$Negative==0] = NA
# tdist$Negative[is.na(tdist$Positive)] = NA

B = tdist[,c("X","Positive","Negative")]
B = melt(B,id.vars = "X", variable.name = "Dist" )
ggplot(B, aes(x=value)) +
  geom_histogram( color="#e9ecef", position = 'identity', bins =50)


############################# simctest test---------
#Example used in the above paper
p_load(simctest)
dat <- matrix(nrow=5,ncol=7,byrow=TRUE,
              c(1,2,2,1,1,0,1, 2,0,0,2,3,0,0, 0,1,1,1,2,7,3, 1,1,2,0,0,0,1, 0,1,1,1,1,0,0))
loglikrat <- function(data){
  cs <- colSums(data)
  rs <- rowSums(data)
  mu <- outer(rs,cs)/sum(rs)
  2*sum(ifelse(data<=0.5, 0,data*log(data/mu)))
}
resample <- function(data){
  cs <- colSums(data)
  rs <- rowSums(data)
  n <- sum(rs)
  mu <- outer(rs,cs)/n/n
  matrix(rmultinom(1,n,c(mu)),nrow=dim(data)[1],ncol=dim(data)[2])
}
t <- loglikrat(dat);

# function to generate samples
gen <- function(){loglikrat(resample(dat))>=t}

res <- simctest(gen,maxsteps=5000)
res
############################### test my Own Data------
# this data should have difference in timepoint = 5seconds
RunEyeTrackingRRoutine <- function(d,samples){
  # d = read.csv("sampleData_0.2.csv")
  d$TrackLoss = unique(FALSE)
  d$AOI1 = ifelse(d$AOI==1,TRUE,FALSE)
  d$AOI2 = ifelse(d$AOI==0,TRUE,FALSE)
  
  data <- make_eyetrackingr_data(d, 
                                 participant_column = "ID",
                                 trial_column = "trial",
                                 time_column = "timepoint",
                                 trackloss_column = "TrackLoss",
                                 aoi_columns = c("AOI1","AOI2"),
                                 treat_non_aoi_looks_as_missing = TRUE)
  
  response_time <- make_time_sequence_data(data, time_bin_size = 1, aois = "AOI1", 
                                           predictor_columns = "condition",summarize_by = "ID")
  
  plot(response_time, predictor_column = "condition") 
  
  num_sub = length(unique(data$ID))  
  threshold_t = qt(p=1-.05/2, df=num_sub-1)
  
  time_cluster_data <- make_time_cluster_data(data = response_time, predictor_column = "condition", 
                                              aoi = "AOI1", test = "t.test",paired=T, 
                                              threshold = threshold_t)
  # plot(time_cluster_data) 
  # summary(time_cluster_data)
  set.seed(5)
  clust_analysis <- analyze_time_clusters(time_cluster_data, within_subj=TRUE, paired=TRUE,
                                          samples=samples)
  return(clust_analysis)

}

summary(clust_analysis)

plot(clust_analysis)
clust_analysis1000 = clust_analysis
clust_analysis2000 = clust_analysis


############################## test my dataset myown permutation -----
p_load(combinat,simctest,rray,purrr,utils)
# remotes::install_github("r-lib/rray")
options(dplyr.summarise.inform = FALSE)

d = read.csv("sampleData_0.2.csv")

##----------------- function development tests 
resp_time = as.data.frame(summarise(group_by(d,ID,timepoint,condition),prop = mean(AOI)))
tValues = ComputeTValues(resp_time,paired = T)
tValues = FindClusters(tValues, length(unique(resp_time$ID)))
sdat = melt(tValues,id.vars = c("timepoint","value"),variable.name = "Direction", value.name = "index")
sdat = sdat[sdat$index!=0,]
sdat = as.data.frame(summarise(group_by(sdat,Direction,index),tStatistic = sum(value), timeStart = min(timepoint), timeEnd = max(timepoint)))
sdat

Res = PermutationTest(d,samples = 2000,permuteTrialsWithinSubject = F)
Res[1]


Res3 = PermutationTest(d,samples = 500,permuteTrialsWithinSubject = T)
Res3[1]

B = matrix(unlist(Res3[2]),nrow = 500)
B = data.frame(Positive = B[,1], Negative = B[,2])
B$X = 1:nrow(B)
B = melt(B,id.vars = "X", variable.name = "Dist" )
ggplot(B, aes(x=value, fill=Dist)) +
  geom_histogram( color="#e9ecef", position = 'identity', bins =50)+
  facet_wrap(~Dist, nrow = 2)

B = matrix(unlist(Res2[2]),nrow = 200)
B = data.frame(Positive = B[,1], Negative = B[,2])
B$X = 1:nrow(B)
B = melt(B,id.vars = "X", variable.name = "Dist" )
ggplot(B, aes(x=value, fill=Dist)) +
  geom_histogram( color="#e9ecef", position = 'identity', bins =50)+
  facet_wrap(~Dist, nrow = 2)


##########################
Cdat = read.csv("TdistributionUnderNull.csv")

Cdat = melt(Cdat,id.vars = "X", variable.name = "Dist" )

ggplot(Cdat, aes(x=value, fill=Dist)) +
  geom_histogram( color="#e9ecef", position = 'identity', bins =50)+
  facet_wrap(~Dist, nrow = 3)

############ My functions -------
ComputeTValues <- function(respTime, paired = TRUE){
  respTime = respTime[order(respTime$ID,respTime$timepoint,respTime$condition),]
  if(paired){
    # respTime = reshape2::dcast(respTime,ID+timepoint~condition, value.var = "prop")
    # respTime$d = respTime$C1 - respTime$C2
    respTime = as.data.frame(summarise(group_by(respTime,ID,timepoint), d = -diff(prop)))
    tValues = as.data.frame(summarise(group_by(respTime,timepoint), value = sum(d)/sqrt((n()*sum(d^2)-sum(d)^2)/(n()-1)) ))
  }else{
    temp = as.data.frame(summarise(group_by(respTime,timepoint,condition), N = n(), M = mean(prop), SD = sd(prop)))
    tValues = as.data.frame(summarise(group_by(temp,timepoint), value = (-diff(M)/sqrt(sum(SD^2/N))) ))
  }
  return(tValues)
}

FindClusters <- function(tValues, num_sub){
  threshold_t = qt(p=1-.05/2, df=num_sub-1)
  tValues$Positive = unique(0)
  tValues$Negative = unique(0)
  tValues$Positive[tValues$value>threshold_t]=1
  tValues$Negative[tValues$value<(-1*threshold_t)]=1
  
  tValues$temp = ifelse(diff(c(0,tValues$Negative))==1,1,0)
  tValues$temp = cumsum(tValues$temp)
  tValues$Negative = tValues$Negative*tValues$temp
  
  tValues$temp = ifelse(diff(c(0,tValues$Positive))==1,1,0)
  tValues$temp = cumsum(tValues$temp)
  tValues$Positive = tValues$Positive*tValues$temp
  
  tValues = tValues[,c("timepoint","value","Positive","Negative")]
  return(tValues)
}

UniquePermutations <- function(listInput, n = 1){
  L = length(listInput)
  indexList0 = rray(1:L, dim = c(L, 1))
  indexList = indexList0
  
  pb = txtProgressBar(min = 0, max = 1 , initial = 0, style = 3)
  # rep = 0                                                   # ------------------> Debugging
  for (i in 1:n){
    rep = 0                                                   
    repeat{
      newComb = rray(sample(indexList0,L,replace = F), dim = c(L,1))
      if(min(rray_sum(abs(indexList - newComb),axes = 1))>1 | rep>5000){
        # print(rray_sum(abs(indexList - newComb),axes = 1))  # ------------------> Debugging
        break
      }
      rep = rep+1
    }
    indexList = rray_bind (indexList,newComb, .axis = 2)
    setTxtProgressBar(pb,i/n)
  }
  # print(rep)                                              # ------------------> Debugging
  close(pb)
  return(matrix(listInput[indexList],nrow = L))
}



ComputeSubjectLevelPerm <- function(labels, n = 1){
  condNum = length(unique(labels$condition))
  labelNew = unique(labels[,c("ID","timepoint")])
  listInput = sample(x = 1:factorial(condNum), size =  nrow(labelNew), replace = T)
  subjLevelPerms = UniquePermutations(listInput, n-1)
  condList = levels(factor(labels$condition))
  condLevelPerms = permn(factorial(condNum))
  
  print("Converting labels:")
  pb = txtProgressBar(min = 0, max = 1 , initial = 0, style = 3)
  for (i in 1:n){
    labels <- cbind(labels,
    map_dfr(as.list(subjLevelPerms[,i]),function(x){
      data.frame(perm = c(condList[unlist(condLevelPerms[x])]))
    }))
    setTxtProgressBar(pb,i/n)
  }
  close(pb)
  names(labels) = c(names(labels)[1:3],paste(names(labels)[-(1:3)],1:n,sep = ""))
  return(labels)
}

PermutationTest <- function(data, samples = 2000, permuteTrialsWithinSubject = F){
  print("Creating unique permutation labels:")
  if(permuteTrialsWithinSubject){
    labels = unique(data[,c("ID","trial","condition")])
    labelsNew = NULL
    for (sID in unique(labels$ID)){
      L = UniquePermutations(labels[labels$ID==sID,"condition"], n = samples) 
      labelsNew = rbind(labelsNew,L[,-1])
    }
    labels = cbind(labels,labelsNew)
    names(labels) = c(names(labels)[1:3],paste("perm",names(labels)[-(1:3)],sep = ""))
    
    #----------------------------- Perform Permutation tests
    print("Estimate tStatistic distribution:")
    pb = txtProgressBar(min = 0, max = 1 , initial = 0, style = 3)
    
    tValueDist = NULL
    for (itt in 1:(samples)){
      datItt = merge(data,labels[,c(1,2,3,3+itt)],by = c("ID","trial","condition"),all.x=T)
      if(nrow(data)!=nrow(datItt)){
        print(paste("Error in itt =", itt))
      }
      datItt$condition = datItt[, paste("perm",itt,sep = "")]
      resp_time = as.data.frame(summarise(group_by(datItt,ID,timepoint,condition),prop = mean(AOI)))
      tValues = ComputeTValues(resp_time,paired = T)
      tValues = FindClusters(tValues,length(unique(resp_time$ID)))
      sdat = melt(tValues,id.vars = c("timepoint","value"),variable.name = "Direction", value.name = "index")
      sdat = sdat[sdat$index!=0,]
      if(nrow(sdat)!=0){
        sdat = as.data.frame(summarise(group_by(sdat,Direction,index),tStatistic = sum(value)))
        tValueTemp = data.frame(Positive =  max(sdat$tStatistic), Negative = min(sdat$tStatistic))
      }else{
        tValueTemp = data.frame(Positive =  0, Negative =0)
      }
      tValueDist = rbind(tValueDist,tValueTemp)
      setTxtProgressBar(pb,itt/samples)
    }

  }else{
    resp_time = as.data.frame(summarise(group_by(data,ID,timepoint,condition),prop = mean(AOI)))
    labels = unique(resp_time[,c("ID","timepoint","condition")])
    labels = ComputeSubjectLevelPerm(labels, n = samples)
    
    #----------------------------- Perform Permutation tests
    print("Estimate tStatistic distribution:")
    pb = txtProgressBar(min = 0, max = 1 , initial = 0, style = 3)
    
    tValueDist = NULL
    for (itt in 1:(samples)){
      resp_time$condition = labels[,paste("perm",itt,sep = "")]
      tValues = ComputeTValues(resp_time,paired = T)
      tValues = FindClusters(tValues,length(unique(resp_time$ID)))
      sdat = melt(tValues,id.vars = c("timepoint","value"),variable.name = "Direction", value.name = "index")
      sdat = sdat[sdat$index!=0,]
      if(nrow(sdat)!=0){
        sdat = as.data.frame(summarise(group_by(sdat,Direction,index),tStatistic = sum(value)))
        tValueTemp = data.frame(Positive =  max(sdat$tStatistic), Negative = min(sdat$tStatistic))
      }else{
        tValueTemp = data.frame(Positive =  0, Negative =0)
      }
      tValueDist = rbind(tValueDist,tValueTemp)
      setTxtProgressBar(pb,itt/samples)
    }
  }
  
  
  close(pb)
  resp_time = as.data.frame(summarise(group_by(data,ID,timepoint,condition),prop = mean(AOI)))
  tValues = ComputeTValues(resp_time,paired = T)
  tValues = FindClusters(tValues,length(unique(resp_time$ID))) 
  sdat = melt(tValues,id.vars = c("timepoint","value"),variable.name = "Direction", value.name = "index")
  sdat = sdat[sdat$index!=0,]
  sdat = as.data.frame(summarise(group_by(sdat,Direction,index),tStatistic = sum(value), timeStart = min(timepoint), timeEnd = max(timepoint)))
  sdat$pValue = unique(NA)
  for(i in 1:nrow(sdat)){
    if(sdat$tStatistic[i]>=0){
      sdat$pValue[i]=mean(as.numeric(tValueDist$Positive>sdat$tStatistic[i]))
    }else{
      sdat$pValue[i]=mean(as.numeric(tValueDist$Negative<sdat$tStatistic[i]))
    }
  }
  return(list(sdat,tValueDist,tValues))
}






######################################### Debug function ---------
source("PermutationFunc.R")
data = time_cluster_data
attrs <- attr(data, "eyetrackingR")
data_options <- attrs$data_options
summarized_by <- attrs$summarized_by
shuffle_by <- attrs$predictor_column
participants <- unique(data[[summarized_by]])

list_of_list_of_rows <- lapply(X = participants, FUN = function(ppt) {
  ppt_logical <- (data[[summarized_by]] == ppt)
  this_ppt_levels <- unique(data[[shuffle_by]][ppt_logical])
  out <- lapply(X = this_ppt_levels, FUN = function(lev) {
    which(ppt_logical & data[[shuffle_by]] == lev)
  })
  names(out) <- this_ppt_levels
  return(out)
})

df_resampled <- data
for (list_of_rows in list_of_list_of_rows) {
  resampled <- sample(x = list_of_rows, size = length(list_of_rows), 
                      replace = FALSE)
  for (i in seq_along(resampled)) {
    rows_orig <- list_of_rows[[i]]
    rows_new <- resampled[[i]]
    df_resampled[rows_new, attrs$predictor_column] <- data[first(rows_orig), 
                                                           attrs$predictor_column]
  }
}
