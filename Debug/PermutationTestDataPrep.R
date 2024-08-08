library(HMLET)
library(dplyr)
library(stats)
trialNum = 20 
subjectNum = 20
timeMax = 1000
samplingInterval = 17
effectInterval = c(250,350)
effectSize = .2
sdCond1 = 1
sdCond2 = 1
smoothingPar = .4 

sdPooled = sqrt((sdCond1^2+sdCond2)/2)
meanDiff = effectSize*sdPooled;
time = seq(from = samplingInterval, to = timeMax, by = samplingInterval)
sampleNum = length(time)  
  

Data = NULL
for (ID in 1:subjectNum){
  for (trial in 1:trialNum/2){
    A = rnorm(n = sampleNum, mean = 0, sd = sdCond1)
    A =  A+effectSize/2*exp(-(time-mean(effectInterval))^2/diff(effectInterval)^2) 
    A = smooth.spline(x = time,y = A, spar = smoothingPar)
    gazeMeasure  = A$y
    timePoint = time
    dataTrial = data.frame(timePoint, gazeMeasure)
    dataTrial$condition = "Cond1"
    dataTrial$trial = trial
    dataTrial$ID = ID
    Data = rbind(Data,dataTrial)
  }
  for (trials in ((trialNum/2+1):trialNum)){
    A = rnorm(n = sampleNum, mean = 0, sd = sdCond1)
    A =  A - effectSize/2*exp(-(time-mean(effectInterval))^2/diff(effectInterval)^2) 
    A = smooth.spline(x = time,y = A, spar = smoothingPar)
    gazeMeasure  = A$y
    timePoint = time
    dataTrial = data.frame(timePoint, gazeMeasure)
    dataTrial$condition = "Cond2"
    dataTrial$trial = trial
    dataTrial$ID = ID
    Data = rbind(Data,dataTrial)
  }
}

Data = PermutationTestDataPrep_HMLET(Data, ID = "ID",trial = "trial", 
                              timePoint = "timePoint",condition = "condition",
                              gazeMeasure = "gazeMeasure",conditionLevels = c("Cond1","Cond2"))
PlotTimeSeries_HMLET(Data)
