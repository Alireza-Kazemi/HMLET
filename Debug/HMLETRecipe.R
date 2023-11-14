# Sample Recipe for the HMLET


#-------------------------------------------------------- Interpolation -----
# requires zoo package
Interpolation_HMLET <- function(data, gazeXCoordinate, gazeYCoordinate, timePoint,
                                trial = "trial", ID = "ID", groupingVars = NULL,
                                maxBlinkDuration = 75, samplingDuration = NULL){
  if(is.null(samplingDuration)){
    samplingDuration = mean(data[,timePoint])
  }
  maxGap = ceiling(maxBlinkDuration/samplingDuration)
  datapointIdx = ""
  
  for (gVar in unique(c(trial, ID, groupingVars))){
    datapointIdx = paste(datapointIdx, data[,gVar], sep = "_")
  }

  gazeXInterp = paste(gazeXCoordinate,"_Interp",sep = "")
  gazeYInterp = paste(gazeYCoordinate,"_Interp",sep = "")
  
  data[, gazeXInterp] = unique(NA)
  data[, gazeYInterp] = unique(NA)
  
  for(dpIdx in unique(datapointIdx)){
    timePoints = data[datapointIdx == dpIdx, timePoint]
    
    values = data[datapointIdx == dpIdx, gazeXCoordinate]
    values = zoo::na.approx(values, x = timePoints, na.rm = FALSE, maxgap = maxGap)
    data[datapointIdx == dpIdx, gazeXInterp] = values
    
    values = data[datapointIdx == dpIdx, gazeYCoordinate]
    values = zoo::na.approx(values, x = timePoints, na.rm = FALSE, maxgap = maxGap)
    data[datapointIdx == dpIdx, gazeYInterp] = values
  }
  return(data)
}


RD = "D:\\Projects\\HMLET\\DataFiles\\Debug\\"
dat = read.table(paste(RD,"AlirezaTest2022.csv",sep = ""), header=TRUE, sep=",", strip.white = TRUE)
dat = dat[dat$PostDec == 0,]
dat = dat[!(dat$MediaName %in% c("center.jpg","waitTime","Retrieval summary.png")),]
dat$RecordingTimestamp = dat$RecordingTimestamp-min(dat$RecordingTimestamp)+1
names(dat)
dat = dat %>% group_by(trialName) %>%
          mutate(timeStamp = RecordingTimestamp-min(RecordingTimestamp)+1)%>%
          as.data.frame()

datN = Interpolation_HMLET(data = dat, gazeXCoordinate = "GazeX", 
                    gazeYCoordinate = "GazeY", 
                    timePoint = "timeStamp",
                    trial = "trialName", ID = "ParticipantName", groupingVars = NULL,
                    maxBlinkDuration = 75, samplingDuration = NULL)

write.csv(datN,paste(RD,"interpolated.csv",sep = ""), row.names = F)


#--------------------------------------------------------- Permutation Test N>5samples----
dat = datRet[datRet$PostDec==0,c("SID","TrialNum","Retrieval","ConditionName","timeStamp","Target2","RespType2","FixatedOn")]
dat = dat[dat$RespType2 %in% c("Corr Loc","Incorr Loc"),]
dat = dat[dat$timeStamp>=200,]
dat = dat[dat$timeStamp<=1800,]
groupingColumns = c("SID","TrialNum","Retrieval","timeStamp","ConditionName","Target2","RespType2")
dat = CreateTimeBinData_HMLET(data = dat,  timeBinWidth =  16.67, timeMax = 1800, timepoint = "timeStamp",
                              FixatedOn = "FixatedOn",groupingColumns = groupingColumns)

dat$Gazeprop = case_when(dat$RespType2 == "Corr Loc" ~  dat$AOI_Target/
                           (dat$AOI_Target+dat$AOI_Lure),
                         dat$RespType2 == "Incorr Loc" ~  dat$AOI_Lure/
                           (dat$AOI_Target+dat$AOI_Lure))

dat = PermutationTestDataPrep_HMLET(data = dat, ID = "SID", trial = "TrialNum", 
                                    timepoint = "timeStamp",
                                    condition = "RespType2", conditionLevels = c("Corr Loc","Incorr Loc"),
                                    gazeMeasure = "Gazeprop", testName = "ConditionName")

dat = dat[!is.nan(dat$AOI),]
dat = group_by(dat,ID,testName,condition,timepoint) %>%
  mutate(timepointCount = n()) %>%
  as.data.frame() %>%
  group_by(testName,condition,timepoint) %>%
  mutate(SubjCount = n()) %>%
  as.data.frame()

dat = dat[dat$timepointCount>=5,] 

# Res = ClusterStats_HMLET(dat, paired = T, detailed = F)
# PlotTemporalGazeTrends_HMLET(dat,showDataPointProp = F,clusterData = Res)

set.seed(5)
samples = 1000
resPT = PermutationTest_HMLET(dat, samples = samples, paired = T, permuteTrialsWithinSubject = F)
resPT[[1]]
write.csv(file = paste(WD,"PT_Gazeproportion_N5.csv",sep = ""), resPT[[1]], row.names = F)

PlotNullDistribution_HMLET(resPT)
p = PlotResPermutation(resPT,"Point",pointSize = .5)
plot(p)
graph2ppt(file= paste(WD,"PT_GazeproportionWith_N5_Point.pptx",sep = ""), width = 14, height = 7)

#-----------------------------------Overall Values:
sDat = resPT[[5]]
sDat = as.data.frame(summarise(group_by(sDat,ID,trial,testName,condition),  value = mean(AOI,na.rm=T)))
sDat = as.data.frame(summarise(group_by(sDat,ID,testName,condition), 
                               value = mean(value,na.rm=T)))


ggplot(sDat,aes(x=testName, y=value, fill=condition)) + 
  geom_bar(stat="summary",fun="mean",position="dodge")+
  stat_summary(fun.data = "mean_se", geom="errorbar",position="dodge")+
  theme_bw(base_family = "serif")+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank())

sDat = reshape2::dcast(sDat, ID+testName~condition, value.var = "value")
sDat = sDat[complete.cases(sDat),]

t.test(sDat$`Corr Loc`[sDat$testName == "Far"],
       sDat$`Incorr Loc`[sDat$testName == "Far"],paired = T)

t.test(sDat$`Corr Loc`[sDat$testName == "Close"],
       sDat$`Incorr Loc`[sDat$testName == "Close"],paired = T)



#---------------------------------------------------------  Gaze disproportion Distribution  final ----
dat = datRet[datRet$PostDec==0,c("SID","TrialNum","Retrieval","ConditionName","timeStamp","Target2","RespType2","FixatedOn")]
dat = dat[dat$timeStamp>200,]
dat = dat[dat$timeStamp<=2000,]
dat = dat[dat$RespType2 %in% c("Corr Loc","Incorr Loc"),]
dat = dat[dat$FixatedOn %in% c("Target","Lure","Content"),]


data = dat
ID = "SID"
trial = "TrialNum"
AOIName = "FixatedOn"
comparingAOI = c("Target","Lure")
targetAOI = "Target"
dispropotionCriterion =10

groupingColumns = c(ID, trial, AOIName)
data = data %>%
  dplyr::group_by_at(c(ID, trial, AOIName)) %>%
  dplyr::summarise(N = n()) %>%
  dplyr::group_by_at(c(ID,trial)) %>%
  dplyr::mutate(prop = sum(N)) %>%
  as.data.frame()
data$prop = data$N/data$prop *100
data = data[data[,AOIName] %in% comparingAOI,]
data = reshape2::dcast(data, paste(ID,"+",trial,"~",AOIName,sep = ""), value.var = "prop",fill = 0.001)

data$DP = dplyr::case_when((data[,comparingAOI[1]] - data[,comparingAOI[2]])>dispropotionCriterion ~ paste("DP",comparingAOI[1],sep = "_"),
                           (data[,comparingAOI[2]] - data[,comparingAOI[1]])>dispropotionCriterion ~ paste("DP",comparingAOI[2],sep = "_"),
                            TRUE ~ "Equal")


sDat$condition = paste(sDat$RespType2 , sDat$DP, sep = "_")

binWidth = 5
sDat$TargetViewing = ceiling(sDat$Target/binWidth-1)*binWidth+binWidth/2

datDensity = sDat


equalTrials = datDensity
equalTrials = as.data.frame(mutate(group_by(equalTrials, SID, ConditionName), trialCount=n()))
d = equalTrials %>% group_by(SID, ConditionName, condition) %>% 
  summarise(trialCount = round(mean(n()/trialCount, na.rm = T), 2)) %>%
  group_by(ConditionName, condition) %>% 
  summarise(M = round(mean(trialCount, na.rm = T), 2),
            SD = round(sd(trialCount, na.rm = T), 2)) %>%
  as.data.frame()
# d = reshape2::dcast(d, SID+ConditionName ~condition)

equalTrials = equalTrials[equalTrials$DP == "Equal",]
equalTrials = as.data.frame(summarise(group_by(equalTrials,SID,ConditionName,trialCount),equalTrialCount=n()))
as.data.frame(summarise(group_by(equalTrials,ConditionName), 
                        M = round(mean(equalTrialCount/trialCount,na.rm = T),2),
                        SD = round(sd(equalTrialCount/trialCount,na.rm = T),2)))

datDensity = datDensity[datDensity$DP != "Equal",]
meanDat = as.data.frame(summarise(group_by(datDensity,ConditionName,condition),grp.mean = mean(TargetViewing,na.rm=T) ))


ggplot(datDensity, aes(x = TargetViewing, color = condition)) +
  geom_density(aes(y = after_stat(density*binWidth)),alpha = 0.5, linewidth = 1,kernel = "gaussian") +
  geom_vline(data = meanDat,aes(xintercept = grp.mean,color = condition),linetype="dashed", linewidth=1)+
  facet_wrap(~ConditionName , ncol = 2)+
  ylab("Proportion of Trials")+
  xlab("% Viewing Time on the Target")+
  theme_bw(base_family = "serif")+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank())+
  gDefault

graph2ppt(file= paste(WD,"GazeDisproportionDistributions.pptx",sep = ""), width = 14, height = 7)

sDat = datDensity
sDat = as.data.frame(summarise(group_by(sDat,TargetViewing,ConditionName,condition), N = n()))
sDat = as.data.frame(mutate(group_by(sDat,ConditionName,condition), TrialPercent = sum(N)))
sDat$TrialPercent = sDat$N/sDat$TrialPercent*100


sDat = as.data.frame(summarise(group_by(sDat,TargetViewing,ConditionName,condition),TrialPercent = mean(TrialPercent,na.rm=T) ))
sDat = reshape2::dcast(sDat, ConditionName+condition~TargetViewing, value.var = "TrialPercent", fill = 0)
sDat = reshape2::melt(sDat, id.vars = c("ConditionName","condition"), variable.name = "TargetViewing",
                      value.name = "TrialPercent")

sDat$TargetViewing = as.numeric(as.character(sDat$TargetViewing))
sDat$TrialPercent = as.numeric(as.character(sDat$TrialPercent))
sDat = sDat[!grepl("Equal",sDat$condition),]

as.data.frame(summarise(group_by(sDat,ConditionName,condition),N=n() ))
ggplot(sDat) +
  geom_line(aes(x = TargetViewing,y = TrialPercent, color = condition), stat = "spline", linewidth = 1)+
  facet_wrap(~ConditionName , ncol = 2)+
  theme_bw(base_family = "serif")+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank())


ggplot(datDensity) +
  geom_density(aes(y = after_stat(density*binWidth*100), x = TargetViewing, color = condition), alpha = 0.5, linewidth = 1) +
  geom_line(data = sDat, aes(x = TargetViewing,y = TrialPercent, color = condition),
            stat = "spline", alpha = 0.5,linewidth = 1)+
  facet_wrap(~ConditionName , ncol = 2)+
  theme_bw(base_family = "serif")+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank())

#-----------------------------------Runtest:

sDat = datDensity
as.data.frame(summarise(group_by(sDat,ConditionName,condition), 
                        M = mean(TargetViewing,na.rm=T),
                        SD = sd(TargetViewing,na.rm=T)))


t.test(sDat$TargetViewing[sDat$condition == "Corr Loc_DPL" & sDat$ConditionName == "Close"],
       sDat$TargetViewing[sDat$condition == "Incorr Loc_DPL" & sDat$ConditionName == "Close"],paired = F)

t.test(sDat$TargetViewing[sDat$condition == "Corr Loc_DPL" & sDat$ConditionName == "Far"],
       sDat$TargetViewing[sDat$condition == "Incorr Loc_DPL" & sDat$ConditionName == "Far"],paired = F)

t.test(sDat$TargetViewing[sDat$condition == "Corr Loc_DPT" & sDat$ConditionName == "Close"],
       sDat$TargetViewing[sDat$condition == "Incorr Loc_DPT" & sDat$ConditionName == "Close"],paired = F)

t.test(sDat$TargetViewing[sDat$condition == "Corr Loc_DPT" & sDat$ConditionName == "Far"],
       sDat$TargetViewing[sDat$condition == "Incorr Loc_DPT" & sDat$ConditionName == "Far"],paired = F)

t.test(sDat$TargetViewing[sDat$condition == "Corr Loc_DPT" & sDat$ConditionName == "Far"],
       sDat$TargetViewing[sDat$condition == "Corr Loc_DPT" & sDat$ConditionName == "Close"],paired = F)

t.test(sDat$TargetViewing[sDat$condition == "Incorr Loc_DPT" & sDat$ConditionName == "Far"],
       sDat$TargetViewing[sDat$condition == "Incorr Loc_DPT" & sDat$ConditionName == "Close"],paired = F)


#-----------------------------------Runtest within Participants:

sDat = datDensity
sDat = as.data.frame(summarise(group_by(sDat,SID,ConditionName,condition), TargetViewing = mean(TargetViewing,na.rm=T)))
sDat = reshape2::dcast(sDat, SID+ConditionName~condition, value.var = "TargetViewing", fill = 0)


t.test(sDat$`Corr Loc_DPL`[sDat$ConditionName=="Close"],
       sDat$`Incorr Loc_DPL`[sDat$ConditionName == "Close"],paired = T)
t.test(sDat$`Corr Loc_DPL`[sDat$ConditionName=="Far"],
       sDat$`Incorr Loc_DPL`[sDat$ConditionName == "Far"],paired = T)
t.test(sDat$`Corr Loc_DPT`[sDat$ConditionName=="Close"],
       sDat$`Incorr Loc_DPT`[sDat$ConditionName == "Close"],paired = T)
t.test(sDat$`Corr Loc_DPT`[sDat$ConditionName=="Far"],
       sDat$`Incorr Loc_DPT`[sDat$ConditionName == "Far"],paired = T)

t.test(sDat$`Corr Loc_DPL`[sDat$ConditionName=="Far"],
       sDat$`Corr Loc_DPL`[sDat$ConditionName == "Close"],paired = T)
t.test(sDat$`Incorr Loc_DPL`[sDat$ConditionName=="Far"],
       sDat$`Incorr Loc_DPL`[sDat$ConditionName == "Close"],paired = T)
t.test(sDat$`Corr Loc_DPT`[sDat$ConditionName=="Far"],
       sDat$`Corr Loc_DPT`[sDat$ConditionName == "Close"],paired = T)
t.test(sDat$`Incorr Loc_DPT`[sDat$ConditionName=="Far"],
       sDat$`Incorr Loc_DPT`[sDat$ConditionName == "Close"],paired = T)



t.test(sDat$`Corr Loc_DPT`[sDat$ConditionName=="Close"],
       sDat$`Corr Loc_DPL`[sDat$ConditionName == "Close"],paired = T)

t.test(sDat$`Incorr Loc_DPT`[sDat$ConditionName=="Far"],
       sDat$`Incorr Loc_DPL`[sDat$ConditionName == "Far"],paired = T)


#-----------------------------------Graph within Participants:
sDat = datDensity
sDat = as.data.frame(summarise(group_by(sDat,SID,ConditionName,condition), TargetViewing = mean(TargetViewing,na.rm=T)))
sDat = reshape2::dcast(sDat, SID+ConditionName~condition, value.var = "TargetViewing", fill = 0)
sDat = reshape2::melt(sDat, id.vars = c("SID","ConditionName"), variable.name = "condition",
                      value.name = "TargetViewing")

ggplot(sDat,aes(x=ConditionName, y=TargetViewing, color=condition, fill=condition)) + 
  geom_bar(stat="summary",fun="mean",position="dodge")+
  stat_summary(fun.data = "mean_se", geom="errorbar",position="dodge",color = "Black")+
  geom_point(position = position_jitterdodge(jitter.width = .4,
                                             jitter.height = 0,
                                             dodge.width = .9),shape = 21,fill="grey")+
  ylab("% Viewing Time on the Target")+
  xlab("% Viewing Time on the Target")+
  theme_bw(base_family = "serif")+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank())+
  gDefault

graph2ppt(file= paste(WD,"GazeDisproportionDistributions_WithinParticipant.pptx",sep = ""), width = 14, height = 7)
results=as.data.frame(ezANOVA(data=sDat, dv="TargetViewing", wid=.("SID"), within=.("condition","ConditionName"),
                              type=1,detailed=T)$ANOVA)
results$pareta=results$SSn/(results$SSn+results$SSd)
is.num=sapply(results, is.numeric)
results[is.num] =lapply(results[is.num], round, 3)
results


