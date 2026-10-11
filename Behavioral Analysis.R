########################### Initialization ##########################
rm(list=ls(all=TRUE))

x = readline()
D:\Projects\Hippotime\DataFiles\L12_Children_data
setwd(x)
getwd()

library(pacman)
p_load(data.table,
       reshape2,
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
       ggformula,
       export,
       scales,
       correlation,
       sjPlot,
       gghalves,
       car)
library(HMLET)

RD = "D:\\Projects\\Hippotime\\DataFiles\\L12_Children_data\\"
WD = "D:\\Projects\\Hippotime\\DataFiles\\L15_Children_Manuscript\\"
###################################### Functions -----
#--------------------------- Post Hoc test Func.
PostHocT_Tests <- function(data , CompList = NULL, Paired = T, PAdjustM = "bonferroni", VarEqual = T,
                           Detailed = T, PoolSD = F, Alternative = "two.sided",PrintOutput = T){
  results <- data  %>%
    pairwise_t_test(value ~ Conds, pool.sd = PoolSD, var.equal=VarEqual, paired = Paired,
                    detailed = Detailed,
                    alternative = Alternative,
                    comparisons = CompList,
                    p.adjust.method = PAdjustM) %>%
    mutate_if(is.numeric, round,dPVts = 3) %>%
    as.data.frame()
  
  CD <- data %>%
    cohens_d(value~Conds, var.equal = VarEqual,paired = Paired,
             comparisons = CompList) %>%
    as.data.frame()
  
  Marginals <- data %>%
    group_by(Conds)%>%
    summarise(M = mean(value, na.rm=T),
              SD = sd(value, na.rm=T)) %>%
    mutate_if(is.numeric, round,dPVts = 2) %>%
    as.data.frame()
  
  results = results[,c("group1", "group2", "statistic", "df", "p", "p.adj", "p.adj.signif", "conf.low", "conf.high")]
  results %>% mutate_at(c("statistic", "df", "conf.low", "conf.high"), round,dPVts = 2)
  results$cohenD = round(CD$effsize,2)
  if(PrintOutput){
    print(results)
    print(Marginals)
  }
  out = list(results,Marginals)
}

#--------------------------- Graph Permutation Rsults preset function
PlotResPermutation<- function(res,PointorLine = "Point", yLab = "Gaze Proportion", pointSize = .5){
  p = PlotTemporalGazeTrends_HMLET(res, showDataPointProp =F,
                                   showOverallMean = PointorLine,
                                   tickSizeOverallMean = 100,
                                   gazePropRibbonAlpha = 0.2,
                                   clusterFillAlpha  = 0.7,
                                   alphaOverallMean = .7,
                                   yLabel = yLab,
                                   pointSize = pointSize) 
  p = p + 
    facet_wrap(~testName,nrow = 2)+
    scale_x_continuous(
      breaks = c(200, seq(200, 2100, by = 200)),
      limits = c(200, 2000),
      expand = c(0, 0)
    )+
    gDefault
  return(p)
}
#--------------------------- Correlation Function 
ComputeCorrelations <- function(sDat, Par1Names, Par2Names,condName , par1ColName = "Cluster", par2ColName = "Subfield"){
  CorVals = NULL
  for (Par1 in Par1Names)
  {
    for (Par2 in Par2Names)
    {
      Vals = correlation(data = sDat,
                         select = Par1,
                         select2 = c(Par2,"ICV"),
                         partial = T,
                         p_adjust = "none")
      CorVals = rbind(CorVals,Vals)
    }
  }
  
  A = as.data.frame(CorVals[CorVals$Parameter2!="ICV",c("Parameter1","Parameter2","r","p","t","df_error","CI_low","CI_high")])
  A$Sig = unique("")
  A$Sig[A$p<0.05] = unique("*")
  names(A) = c(par1ColName, par2ColName, "r", "p","t","df","CI_low","CI_high", "Sig")
  A$Accuracy = sapply(A[,par1ColName], function(x) strsplit(x,split = "_")[[1]][1])
  A[,par1ColName] = sapply(A[,par1ColName], function(x) strsplit(x,split = "_")[[1]][2])
  A$Condition = condName
  A = A[,c(par1ColName, par2ColName,"Accuracy","Condition", "r", "p","t","df","CI_low","CI_high", "Sig")]
  return(A)
}
#--------------------------- Get average across specific time windows
ComputeAverageAcrossTime <- function(dat,sigCStart,sigCEnd, condName){
  sDat = NULL;
  for (idx in (1:length(sigCStart))){
    d = dat[dat$testName == condName,]
    d = d[(d$timePoint >= sigCStart[idx]) & (d$timePoint < sigCEnd[idx]),]
    d = d %>% group_by(ID,trial,condition) %>%
      summarise(value = mean(AOI,na.rm=T)) %>% 
      group_by(ID,condition) %>%
      summarise(value = mean(value,na.rm=T)) %>% as.data.frame()
    d$clusterID = paste("C",sigCStart[idx],"TO",sigCEnd[idx], sep = "")
    sDat = rbind(sDat,d)
  }
  sDat = reshape2::dcast(sDat,ID+clusterID~condition, value.var = "value")
  sDat$Diff = sDat$CorrLoc - sDat$IncorrLoc
  # sDat = reshape2::dcast(sDat,ID~clusterID, value.var = c("value","CorrLoc","IncorrLoc"))
  sDat =  pivot_wider(sDat, names_from = clusterID, values_from = c(Diff,CorrLoc,IncorrLoc),names_sep = "_")
  names(sDat) = c("SID",names(sDat)[-1])
  return(sDat)
}

#--------------------------- Get average across specific time windows and Far Close
ComputeAverageAcrossTimeFC <- function(dat,sigCStart,sigCEnd, condName){
  sDat = NULL;
  for (idx in (1:length(sigCStart))){
    # d = dat[dat$testName == condName,]
    d=dat
    d = d[(d$timePoint >= sigCStart[idx]) & (d$timePoint < sigCEnd[idx]),]
    d = d %>% group_by(ID,trial,condition) %>%
      summarise(value = mean(AOI,na.rm=T)) %>% 
      group_by(ID,condition) %>%
      summarise(value = mean(value,na.rm=T)) %>% as.data.frame()
    d$clusterID = paste("C",sigCStart[idx],"TO",sigCEnd[idx], sep = "")
    sDat = rbind(sDat,d)
  }
  sDat = reshape2::dcast(sDat,ID+clusterID~condition, value.var = "value")
  sDat$Diff = sDat$CorrLoc - sDat$IncorrLoc
  # sDat = reshape2::dcast(sDat,ID~clusterID, value.var = c("value","CorrLoc","IncorrLoc"))
  sDat =  pivot_wider(sDat, names_from = clusterID, values_from = c(Diff,CorrLoc,IncorrLoc),names_sep = "_")
  names(sDat) = c("SID",names(sDat)[-1])
  return(sDat)
}

###################################################### Load data -----
datRet = read.csv(paste(RD,"Children_Retrieval_Filtered.csv",sep = ""))
datRet$RespType2 = factor(datRet$RespType2,levels = c("Corr Loc","Incorr Loc","Miss"))
datRet$ConditionName = factor(datRet$ConditionName,levels = c("Far", "Close"))
datRet = datRet[datRet$timeStamp<2500,]


datBehavior = read.csv(paste(RD,"Children_Behavior_Filtered.csv",sep = ""))
datBehavior$GroupSession = factor(datBehavior$GroupSession, 
                                  levels = c("youngerT2","olderT2","youngerT3","olderT3"))
datBehavior$RespType2 = factor(datBehavior$RespType2,
                               levels = c("Corr Loc","Incorr Loc","Miss","FA", "CR"))
datBehavior$Target2 = factor(datBehavior$Target2,levels = c("Old", "Novel"))
datBehavior$ConditionName = factor(datBehavior$ConditionName,levels = c("Far", "Close"))
datBehavior$timePoint = factor(datBehavior$timePoint,levels = c("t2", "t3"))

datGrouping = read.csv(paste(RD,"ChildrenGroupingData.csv",sep = ""))
datSex = unique(datGrouping[,c("SID","Gender")])

datPerformance = read.csv(paste(RD,"Children_Performance_Filtered.csv",sep = ""))
datPerformance$Group = factor(datPerformance$Group, 
                              levels = c("Younger","Older"))
datPerformance$GroupSession = factor(datPerformance$GroupSession, 
                                     levels = c("youngerT2","olderT2","youngerT3","olderT3"))
datPerformance$ConditionName = factor(datPerformance$ConditionName,levels = c("Far", "Close"))
datPerformance$timePoint = factor(datPerformance$timePoint,levels = c("t2", "t3"))
datPerformance = merge(datPerformance,datSex,by = "SID",all.x = T)
#----------------> Load Vision Data
RDVision = "D:\\Projects\\Hippotime\\DataFiles\\L2_Interpolation\\"
datVision = read.csv(paste(RDVision,"Glasses_Children.csv",sep = ""))
datVision$SID = gsub("t.","",datVision$Participant_ID)
datVision$sessID = ifelse(grepl("t2",datVision$Participant_ID),"t2","t3")
datVision = datVision[datVision$Participant_ID %in% unique(datPerformance$ID),]
#--------------------------------------------------- Load Adult Performance  ----
RDA = "D:\\Projects\\Hippotime\\DataFiles\\L2_Interpolation\\"
datRetA = read.table(paste(RDA,"DataAdults_Retrieval.csv",sep = ""), header=TRUE, sep=",", strip.white = TRUE)
datRetA$TrialNum = datRetA$TrialNum + (datRetA$Retrieval-1) *max(datRetA$TrialNum)
datRetA = datRetA[datRetA$RespType2 %in% c("Corr Loc","Incorr Loc","Novel","FA", "CR"),]
datRetA$RespType2 = factor(datRetA$RespType2,levels = c("Corr Loc","Incorr Loc","Novel","FA", "CR"),
                           labels = c("Corr Loc","Incorr Loc","Miss","FA", "CR"))
datRetA$Target2 = factor(datRetA$Target2,levels = c("Old", "Novel"))
datRetA$ConditionName = factor(datRetA$ConditionName,levels = c("Far", "Close"))
datRetA$TrialState = factor(datRetA$TrialState,levels = c("PreResp", "PostResp"))
dat = unique(datRetA[,c("SID","Retrieval","TrialName","Pos1","Pos2","ConditionName","Target2","RT","RespType2","Accuracy")])
unique(dat$RespType2)
sDat = dat[dat$RespType2 %in% c("Corr Loc","Incorr Loc","Miss","FA","CR"),]
sDat = sDat %>%
  group_by(SID,RespType2,ConditionName) %>%
  summarise(N=n(),) %>%
  as.data.frame()
sDat = reshape2::dcast(sDat, SID+ConditionName~RespType2, value.var = "N" , fill = 0)

sDat$N_Old = sDat$`Corr Loc`+sDat$`Incorr Loc`+sDat$Miss
sDat$N_New = sDat$FA+sDat$CR

#Correct Location Rate
sDat$CLR = sDat$`Corr Loc`/(sDat$N_Old)
names(sDat)

#item recognition
sDat$HR = (sDat$`Corr Loc`+sDat$`Incorr Loc`)/(sDat$N_Old)
sDat$HR[sDat$HR==1] = (sDat$N_Old[sDat$HR==1]-0.5)/sDat$N_Old[sDat$HR==1]

sDat$FAR = sDat$FA/(sDat$N_New)
sDat$FAR[sDat$FAR==0] = .5/sDat$N_New[sDat$FAR==0] 

sDat$dPrime = qnorm(sDat$HR)-qnorm(sDat$FAR)

datBehaviorAdult = sDat
datBehaviorAdult$timePoint = "t2"
temp = datBehaviorAdult
temp$timePoint = "t3"
datBehaviorAdult = rbind(datBehaviorAdult,temp)
###################################################### Demographics ----
# datDemInfo = read.csv(paste(RD,"DemographicsChildren.csv",sep = ""))
# datDemInfo$SID = gsub("t.","",datDemInfo$Participant_ID)
# datDemInfo$sessID = case_when(grepl("t1",datDemInfo$Participant_ID)~"t1",
#                               grepl("t2",datDemInfo$Participant_ID)~"t2",
#                               grepl("t3",datDemInfo$Participant_ID)~"t3")
# datDemInfo$Race = case_when(datDemInfo$Race == 1 ~ "African American",
#                             datDemInfo$Race == 2 ~ "Asian American",
#                             datDemInfo$Race == 3 ~ "White",
#                             datDemInfo$Race == 4 ~ "Native American",
#                             datDemInfo$Race == 5 ~ "Pacific Islander",
#                             datDemInfo$Race == 6 ~ "Mixed",
#                             datDemInfo$Race == 7 ~ "Other",
#                             TRUE ~ "Not Reported")
# datDemInfo$Ethnicity = case_when(grepl("Hispanic",datDemInfo$Race_Other)~"Hispanic",
#                                  TRUE ~ "Not Hispanic")
# 
# datDemInfo = pivot_wider(datDemInfo, id_cols = "SID",
#                          values_from = c("Hand_Preference","Ethnicity","Race"),
#                          names_from = "sessID")
# datDemInfo = datDemInfo[datDemInfo$SID %in% unique(datPerformance$SID),]
# write.csv(datDemInfo,paste(WD,"Children_Race.csv",sep = ""),row.names = F)
datDemInfo = read.csv(paste(RD,"Children_Race.csv",sep = ""))
A = as.data.frame(summarise(group_by(datDemInfo,Race),N = n(), Percent = round(n()/131*100,2)))
as.data.frame(summarise(group_by(datDemInfo,Ethnicity),N = n()))
summary(as.factor(datDemInfo$Hand_Preference))

as.data.frame(summarise(group_by(datDemInfo,Race_t1),N = n()))
sum(A$N)

###################################################### Vision Data ----
datVision$Contacts[is.na(datVision$Contacts)]=0
datVision$Glasses[is.na(datVision$Glasses)]=0
datVision$Wear = ifelse(datVision$Contacts+datVision$Glasses>=1,1,0)
sDat = unique(datVision[,c("SID","sessID","Wear")])
# sDat = pivot_wider(sDat, id_cols = "SID",
#                    names_from = "sessID",
#                    values_from = "Wear") %>% as.data.frame()
sDat = group_by(sDat,SID) %>% summarise(value = max(Wear)) %>% as.data.frame()
print("Wearing Glass")
sum(sDat$value)
###################################################### Participants Age Info Within Session ----
print("Original Data")
A = datGrouping %>% group_by() %>%
  summarise(N = sum(t2),M = round(mean(age2,na.rm=T),2),SD = round(sd(age2,na.rm=T),2)) %>% as.data.frame()
A$timePoint = "t2"
B = datGrouping %>% group_by() %>%
  summarise(N = sum(t3),M = round(mean(age3,na.rm=T),2),SD = round(sd(age3,na.rm=T),2)) %>% as.data.frame()
B$timePoint = "t3"
print(rbind(A,B))
print("total Unique Participants")
print(nrow(datGrouping))

print("Included Data")
unique(datPerformance[,c("SID","timePoint","Age")]) %>% group_by(timePoint) %>%
  summarise(N = n(),M = round(mean(Age,na.rm=T),2),SD2 = round(sd(Age,na.rm=T),2)) %>% as.data.frame()

print("Included Data Split By Gender")
unique(datPerformance[,c("SID","timePoint","Age","Gender")]) %>% group_by(timePoint,Gender) %>%
  summarise(N = n(),M = round(mean(Age,na.rm=T),2),SD2 = round(sd(Age,na.rm=T),2)) %>% as.data.frame()

print("Number of Unique Participants")
length(unique(datPerformance$SID))

A = unique(datPerformance[,c("SID","timePoint")])
A$value = 1
A = reshape2::dcast(A, SID~timePoint, fill = 0)
A$shared = ifelse(A$t2+A$t3==2,1,0)
print("Number of Shared Participants")
print(sum(A$shared))
print(sum(A$t2-A$shared))
print(sum(A$t3-A$shared))


graphDat = unique(datPerformance[,c("SID","timePoint","Age")])
graphDat = pivot_wider(graphDat,id_cols = "SID",
                       names_from = "timePoint",
                       values_from = "Age")
graphDat$tOrder = graphDat$t2
graphDat$tOrder[is.na(graphDat$tOrder)] = graphDat$t3[is.na(graphDat$tOrder)]
graphDat = graphDat[order(graphDat$tOrder),]
graphDat$IDs = seq(1,nrow(graphDat))
names(graphDat) = c("SID","S1","S2","tOrder","IDs")
graphDat = pivot_longer(graphDat,cols = c("S1","S2"),
                        names_to = "Session",
                        values_to = "Age")
fSize = 16
ggplot(graphDat,aes(x = Age, y = IDs, color = Session, group = IDs))+
  geom_line(color = "#585858")+
  geom_point(aes(shape = Session))+
  theme(text = element_text(family = "serif"),
        strip.text = element_text(size=fSize-2, face="bold"),
        plot.title = element_text(size = fSize),
        axis.title = element_text(size = fSize),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank(),
        axis.title.y=element_blank(),
        axis.line.y = element_blank(),
        legend.title = element_text(size=fSize),
        legend.text=element_text(size=fSize-4),
        axis.text = element_text(size = fSize-4),
        panel.background = element_blank(),
        # strip.background = element_rect(fill=NA),
        plot.background = element_rect(fill = "transparent"),
        axis.line.x = element_line(linewidth = .7, linetype =  "solid"))+
  scale_x_continuous(name = "Age(years)", 
                     breaks = seq(8,16), 
                     labels = seq(8,16))+
  scale_color_manual(values=c("#CC6666", "#2297E6"))

graph2ppt(file= paste(WD,"AgeGrowth.pptx",sep = ""), width = 14, height = 7)

graphDat = unique(datPerformance[,c("SID","timePoint","Age", "Gender")])
graphDat = pivot_wider(graphDat,id_cols = c("SID","Gender"),
                       names_from = "timePoint",
                       values_from = "Age")
graphDat$tOrder = graphDat$t2
graphDat$tOrder[is.na(graphDat$tOrder)] = graphDat$t3[is.na(graphDat$tOrder)]
graphDat = graphDat[order(graphDat$tOrder),]
graphDat$IDs = seq(1,nrow(graphDat))
names(graphDat) = c("SID","Gender","S1","S2","tOrder","IDs")
graphDat = pivot_longer(graphDat,cols = c("S1","S2"),
                        names_to = "Session",
                        values_to = "Age")
graphDat$Gender = ifelse(grepl("F",graphDat$Gender),"Female","Male")
fSize = 16
ggplot(graphDat,aes(x = Age, y = IDs, color = Gender, group = IDs))+
  geom_line(color = "#585858")+
  geom_point(aes(shape = Gender))+
  theme(text = element_text(family = "serif"),
        strip.text = element_text(size=fSize-2, face="bold"),
        plot.title = element_text(size = fSize),
        axis.title = element_text(size = fSize),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank(),
        axis.title.y=element_blank(),
        axis.line.y = element_blank(),
        legend.title = element_text(size=fSize),
        legend.text=element_text(size=fSize-4),
        axis.text = element_text(size = fSize-4),
        panel.background = element_blank(),
        # strip.background = element_rect(fill=NA),
        plot.background = element_rect(fill = "transparent"),
        axis.line.x = element_line(linewidth = .7, linetype =  "solid"))+
  scale_x_continuous(name = "Age(years)", 
                     breaks = seq(8,16), 
                     labels = seq(8,16))+
  scale_color_manual(values=c("#CC6666", "#2297E6"))
graph2ppt(file= paste(WD,"AgeGrowth.pptx",sep = ""), width = 8, height = 6)

########################################################## Age Change/Session Difference ----
ageDist = unique(datPerformance[,c("SID","timePoint","Age")])
ageDist$Age = round(ageDist$Age,1)
ageDist = reshape2::dcast(ageDist, SID~timePoint, value.var = "Age")
ageDist$diff = ageDist$t3-ageDist$t2
ageDist$return = case_when(is.na(ageDist$t3)~"Not Returned(N=23)",
                           is.na(ageDist$t2)~"New(N=30)",
                           TRUE~"Returned(N=78)")
ageDist %>% group_by(return) %>%
  summarise(M = round(mean(t2,na.rm = T),2),
            SD = round(sd(t2,na.rm = T),2),
            N = sum(!is.na(t2))) %>% as.data.frame()
ageDist %>% group_by(return) %>%
  summarise(M = round(mean(t3,na.rm = T),2),
            SD = round(sd(t3,na.rm = T),2),
            N = sum(!is.na(t3))) %>% as.data.frame()


# ------------------ Age change dist
ggplot(data = ageDist,aes(x = diff))+
  geom_histogram(aes(y = after_stat(count)),binwidth = 0.25,center = 0,
                 position = "identity",color = "black",fill = "gray")+
  scale_x_continuous(name = "Age Change year", breaks = seq(0,3,.25),
                     labels = seq(0,3,.25))

mean(ageDist$diff,na.rm = T)
sd(ageDist$diff,na.rm = T)

ggplot(data = ageDist,aes(x = diff*12))+
  geom_histogram(aes(y = after_stat(count)),binwidth = 3,center = 0,
                 position = "identity",color = "black",fill = "gray")+
  scale_x_continuous(name = "Age Change Months", breaks = seq(0,30,3),
                     labels = seq(0,30,3))
names(ageDist)
ageDist %>% group_by() %>% summarise(M_sessDMonth = round(mean(diff,na.rm = T)*12,2),
                                     SD_sessDMonth = round(sd(diff,na.rm = T)*12,2),
                                     M_sessDYear = round(mean(diff,na.rm = T),2),
                                     SD_sessDYear = round(sd(diff,na.rm = T),2)) %>% as.data.frame()

ageDist %>% group_by(return) %>% summarise(N = sum(!is.na(diff)),
                                           M_sessDMonth = round(mean(diff,na.rm = T)*12,2),
                                           SD_sessDMonth = round(sd(diff,na.rm = T)*12,2),
                                           M_sessDYear = round(mean(diff,na.rm = T),2),
                                           SD_sessDYear = round(sd(diff,na.rm = T),2)) %>% as.data.frame()

########################################################## Age Distribution Graphs ----
ageDist = unique(datPerformance[,c("SID","timePoint","Age")])
ageDist$Age = round(ageDist$Age,1)
ageDist = reshape2::dcast(ageDist, SID~timePoint, value.var = "Age")
ageDist$diff = ageDist$t3-ageDist$t2
ageDist$return = case_when(is.na(ageDist$t3)~"Not Returned(N=23)",
                           is.na(ageDist$t2)~"New(N=30)",
                           TRUE~"Returned(N=78)")


# ------------------ Age change dist
ggplot(data = ageDist,aes(x = diff))+
  geom_histogram(aes(y = after_stat(count)),binwidth = 0.25,center = 0,
                 position = "identity",color = "black",fill = "gray")+
  scale_x_continuous(name = "Age Change year", breaks = seq(0,3,.25),
                     labels = seq(0,3,.25))


ggplot(data = ageDist,aes(x = diff*12))+
  geom_histogram(aes(y = after_stat(count)),binwidth = 3,center = 0,
                 position = "identity",color = "black",fill = "gray")+
  scale_x_continuous(name = "Age Change Months", breaks = seq(0,30,3),
                     labels = seq(0,30,3))

#------------------------ Age Dist at t2
graphDat = ageDist[,c("t2","return")]
graphDat = graphDat[complete.cases(graphDat),]
ggplot(data = graphDat,aes(x = t2))+
  geom_histogram(aes(y = after_stat(count)),binwidth = 0.5,center = 0,
                 position = "identity",color = "black",fill = "gray")+
  scale_x_continuous(name = "Age at t2", breaks = seq(8,15,.5),
                     labels = seq(8,15,.5))

#------------------------ Age Dist at t3
graphDat = ageDist[,c("t3","return")]
graphDat = graphDat[complete.cases(graphDat),]
ggplot(data = graphDat,aes(x = t3))+
  geom_histogram(aes(y = after_stat(count)),binwidth = 0.5,center = 0,
                 position = "identity",color = "black",fill = "gray")+
  scale_x_continuous(name = "Age at t2", breaks = seq(8,16,.5),
                     labels = seq(8,16,.5))

#------------------------ Age Dist split by timePoint
graphDat = datPerformance[,c("timePoint","Age")]
graphDat$session = graphDat$timePoint
ggplot(data = graphDat,aes(x = Age, color=session, group = session))+
  geom_histogram(aes(y = after_stat(count)),binwidth = 1,center = 0,
                 position = "identity",fill = "gray")+
  scale_x_continuous(name = "Age at t2", breaks = seq(8,15,1),
                     labels = seq(8,15,1))+
  facet_wrap(~session,nrow = 2)

ggplot(data = graphDat, aes(x = session, y = Age, fill = session))+
  geom_boxplot()+
  geom_half_violin(side="r",stat = "half_ydensity",fill = NA)+
  geom_jitter(position = position_jitterdodge(jitter.width = 1),shape = 21)+
  geom_point(stat = "summary",fun = "mean", aes(shape = "MeanShape"), size = 3,color = "red")+
  guides(fill = "none")+
  scale_shape_manual(name = NULL,
                     labels =c("Mean"),
                     limits = "MeanShape",
                     values = c(17))

#------------------------ Age Dist split by return status
graphDat = ageDist[,c("t2","return")]
graphDat = graphDat[complete.cases(graphDat),]
ggplot(data = graphDat,aes(x = t2, color=return, group = return))+
  geom_histogram(aes(y = after_stat(count)),binwidth = 1,center = 0,
                 position = "identity",fill = "gray")+
  scale_x_continuous(name = "Age at t2", breaks = seq(8,15,1),
                     labels = seq(8,15,1))+
  facet_wrap(~return,nrow = 2)


graphDat = ageDist[,c("t3","return")]
graphDat = graphDat[complete.cases(graphDat),]
ggplot(data = graphDat,aes(x = t3, color=return, group = return))+
  geom_histogram(aes(y = after_stat(count)),binwidth = 1,center = 0,
                 position = "identity",fill = "gray")+
  scale_x_continuous(name = "Age at t3", breaks = seq(8,16,1),
                     labels = seq(8,16,1))+
  facet_wrap(~return,nrow = 2)

########################################################## Age Change Tests ----
ageDist = unique(datPerformance[,c("SID","timePoint","Age")])
ageDist$session = ageDist$timePoint
ageDist$Age = round(ageDist$Age,2)
ageDist$Group = ifelse(ageDist$Age<11,"Younger","Older")

graphDat = ageDist
ggplot(data = graphDat, aes(x = session, y = Age, fill = session))+
  geom_boxplot()+
  geom_half_violin(side="r",stat = "half_ydensity",fill = NA)+
  geom_jitter(position = position_jitterdodge(jitter.width = 1),shape = 21)+
  geom_point(stat = "summary",fun = "mean", aes(shape = "MeanShape"), size = 3,color = "red")+
  guides(fill = "none")+
  scale_shape_manual(name = NULL,
                     labels =c("Mean"),
                     limits = "MeanShape",
                     values = c(17))


sDat = ageDist %>% group_by(session,Group) %>%
  summarise(N=n()) %>% as.data.frame()

sDat = pivot_wider(sDat, id_cols = "session",
                   values_from = "N",
                   names_from = "Group")

M <- as.table(cbind(sDat$Older, sDat$Younger))
dimnames(M) <- list(session = c("t2", "t3"),
                    group = c("Older","Younger"))

chisq_test(M)


sDat = pivot_wider(ageDist, id_cols = "SID",
                   values_from = c("Group","Age"),
                   names_from = "timePoint")

bins = seq(8,16,1)
A = hist(sDat$Age_t2,breaks = bins)
B = hist(sDat$Age_t3,breaks = bins)
M <- as.table(rbind(A$counts, B$counts))
dimnames(M) <- list(session = c("t2", "t3"),
                    Age = as.character(A$mids))

A = chisq.test(M,simulate.p.value = TRUE)
A
A$observed
A$expected

bins = c(8,11,17)
A = hist(sDat$Age_t2,breaks = bins)
B = hist(sDat$Age_t3,breaks = bins)
M <- as.table(rbind(A$counts, B$counts))
dimnames(M) <- list(session = c("t2", "t3"),
                    Age = as.character(A$mids))

A = chisq.test(M)
A
A$observed
A$expected

###################################################### Compare Overall accuracy between returned and not returned ----
dat = datBehavior
unique(dat$RespType2)

sDat = dat[dat$RespType2 %in% c("Corr Loc","Incorr Loc","Miss","FA","CR"),]

sDat = sDat %>%
  group_by(SID,ID,Age,GroupSession,timePoint,RespType2) %>%
  summarise(N=n(),) %>%
  as.data.frame()

sDat = reshape2::dcast(sDat, SID+ID+Age+GroupSession+timePoint~RespType2, value.var = "N" , fill = 0)

sDat$N_Old = sDat$`Corr Loc`+sDat$`Incorr Loc`+sDat$Miss
sDat$N_New = sDat$FA+sDat$CR

#Correct Location Rate
sDat$CLROld = sDat$`Corr Loc`/(sDat$N_Old)
sDat$CLR = sDat$`Corr Loc`/(sDat$`Corr Loc`+sDat$`Incorr Loc`)
sDat$HR = (sDat$`Corr Loc`+sDat$`Incorr Loc`)/(sDat$N_Old)
sDat$HR[sDat$HR==0] = .5/sDat$N_Old[sDat$HR==0] 
sDat$HR[sDat$HR==1] = (sDat$N_Old[sDat$HR==1]-0.5)/sDat$N_Old[sDat$HR==1]
sDat$FAR = sDat$FA/(sDat$N_New)
sDat$FAR[sDat$FAR==0] = .5/sDat$N_New[sDat$FAR==0] 
sDat$FAR[sDat$FAR==1] = (sDat$N_New[sDat$FAR==1]-0.5)/sDat$N_New[sDat$FAR==1] 
sDat$dPrime = qnorm(sDat$HR)-qnorm(sDat$FAR)


sDat = unique(sDat[,c("SID","timePoint","dPrime")])
sDat = reshape2::dcast(sDat, SID~timePoint, value.var = "dPrime")
sDat$diff = sDat$t3-sDat$t2
sDat$return = case_when(is.na(sDat$t3)~"Not Returned(N=23)",
                        is.na(sDat$t2)~"New(N=30)",
                        TRUE~"Returned(N=78)")
graphDat = pivot_longer(sDat,cols = c("t2","t3"),
                        values_to = "dPrime",
                        names_to = "timePoint")

graphDat = graphDat[,c("timePoint","dPrime","return")]
graphDat$session = graphDat$timePoint
ggplot(data = graphDat, aes(x = return, y = dPrime, fill = return))+
  geom_boxplot()+
  geom_half_violin(side="r",stat = "half_ydensity",fill = NA)+
  geom_jitter(position = position_jitterdodge(jitter.width = 1),shape = 21)+
  geom_point(stat = "summary",fun = "mean", aes(shape = "MeanShape"), size = 3,color = "red")+
  guides(fill = "none")+
  scale_shape_manual(name = NULL,
                     labels =c("Mean"),
                     limits = "MeanShape",
                     values = c(17))+
  facet_wrap(~session)
#------------------------------- Returned vs not Returned at t2
perfReturned = sDat$t2[sDat$return == "Returned(N=78)"]
perfNotReturned = sDat$t2[sDat$return == "Not Returned(N=23)"]
hist(perfReturned)
hist(perfNotReturned)
bins = seq(-1,5,1)
A = hist(perfReturned,breaks = bins)
B = hist(perfNotReturned,breaks = bins)
M <- as.table(rbind(A$counts, B$counts))
dimnames(M) <- list(session = c("t2", "t3"),
                    Age = as.character(A$mids))
M
A = chisq.test(M,simulate.p.value = TRUE)
A
A$observed
A$expected

sDat %>% group_by(return) %>%
  summarise(M = round(mean(t2,na.rm = T),2),
            SD = round(sd(t2,na.rm = T),2),
            N = sum(!is.na(t2)))

# Check for normality
qqnorm(perfReturned)
qqline(perfReturned, col = "blue")
shapiro.test(perfReturned)

qqnorm(perfNotReturned)
qqline(perfNotReturned, col = "blue")
shapiro.test(perfNotReturned)

# Check for homogeneity of variances
leveneTest(c(perfReturned, perfNotReturned), factor(c(rep(1, length(perfReturned)), rep(2, length(perfNotReturned)))))

t.test(x = perfReturned,
       y = perfNotReturned,paired = F,var.equal = T)

#------------------------------- Returned vs New at t3
perfReturned = sDat$t3[sDat$return == "Returned(N=78)"]
perfNew = sDat$t3[sDat$return == "New(N=30)"]
hist(perfReturned)
hist(perfNew)
bins = seq(0,5,1)
A = hist(perfReturned,breaks = bins)
B = hist(perfNotReturned,breaks = bins)
M <- as.table(rbind(A$counts, B$counts))
dimnames(M) <- list(session = c("t2", "t3"),
                    Age = as.character(A$mids))
M
A = chisq.test(M,simulate.p.value = TRUE)
A
A$observed
A$expected

sDat %>% group_by(return) %>%
  summarise(M = round(mean(t3,na.rm = T),2),
            SD = round(sd(t3,na.rm = T),2),
            N = sum(!is.na(t3)))

# Check for normality
qqnorm(perfReturned)
qqline(perfReturned, col = "blue")
shapiro.test(perfReturned)

qqnorm(perfNotReturned)
qqline(perfNotReturned, col = "blue")
shapiro.test(perfNotReturned)

# Check for homogeneity of variances
leveneTest(c(perfReturned, perfNotReturned), factor(c(rep(1, length(perfReturned)), rep(2, length(perfNotReturned)))))

t.test(x = perfReturned,
       y = perfNew,paired = F,var.equal = T)


###################################################### Grouping Data  ----
datPerformance %>% 
  group_by(GroupSession) %>%
  summarise(N=n(), M = round(mean(Age),2), SD = round(sd(Age),2)) %>%
  as.data.frame()

####################################### Behavioral Performance ----

graphDat = datPerformance
head(graphDat)

ggplot(graphDat,aes(x=timePoint,y=CLROld,fill=ConditionName,color=ConditionName)) + 
  geom_bar(stat="summary",fun="mean",position="dodge")+
  geom_point(position = position_jitterdodge(jitter.width = .4,
                                             jitter.height = 0,
                                             dodge.width = .9),shape = 21,fill="grey")+
  stat_summary(fun.data = "mean_se", geom="errorbar",position="dodge",color = "Black")+
  theme_bw(base_family = "serif")

Inds1 = graphDat$timePoint == "t2" & graphDat$ConditionName == "Close"
Inds2 = graphDat$timePoint == "t2" & graphDat$ConditionName == "Far"
t.test(graphDat$CLROld[Inds1],graphDat$CLROld[Inds2],paired = T)

Inds1 = graphDat$timePoint == "t2" & graphDat$ConditionName == "Close"
Inds2 = graphDat$timePoint == "t2" & graphDat$ConditionName == "Far"
t.test(graphDat$CLROld[Inds1],graphDat$CLROld[Inds2],paired = T)




graphDat = datPerformance
head(graphDat)

ggplot(graphDat,aes(x=timePoint,y=dPrime,fill=ConditionName,color=ConditionName)) + 
  geom_bar(stat="summary",fun="mean",position="dodge")+
  geom_point(position = position_jitterdodge(jitter.width = .4,
                                             jitter.height = 0,
                                             dodge.width = .9),shape = 21,fill="grey")+
  stat_summary(fun.data = "mean_se", geom="errorbar",position="dodge",color = "Black")+
  theme_bw(base_family = "serif")

Inds1 = graphDat$timePoint == "t2" & graphDat$ConditionName == "Close"
Inds2 = graphDat$timePoint == "t2" & graphDat$ConditionName == "Far"
t.test(graphDat$dPrime[Inds1],graphDat$dPrime[Inds2],paired = T)

Inds1 = graphDat$timePoint == "t3" & graphDat$ConditionName == "Close"
Inds2 = graphDat$timePoint == "t3" & graphDat$ConditionName == "Far"
t.test(graphDat$dPrime[Inds1],graphDat$dPrime[Inds2],paired = T)


graphDat = datBehavior %>% 
  group_by(ID,SID,timePoint,ConditionName) %>%
  summarise(RT = round(mean(RT),2)) %>%
  as.data.frame()
head(graphDat)

ggplot(graphDat,aes(x=timePoint,y=RT,fill=ConditionName,color=ConditionName)) + 
  geom_bar(stat="summary",fun="mean",position="dodge")+
  geom_point(position = position_jitterdodge(jitter.width = .4,
                                             jitter.height = 0,
                                             dodge.width = .9),shape = 21,fill="grey")+
  stat_summary(fun.data = "mean_se", geom="errorbar",position="dodge",color = "Black")+
  theme_bw(base_family = "serif")

Inds1 = graphDat$timePoint == "t2" & graphDat$ConditionName == "Close"
Inds2 = graphDat$timePoint == "t2" & graphDat$ConditionName == "Far"
t.test(graphDat$dPrime[Inds1],graphDat$dPrime[Inds2],paired = T)

Inds1 = graphDat$timePoint == "t1" & graphDat$ConditionName == "Close"
Inds2 = graphDat$timePoint == "t1" & graphDat$ConditionName == "Far"
t.test(graphDat$dPrime[Inds1],graphDat$dPrime[Inds2],paired = T)


###################################################### RT  ----
#-------------> RT
sDat = datBehavior %>% 
  group_by(ID,SID,timePoint,ConditionName) %>%
  summarise(RT = round(mean(RT),2)) %>%
  as.data.frame()

ggplot(sDat,aes(x=timePoint,y=RT,fill=ConditionName,color=ConditionName)) + 
  geom_bar(stat="summary",fun="mean",position="dodge")+
  geom_point(position = position_jitterdodge(jitter.width = .4,
                                             jitter.height = 0,
                                             dodge.width = .9),shape = 21,fill="grey")+
  stat_summary(fun.data = "mean_se", geom="errorbar",position="dodge",color = "Black")+
  theme_bw(base_family = "serif")

tpStr = "t2"
t.test(sDat$RT[sDat$ConditionName=="Far" & sDat$timePoint == tpStr],
       sDat$RT[sDat$ConditionName=="Close" & sDat$timePoint == tpStr],
       paired = T)
tpStr = "t3"
t.test(sDat$RT[sDat$ConditionName=="Far" & sDat$timePoint == tpStr],
       sDat$RT[sDat$ConditionName=="Close" & sDat$timePoint == tpStr],
       paired = T)

sDat %>% 
  group_by(timePoint,ConditionName) %>%
  summarise(M = round(mean(RT),2), SD = round(sd(RT),2)) %>%
  as.data.frame()

sDat %>% 
  group_by(ConditionName) %>%
  summarise(M = round(mean(RT),2), SD = round(sd(RT),2)) %>%
  as.data.frame()

#-------------> RT x SessionGroup x Accuracy
sDat = datBehavior %>% 
  group_by(ID,SID,timePoint,GroupSession,ConditionName,Accuracy) %>%
  summarise(RT = round(mean(RT),2)) %>%
  as.data.frame()
sDat$Accuracy = factor(sDat$Accuracy,levels = c(0,1), labels = c("Incorrect","Correct") )

ggplot(sDat,aes(x=GroupSession,y=RT,fill=ConditionName,color=ConditionName)) + 
  geom_bar(stat="summary",fun="mean",position="dodge")+
  geom_point(position = position_jitterdodge(jitter.width = .4,
                                             jitter.height = 0,
                                             dodge.width = .9),shape = 21,fill="grey")+
  stat_summary(fun.data = "mean_se", geom="errorbar",position="dodge",color = "Black")+
  theme_bw(base_family = "serif")+
  facet_wrap(Accuracy~timePoint)

#-------------> RT x SessionGroup
sDat = datBehavior %>% 
  group_by(ID,SID,Group,timePoint,GroupSession,ConditionName) %>%
  summarise(RT = round(mean(RT),2)) %>%
  as.data.frame()

ggplot(sDat,aes(x=Group,y=RT,fill=ConditionName,color=ConditionName)) + 
  geom_bar(stat="summary",fun="mean",position="dodge")+
  geom_point(position = position_jitterdodge(jitter.width = .4,
                                             jitter.height = 0,
                                             dodge.width = .9),shape = 21,fill="grey")+
  stat_summary(fun.data = "mean_se", geom="errorbar",position="dodge",color = "Black")+
  theme_bw(base_family = "serif")+
  facet_wrap(~timePoint)

tpStr = "t2"
t.test(sDat$RT[sDat$ConditionName=="Far" & sDat$timePoint == tpStr & sDat$Group == "Younger"],
       sDat$RT[sDat$ConditionName=="Close" & sDat$timePoint == tpStr & sDat$Group == "Younger"],
       paired = T)
t.test(sDat$RT[sDat$ConditionName=="Far" & sDat$timePoint == tpStr & sDat$Group == "Older"],
       sDat$RT[sDat$ConditionName=="Close" & sDat$timePoint == tpStr & sDat$Group == "Older"],
       paired = T)
tpStr = "t3"
t.test(sDat$RT[sDat$ConditionName=="Far" & sDat$timePoint == tpStr & sDat$Group == "Younger"],
       sDat$RT[sDat$ConditionName=="Close" & sDat$timePoint == tpStr & sDat$Group == "Younger"],
       paired = T)
t.test(sDat$RT[sDat$ConditionName=="Far" & sDat$timePoint == tpStr & sDat$Group == "Older"],
       sDat$RT[sDat$ConditionName=="Close" & sDat$timePoint == tpStr & sDat$Group == "Older"],
       paired = T)


###################################################### Accuracy  ----
#-------------> All Performances
sDat = melt(datPerformance, id.vars = c("SID","ID","Age","Group","timePoint","ConditionName"),
            variable.name = "Measure",measure.vars = c("CLR","HR","FAR")) 
ggplot(sDat,aes(x=Measure,y=value,fill=Group,color=Group)) + 
  geom_bar(stat="summary",fun="mean",position="dodge")+
  # geom_line(aes(group = factor(SID)),position = position_jitterdodge(jitter.width = .4,
  #                                                         jitter.height = 0,
  #                                                         dodge.width = .9)) +
  geom_point(position = position_jitterdodge(jitter.width = .4,
                                             jitter.height = 0,
                                             dodge.width = .9),shape = 21,fill="grey")+
  stat_summary(fun.data = "mean_se", geom="errorbar",position="dodge",color = "Black")+
  theme_bw(base_family = "serif")+
  facet_wrap(ConditionName~timePoint)

#-------------> Hit Rate x SessionGroup
sDat = datPerformance
ggplot(sDat,aes(x=Group,y=HR,fill=ConditionName,color=ConditionName)) + 
  geom_bar(stat="summary",fun="mean",position="dodge")+
  geom_point(position = position_jitterdodge(jitter.width = .4,
                                             jitter.height = 0,
                                             dodge.width = .9),shape = 21,fill="grey")+
  stat_summary(fun.data = "mean_se", geom="errorbar",position="dodge",color = "Black")+
  theme_bw(base_family = "serif")+
  facet_wrap(~timePoint)

tpStr = "t2"
t.test(sDat$HR[sDat$ConditionName=="Far" & sDat$timePoint == tpStr & sDat$Group == "Younger"],
       sDat$HR[sDat$ConditionName=="Close" & sDat$timePoint == tpStr & sDat$Group == "Younger"],
       paired = T)
t.test(sDat$HR[sDat$ConditionName=="Far" & sDat$timePoint == tpStr & sDat$Group == "Older"],
       sDat$HR[sDat$ConditionName=="Close" & sDat$timePoint == tpStr & sDat$Group == "Older"],
       paired = T)
tpStr = "t3"
t.test(sDat$HR[sDat$ConditionName=="Far" & sDat$timePoint == tpStr & sDat$Group == "Younger"],
       sDat$HR[sDat$ConditionName=="Close" & sDat$timePoint == tpStr & sDat$Group == "Younger"],
       paired = T)
t.test(sDat$HR[sDat$ConditionName=="Far" & sDat$timePoint == tpStr & sDat$Group == "Older"],
       sDat$HR[sDat$ConditionName=="Close" & sDat$timePoint == tpStr & sDat$Group == "Older"],
       paired = T)

#-------------> CLR Rate x SessionGroup
sDat = datPerformance
ggplot(sDat,aes(x=Group,y=CLR,fill=ConditionName,color=ConditionName)) + 
  geom_bar(stat="summary",fun="mean",position="dodge")+
  geom_point(position = position_jitterdodge(jitter.width = .4,
                                             jitter.height = 0,
                                             dodge.width = .9),shape = 21,fill="grey")+
  stat_summary(fun.data = "mean_se", geom="errorbar",position="dodge",color = "Black")+
  theme_bw(base_family = "serif")+
  facet_wrap(~timePoint)

tpStr = "t2"
t.test(sDat$CLR[sDat$ConditionName=="Far" & sDat$timePoint == tpStr & sDat$Group == "Younger"],
       sDat$CLR[sDat$ConditionName=="Close" & sDat$timePoint == tpStr & sDat$Group == "Younger"],
       paired = T)
t.test(sDat$CLR[sDat$ConditionName=="Far" & sDat$timePoint == tpStr & sDat$Group == "Older"],
       sDat$CLR[sDat$ConditionName=="Close" & sDat$timePoint == tpStr & sDat$Group == "Older"],
       paired = T)
tpStr = "t3"
t.test(sDat$CLR[sDat$ConditionName=="Far" & sDat$timePoint == tpStr & sDat$Group == "Younger"],
       sDat$CLR[sDat$ConditionName=="Close" & sDat$timePoint == tpStr & sDat$Group == "Younger"],
       paired = T)
t.test(sDat$CLR[sDat$ConditionName=="Far" & sDat$timePoint == tpStr & sDat$Group == "Older"],
       sDat$CLR[sDat$ConditionName=="Close" & sDat$timePoint == tpStr & sDat$Group == "Older"],
       paired = T)


#-------------> FAR Rate x SessionGroup
sDat = datPerformance
ggplot(sDat,aes(x=Group,y=FAR,fill=ConditionName,color=ConditionName)) + 
  geom_bar(stat="summary",fun="mean",position="dodge")+
  geom_point(position = position_jitterdodge(jitter.width = .4,
                                             jitter.height = 0,
                                             dodge.width = .9),shape = 21,fill="grey")+
  stat_summary(fun.data = "mean_se", geom="errorbar",position="dodge",color = "Black")+
  theme_bw(base_family = "serif")+
  facet_wrap(~timePoint)

tpStr = "t2"
t.test(sDat$FAR[sDat$ConditionName=="Far" & sDat$timePoint == tpStr & sDat$Group == "Younger"],
       sDat$FAR[sDat$ConditionName=="Close" & sDat$timePoint == tpStr & sDat$Group == "Younger"],
       paired = T)
t.test(sDat$FAR[sDat$ConditionName=="Far" & sDat$timePoint == tpStr & sDat$Group == "Older"],
       sDat$FAR[sDat$ConditionName=="Close" & sDat$timePoint == tpStr & sDat$Group == "Older"],
       paired = T)
tpStr = "t3"
t.test(sDat$FAR[sDat$ConditionName=="Far" & sDat$timePoint == tpStr & sDat$Group == "Younger"],
       sDat$FAR[sDat$ConditionName=="Close" & sDat$timePoint == tpStr & sDat$Group == "Younger"],
       paired = T)
t.test(sDat$FAR[sDat$ConditionName=="Far" & sDat$timePoint == tpStr & sDat$Group == "Older"],
       sDat$FAR[sDat$ConditionName=="Close" & sDat$timePoint == tpStr & sDat$Group == "Older"],
       paired = T)


#-------------> dPrime x SessionGroup
sDat = datPerformance
ggplot(sDat,aes(x=Group,y=dPrime,fill=ConditionName,color=ConditionName)) + 
  geom_bar(stat="summary",fun="mean",position="dodge")+
  geom_point(position = position_jitterdodge(jitter.width = .4,
                                             jitter.height = 0,
                                             dodge.width = .9),shape = 21,fill="grey")+
  stat_summary(fun.data = "mean_se", geom="errorbar",position="dodge",color = "Black")+
  theme_bw(base_family = "serif")+
  facet_wrap(~timePoint)

tpStr = "t2"
t.test(sDat$dPrime[sDat$ConditionName=="Far" & sDat$timePoint == tpStr & sDat$Group == "Younger"],
       sDat$dPrime[sDat$ConditionName=="Close" & sDat$timePoint == tpStr & sDat$Group == "Younger"],
       paired = T)
t.test(sDat$dPrime[sDat$ConditionName=="Far" & sDat$timePoint == tpStr & sDat$Group == "Older"],
       sDat$dPrime[sDat$ConditionName=="Close" & sDat$timePoint == tpStr & sDat$Group == "Older"],
       paired = T)
tpStr = "t3"
t.test(sDat$dPrime[sDat$ConditionName=="Far" & sDat$timePoint == tpStr & sDat$Group == "Younger"],
       sDat$dPrime[sDat$ConditionName=="Close" & sDat$timePoint == tpStr & sDat$Group == "Younger"],
       paired = T)
t.test(sDat$dPrime[sDat$ConditionName=="Far" & sDat$timePoint == tpStr & sDat$Group == "Older"],
       sDat$dPrime[sDat$ConditionName=="Close" & sDat$timePoint == tpStr & sDat$Group == "Older"],
       paired = T)

#-------------> dPrime x timePoint
sDat = datPerformance
ggplot(sDat,aes(x=ConditionName,y=dPrime,fill=ConditionName,color=ConditionName)) + 
  geom_bar(stat="summary",fun="mean",position="dodge")+
  geom_point(position = position_jitterdodge(jitter.width = .4,
                                             jitter.height = 0,
                                             dodge.width = .9),shape = 21,fill="grey")+
  stat_summary(fun.data = "mean_se", geom="errorbar",position="dodge",color = "Black")+
  theme_bw(base_family = "serif")+
  facet_wrap(~timePoint)

tpStr = "t2"
t.test(sDat$dPrime[sDat$ConditionName=="Far" & sDat$timePoint == tpStr ],
       sDat$dPrime[sDat$ConditionName=="Close" & sDat$timePoint == tpStr],
       paired = T)
tpStr = "t3"
t.test(sDat$dPrime[sDat$ConditionName=="Far" & sDat$timePoint == tpStr],
       sDat$dPrime[sDat$ConditionName=="Close" & sDat$timePoint == tpStr],
       paired = T)

#-------------> Hit Rate x ageGroup
ggplot(sDat[!is.na(sDat$ageGroup),],aes(x=ConditionName,y=HR,fill=ConditionName,color=ConditionName)) + 
  geom_bar(stat="summary",fun="mean",position="dodge")+
  geom_point(position = position_jitterdodge(jitter.width = .4,
                                             jitter.height = 0,
                                             dodge.width = .9),shape = 21,fill="grey")+
  stat_summary(fun.data = "mean_se", geom="errorbar",position="dodge",color = "Black")+
  theme_bw(base_family = "serif")+
  facet_wrap(~ageGroup)

tpStr = "Younger"
t.test(sDat$HR[sDat$ConditionName=="Far" & sDat$ageGroup == tpStr],
       sDat$HR[sDat$ConditionName=="Close" & sDat$ageGroup == tpStr],
       paired = T)
tpStr = "Older"
t.test(sDat$HR[sDat$ConditionName=="Far" & sDat$ageGroup == tpStr],
       sDat$HR[sDat$ConditionName=="Close" & sDat$ageGroup == tpStr],
       paired = T)


