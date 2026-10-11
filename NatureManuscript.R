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
       car,
       nlme,
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
       officer,rvg,
       psych,
       rstatix,
       emmeans,
       ggformula,
       export,
       scales,
       correlation,
       sjPlot,
       interactions,
       jtools)
library(HMLET)

fSize = 12
gDefault =   theme(
  text = element_text(family = "serif"),
  strip.text = element_text(size=fSize-2, face="bold"),
  plot.title = element_text(size = fSize),
  axis.title = element_text(size = fSize),
  legend.title = element_text(size=fSize),
  legend.text=element_text(size=fSize-4),
  axis.text = element_text(size = fSize-4),
  panel.background = element_rect(fill = "transparent", color = "black", 
                                  linewidth = 0.7, linetype =  "solid"),
  plot.background = element_rect(fill = "transparent"),
  axis.line = element_line(linewidth = .7, linetype =  "solid"),
  strip.background = element_rect(fill="transparent"),
  panel.grid.major.x = element_line(color = alpha("gray", alpha = 0.3)),
  panel.grid.minor = element_blank()
)

RD = "D:\\Projects\\Hippotime\\DataFiles\\L12_Children_data\\"
WD = "D:\\Projects\\Hippotime\\DataFiles\\L15_Children_Manuscript\\"

###################################### Functions -----
#--------------------------- Remove Outliers 
RemoveOutliers <- function(data,factorNames,varNames,Criteria=3.29){
  # Using mutate
  data = data %>% group_by_at(factorNames)%>%
    mutate_at(.vars = varNames,
              ~ ifelse(as.vector(abs(scale(.,center = TRUE, scale = TRUE)))>3,NA,.)) %>%
    as.data.frame()
  return(data)
}
#------------------------- Integrate over Vectors 
IntegrateVector <- function(vector,dt=1,timeVec = NULL, 
                            interpolate = F, method = "cubic",
                            t0=0){
  #------------- Example
  ## requires: pracma and stats packages
  # dt=1
  # timeVec = NULL
  # interpolate = F
  # method = "cubic"
  # t0=0
  # t = seq(0,3*pi/4,by = pi/100)
  # y = sin(t)
  # stats::integrate(sin,t[1],t[length(t)])
  # IntegrateVector(y,dt = pi/100)
  # IntegrateVector(y,dt = pi/100,interpolate = T,method = "linear")
  # IntegrateVector(y,dt = pi/100,interpolate = T,method = "cubic")
  #---------------test
  # fy = ppfit(t,y,t,method = "linear")
  # ti = seq(0,3*pi/4,by = pi/1000)
  # yi = ppval(fy,ti)
  # 
  # plot(t,y,type = "b", main = "Piecewise polynomial approximation")
  # lines(ti, yi, col="red",lwd = 2)
  # 
  # t = seq(0,pi,by = pi/100)
  # y = sin(t)
  # yi = ppval(fy,t)
  # plot(t,y,type = "b", main = "Piecewise polynomial approximation")
  # lines(t, yi, col="red",lwd = 2)
  
  if(is.null(timeVec)){
    timeVec = seq(0,length(vector)-1,1)*dt+t0
  }
  if(interpolate & method == "cubic" & length(vector)<3){
    # At least three points needed for cubic interpolation.
    interpolate = F
  }
  
  if(interpolate & length(vector)>1){
    fy = pracma::ppfit(x = timeVec, y = vector, xi = timeVec, method = method)
    S = stats::integrate(f = function(x) pracma::ppval(fy,x), lower = timeVec[1],upper = timeVec[length(timeVec)])
    S = S$value
  }else{
    if(length(vector)>1){
      S = (sum(vector)-vector[1]/2-vector[length(vector)-1]/2)*dt
    }else{
      S = vector*dt
    }
  }
  return(S)
}
###################################################### Load data -----
PTPath = "Results_WithinSessionCloseFar\\"
datGaze = read.csv(paste(RD,PTPath,"LabeledTimePointLevel.csv",sep = ""))
datGaze$SID = gsub("t.","",datGaze$ID)
datGaze$sessID = ifelse(grepl("t2",datGaze$ID),"t2","t3")
datGaze$testName = gsub("_t.","",datGaze$testName)

datGaze$condition = ifelse(grepl("Incorr",datGaze$condition),"IncorrL","CorrL")
datGaze$clusterTime = gsub("\\.\\d+","",datGaze$clusterTime)
datGaze$clusterTime = gsub("->","TO",datGaze$clusterTime)


datPerformance = read.csv(paste(RD,"Children_Performance_Filtered.csv",sep = ""))
datPerformance$timePoint = factor(datPerformance$timePoint,levels = c("t2", "t3"))
datAge = datPerformance[,c("ID","timePoint","ConditionName","Age","dPrime","CLROld","CLR")]
datAge$SID =  gsub("t.","",datAge$ID)
datAge$sessID = datAge$timePoint
datAge = datAge %>% group_by(SID,ConditionName) %>%
  mutate(ageChange = c(0,abs(diff(Age))),
         ageBase = min(Age)) %>%
  as.data.frame()
datAge = datAge[,c("SID","ID","sessID","ConditionName","Age","ageBase","ageChange","dPrime","CLROld","CLR")]
names(datAge) = c("SID","ID","sessID","testName","Age","ageBase","ageChange","dPrime","CLR","CLR_50")

datVol = read.csv(paste(RD,"DatSubFieldVolumeChildren.csv",sep = ""))
datVol = datVol[,c("SID","timePoint","ICV","HPC","CA1","CA3","DG")]
datVol$sessID = datVol$timePoint
datVol = datVol[,!(names(datVol)%in%c("timePoint"))]

datVol$Vol = datVol$DG
datRes = NULL
for (sessID in unique(datVol$sessID)){
  datModel = datVol[datVol$sessID == sessID,]
  for (Volume in c("HPC","CA1","CA3","DG")){
    datModel$Vol = datModel[,Volume]
    m = lm(Vol ~  ICV, data = datModel, na.action=na.exclude)
    datModel[,paste(Volume,"_res",sep = "")] = m$resid
  }
  datRes = rbind(datRes,datModel)
}
datVol = datRes
datVol = datVol[,!names(datVol) %in% c("Vol")]
temp = pivot_wider(datVol, id_cols = c("SID"),
                   names_from = sessID,
                   values_from = c("HPC_res", "CA1_res", "CA3_res", "DG_res")) %>% as.data.frame()
for (Volume in c("HPC","CA1","CA3","DG")){
  temp[,paste("volChange_",Volume,sep = "")] = 
    (temp[,paste(Volume,"_res_t3",sep = "")]-
       temp[,paste(Volume,"_res_t2",sep = "")])/temp[,paste(Volume,"_res_t2",sep = "")]
  naIdx =is.na(temp[,paste("volChange_",Volume,sep = "")])
  temp[naIdx, paste("volChange_",Volume,sep = "")] = 
    temp[naIdx, paste(Volume,"_res_t3",sep = "")]-temp[naIdx, paste(Volume,"_res_t1",sep = "")]
  naIdx =is.na(temp[,paste("volChange_",Volume,sep = "")])
  temp[naIdx, paste("volChange_",Volume,sep = "")] = 
    temp[naIdx, paste(Volume,"_res_t2",sep = "")]-temp[naIdx, paste(Volume,"_res_t1",sep = "")]
}
datvolChange = temp[,c("SID","volChange_HPC","volChange_CA1","volChange_CA3","volChange_DG")]
datvolChange$sessID = "t3"

datVol = datVol[datVol$sessID %in% c("t2","t3"),]

datAge = merge(datAge,datVol,by = c("SID","sessID"), all.x = T)
datAge = merge(datAge,datvolChange,by = c("SID","sessID"), all.x = T)
datAge[datAge$sessID == "t2",c("volChange_HPC","volChange_CA1","volChange_CA3","volChange_DG")] = 0

# #---------------> Load Joshh old Volumes
volPath = "D:\\Projects\\Hippotime\\DataFiles\\SubFields\\"
datVol2 = read.csv(paste(volPath,"JoshVols.csv",sep = ""))
datVol2$ID = paste(datVol2$SID,datVol2$sessID,sep = "")
IDs = unique(datGaze$ID)
datVol2 = datVol2[datVol2$ID %in%IDs,]
datVol2$HPC2 = datVol2$HPC
datVol2 = datVol2[,names(datVol2) !="HPC"]
datVol2$bodytail = datVol2$body+datVol2$tail
# head(datAge)
datAge = merge(datAge,datVol2,by = c("SID","sessID","ID"), all.x = T)



##################################################################################################### Proportional Vicinity For Table 1 and Figure 5 ----
#----------------- prepare cluster -------------
dat = datGaze[datGaze$MeasureName == "PV",]
dat$timePoint = round(dat$timePoint)
dat =  pivot_wider(dat,id_cols = c("SID","sessID","timePoint","testName"),
                   names_from = c("condition"), 
                   values_from = c("value"),names_sep = "_") %>%as.data.frame()
dat$IncorrL = 1-dat$IncorrL
dat$diff = dat$CorrL - dat$IncorrL 

clusterFar = c(1050,1600)
clusterClose = c(650,1100)
clusterClose2 = c(1166,1467)
d = dat[(dat$testName == "Far" &
           (dat$timePoint >= clusterFar[1]) & (dat$timePoint <= clusterFar[2]))|
          (dat$testName == "Close" &
             (dat$timePoint >= clusterClose[1]) & (dat$timePoint <= clusterClose[2]))|
          (dat$testName == "Close" &
             (dat$timePoint >= clusterClose2[1]) & (dat$timePoint <= clusterClose2[2])),]

# d = RemoveOutliers(d,factorNames = c("sessID","testName","timePoint"),varNames = "diff",Criteria=3)
# d = d[!is.na(d$diff),]

d = d[order(d$SID,d$sessID,d$timePoint,d$testName),]
# windowSize = 6.6667
windowSize = 100
# groupSize = 6
sDat = d %>% group_by(SID,sessID,testName) %>%
  mutate(diffn = na.approx(diff),
         timeStamp = floor(timePoint/windowSize),
         # timeStamp = floor((round(timePoint*60/1000)-min(round(unique(timePoint)*60/1000)))/groupSize),
  ) %>%
  group_by(SID,sessID,testName,timeStamp) %>%
  summarize(CorrL = mean(CorrL,na.rm = T),
            diff = mean(diffn,na.rm = T),
            # integ = IntegrateVector(diffn,dt = 1000/60,t0 = timePoint[1],
            #                         interpolate = T,method = "cubic")/n(),
            IncorrL = mean(IncorrL,na.rm = T),
            N=n()) %>%
  group_by(testName) %>%
  mutate(timeStamp = timeStamp - min(timeStamp)+1) %>%
  as.data.frame()

sDat$ID = paste(sDat$SID,sDat$sessID,sep = "")
unique(sDat$timeStamp)
sDat = merge(sDat, datAge, all.x = T,by = c("SID","sessID","ID","testName"))

#-------------- CLR Model  ----
datModel = sDat
datModel$Condition = datModel$testName
datModel$Vicinity = datModel$diff
# datModel$Vicinity = datModel$CorrL - (1-datModel$IncorrL)

datModel$Memory  = datModel$CLR*100
datModel$Order = factor(datModel$timeStamp,levels = order(unique(datModel$timeStamp)))
# datModel$Order = as.numeric(datModel$timeStamp)
datModel$ageBase = datModel$ageBase - min(datModel$ageBase)
datModel$Age = datModel$Age - min(datModel$Age)
# datModel$ageChange = scale(datModel$ageChange)
datModel$TimePoint = ifelse(datModel$ageChange==0,"T1","T2")

datModel = RemoveOutliers(datModel,factorNames = c("sessID","Condition"),varNames = "Vicinity")
datModel = datModel%>% group_by(SID,Condition,TimePoint) %>% fill(Vicinity, .direction = "downup")
datModel = datModel[!is.na(datModel$Vicinity),]

datModel %>% group_by(SID,Condition,TimePoint) %>%
  summarise(Vicinity = mean(Vicinity, na.rm =T),N=n(),
            CLR = mean(CLR, na.rm =T)) %>%
  group_by(Condition,TimePoint) %>%
  summarise(meanV = mean(Vicinity, na.rm =T),
            sdV = sd(Vicinity, na.rm =T),
            MaxV = max(Vicinity, na.rm =T),
            MinV = min(Vicinity, na.rm =T),
            N=n(),
            ,CLR = mean(CLR, na.rm =T)) %>% as.data.frame()


A = datModel %>% group_by(SID,Condition,TimePoint) %>%
  summarise(Vicinity = mean(Vicinity, na.rm =T)) %>% as.data.frame()

A$TimePoint = as.factor(A$TimePoint)
ggplot(A, aes(x=TimePoint, y=Vicinity, fill=TimePoint)) +
  geom_boxplot() +
  # scale_fill_viridis(discrete = TRUE, alpha=0.6) +
  geom_jitter(color="black", size=0.4, alpha=0.9)+
  geom_line(aes(group=SID))+
  facet_wrap(~Condition)
# datModel = datModel[,c("SID", "Memory", "ageBase", "ageChange", "Condition", 'Vicinity',"Order","TimePoint")]
# datModelVic = datModel

DV = "Memory ~"
rEff = "+(1+Condition|SID)+(1+Vicinity|Order)"
models = c("1",
           "ageBase+",
           "ageBase+ageChange",
           "ageBase+ageChange+Condition",
           "ageBase+ageChange+Condition+Vicinity+",
           # "ageBase+ageChange+Condition+Vicinity+Condition*Vicinity",
           "ageBase+ageChange+Condition+Vicinity+Condition*Vicinity+ageBase*Vicinity",
           "ageBase+ageChange+Condition+Vicinity+Condition*Vicinity+ageBase*Vicinity+ageChange*Vicinity",
           "ageBase+ageChange+Condition+Vicinity+Condition*Vicinity+ageBase*Vicinity+ageChange*Vicinity+ageChange:Condition:Vicinity")
i=1
m=list()
for (idx in seq(1,length(models))){
  m[[idx]] = lmer(paste(DV,models[idx],rEff,sep = ""), 
                  data=datModel, na.action=na.exclude)
}
mComp <- do.call(anova,m)
# row.names(mComp) = models
mComp$Best = seq(1,nrow(mComp))*as.numeric(mComp$`Pr(>Chisq)`<0.05)
print(mComp)

M = m[[max(mComp$Best,na.rm=T)]]
tab_model(M,show.se = T,show.stat = T,show.est = T)


summary(M)
vif(M)
hist(resid(M))
anova(mNULL,M)
VarCorr(M)

tab_model(m[[5]],show.se = T,show.stat = T,show.est = T)
#------ Condition Main effect -----
anova(m[[5]],m[[1]])
tab_model(m[[5]],show.se = T,show.stat = T,show.est = T)
em <- emmeans(m[[5]], ~ Condition, pbkrtest.limit = 3334)
plot(em, by = NULL , comparisons = TRUE, adjust = "tukey", 
     horizontal = FALSE)
summary(em)
contrast(em, method = "pairwise", adjust = "tukey")

datModel %>% group_by(SID,Condition,TimePoint) %>%
  summarise(Vicinity = mean(Vicinity, na.rm =T),N=n(),
            Memory = mean(Memory, na.rm =T)) %>%
  group_by(Condition) %>%
  summarise(MeanDV = mean(Memory, na.rm =T),
            SDDV = sd(Memory,  na.rm= T)) %>% as.data.frame()


tab_model(m[[1]],show.se = T,show.stat = T,show.est = T)


em1 <- emmeans(M, ~ Condition, pbkrtest.limit = 3334)
plot(em1, by = NULL , comparisons = TRUE, adjust = "tukey", 
     horizontal = FALSE)
summary(em1)
contrast(em1, method = "pairwise", adjust = "tukey")


#------ Condition*Vicinity effect -----
anova(m[[5]],m[[8]])

A = sim_slopes(M, pred = Vicinity, modx = Condition, 
               confint = F, jnplot =T)
print(A)
plot(A)

# datM = datModel %>% group_by(SID,Condition,TimePoint) %>%
#   summarise(Vicinity = median(Vicinity, na.rm =T),
#             Memory = mean(Memory, na.rm =T),
#             resid = mean(resid,na.rm=T),
#             ageBase = mean(ageBase,na.rm=T),
#             ageChange = mean(ageChange,na.rm=T),
#             .groups = "drop") %>%
#   as.data.frame()

datM = datModel %>% group_by(SID,Condition,TimePoint) %>%
  reframe(
    Vicinity = c(min(Vicinity, na.rm =T),max(Vicinity, na.rm =T)),
    # Memory    =  c(min(Memory, na.rm =T)+(sd(Memory, na.rm =T)),max(Memory, na.rm =T)-(sd(Memory, na.rm =T))),
    Memory    =  c(min(Memory, na.rm =T),max(Memory, na.rm =T)),
    ) %>%
  # summarise(
  #   Vicinity  = mean(Vic, na.rm = TRUE),
  #   Memory    = mean(Memory, na.rm = TRUE),
  #   ageBase   = mean(ageBase, na.rm = TRUE),
  #   ageChange = mean(ageChange, na.rm = TRUE),
  #   .groups = "drop"
  # ) %>%
  as.data.frame()

p =  interact_plot(M, pred = Vicinity, modx = Condition,
                   plot.points = F, point.alpha = 0.3,point.size = .5, point.shape = F,
                   line.thickness = 1.5,vary.lty=F, interval = T,int.type = "confidence",
                   linearity.check =F, rug =F,partial.residuals = F,centered = "all",
                   y.label = "Item-Location Memory Accuracy",
                   x.label = "Differential Vicinity")
p = p+
  geom_point(
    data = datM,
    aes(x = Vicinity, y = Memory, color = Condition),
    alpha = 0.3,
    size = .25,
    position = position_jitter(width = 0.01, height = 0)
  )+
  # geom_line(
  #   data = datM,
  #   aes(x = Vicinity, y = Memory, group = paste(SID,Condition)),
  #   alpha = 0.4,
  # )+
  labs(
    x = "Differential Vicinity",
    y = "Memory",
    color = "Condition",
    fill = "Condition"
  ) +
  scale_x_continuous(
    limits = c(-.5, .9),
    breaks = round(seq(-.5, .9,length.out = 5),1),
    labels = round(seq(0, 1,length.out = 5),1)
  ) +
  scale_y_continuous(
    limits = c(30,65),
    breaks = c(30,33,seq(40, 60, by = 10))
  ) +
  geom_hline(
    yintercept = 33,
    linetype = "dashed",
    color = "gray50",
    linewidth = 0.6
  )+
  # scale_color_manual(values = c("#7ED7AFFF","#3573A1FF","#3A2C59FF") ) +
  # scale_fill_manual(values = c("#7ED7AFFF","#3573A1FF","#3A2C59FF") ) +
  theme_bw()+gDefault
plot(p)

graph2ppt(file= paste(WD,"Vicinity_Figures.pptx",sep = ""),
          width = 4, 
          height = 4,
          paper = "letter",
          orient = "portrait",
          scaling = 100,
          append = T,
          center = T,
          margins = c(top = 0, right = 0, bottom = 0, left = 0),
          )

  
datModel$resid <- resid(M)
datPoint = datModel %>% group_by(SID,Condition,TimePoint) %>%
  summarise(Vicinity = median(Vicinity, na.rm =T),
            Memory = mean(Memory, na.rm =T),
            resid = mean(resid,na.rm=T),
            ageBase = mean(ageBase),
            ageChange = mean(ageChange)) %>%
  group_by(SID,Condition) %>%
  summarise(Vicinity = max(Vicinity, na.rm =T),
            Memory = mean(Memory, na.rm =T),
            resid = mean(resid,na.rm=T),
            ageBase = mean(ageBase),
            ageChange = mean(ageChange)) %>%
  as.data.frame()

pred_dat <- ggeffects::ggpredict(
  M,
  terms = c("Vicinity [-.5:1 by=0.01]", "Condition"),
  condition = c(
    ageBase = mean(datModel$ageBase, na.rm = TRUE),
    ageChange = mean(datModel$ageChange, na.rm = TRUE)
  )
)



######### ageChange*Vicinity effect
A = sim_slopes(M, pred = Vicinity, modx = ageChange, modx.values = c(8.92), 
               confint = F, jnplot =T)
print(A)

p =  interact_plot(M, pred = Vicinity, modx = ageChange, modx.values = c(.5,1.5),
                   plot.points = F, point.alpha = 0.3,point.size = .5,point.shape = F,
                   line.thickness = 2,vary.lty=F, interval = T,int.type = "confidence",
                   linearity.check =F, rug =F, partial.residuals = T,centered = "all",
                   y.label = "Item-Location Memory Accuracy",
                   x.label = "Proportional Vicinity",
                   colors =c("#7ED7AFFF","#3A2C59FF") )
p = p+theme_bw()+gDefault
plot(p)
graph2ppt(file= paste(WD,"Vicinity_Fig5B.pptx",sep = ""), width = 14, height = 10)
# graph2ppt(file= paste(WD,"Vicinity_Fig5B.pptx",sep = ""), width = 6, height = 5)



######### Condition*Vicinity effect
A = sim_slopes(M, pred = Vicinity, modx = Condition, 
               confint = F, jnplot =T)
print(A)


p =  interact_plot(M, pred = Vicinity, modx = Condition, 
                   plot.points = F, point.alpha = 0.25,point.size = .5,point.shape = T,
                   line.thickness = 1,vary.lty=F, interval = T,int.type = "confidence",
                   linearity.check =F, rug =F,partial.residuals = T,centered = "all",
                   y.label = "Item-Location Memory Accuracy",
                   x.label = "Differential Vicinity")
p = p+theme_bw()+gDefault
plot(p)
graph2ppt(file= paste(WD,"Vicinity_Fig5A.pptx",sep = ""), width = 10, height = 5)
rocket(20, alpha = 1, begin = 0, end = 1, direction = 1)

p =  interact_plot(M, pred = Vicinity, modx = Condition, 
                   # modx.values = c(8,10,12),
                   plot.points = F, point.alpha = 0.1,point.size = .5,point.shape = F,
                   line.thickness = 1.2,vary.lty=T, interval = F,int.type = "confidence",
                   linearity.check =F, rug =F,partial.residuals = T,centered = "all",
                   y.label = "CLR",#"Item-Location Memory Accuracy",
                   x.label = "Differential Vicinity",
                   colors =c("#7ED7AFFF","#3573A1FF","#3A2C59FF") )
p = p+
  # theme_bw()+gDefault+
  # theme(legend.position.inside="top")+
  theme(
    legend.position = "inside", legend.position.inside = c(0.5, 0.9),
    legend.direction = "horizontal", legend.box.background = element_rect(color="red") )
plot(p)
#------ ageBase*Vicinity effect Separate Conditions ----
mClose = lmer(Memory ~ ageBase+ageChange+Vicinity+
                ageBase:Vicinity+
                ageChange:Vicinity+
                # I(Age^2)+
                (1|SID)+(1|Order),
              data=datModel[datModel$Condition=="Close",], na.action=na.exclude)
tab_model(mClose,show.se = T,show.stat = T,show.est = T)
sort(unique(datModel$ageChange))
sort(unique(datModel$ageBase)); hist(datModel$ageBase)


mFar = lmer(Memory ~ ageBase+ageChange+Vicinity+
              ageBase:Vicinity+
              ageChange:Vicinity+
              # I(Age^2)+
              (1|SID)+(1|Order),
            data=datModel[datModel$Condition=="Far",], na.action=na.exclude)
tab_model(mFar,show.se = T,show.stat = T,show.est = T)
sort(unique(datModel$ageChange))
sort(unique(datModel$ageBase)); hist(datModel$ageBase)

#------ ageBase*Vicinity effect -----

sim_slopes(M, pred = Vicinity, modx = ageBase,
           confint = F, jnplot =T)
A = sim_slopes(M, pred = Vicinity, modx = ageBase,
               modx.values = c(2.84),
               confint = F, jnplot =T)
print(A)
p = A$jn[[1]]$plot

sapply(p$layers, function(x) class(x$geom)[1])
p$layers[[9]]$aes_params$colour = "#595959"

p = p+annotate(geom="text", x=round(A$slopes$`Value of ageBase`,2), y=round(A$slopes$Est.,2), 
               label=paste("ageBase = ",round(A$slopes$`Value of ageBase`+min(sDat$ageBase),2),
                           ", Beta = ",round(A$slopes$Est.,2),sep = ""),
               color="#595959")+
  theme(legend.position = "bottom")
p=p+
  scale_x_continuous(
    limits = c(-.5, 6),
    breaks = seq(-.17, 5.83,length.out = 5),
    labels = round(seq(0, 6,length.out = 5)+8,1)
  )
  
plot(p)
graph2ppt(file= paste(WD,"Vicinity_Figures.pptx",sep = ""),
          width = 5, 
          height = 5,
          paper = "letter",
          orient = "portrait",
          scaling = 100,
          append = T,
          center = T,
          margins = c(top = 0, right = 0, bottom = 0, left = 0)
          )

#------ ageChange*Vicinity effect ------
sim_slopes(M, pred = Vicinity, modx = ageChange, 
               confint = F, jnplot =T)
A = sim_slopes(M, pred = Vicinity, modx = ageChange, modx.values = c(0.42), 
               confint = F, jnplot =T)
print(A)


datM = datModel %>% group_by(SID,ageChange,TimePoint) %>%
  reframe(
    Vicinity = c(min(Vicinity, na.rm =T),median(Vicinity, na.rm =T),max(Vicinity, na.rm =T)),
    Memory    =  c(min(Memory, na.rm =T),median(Memory, na.rm =T),max(Memory, na.rm =T)),
  ) %>%
  as.data.frame()
datM = datM[datM$ageChange>0,]
datM$ageChange = case_when(datM$ageChange>1 ~ 1.5,
                           datM$ageChange<1  ~ 0.5)

p =  interact_plot(M, pred = Vicinity, modx = ageChange, modx.values = c(.5,1.5),
                   plot.points = F, point.alpha = 0.3,point.size = .5,point.shape = F,
                   line.thickness = 1,vary.lty=F, interval = T,int.type = "confidence",
                   linearity.check =F, rug =F, partial.residuals = F,centered = "all",
                   y.label = "Item-Location Memory Accuracy",
                   x.label = "Proportional Vicinity",
                   colors =c("#A6BC09","#005148","#3A2C59FF")  )
p = p+
  geom_point(
    data = datM,
    aes(x = Vicinity, y = Memory, color = ageChange),
    alpha = 0.3,
    size = .25
  )+
  labs(
    x = "Differential Vicinity",
    y = "Memory",
    color = "ageChange",
    fill = "ageChange"
  ) +
  scale_x_continuous(
    limits = c(-.5, .9),
    breaks = round(seq(-.5, .9,length.out = 5),1),
    labels = round(seq(0, 1,length.out = 5),1)
  ) +
  scale_y_continuous(
    limits = c(30,65),
    breaks = c(30,33,seq(40, 60, by = 10))
  ) +
  geom_hline(
    yintercept = 33,
    linetype = "dashed",
    color = "gray50",
    linewidth = 0.6
  )+
  # scale_color_manual(values = c("#7ED7AFFF","#3573A1FF","#3A2C59FF") ) +
  # scale_fill_manual(values = c("#7ED7AFFF","#3573A1FF","#3A2C59FF") ) +
  theme_bw()+gDefault
plot(p)

graph2ppt(file= paste(WD,"Vicinity_Figures.pptx",sep = ""),
          width = 4, 
          height = 4,
          paper = "letter",
          orient = "portrait",
          scaling = 100,
          append = T,
          center = T,
          margins = c(top = 0, right = 0, bottom = 0, left = 0),
)

#-------------- CLR Model Testing ----
datModel = sDat
datModel$Condition = datModel$testName

# datModel$Vicinity = datModel$diff
datModel$Vicinity = datModel$diff+abs(min(datModel$diff,na.rm = T))
datModel$Vicinity = datModel$Vicinity/max(datModel$Vicinity,na.rm=T)
# datModel$Vicinity[datModel$Condition=="Close"] = scale(datModel$Vicinity[datModel$Condition=="Close"],center = T,scale = T)
# datModel$Vicinity[datModel$Condition=="Far"] = scale(datModel$Vicinity[datModel$Condition=="Far"],center = T,scale = T)

datModel$Memory  = datModel$CLR*100
datModel$Order = factor(datModel$timeStamp,levels = order(unique(datModel$timeStamp)))
# datModel$Order = as.numeric(datModel$timeStamp)
datModel$ageBase = datModel$ageBase - min(datModel$ageBase)
datModel$Age = datModel$Age - min(datModel$Age)
# datModel$ageChange = scale(datModel$ageChange)
datModel$TimePoint = ifelse(datModel$ageChange==0,1,2)

datModel = RemoveOutliers(datModel,factorNames = c("sessID","Condition"),varNames = "Vicinity")
datModel = datModel%>% group_by(SID,Condition,TimePoint) %>% fill(Vicinity, .direction = "downup")
datModel = datModel[!is.na(datModel$Vicinity),]

datModel %>% group_by(SID,Condition,TimePoint) %>%
  summarise(Vicinity = mean(Vicinity, na.rm =T),N=n(),
            CLR = mean(CLR, na.rm =T),CLR_50 = mean(CLR_50, na.rm =T)) %>%
  group_by(Condition,TimePoint) %>%
  summarise(meanV = mean(Vicinity, na.rm =T),
            sdV = sd(Vicinity, na.rm =T),
            MaxV = max(Vicinity, na.rm =T),
            MinV = min(Vicinity, na.rm =T),
            N=n(),
            ,CLR = mean(CLR, na.rm =T),CLR_50 = mean(CLR_50, na.rm =T)) %>% as.data.frame()


A = datModel %>% group_by(SID,Condition,TimePoint) %>%
  summarise(Vicinity = mean(Vicinity, na.rm =T),N=n()) %>% as.data.frame()

A$TimePoint = as.factor(A$TimePoint)
ggplot(A, aes(x=TimePoint, y=Vicinity, fill=TimePoint)) +
  geom_boxplot() +
  # scale_fill_viridis(discrete = TRUE, alpha=0.6) +
  geom_point(color="black", size=1, alpha=0.7)+
  geom_line(aes(group=SID), alpha=0.5)+
  facet_wrap(~Condition)

# datModel = datModel[,c("SID", "Memory", "ageBase", "ageChange", "Condition", 'Vicinity',"Order","TimePoint")]
# datModelVic = datModel

DV = "Memory ~"
rEff = "+(1+Condition|SID)+(1+Vicinity|Order)"
models = c("1",
           "ageBase+",
           "ageBase+ageChange",
           "ageBase+ageChange+Condition",
           "ageBase+ageChange+Condition+Vicinity+",
           "ageBase+ageChange+Condition+Vicinity+",
           "ageBase+ageChange+Condition+Vicinity+ageChange*Vicinity",
           "ageBase+ageChange+Condition+Vicinity+ageChange*Vicinity+ageBase*Vicinity",
           "ageBase+ageChange+Condition+Vicinity+ageChange*Vicinity+ageBase*Vicinity+Condition*Vicinity",
           "ageBase+ageChange+Condition+Vicinity+ageChange*Vicinity+ageBase*Vicinity+Condition*Vicinity+ageChange:Condition:Vicinity")
i=1
m=list()
for (idx in seq(1,length(models))){
  m[[idx]] = lmer(paste(DV,models[idx],rEff,sep = ""), 
                  data=datModel, na.action=na.exclude)
}
mComp <- do.call(anova,m)
# row.names(mComp) = models
mComp$Best = seq(1,nrow(mComp))*as.numeric(mComp$`Pr(>Chisq)`<0.05)
print(mComp)

mNULL = lmer(Vicinity ~ 1+(1|SID), data=datModel, na.action=na.exclude)
M = m[[max(mComp$Best,na.rm=T)]]
tab_model(M,show.se = T,show.stat = T,show.est = T)


p =  interact_plot(M, pred = Vicinity, modx = ageChange, mod2 = Condition, 
                   plot.points = F, point.alpha = 0.25,point.size = .5,point.shape = T,
                   line.thickness = 1,vary.lty=F, interval = T,int.type = "confidence",
                   linearity.check =F, rug =F,partial.residuals = F,centered = "all",
                   y.label = "Item-Location Memory Accuracy",
                   x.label = "Differential Vicinity")
p = p+theme_bw()+gDefault
plot(p)

p =  interact_plot(M, pred = Vicinity, modx = Condition, 
                   plot.points = F, point.alpha = 0.25,point.size = .5,point.shape = T,
                   line.thickness = 1,vary.lty=F, interval = T,int.type = "confidence",
                   linearity.check =F, rug =F,partial.residuals = T,centered = "all",
                   y.label = "Item-Location Memory Accuracy",
                   x.label = "Differential Vicinity")
p = p+theme_bw()+gDefault
plot(p)

newdat <- make_predictions(
  M,
  pred = "Vicinity",
  at = list(
    Vicinity = seq(0.1, 1.0, by = 0.1),
    Condition = levels(datModel$Condition)
  )
)

ggplot(newdat, aes(x = Vicinity, y = predicted, color = Condition)) +
  geom_line()

summary(M)
vif(M)
hist(resid(M))
anova(mNULL,M)
VarCorr(M)
M = lmer(Memory ~ 
           Vicinity*(ageBase+ageChange+Condition)+
           # ageChange:Condition:Vicinity+
           (1+Condition|SID)+
           (1+Vicinity|Order), 
                data=datModel, na.action=na.exclude)
tab_model(M,show.se = T,show.stat = T,show.est = T)




plot(allEffects(M))
######### ageChange*Vicinity effect
A = sim_slopes(M, pred = Vicinity, modx = ageChange, modx.values = c(8.92), 
               confint = F, jnplot =T)
print(A)

p =  interact_plot(M, pred = Vicinity, modx = ageChange, modx.values = c(.5,1.5),
                   plot.points = F, point.alpha = 0.3,point.size = .5,point.shape = F,
                   line.thickness = 2,vary.lty=F, interval = T,int.type = "confidence",
                   linearity.check =F, rug =F, partial.residuals = T,centered = "all",
                   y.label = "Item-Location Memory Accuracy",
                   x.label = "Proportional Vicinity",
                   colors =c("#7ED7AFFF","#3A2C59FF") )
p = p+theme_bw()+gDefault
plot(p)
graph2ppt(file= paste(WD,"Vicinity_Fig5B.pptx",sep = ""), width = 14, height = 10)
# graph2ppt(file= paste(WD,"Vicinity_Fig5B.pptx",sep = ""), width = 6, height = 5)



######### Condition*Vicinity effect
A = sim_slopes(M, pred = Vicinity, modx = Condition, 
               confint = F, jnplot =T)
print(A)


p =  interact_plot(M, pred = Vicinity, modx = Condition, 
                   plot.points = F, point.alpha = 0.25,point.size = .5,point.shape = T,
                   line.thickness = 1,vary.lty=F, interval = T,int.type = "confidence",
                   linearity.check =F, rug =F,partial.residuals = T,centered = "all",
                   y.label = "Item-Location Memory Accuracy",
                   x.label = "Differential Vicinity")
p = p+theme_bw()+gDefault
plot(p)
graph2ppt(file= paste(WD,"Vicinity_Fig5A.pptx",sep = ""), width = 10, height = 5)
rocket(20, alpha = 1, begin = 0, end = 1, direction = 1)

p =  interact_plot(M, pred = Vicinity, modx = Condition, 
                   # modx.values = c(8,10,12),
                   plot.points = F, point.alpha = 0.1,point.size = .5,point.shape = F,
                   line.thickness = 1.2,vary.lty=T, interval = F,int.type = "confidence",
                   linearity.check =F, rug =F,partial.residuals = T,centered = "all",
                   y.label = "CLR",#"Item-Location Memory Accuracy",
                   x.label = "Differential Vicinity",
                   colors =c("#7ED7AFFF","#3573A1FF","#3A2C59FF") )
p = p+
  # theme_bw()+gDefault+
  # theme(legend.position.inside="top")+
  theme(
    legend.position = "inside", legend.position.inside = c(0.5, 0.9),
    legend.direction = "horizontal", legend.box.background = element_rect(color="red") )
plot(p)
#-------------- CLR Model Vicinity Correct+Incorrect ----
datModel = sDat
datModel$Condition = datModel$testName
datModel$Vicinity = datModel$diff
datModel$Vicinity = datModel$CorrL - (1-datModel$IncorrL)
# datModel$VicinityI = datModel$IncorrL 
# datModel$VicinityC = datModel$CorrL
# datModel$VicinityI = datModel$IncorrL 

datModel$Memory  = datModel$CLR*100
datModel$Order = factor(datModel$timeStamp,levels = order(unique(datModel$timeStamp)))
# datModel$Order = as.numeric(datModel$timeStamp)
datModel$ageBase = datModel$ageBase - min(datModel$ageBase)
datModel$Age = datModel$Age - min(datModel$Age)
# datModel$ageChange = scale(datModel$ageChange)
datModel$TimePoint = ifelse(datModel$ageChange==0,1,2)

datModel = RemoveOutliers(datModel,factorNames = c("sessID","Condition"),varNames = "Vicinity")
datModel = datModel%>% group_by(SID,Condition,TimePoint) %>% fill(Vicinity, .direction = "downup")
datModel = datModel[!is.na(datModel$Vicinity),]

datModel %>% group_by(SID,Condition,TimePoint) %>%
  summarise(Vicinity = mean(Vicinity, na.rm =T),CLR = mean(CLR, na.rm =T),CLR_50 = mean(CLR_50, na.rm =T)) %>%
  group_by(Condition,TimePoint) %>%
  summarise(Vicinity = mean(Vicinity, na.rm =T),CLR = mean(CLR, na.rm =T),CLR_50 = mean(CLR_50, na.rm =T)) %>% as.data.frame()

# datModel = datModel[,c("SID", "Memory", "ageBase", "ageChange", "Condition", 'Vicinity',"Order","TimePoint")]
# datModelVic = datModel

DV = "Memory ~"
rEff = "+(1+Condition|SID)+(1+Vicinity|Order)"
models = c("1",
           "ageBase+",
           "ageBase+ageChange",
           "ageBase+ageChange+Condition",
           "ageBase+ageChange+Condition+Vicinity+",
           "ageBase+ageChange+Condition+(VicinityC+VicinityI)+",
           "ageBase+ageChange+Condition+(VicinityC+VicinityI)+ageChange*(VicinityC+VicinityI)",
           "ageBase+ageChange+Condition+(VicinityC+VicinityI)+ageChange*(VicinityC+VicinityI)+ageBase*(VicinityC+VicinityI)",
           "ageBase+ageChange+Condition+(VicinityC+VicinityI)+ageChange*(VicinityC+VicinityI)+ageBase*(VicinityC+VicinityI)+Condition*(VicinityC+VicinityI)",
           "ageBase+ageChange+Condition+(VicinityC+VicinityI)+ageChange*(VicinityC+VicinityI)+ageBase*(VicinityC+VicinityI)+Condition*(VicinityC+VicinityI)+ageChange:Condition:Vicinity")
i=1
m=list()
for (idx in seq(1,length(models))){
  m[[idx]] = lmer(paste(DV,models[idx],rEff,sep = ""), 
                  data=datModel, na.action=na.exclude)
}
mComp <- do.call(anova,m)
# row.names(mComp) = models
mComp$Best = seq(1,nrow(mComp))*as.numeric(mComp$`Pr(>Chisq)`<0.05)
print(mComp)

mNULL = lmer(Vicinity ~ 1+(1|SID), data=datModel, na.action=na.exclude)
M = m[[max(mComp$Best,na.rm=T)]]
tab_model(M,show.se = T,show.stat = T,show.est = T)

p =  interact_plot(M, pred = Vicinity, modx = Condition, 
                   plot.points = F, point.alpha = 0.25,point.size = .5,point.shape = T,
                   line.thickness = 1,vary.lty=F, interval = T,int.type = "confidence",
                   linearity.check =F, rug =F,partial.residuals = T,centered = "all",
                   y.label = "Item-Location Memory Accuracy",
                   x.label = "Differential Vicinity")
p = p+theme_bw()+gDefault
plot(p)



summary(M)
vif(M)
hist(resid(M))
anova(mNULL,M)
VarCorr(M)
M = lmer(Memory ~ 
           Vicinity*(ageBase+ageChange+Condition)+
           # ageChange:Condition:Vicinity+
           (1+Condition|SID)+
           (1+Vicinity|Order), 
         data=datModel, na.action=na.exclude)
summary(M)

tab_model(M,show.se = T,show.stat = T,show.est = T)

plot(allEffects(M))
######### ageChange*Vicinity effect
A = sim_slopes(M, pred = Vicinity, modx = ageChange, modx.values = c(8.92), 
               confint = F, jnplot =T)
print(A)

p =  interact_plot(M, pred = Vicinity, modx = ageChange, modx.values = c(.5,1.5),
                   plot.points = F, point.alpha = 0.3,point.size = .5,point.shape = F,
                   line.thickness = 2,vary.lty=F, interval = T,int.type = "confidence",
                   linearity.check =F, rug =F, partial.residuals = T,centered = "all",
                   y.label = "Item-Location Memory Accuracy",
                   x.label = "Proportional Vicinity",
                   colors =c("#7ED7AFFF","#3A2C59FF") )
p = p+theme_bw()+gDefault
plot(p)
graph2ppt(file= paste(WD,"Vicinity_Fig5B.pptx",sep = ""), width = 14, height = 10)
# graph2ppt(file= paste(WD,"Vicinity_Fig5B.pptx",sep = ""), width = 6, height = 5)



######### Condition*Vicinity effect
A = sim_slopes(M, pred = Vicinity, modx = Condition, 
               confint = F, jnplot =T)
print(A)


p =  interact_plot(M, pred = Vicinity, modx = Condition, 
                   plot.points = F, point.alpha = 0.25,point.size = .5,point.shape = T,
                   line.thickness = 1,vary.lty=F, interval = T,int.type = "confidence",
                   linearity.check =F, rug =F,partial.residuals = T,centered = "all",
                   y.label = "Item-Location Memory Accuracy",
                   x.label = "Differential Vicinity")
p = p+theme_bw()+gDefault
plot(p)
graph2ppt(file= paste(WD,"Vicinity_Fig5A.pptx",sep = ""), width = 10, height = 5)
rocket(20, alpha = 1, begin = 0, end = 1, direction = 1)

p =  interact_plot(M, pred = Vicinity, modx = Condition, 
                   # modx.values = c(8,10,12),
                   plot.points = F, point.alpha = 0.1,point.size = .5,point.shape = F,
                   line.thickness = 1.2,vary.lty=T, interval = F,int.type = "confidence",
                   linearity.check =F, rug =F,partial.residuals = T,centered = "all",
                   y.label = "CLR",#"Item-Location Memory Accuracy",
                   x.label = "Differential Vicinity",
                   colors =c("#7ED7AFFF","#3573A1FF","#3A2C59FF") )
p = p+
  # theme_bw()+gDefault+
  # theme(legend.position.inside="top")+
  theme(
    legend.position = "inside", legend.position.inside = c(0.5, 0.9),
    legend.direction = "horizontal", legend.box.background = element_rect(color="red") )
plot(p)
#######################################################  Exploration-Map Similarity Table 2 and Figure 6-----
#----------------- prepare cluster ----
dat = datGaze[datGaze$MeasureName == "EMS",]
dat$timePoint = round(dat$timePoint)

# dat = RemoveOutliers(dat,factorNames = c("ID","condition","timePoint"),varNames = "value")
# dat = dat%>% group_by(ID,condition,testName) %>%
#   fill(value, .direction = "downup")

# dat = RemoveOutliers(dat,factorNames = c("ID","condition","testName"),varNames = "value")
# dat = dat%>% group_by(ID,condition,testName) %>%
#   fill(value, .direction = "downup")

# dat = dat%>% group_by(ID,condition,testName) %>% 
#   mutate(value = na.approx(value))%>%
#   as.data.frame()

dat =  pivot_wider(dat,id_cols = c("SID","sessID","timePoint","testName"),
                   names_from = c("condition"), 
                   values_from = c("value"),names_sep = "_") %>%as.data.frame()
# dat$diff = (1-(1-dat$CorrL)/2) - (1-(1-dat$IncorrL)/2)
dat$CorrL = dat$CorrL +abs(min(dat$CorrL,na.rm=T))
dat$IncorrL = dat$IncorrL +abs(min(dat$IncorrL,na.rm=T))
dat$diff = dat$CorrL - dat$IncorrL

clusterFileName = "PT_EMSConv.csv"
testName = c("Far_t2","Close_t2")
datCluster = read.csv(file = paste(RD,PTPath,clusterFileName,sep = ""))
datCluster = datCluster[datCluster$testName %in% testName & datCluster$significant=="*",]
print(datCluster)
clusterFar = c(1000,1500)
clusterClose = c(900,1500)

d = dat[(dat$testName == "Far" &
           (dat$timePoint >= clusterFar[1]) & (dat$timePoint <= clusterFar[2]))|
          (dat$testName == "Close" &
             (dat$timePoint >= clusterClose[1]) & (dat$timePoint <= clusterClose[2]))  ,]

# d = RemoveOutliers(d,factorNames = c("sessID","testName","timePoint"),varNames = "diff",Criteria=3)
# d = d[!is.na(d$diff),]
# head(d)
# d=dat
# dat = PermutationTestDataPrep_HMLET(data = d[d$SID=="hc708",], ID = "SID", trial = NULL, 
#                                     timePoint = "timePoint",
#                                     condition = "testName",
#                                     gazeMeasure = "diff", testName = "sessID")
# PlotTimeSeries_HMLET(dat,showDataPointProp = F, showOverallMean = F)


d = d[order(d$SID,d$sessID,d$timePoint,d$testName),]
windowSize = 100
groupSize = 6
sDat = d %>% group_by(SID,sessID,testName) %>%
  mutate(diffn = na.approx(diff),
         timeStamp = floor(timePoint/windowSize),
         # timeStamp = floor((round(timePoint*60/1000)-min(round(unique(timePoint)*60/1000)))/groupSize),
  ) %>%
  # group_by(SID,sessID,testName,timeIdx) %>%
  group_by(SID,sessID,testName,timeStamp) %>%
  summarize(
            # CorrL = IntegrateVector(CorrL,dt = 1000/60,t0 = timePoint[1],
            #                         interpolate = T,method = "cubic")/n(),
            diff = mean(diffn,na.rm = T),
            # integ = IntegrateVector(diffn,dt = 1000/60,t0 = timePoint[1],
            #                         interpolate = T,method = "cubic")/n(),
            # IncorrL = IntegrateVector(IncorrL,dt = 1000/60,t0 = timePoint[1],
            #                           interpolate = T,method = "cubic")/n(),
            N=n()) %>%
  group_by(testName) %>%
  mutate(timeStamp = timeStamp - min(timeStamp)+1) %>%
  as.data.frame()


sDat$ID = paste(sDat$SID,sDat$sessID,sep = "")
unique(sDat$timeStamp)
sDat = merge(sDat, datAge, all.x = T,by = c("SID","sessID","ID","testName"))

#-------------- CLR Model  ----
datModel = sDat
datModel$Condition = datModel$testName
datModel$Similarity = datModel$diff


datModel$Memory  = datModel$CLR*100
datModel$Order = factor(datModel$timeStamp,levels = order(unique(datModel$timeStamp)))

datModel$ageBase = datModel$ageBase - min(datModel$ageBase)
datModel$Age = datModel$Age - min(datModel$Age)

datModel$TimePoint = ifelse(datModel$ageChange==0,1,2)

datModel = RemoveOutliers(datModel,factorNames = c("sessID","Condition"),varNames = "Similarity")
datModel = datModel%>% group_by(SID,Condition,TimePoint) %>% fill(Similarity, .direction = "downup")
datModel = datModel[!is.na(datModel$Similarity),]

datModel %>% group_by(SID,Condition,TimePoint) %>%
  summarise(Similarity = mean(Similarity, na.rm =T),N=n(),
            CLR = mean(CLR, na.rm =T),CLR_50 = mean(CLR_50, na.rm =T)) %>%
  group_by(Condition,TimePoint) %>%
  summarise(meanV = mean(Similarity, na.rm =T),
            sdV = sd(Similarity, na.rm =T),
            MaxV = max(Similarity, na.rm =T),
            MinV = min(Similarity, na.rm =T),
            N=n(),
            ,CLR = mean(CLR, na.rm =T),CLR_50 = mean(CLR_50, na.rm =T)) %>% as.data.frame()


A = datModel %>% group_by(SID,Condition,TimePoint) %>%
  summarise(Similarity = mean(Similarity, na.rm =T)) %>% as.data.frame()

A$TimePoint = as.factor(A$TimePoint)
ggplot(A, aes(x=TimePoint, y=Similarity, fill=TimePoint)) +
  geom_boxplot() +
  # scale_fill_viridis(discrete = TRUE, alpha=0.6) +
  geom_jitter(color="black", size=0.4, alpha=0.9)+
  geom_line(aes(group=SID))+
  facet_wrap(~Condition)


DV = "Memory ~"
rEff = "+(1+Condition|SID)+(1+Similarity|Order)"
models = c("1",
           "ageBase+",
           "ageBase+ageChange",
           "ageBase+ageChange+Condition",
           "ageBase+ageChange+Condition+Similarity+",
           "ageBase+ageChange+Condition+Similarity+ageChange*Similarity",
           "ageBase+ageChange+Condition+Similarity+ageChange*Similarity+ageBase*Similarity",
           "ageBase+ageChange+Condition+Similarity+ageChange*Similarity+ageBase*Similarity+Condition*Similarity",
           "ageBase+ageChange+Condition+Similarity+ageChange*Similarity+ageBase*Similarity+Condition*Similarity+ageChange:Condition:Similarity")
i=1
m=list()
for (idx in seq(1,length(models))){
  m[[idx]] = lmer(paste(DV,models[idx],rEff,sep = ""), 
                  data=datModel, na.action=na.exclude)
}
mComp <- do.call(anova,m)
# row.names(mComp) = models
mComp$Best = seq(1,nrow(mComp))*as.numeric(mComp$`Pr(>Chisq)`<0.05)
print(mComp)

mNULL = lmer(Similarity ~ 1+(1+Condition|SID)+(1+Similarity|Order), data=datModel, na.action=na.exclude)
M = m[[max(mComp$Best,na.rm=T)]]
tab_model(M,show.se = T,show.stat = T,show.est = T)

tab_model(m[[9]],show.se = T,show.stat = T,show.est = T)
summary(M)
vif(M)
hist(resid(M))
anova(mNULL,M)
VarCorr(M)

tab_model(m[[5]],show.se = T,show.stat = T,show.est = T)
#------ Condition Main effect -----
anova(m[[5]],m[[1]])

tab_model(m[[5]],show.se = T,show.stat = T,show.est = T)

em <- emmeans(m[[5]], ~ Condition, pbkrtest.limit = 3334)
plot(em, by = NULL , comparisons = TRUE, adjust = "tukey", 
     horizontal = FALSE)
summary(em)
contrast(em, method = "pairwise", adjust = "tukey")

datModel %>% group_by(SID,Condition,TimePoint) %>%
  summarise(Vicinity = mean(Vicinity, na.rm =T),N=n(),
            Memory = mean(Memory, na.rm =T)) %>%
  group_by(Condition) %>%
  summarise(MeanDV = mean(Memory, na.rm =T),
            SDDV = sd(Memory,  na.rm= T)) %>% as.data.frame()


tab_model(m[[1]],show.se = T,show.stat = T,show.est = T)


em1 <- emmeans(M, ~ Condition, pbkrtest.limit = 3334)
plot(em1, by = NULL , comparisons = TRUE, adjust = "tukey", 
     horizontal = FALSE)
summary(em1)
contrast(em1, method = "pairwise", adjust = "tukey")

#------ Similarity Main effect -----
em <- emmeans(m[[5]], ~ Similarity, pbkrtest.limit = 3334)
plot(em, by = NULL , comparisons = TRUE, adjust = "tukey", 
     horizontal = FALSE)
summary(em)

plot(allEffects(m[[5]]))

#------ Condition*Similarity effect -----
anova(m[[5]],m[[8]])
tab_model(m[[9]],show.se = T,show.stat = T,show.est = T)
A = sim_slopes(M, pred = Vicinity, modx = Condition, 
               confint = F, jnplot =T)
print(A)
plot(A)

# datM = datModel %>% group_by(SID,Condition,TimePoint) %>%
#   summarise(Vicinity = median(Vicinity, na.rm =T),
#             Memory = mean(Memory, na.rm =T),
#             resid = mean(resid,na.rm=T),
#             ageBase = mean(ageBase,na.rm=T),
#             ageChange = mean(ageChange,na.rm=T),
#             .groups = "drop") %>%
#   as.data.frame()

datM = datModel %>% group_by(SID,Condition,TimePoint) %>%
  reframe(
    Vicinity = c(min(Vicinity, na.rm =T),max(Vicinity, na.rm =T)),
    # Memory    =  c(min(Memory, na.rm =T)+(sd(Memory, na.rm =T)),max(Memory, na.rm =T)-(sd(Memory, na.rm =T))),
    Memory    =  c(min(Memory, na.rm =T),max(Memory, na.rm =T)),
  ) %>%
  # summarise(
  #   Vicinity  = mean(Vic, na.rm = TRUE),
  #   Memory    = mean(Memory, na.rm = TRUE),
  #   ageBase   = mean(ageBase, na.rm = TRUE),
  #   ageChange = mean(ageChange, na.rm = TRUE),
  #   .groups = "drop"
  # ) %>%
  as.data.frame()

p =  interact_plot(M, pred = Vicinity, modx = Condition,
                   plot.points = F, point.alpha = 0.3,point.size = .5, point.shape = F,
                   line.thickness = 1.5,vary.lty=F, interval = T,int.type = "confidence",
                   linearity.check =F, rug =F,partial.residuals = F,centered = "all",
                   y.label = "Item-Location Memory Accuracy",
                   x.label = "Differential Vicinity")
p = p+
  geom_point(
    data = datM,
    aes(x = Vicinity, y = Memory, color = Condition),
    alpha = 0.3,
    size = .25,
    position = position_jitter(width = 0.01, height = 0)
  )+
  # geom_line(
  #   data = datM,
  #   aes(x = Vicinity, y = Memory, group = paste(SID,Condition)),
  #   alpha = 0.4,
  # )+
  labs(
    x = "Differential Vicinity",
    y = "Memory",
    color = "Condition",
    fill = "Condition"
  ) +
  scale_x_continuous(
    limits = c(-.5, .9),
    breaks = round(seq(-.5, .9,length.out = 5),1),
    labels = round(seq(0, 1,length.out = 5),1)
  ) +
  scale_y_continuous(
    limits = c(30,65),
    breaks = c(30,33,seq(40, 60, by = 10))
  ) +
  geom_hline(
    yintercept = 33,
    linetype = "dashed",
    color = "gray50",
    linewidth = 0.6
  )+
  # scale_color_manual(values = c("#7ED7AFFF","#3573A1FF","#3A2C59FF") ) +
  # scale_fill_manual(values = c("#7ED7AFFF","#3573A1FF","#3A2C59FF") ) +
  theme_bw()+gDefault
plot(p)

graph2ppt(file= paste(WD,"Vicinity_Figures.pptx",sep = ""),
          width = 4, 
          height = 3,
          paper = "letter",
          orient = "portrait",
          scaling = 100,
          append = T,
          center = T,
          margins = c(top = 0, right = 0, bottom = 0, left = 0),
)


datModel$resid <- resid(M)
datPoint = datModel %>% group_by(SID,Condition,TimePoint) %>%
  summarise(Vicinity = median(Vicinity, na.rm =T),
            Memory = mean(Memory, na.rm =T),
            resid = mean(resid,na.rm=T),
            ageBase = mean(ageBase),
            ageChange = mean(ageChange)) %>%
  group_by(SID,Condition) %>%
  summarise(Vicinity = max(Vicinity, na.rm =T),
            Memory = mean(Memory, na.rm =T),
            resid = mean(resid,na.rm=T),
            ageBase = mean(ageBase),
            ageChange = mean(ageChange)) %>%
  as.data.frame()

pred_dat <- ggeffects::ggpredict(
  M,
  terms = c("Vicinity [-.5:1 by=0.01]", "Condition"),
  condition = c(
    ageBase = mean(datModel$ageBase, na.rm = TRUE),
    ageChange = mean(datModel$ageChange, na.rm = TRUE)
  )
)



######### ageChange*Vicinity effect
A = sim_slopes(M, pred = Vicinity, modx = ageChange, modx.values = c(8.92), 
               confint = F, jnplot =T)
print(A)

p =  interact_plot(M, pred = Vicinity, modx = ageChange, modx.values = c(.5,1.5),
                   plot.points = F, point.alpha = 0.3,point.size = .5,point.shape = F,
                   line.thickness = 2,vary.lty=F, interval = T,int.type = "confidence",
                   linearity.check =F, rug =F, partial.residuals = T,centered = "all",
                   y.label = "Item-Location Memory Accuracy",
                   x.label = "Proportional Vicinity",
                   colors =c("#7ED7AFFF","#3A2C59FF") )
p = p+theme_bw()+gDefault
plot(p)
graph2ppt(file= paste(WD,"Vicinity_Fig5B.pptx",sep = ""), width = 14, height = 10)
# graph2ppt(file= paste(WD,"Vicinity_Fig5B.pptx",sep = ""), width = 6, height = 5)



######### Condition*Vicinity effect
A = sim_slopes(M, pred = Vicinity, modx = Condition, 
               confint = F, jnplot =T)
print(A)


p =  interact_plot(M, pred = Vicinity, modx = Condition, 
                   plot.points = F, point.alpha = 0.25,point.size = .5,point.shape = T,
                   line.thickness = 1,vary.lty=F, interval = T,int.type = "confidence",
                   linearity.check =F, rug =F,partial.residuals = T,centered = "all",
                   y.label = "Item-Location Memory Accuracy",
                   x.label = "Differential Vicinity")
p = p+theme_bw()+gDefault
plot(p)
graph2ppt(file= paste(WD,"Vicinity_Fig5A.pptx",sep = ""), width = 10, height = 5)
rocket(20, alpha = 1, begin = 0, end = 1, direction = 1)

p =  interact_plot(M, pred = Vicinity, modx = Condition, 
                   # modx.values = c(8,10,12),
                   plot.points = F, point.alpha = 0.1,point.size = .5,point.shape = F,
                   line.thickness = 1.2,vary.lty=T, interval = F,int.type = "confidence",
                   linearity.check =F, rug =F,partial.residuals = T,centered = "all",
                   y.label = "CLR",#"Item-Location Memory Accuracy",
                   x.label = "Differential Vicinity",
                   colors =c("#7ED7AFFF","#3573A1FF","#3A2C59FF") )
p = p+
  # theme_bw()+gDefault+
  # theme(legend.position.inside="top")+
  theme(
    legend.position = "inside", legend.position.inside = c(0.5, 0.9),
    legend.direction = "horizontal", legend.box.background = element_rect(color="red") )
plot(p)
#------ ageBase*Similarity effect Separate Conditions ----
mClose = lmer(Memory ~ ageBase+ageChange+Similarity+
                ageBase:Similarity+
                ageChange:Similarity+
                # I(Age^2)+
                (1|SID)+(1|Order),
              data=datModel[datModel$Condition=="Close",], na.action=na.exclude)
tab_model(mClose,show.se = T,show.stat = T,show.est = T)
sort(unique(datModel$ageChange))
sort(unique(datModel$ageBase)); hist(datModel$ageBase)


mFar = lmer(Memory ~ ageBase+ageChange+Similarity+
              ageBase:Similarity+
              ageChange:Similarity+
              # I(Age^2)+
              (1|SID)+(1|Order),
            data=datModel[datModel$Condition=="Far",], na.action=na.exclude)
tab_model(mFar,show.se = T,show.stat = T,show.est = T)
sort(unique(datModel$ageChange))
sort(unique(datModel$ageBase)); hist(datModel$ageBase)

#------ ageBase*Similarity effect -----

sim_slopes(M, pred = Similarity, modx = ageBase,
           confint = F, jnplot =T)
A = sim_slopes(M, pred = Similarity, modx = ageBase,
               modx.values = c(1.78),
               confint = F, jnplot =T)
print(A)
p = A$jn[[1]]$plot

sapply(p$layers, function(x) class(x$geom)[1])
p$layers[[9]]$aes_params$colour = "#595959"

p = p+annotate(geom="text", x=round(A$slopes$`Value of ageBase`,2), y=round(A$slopes$Est.,2), 
               label=paste("ageBase = ",round(A$slopes$`Value of ageBase`+min(sDat$ageBase),2),
                           ", Beta = ",round(A$slopes$Est.,2),sep = ""),
               color="#595959")+
  theme(legend.position = "bottom")+
  scale_x_continuous(
    limits = c(-.5, 6),
    breaks = seq(-.17, 5.83,length.out = 5),
    labels = round(seq(0, 6,length.out = 5)+8,1)
  )

plot(p)
graph2ppt(file= paste(WD,"Similarity_Figures.pptx",sep = ""),
          width = 5, 
          height = 5,
          paper = "letter",
          orient = "portrait",
          scaling = 100,
          append = T,
          center = T,
          margins = c(top = 0, right = 0, bottom = 0, left = 0)
)

#------ ageChange*Similarity effect ------
sim_slopes(M, pred = Similarity, modx = ageChange, 
           confint = F, jnplot =T)
A = sim_slopes(M, 
               pred = Similarity, 
               modx = ageChange, modx.values = c(0), 
               confint = F, jnplot =T)
print(A)


p = A$jn[[1]]$plot

sapply(p$layers, function(x) class(x$geom)[1])
p$layers[[9]]$aes_params$colour = "#595959"

# p = p+annotate(geom="text", x=round(A$slopes$`Value of ageChange`,2), y=round(A$slopes$Est.,2), 
#                label=paste("ageChange = ",round(A$slopes[[1]]$`Value of ageChange`,2),
#                            ", Beta = ",round(A$slopes$Est.,2),sep = ""),
#                color="#595959")+
#   theme(legend.position = "bottom")+
#   scale_x_continuous(
#     limits = c(-.5, 6),
#     breaks = seq(-.17, 5.83,length.out = 5),
#     labels = round(seq(0, 6,length.out = 5)+8,1)
#   )
# 
# plot(p)


datM = datModel %>% group_by(SID,ageChange,Condition,TimePoint) %>%
  reframe(
    Similarity = c(min(Similarity, na.rm =T),max(Similarity, na.rm =T)),
    Memory    =  c(min(Memory, na.rm =T),max(Memory, na.rm =T)),
  ) %>%
  as.data.frame()
datM = datM[datM$ageChange>0,]
datM$ageChange = case_when(datM$ageChange>1 ~ 1.5,
                           datM$ageChange<1  ~ 0.5)

p =  interact_plot(M, pred = Similarity, modx = ageChange, modx.values = c(.5,1.5),
                   plot.points = F, point.alpha = 0.3,point.size = .5,point.shape = F,
                   line.thickness = 1,vary.lty=F, interval = T,int.type = "confidence",
                   linearity.check =F, rug =F, partial.residuals = F,centered = "all",
                   y.label = "Item-Location Memory Accuracy",
                   x.label = "Proportional Similarity",
                   colors =c("#F6AE86FF","#661F54FF") )
p = p+
  geom_point(
    data = datM,
    aes(x = Similarity, y = Memory, color = ageChange),
    alpha = 0.3,
    size = .25
  )+
  labs(
    x = "Differential Similarity",
    y = "Memory",
    color = "ageChange",
    fill = "ageChange"
  ) +
  scale_x_continuous(
    limits = c(-.07, .1),
    breaks = seq(-.07, .1,length.out = 5),
    labels = round(seq(0, 1,length.out = 5),1)
  ) +
  scale_y_continuous(
    limits = c(30,60),
    breaks = c(30,33,seq(40, 60, by = 10))
  ) +
  geom_hline(
    yintercept = 33,
    linetype = "dashed",
    color = "gray50",
    linewidth = 0.6
  )+
  # scale_color_manual(values = c("#7ED7AFFF","#3573A1FF","#3A2C59FF") ) +
  # scale_fill_manual(values = c("#7ED7AFFF","#3573A1FF","#3A2C59FF") ) +
  theme_bw()+gDefault
plot(p)

# graph2ppt(file= paste(WD,"Similarity_Figures.pptx",sep = ""),
#           width = 4, 
#           height = 3,
#           paper = "letter",
#           orient = "portrait",
#           scaling = 100,
#           append = T,
#           center = T,
#           margins = c(top = 0, right = 0, bottom = 0, left = 0),
# )
#------ Condition*ageChange*Similarity effect ------
anova(m[[8]],m[[9]])
tab_model(m[[9]],show.se = T,show.stat = T,show.est = T)

sim_slopes(M, 
           pred = Similarity, 
           modx = ageChange, 
           mod2 = Condition, 
           confint = F, jnplot =T)
A = sim_slopes(M, pred = Similarity, 
               modx = ageChange, modx.values = c(-0.03,.91), 
               mod2 = Condition,
               confint = F, jnplot =T)
print(A)


p = A$jn[[1]]$plot

p$layers[[9]]$aes_params$colour = "#595959"

p = p+annotate(geom="text", x=round(A$slopes[[1]]$`Value of ageChange`,2), y=round(A$slopes[[1]]$Est.,2),
               label=paste("ageChange = ",round(A$slopes[[1]]$`Value of ageChange`,2),
                           ", Beta = ",round(A$slopes[[1]]$Est.,2),sep = ""),
               color="#595959")+
  theme(legend.position = "bottom")+
  scale_x_continuous(
    limits = c(-.1, 3.5),
    breaks = seq(0,3,length.out = 3),
    labels = round(seq(0, 3,length.out = 3),1)
  )+ ggtitle("Johnson-Neyman Plot for Close")

plot(p)

plot(p)
graph2ppt(file= paste(WD,"Similarity_Figures.pptx",sep = ""),
          width = 3.5, 
          height = 4.5,
          paper = "letter",
          orient = "portrait",
          scaling = 100,
          append = T,
          center = T,
          margins = c(top = 0, right = 0, bottom = 0, left = 0)
)

p = A$jn[[2]]$plot

p$layers[[9]]$aes_params$colour = "#595959"

p = p+annotate(geom="text", x=round(A$slopes[[2]]$`Value of ageChange`,2), y=round(A$slopes[[2]]$Est.,2),
               label=paste("ageChange = ",round(A$slopes[[2]]$`Value of ageChange`,2),
                           ", Beta = ",round(A$slopes[[2]]$Est.,2),sep = ""),
               color="#595959")+
  theme(legend.position = "bottom")+
  scale_x_continuous(
    limits = c(-.1, 3.5),
    breaks = seq(0,3,length.out = 3),
    labels = round(seq(0, 3,length.out = 3),1)
  )+ ggtitle("Johnson-Neyman Plot for Far")

plot(p)
graph2ppt(file= paste(WD,"Similarity_Figures.pptx",sep = ""),
          width = 3.5, 
          height = 4.5,
          paper = "letter",
          orient = "portrait",
          scaling = 100,
          append = T,
          center = T,
          margins = c(top = 0, right = 0, bottom = 0, left = 0)
)




datM = datModel %>% group_by(SID,ageChange,Condition,TimePoint) %>%
  reframe(
    Similarity = c(min(Similarity, na.rm =T),median(Similarity, na.rm =T),max(Similarity, na.rm =T)),
    Memory    =  c(min(Memory, na.rm =T),median(Memory, na.rm =T),max(Memory, na.rm =T)),
  ) %>%
  as.data.frame()
datM = datM[datM$ageChange>0,]
datM$ageChange = case_when(datM$ageChange>1 ~ 1.5,
                           datM$ageChange<1  ~ 0.5)

p =  interact_plot(M, pred = Similarity, modx = ageChange, modx.values = c(.5,1.5),
                   mod2 = Condition,mod2.values = "Close",
                   plot.points = F, point.alpha = 0.3,point.size = .5,point.shape = F,
                   line.thickness = 1,vary.lty=F, interval = T,int.type = "confidence",
                   linearity.check =F, rug =F, partial.residuals = F,centered = "all",
                   y.label = "Item-Location Memory Accuracy",
                   x.label = "Proportional Similarity",
                   colors =c("#A6BC09","#005148","#CCEA8D","#3A2C59FF") )
p = p+
  geom_point(
    data = datM[datM$Condition=="Close",],
    aes(x = Similarity, y = Memory, color = ageChange),
    alpha = 0.3,
    size = .25
  )+
  labs(
    x = "Differential Similarity",
    y = "Memory",
    color = "ageChange",
    fill = "ageChange"
  ) +
  scale_x_continuous(
    limits = c(-.07, .1),
    breaks = seq(-.07, .1,length.out = 5),
    labels = round(seq(0, 1,length.out = 5),1)
  ) +
  scale_y_continuous(
    limits = c(30,65),
    breaks = c(30,33,seq(40, 60, by = 10))
  ) +
  geom_hline(
    yintercept = 33,
    linetype = "dashed",
    color = "gray50",
    linewidth = 0.6
  )+
  # scale_color_manual(values = c("#7ED7AFFF","#3573A1FF","#3A2C59FF") ) +
  # scale_fill_manual(values = c("#7ED7AFFF","#3573A1FF","#3A2C59FF") ) +
  theme_bw()+gDefault
plot(p)


graph2ppt(file= paste(WD,"Similarity_Figures.pptx",sep = ""),
          width = 4, 
          height = 4,
          paper = "letter",
          orient = "portrait",
          scaling = 100,
          append = T,
          center = T,
          margins = c(top = 0, right = 0, bottom = 0, left = 0),
          )

p =  interact_plot(M, pred = Similarity, modx = ageChange, modx.values = c(.5,1.5),
                   mod2 = Condition,mod2.values = "Far",
                   plot.points = F, point.alpha = 0.3,point.size = .5,point.shape = F,
                   line.thickness = 1,vary.lty=F, interval = T,int.type = "confidence",
                   linearity.check =F, rug =F, partial.residuals = F,centered = "all",
                   y.label = "Item-Location Memory Accuracy",
                   x.label = "Proportional Similarity",
                   colors =c("#A6BC09","#005148","#3A2C59FF") )
p = p+
  geom_point(
    data = datM[datM$Condition=="Far",],
    aes(x = Similarity, y = Memory, color = ageChange),
    alpha = 0.3,
    size = .25
  )+
  labs(
    x = "Differential Similarity",
    y = "Memory",
    color = "ageChange",
    fill = "ageChange"
  ) +
  scale_x_continuous(
    limits = c(-.07, .1),
    breaks = seq(-.07, .1,length.out = 5),
    labels = round(seq(0, 1,length.out = 5),1)
  ) +
  scale_y_continuous(
    limits = c(30,65),
    breaks = c(30,33,seq(40, 60, by = 10))
  ) +
  geom_hline(
    yintercept = 33,
    linetype = "dashed",
    color = "gray50",
    linewidth = 0.6
  )+
  # scale_color_manual(values = c("#7ED7AFFF","#3573A1FF","#3A2C59FF") ) +
  # scale_fill_manual(values = c("#7ED7AFFF","#3573A1FF","#3A2C59FF") ) +
  theme_bw()+gDefault
plot(p)


graph2ppt(file= paste(WD,"Similarity_Figures.pptx",sep = ""),
          width = 4, 
          height = 4,
          paper = "letter",
          orient = "portrait",
          scaling = 100,
          append = T,
          center = T,
          margins = c(top = 0, right = 0, bottom = 0, left = 0),
)
#------ ageBase*Similarity effect Separate Conditions ----
emm <- emmeans(M, ~ Condition * Similarity * ageChange , pbkrtest.limit = 4968)
summary(emm)


mClose = lmer(Memory ~ ageBase+ageChange+Similarity+
                ageBase:Similarity+
                ageChange:Similarity+
                # I(Age^2)+
                (1|SID)+(1|Order),
              data=datModel[datModel$Condition=="Close",], na.action=na.exclude)
tab_model(mClose,show.se = T,show.stat = T,show.est = T)

sort(unique(datModel$ageChange))
sort(unique(datModel$ageBase)); hist(datModel$ageBase)


mFar = lmer(Memory ~ ageBase+ageChange+Similarity+
              # ageBase:Similarity+
              ageChange:Similarity+
              # I(Age^2)+
              (1|SID)+(1+Similarity|Order),
            data=datModel[datModel$Condition=="Far",], na.action=na.exclude)
tab_model(mFar,show.se = T,show.stat = T,show.est = T)
sort(unique(datModel$ageChange))
sort(unique(datModel$ageBase)); hist(datModel$ageBase)

#-------------- CLR Model test ----
datModel = sDat
datModel$Condition = datModel$testName
datModel$Similarity = datModel$diff

# datModel$Similarity = datModel$Similarity + abs(min(datModel$Similarity,na.rm=T))
# datModel$Similarity = as.numeric(scale(datModel$Similarity,center = F))
# datModel$Similarity = datModel$Similarity +abs(min(datModel$Similarity,na.rm=T))
# datModel$Similarity = datModel$Similarity/max(datModel$Similarity,na.rm=T)


datModel$Memory  = datModel$CLR*100
# datModel$Memory  = datModel$CLR_50*100
datModel$Order = datModel$timeStamp 
datModel$ageBase = datModel$ageBase - min(datModel$ageBase)
datModel$Age = datModel$Age - min(datModel$Age)
# datModel$ageChange = scale(datModel$ageChange)
datModel$TimePoint = ifelse(datModel$ageChange==0,1,2)

datModel = RemoveOutliers(datModel,factorNames = c("sessID","Condition"),varNames = "Similarity")
datModel = datModel%>% group_by(SID,Condition,TimePoint) %>% fill(Similarity, .direction = "downup")
datModel = datModel[!is.na(datModel$Similarity),]



datModel %>% group_by(SID,Condition,TimePoint) %>%
  summarise(Similarity = mean(Similarity, na.rm =T),CLR = mean(CLR, na.rm =T),CLR_50 = mean(CLR_50, na.rm =T)) %>%
  group_by(Condition,TimePoint) %>%
  summarise(Similarity = mean(Similarity, na.rm =T),CLR = mean(CLR, na.rm =T),CLR_50 = mean(CLR_50, na.rm =T)) %>% as.data.frame()



datModel = datModel[,c("SID", "Memory", "ageBase", "ageChange", "Condition", 'Similarity',"Order")]
datModelEMS = datModel

DV = "Memory ~"
rEff = "+(1+Condition|SID)+(1|Order)"
models = c("1",
           "ageBase", 
           "ageBase+ageChange",
           "ageBase+ageChange+Condition",
           "ageBase+ageChange+Condition+Similarity",
           "ageBase+ageChange+Condition+Similarity+ageChange:Similarity", 
           "ageBase+ageChange+Condition+Similarity+ageChange*Similarity+ageBase*Similarity", 
           "ageBase+ageChange+Condition+Similarity+ageChange*Similarity+ageBase*Similarity+ageChange:Condition:Similarity",
           "ageBase+ageChange+Condition+Similarity+ageChange*Similarity+ageBase*Similarity+Condition*Similarity+ageChange:Condition:Similarity")
i=1
m=list()
for (idx in seq(1,length(models))){
  m[[idx]] = lmer(paste(DV,models[idx],rEff,sep = ""), 
                  data=datModel, na.action=na.exclude)
}
mComp <- do.call(anova,m)
row.names(mComp) = models
mComp$Best = seq(1,nrow(mComp))*as.numeric(mComp$`Pr(>Chisq)`<0.05)
print(mComp)
tab_model(m[[max(mComp$Best,na.rm=T)]],show.se = T,show.stat = T,show.est = T)

tab_model(m[5],show.se = T,show.stat = T,show.est = T)

mNULL = lmer(Similarity ~ 1+(1|SID), data=datModel, na.action=na.exclude)
M = m[[max(mComp$Best,na.rm=T)]]
tab_model(M,show.se = T,show.stat = T,show.est = T)

# summary(M)
# vif(M)
# hist(resid(M))
# anova(mNULL,M)


p =  interact_plot(M, pred = Similarity, modx = ageBase, 
                   # modx.values = c(8,10,12),
                   plot.points = F, point.alpha = 0.2,point.size = .75,point.shape = F,
                   line.thickness = 1.2,vary.lty=F, interval = T,int.type = "confidence",
                   linearity.check =F, rug =F,partial.residuals = F,centered = "all",
                   y.label = "Item-Location Memory Accuracy",
                   x.label = "Differential EMS",
                   colors =c("#7ED7AFFF","#3573A1FF","#3A2C59FF") )
p = p+theme_bw()+gDefault
plot(p)

p = interact_plot(M, pred = Similarity, modx = Condition, 
                  mod2 = ageChange, mod2.values = c(.5,1.5),
                  plot.points = F, point.alpha = 0.5,point.size = .5,point.shape = T,
                  line.thickness = 1,vary.lty=F, interval = T,int.type = "confidence",
                  linearity.check =F, rug =F,partial.residuals = T,centered = "all",
                  y.label = "Item-Location Memory Accuracy",
                  x.label = "Differential EMS")
p = p+theme_bw()+gDefault
plot(p)

p = interact_plot(M, pred = Similarity, mod2 = Condition, mod2.values = "Close", 
                  modx = ageChange, modx.values = c(.5,1.5),
                  plot.points = F, point.alpha = 0.3,point.size = .5,point.shape = T,
                  line.thickness = 2,vary.lty=F, interval = T,int.type = "confidence",
                  linearity.check =F, rug =F,partial.residuals = T,centered = "all",
                  y.label = "Item-Location Memory Accuracy",
                  x.label = "Differential EMS",
                  colors =c("#7ED7AFFF","#3A2C59FF"))
p = p+theme_bw()+gDefault
plot(p)

p = interact_plot(M, pred = Similarity, mod2 = Condition,  
                  modx = ageChange, modx.values = c(.5,1.5),
                  plot.points = F, point.alpha = 0.3,point.size = .5,point.shape = T,
                  line.thickness = 2,vary.lty=F, interval = T,int.type = "confidence",
                  linearity.check =F, rug =F,partial.residuals = T,centered = "all",
                  y.label = "Item-Location Memory Accuracy",
                  x.label = "Differential EMS",
                  colors =c("#F6AE86FF","#661F54FF"))
p = p+theme_bw()+gDefault+theme(
  legend.position = "inside", legend.position.inside = c(0.5, 0.9),
  legend.direction = "horizontal", legend.box.background = element_rect(color="red") )
plot(p)

graph2ppt(file= paste(WD,"EMS_Fig6CD.pptx",sep = ""), width = 12, height = 7)



















########################################## Invetigate Perfomance changes ------------------

datM = unique(datModel[,c("SID","Condition","TimePoint","Memory")])
ggplot(datM, aes(x=TimePoint, y=CLR, fill=TimePoint)) +
  geom_violin() +
  # scale_fill_viridis(discrete = TRUE, alpha=0.6) +
  geom_point(color="black", size=1, alpha=0.9)+
  geom_line(aes(group=SID))+
  facet_wrap(~Condition)

datM = datAge[,c("SID","testName","CLR","CLR_50","dPrime","ageChange","ageBase")]
datM$TimePoint = ifelse(datM$ageChange==0,"T1","T2")
ggplot(datM, aes(x=TimePoint, y=CLR, fill=TimePoint)) +
  geom_violin() +
  # scale_fill_viridis(discrete = TRUE, alpha=0.6) +
  geom_point(color="black", size=1, alpha=0.9)+
  geom_line(aes(group=SID))+
  facet_wrap(~testName)

datM$DV = datM$dPrime
datM$CLR_50-datM$CLR
ggplot(datM, aes(x=TimePoint, y=DV, fill=TimePoint)) +
  geom_violin() +
  # scale_fill_viridis(discrete = TRUE, alpha=0.6) +
  geom_point(color="black", size=1, alpha=0.9)+
  geom_line(aes(group=SID))+
  facet_wrap(~testName)

# -----------------------------
# user setting: number of groups
# -----------------------------
n_groups <- 3   # change to 3, 4, 5, etc.

# -----------------------------
# helper function:
# groups a numeric variable into quantile-based bins
# -----------------------------
make_group <- function(x, n_groups, prefix = "G") {
  if (all(is.na(x))) return(rep(NA_character_, length(x)))
  
  # ntile handles ties reasonably and makes near-equal sized groups
  out <- dplyr::ntile(x, n_groups)
  out <- ifelse(is.na(x), NA, paste0(prefix, out))
  return(out)
}

# -----------------------------
# 1) ageBase grouping
# assumes ageBase is already in datM
# -----------------------------
age_group_df <- datM %>%
  filter(TimePoint == "T1") %>%
  group_by(SID) %>%
  summarise(ageBase = first(ageBase), .groups = "drop") %>%
  mutate(ageBase_group = make_group(ageBase, n_groups, prefix = "AgeG"))

ageChange_group_df <- datM %>%
  filter(TimePoint == "T2") %>%
  group_by(SID) %>%
  summarise(ageChange = first(ageChange), .groups = "drop") %>%
  mutate(ageChange_group = make_group(ageChange, n_groups, prefix = "AgeChG"))

# -----------------------------
# 2) dPrime at T1 grouping
# assumes dPrime exists for TimePoint == "T1"
# -----------------------------
dprime_t1C_df <- datM %>%
  filter(TimePoint == "T1", testName=="Close") %>%
  group_by(SID) %>%
  summarise(dPrime_T1 = first(dPrime), .groups = "drop") %>%
  mutate(dPrime_T1C_group = make_group(dPrime_T1, n_groups, prefix = "dPrimeT1_G"))

dprime_t1F_df <- datM %>%
  filter(TimePoint == "T1", testName=="Far") %>%
  group_by(SID) %>%
  summarise(dPrime_T1 = first(dPrime), .groups = "drop") %>%
  mutate(dPrime_T1F_group = make_group(dPrime_T1, n_groups, prefix = "dPrimeT1_G"))

# -----------------------------
# 3 & 4) CLR difference between T1 and T2
# separately for Close and Far
#
# assumes testName identifies Close/Far
# and TimePoint identifies T1/T2
# -----------------------------
clr_diff_df <- datM %>%
  filter(TimePoint %in% c("T1", "T2"),
         testName %in% c("Close", "Far")) %>%
  select(SID, testName, TimePoint, CLR) %>%
  distinct() %>%
  pivot_wider(
    names_from = TimePoint,
    values_from = CLR
  ) %>%
  mutate(CLR_diff = T2 - T1) %>%
  select(SID, testName, CLR_diff) %>%
  pivot_wider(
    names_from = testName,
    values_from = CLR_diff,
    names_prefix = "CLRdiff_"
  ) %>%
  mutate(
    CLRdiff_Close_group = make_group(CLRdiff_Close, n_groups, prefix = "CLRclose_G"),
    CLRdiff_Far_group   = make_group(CLRdiff_Far,   n_groups, prefix = "CLRfar_G")
  ) %>%
  select(SID, CLRdiff_Close_group, CLRdiff_Far_group)

# -----------------------------
# merge the 4 grouping variables back into datM
# -----------------------------
datM2 <- datM %>%
  left_join(age_group_df %>% select(SID, ageBase_group), by = "SID") %>%
  left_join(ageChange_group_df %>% select(SID, ageChange_group), by = "SID") %>%
  left_join(dprime_t1C_df %>% select(SID, dPrime_T1C_group), by = "SID") %>%
  left_join(dprime_t1F_df %>% select(SID, dPrime_T1F_group), by = "SID") %>%
  left_join(clr_diff_df, by = "SID")

# check result
datM2 %>%
  select(SID, ageBase_group, dPrime_T1_group, CLRdiff_Close_group, CLRdiff_Far_group) %>%
  distinct() %>%
  head()


#--------------------- plots ----
datM2$DV = datM$dPrime
head(datM2)
ggplot(datM2, aes(x=TimePoint, y=DV, fill = TimePoint)) +
  # geom_violin() +
  geom_point(aes(color = TimePoint), size=2, alpha=0.9)+
  geom_line(aes(group=SID))+
  facet_grid(ageChange_group  ~testName)

ggplot(datM2, aes(x=TimePoint, y=DV, fill = TimePoint,color= ageBase_group)) +
  # geom_violin() +
  geom_point( size=2, alpha=0.9)+
  geom_line(aes(group=SID))+
  facet_grid(~testName)
