dat = datRet[datRet$ID =="hc007t2",]
dat = dat[dat$TrialNum<10,]

dat$Width = 100
dat$Height = 100

data = dat
GazeX = "GazeX"
GazeY = "GazeY"
AOI_Names = c("Target","Lure")
ContentLabel = "Content"
AOIs_Center_X = c(400,800)
AOIs_Center_Y = c(500,500)
AOIs_Widths = c("Width","Width")
AOIs_Heights = c("Height","Height")
EllipticAOI = FALSE
  


d = data
if(!(length(AOIs_Center_X)==length(AOIs_Center_Y) &
     length(AOIs_Center_X)==length(AOI_Names)     &
     length(AOIs_Center_X)==length(AOIs_Radius_X) &
     length(AOIs_Radius_X)==length(AOIs_Radius_Y) )) {
  stop("\nAOI details are missing!
       Length of these variables must be the same:
       AOI names (AOI_Names) 
       AOI center coodinates (AOIs_Center_X, and AOIs_Center_Y)  
       AOI dimensions (AOIs_Radius_X, and AOIs_Radius_Y)")
}


for (AOI_idx in 1:length(AOI_Names)){
  if(is.numeric(AOIs_Center_X)){
    d[,paste("C_X_",AOI_idx,sep = "")] = AOIs_Center_X[AOI_idx]
    d[,paste("C_Y_",AOI_idx,sep = "")] = AOIs_Center_Y[AOI_idx]
  }else{
    d[,paste("C_X_",AOI_idx,sep = "")] = d[,AOIs_Center_X[AOI_idx]]
    d[,paste("C_Y_",AOI_idx,sep = "")] = d[,AOIs_Center_Y[AOI_idx]]
  }
  if(is.numeric(AOIs_Radius_X)){
    d[,paste("R_X_",AOI_idx,sep = "")] = AOIs_Widths[AOI_idx]/2
    d[,paste("R_Y_",AOI_idx,sep = "")] = AOIs_Heights[AOI_idx]/2
  }else{
    d[,paste("R_X_",AOI_idx,sep = "")] = d[,AOIs_Widths[AOI_idx]]/2
    d[,paste("R_Y_",AOI_idx,sep = "")] = d[,AOIs_Heights[AOI_idx]]/2
  }
}


data$AOIs = NA
d$Content = 0
for (AOI_idx in 1:length(AOIs_Radius_X)){
  if(EllipticAOI){
    d[,paste("AOI_",AOI_idx,sep = "")] = sign(1-(
      (d[,GazeX]-d[,paste("C_X_",AOI_idx,sep = "")])^2/(d[,paste("R_X_",AOI_idx,sep = "")])^2 + 
      (d[,GazeY]-d[,paste("C_Y_",AOI_idx,sep = "")])^2/(d[,paste("R_Y_",AOI_idx,sep = "")])^2 ))
    d[,paste("AOI_",AOI_idx,sep = "")] = ifelse(d[,paste("AOI_",AOI_idx,sep = "")]<0,0,1)
  }else{
    # d[,paste("AOI_",AOI_idx,sep = "")] = sign(
    #   round(exp(log(.5)/d[,paste("R_X_",AOI_idx,sep = "")]*
    #         abs(d[,GazeX]-d[,paste("C_X_",AOI_idx,sep = "")]))) +
    #   round(exp(log(.5)/d[,paste("R_Y_",AOI_idx,sep = "")]*
    #         abs(d[,GazeY]-d[,paste("C_Y_",AOI_idx,sep = "")]))) - 1.5 )
    d[,paste("AOI_",AOI_idx,sep = "")] = 
      sign(1-abs(d[,GazeX]-d[,paste("C_X_",AOI_idx,sep = "")])/d[,paste("R_X_",AOI_idx,sep = "")]) + 
      sign(1-abs(d[,GazeY]-d[,paste("C_Y_",AOI_idx,sep = "")])/d[,paste("R_Y_",AOI_idx,sep = "")])-.5
    d[,paste("AOI_",AOI_idx,sep = "")] = ifelse(d[,paste("AOI_",AOI_idx,sep = "")]<0,0,1)
  }
  data$AOIs[d[,paste("AOI_",AOI_idx,sep = "")]==1] = AOI_Names[AOI_idx] 
  d$Content = d$Content + d[,paste("AOI_",AOI_idx,sep = "")]
}
d$Content = sign(d$Content)
data$AOIs[d$Content==0]=ContentLabel


# A = data[,c("DataPointID","Fixation","FixatedOn","AOIs")]
# A = data[data$DataPointID == "hc007_t2_1_6",c("DataPointID","GazeX","GazeY","X_CenterTarget","X_CenterLure","Y_Center","Fixation","FixatedOn","AOIs")]

