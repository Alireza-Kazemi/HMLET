#' Plot temporal trends for time series.
#'
#' @param resultList dataframe of the data that is already prepared by PrepareMLETData_HMLET
#'                   or a list that is the result of PermutationTest_HMLET.
#' @param showDataPointProp optional boolean to display data points on the axes, defaults to True.
#' @param showOverallMean optional string to specify the overall mean. "Line" = to display a horizontal line on the y = average. "Point" to display overall average and range in a pointRange format. "none" to not display it, defaults to "none".
#' @param gazePropRibbonAlpha optional float to change opacity of gaze ribbon, defaults to 0.1.
#' @param clusterFillColor optional string to change fill color of clustered data points, defaults to "#CC9933".
#' @param clusterFillAlpha optional float to change opacity of clustered data points, defaults to 0.5.
#' @param pointSize optional integer to change size of points on plot, defaults to 1.
#' @param pointAlpha optional float to change opacity of points on plot, defaults to 0.7.
#' @param pointFatten optional integer to change size of lines on plot, defaults to 3.
#' @param testNameOrder specify order of test name labels, defaults to NULL.
#' @param conditionOrder specify order of condition levels, defaults to NULL.
#' @param onlySignificantClusters optional Boolean to plot only the significant clusters, defaults to True.
#' @param clusterData data containing all clusters generated from ClusterStats_HMLET to display on plot, defaults to NULL.
#' @param yLabel Proportion" specify label for the y axis, defaults to "Gaze"
#' @param xLabel Specify label for the x axis, defaults to "Time \(ms\)"
#' @param dataAxisLabel Specify label for the second y axis on the right, defaults to "Data Points \(\%\)".
#' @param lineWidthOverallMean Specify line width for overall mean, defaults to 1.
#' @param lineWidthGazeProp Specify line width for gaze proportion, defaults to 1.
#' @param lineWidthDataPointProp Specify line width for data point numbers, defaults to 1.
#' @param lineTypeOverallMean Specify line type for overall mean, defaults to "dotted".
#' @param alphaOverallMean Specify alpha for overall mean, defaults to 0.5.
#' @param shapeCodeOverallMean Specify the shape of the overall mean points when showOverallMean = "Point", defaults to 1 \(circle\).
#' @param tickSizeOverallMean Optional to specify where to plot the overall means when showOverallMean = "Point", defaults to NULL.
#'
#' @import ggplot2
#'
#' @return a plot handle that visualizes the data from PrepareMLETData_HMLET or the list from PermuationTest_HMLET.
#' @export
PlotTimeSeries_HMLET <- function(resultList, showDataPointProp = T,
                                         showOverallMean = "none",
                                         gazePropRibbonAlpha = .1,
                                         clusterFillColor = "#CC9933",
                                         clusterFillAlpha = .5,
                                         pointSize = 1,
                                         pointAlpha = 0.7,
                                         pointFatten = 3,
                                         testNameOrder = NULL,
                                         conditionOrder = NULL,
                                         onlySignificantClusters = T,
                                         clusterData = NULL,
                                         yLabel = "Gaze Proportion",
                                         dataAxisLabel = "Available Observations (%)",
                                         xLabel = "Time (ms)",
                                         lineWidthGazeProp = 1,
                                         lineWidthDataPointProp = 1,
                                         lineWidthOverallMean = 1,
                                         lineTypeOverallMean = "dotted",
                                         alphaOverallMean = 0.5,
                                         shapeCodeOverallMean = 1,
                                         tickSizeOverallMean = NULL){
  #-------------------Update Graph Handle
  P = ggplot()

  if(!is.data.frame(resultList)){
    # Permutation results are being plotted
    graphDat = resultList[[5]]
    clusterData = resultList[[1]]
    if(onlySignificantClusters){
      clusterData = clusterData[clusterData$pValue<0.05,]
    }
    if(nrow(clusterData)==0){
      clusterData = NULL
      warning("No cluster has been found")
    }

  }else{
    # only temporal trends are being plotted
    graphDat = resultList

  }

  timeBinFlag = FALSE
  if("timeBinName" %in% names(graphDat)){
    timeBinFlag = TRUE
    timeBinLabels = levels(graphDat$timeBinName)
  }

  # graphDat = graphDat[complete.cases(graphDat),] ## Added to fix datapoint number issue that counted NaNs


  linedata = as.data.frame(dplyr::summarise(dplyr::group_by(graphDat,
                                                            testName, ID, timePoint, condition),
                                            Prop = mean(AOI, na.rm=T)))

  ## Added to fix datapoint number issue that counted NaNs
  ### Find complete cases in the data
  linedata = linedata[complete.cases(linedata),]
  keepIdx = unique(paste(linedata$ID,linedata$timePoint,sep = "_"))
  dataIdx = paste(graphDat$ID,graphDat$timePoint, sep = "_")
  graphDat = graphDat[dataIdx %in% keepIdx,]

  linedata = as.data.frame(dplyr::summarise(dplyr::group_by(linedata,
                                                            testName, timePoint, condition),
                                            M = mean(Prop, na.rm=T),SD = sd(Prop, na.rm = T),N = dplyr::n()))

  # First over timePoints within IDs+trials
  OverallMean = as.data.frame(dplyr::summarise(dplyr::group_by(graphDat, testName,
                                                               ID, trial, condition),
                                               M = mean(AOI, na.rm=T),
                                               tickSize = mean(diff(timePoint, na.rm = T), na.rm = T)))
  OverallMean$tickSize[is.nan(OverallMean$tickSize)] = mean(OverallMean$tickSize[!is.nan(OverallMean$tickSize)])
  # Second over trials within IDs
  OverallMean = as.data.frame(dplyr::summarise(dplyr::group_by(OverallMean, testName,
                                                               ID, condition),
                                               M = mean(M, na.rm=T),
                                               tickSize = max(tickSize, na.rm = T) ))
  # Finally over IDs
  OverallMean = as.data.frame(dplyr::summarise(dplyr::group_by(OverallMean, testName,
                                                               condition),
                                               yM = mean(M, na.rm=T),
                                               ySE = sd(M, na.rm=T)/sqrt(dplyr::n()),
                                               tickSize = max(tickSize,na.rm = T) ))
  if(!is.null(tickSizeOverallMean)){
    OverallMean$tickSize = tickSizeOverallMean
  }else{
    OverallMean$tickSize = mean(OverallMean$tickSize) # Make sure the tickSize is unique
  }

  ## Added to fix the issue about SD for timePoints with 1 participant
  linedata$SD[is.na(linedata$SD)]=unique(0)


  linedata = linedata %>% dplyr::group_by(testName) %>%
              dplyr::mutate(DataPercent = N/max(N)) %>% as.data.frame()

  linedata$SE = linedata$SD/sqrt(linedata$N)
  secondAxisScale = max(linedata$M+linedata$SE)
  linedata$DataPercent = linedata$DataPercent* secondAxisScale

  if(!is.null(testNameOrder)){
    linedata$testName = factor(linedata$testName, levels = testNameOrder)
  }
  if(!is.null(conditionOrder)){
    linedata$condition = factor(linedata$condition, levels = conditionOrder)
  }else{
    linedata$condition = factor(linedata$condition)
  }


  if(!is.null(clusterData)){
    if(!is.data.frame(clusterData)){
      clusterData = clusterData[[1]]
    }

    if(!is.null(testNameOrder)){
      clusterData$testName = factor(clusterData$testName,levels = testNameOrder)
    }

    linedataR = reshape2::dcast(linedata, timePoint+ testName ~ condition, value.var = c("M"))
    names(linedataR) = c("timePoint","testName","C1","C2")
    linedataR$yMin = pmin(linedataR$C1,linedataR$C2)
    linedataR$yMax = pmax(linedataR$C1,linedataR$C2)

    for(i in 1:nrow(clusterData)){
      Clusters = (linedataR$timePoint>=clusterData$timeStart[i] & linedataR$timePoint<=clusterData$timeEnd[i]) &
        (linedataR$testName == clusterData$testName[i])
      Clusters = linedataR[Clusters,]

      if(nrow(Clusters)==1){
        wRibbon = mean(diff(unique(linedata$timePoint)))/2
        Clusters = rbind(Clusters,Clusters)
        Clusters$timePoint[2] = Clusters$timePoint[1]+wRibbon
      }
      #-----------------------------------------------------------------------Update Graph Handle
      P = P +
        geom_ribbon(data = Clusters,
                    aes(x=timePoint, ymin = yMin, ymax = yMax),fill =  clusterFillColor, alpha = clusterFillAlpha)

    }

  }

  if(showOverallMean == "Line"){
    #-----------------------------------------------------------------------Update Graph Handle
    P = P +
      geom_hline(data = OverallMean,
                 aes(yintercept=yM, color = condition, linetype = "overallMeanLines"),
                 linewidth = lineWidthOverallMean,
                 alpha = alphaOverallMean)
  }else if(showOverallMean == "Point"){
    #-----------------------------------------------------------------------Update Graph Handle
    P = P +
      geom_pointrange(data = OverallMean,
                      aes(x=tickSize+max(linedata$timePoint),
                          y=yM, ymin=yM-ySE, ymax=yM+ySE, color = condition,
                          shape = "overallMeanShape"),
                      size = pointSize, fatten = pointFatten,
                      position = position_dodge(width = max(OverallMean$tickSize)/2))+
      scale_shape_manual(name = NULL,
                         limits = c("overallMeanShape"),
                         values = c(shapeCodeOverallMean),
                         labels = c("Overall Means"))+
      geom_rect(data = OverallMean,
                aes(xmin = tickSize/2+max(linedata$timePoint),
                    xmax = tickSize*3/2+max(linedata$timePoint),
                    ymin = -Inf, ymax = Inf),
                fill = "gray", alpha = 0.2)
  }

  if(showDataPointProp){
    #-----------------------------------------------------------------------Update Graph Handle
    P = P +
      geom_line(data = linedata,
                aes(x=timePoint, y=DataPercent, group=condition, color=condition,linetype="DataPointProp"),
                linewidth=lineWidthDataPointProp, alpha = 0.7, position = position_dodge(width = .7))+
      scale_y_continuous(sec.axis = sec_axis(~.*(1/secondAxisScale)*100, name=dataAxisLabel))
  }

  if((showOverallMean == "Line") & showDataPointProp){
    #-----------------------------------------------------------------------Update Graph Handle
    P = P +
      scale_linetype_manual(name = NULL,
                            limits = c("overallMeanLines","DataPointProp"),
                            values = c(lineTypeOverallMean,"dotdash"),
                            labels = c("Overall Means","Observations"))

  }else if(showOverallMean == "Line"){
    #-----------------------------------------------------------------------Update Graph Handle
    P = P +
      scale_linetype_manual(name = NULL,
                            limits = c("overallMeanLines"),
                            values = c(lineTypeOverallMean),
                            labels = c("Overall Means"))

  }else if(showDataPointProp){
    #-----------------------------------------------------------------------Update Graph Handle
    P = P +
      scale_linetype_manual(name = NULL,
                            limits = c("DataPointProp"),
                            values = c("dotdash"),
                            labels = c("Observations"))
  }

  #-----------------------------------------------------------------------Update Graph Handle
  P = P +
    geom_line(data = linedata,
              aes(x=timePoint, y=M, group=condition, color = condition),
              linewidth=lineWidthGazeProp)

  if(!timeBinFlag){
    #-----------------------------------------------------------------------Update Graph Handle
    P = P +
      geom_ribbon(data = linedata,
                  aes(x=timePoint, ymin = M-SE, ymax = M+SE, fill = condition ),
                  alpha = gazePropRibbonAlpha)


  }else{
    #-----------------------------------------------------------------------Update Graph Handle
    P = P +
      geom_pointrange(data = linedata,
                      aes(x=timePoint, y=M, ymin=M-SE, ymax=M+SE, group=condition, color = condition),
                      alpha = pointAlpha, size = pointSize, fatten = pointFatten) +
      scale_x_continuous(breaks = 1:length(timeBinLabels) , labels=timeBinLabels)
  }

  #-----------------------------------------------------------------------Update Graph Handle
  P = P +
    facet_wrap(~testName,ncol = 1)+
    theme(panel.background = element_rect(fill = "transparent",colour = NA))+
    theme(plot.background = element_rect(fill = "transparent",colour = NA))+
    theme(axis.line = element_line(linewidth = 1, linetype = "solid"))+
    theme(legend.position = "bottom")+
    ylab(yLabel)+
    xlab(xLabel)

  return(P)
}
