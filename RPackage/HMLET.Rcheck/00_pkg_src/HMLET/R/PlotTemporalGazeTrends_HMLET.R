#' Plot temporal gaze trends.
#'
#' @param resultList dataframe of the data that is already prepared by PrepareMLETData_HMLET
#'                   or a list that is the result of PermutationTest_HMLET.
#' @param showDataPointNumbers optional boolean to display data points on the axes, defaults to True.
#' @param gazePropRibbonAlpha optional float to change opacity of gaze ribbon, defaults to 0.1.
#' @param clusterFillColor optional string to change fill color of clustered data points, defaults to "#CC9933".
#' @param clusterFillAlpha optional float to change opacity of clustered data points, defaults to 0.5.
#' @param pointSize optional integer to change size of points on plot, defaults to 1.
#' @param pointAlpha optional float to change opacity of points on plot, defaults to 0.7.
#' @param pointFatten optional integer to change size of lines on plot, defaults to 3.
#' @param testNameOrder specify order of test name labels, defaults to NULL.
#' @param conditionOrder specify order of condition levels, defaults to NULL.
#' @param onlySignificantClusters optional boolean to plot only the significant clusters, defaults to True.
#' @param clusterData data containing all clusters generated from ClusterStats_HMLET to display on plot, defaults to NULL.
#' @param yLabel = "Gaze Proportion" specify label for the y axis.
#' @param xLabel = "Time (ms)" specify label for the x axis.
#' @param dataAxisLabel = "Data Points (%)" specify label for the second y axis on the right.
#'
#' @return a plot handle that visualizes the data from PrepareMLETData_HMLET or the list from PermuationTest_HMLET.
#' @export
PlotTemporalGazeTrends_HMLET <- function(resultList, showDataPointNumbers = T,
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
										dataAxisLabel = "Data Points (%)",
										xLabel = "Time (ms)"){
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
                                              testName, ID, timepoint, condition),
                                     Prop = mean(AOI, na.rm=T)))

  ## Added to fix datapoint number issue that counted NaNs
  ### Find complete cases in the data
  linedata = linedata[complete.cases(linedata),]
  keepIdx = unique(paste(linedata$ID,linedata$timepoint,sep = "_"))
  dataIdx = paste(graphDat$ID,graphDat$timepoint, sep = "_")
  graphDat = graphDat[dataIdx %in% keepIdx,]

  linedata = as.data.frame(dplyr::summarise(dplyr::group_by(linedata,
                                              testName, timepoint, condition),
                                     M = mean(Prop, na.rm=T),SD = sd(Prop, na.rm = T),N = n()))
  ## Added to fix the issue about SD for timepoints with 1 participant
  linedata$SD[is.na(linedata$SD)]=unique(0)

  linedata$SE = linedata$SD/sqrt(linedata$N)
  linedata$DataPercent = linedata$N/max(linedata$N)
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
    # clusterData = clusterData[,c("testName","timeStart","timeEnd","pValue")]

    if(!is.null(testNameOrder)){
      clusterData$testName = factor(clusterData$testName,levels = testNameOrder)
    }

    linedataR = reshape2::dcast(linedata, timepoint+ testName ~ condition, value.var = c("M"))
    names(linedataR) = c("timepoint","testName","C1","C2")
    linedataR$yMin = pmin(linedataR$C1,linedataR$C2)
    linedataR$yMax = pmax(linedataR$C1,linedataR$C2)

    for(i in 1:nrow(clusterData)){
      Clusters = (linedataR$timepoint>=clusterData$timeStart[i] & linedataR$timepoint<=clusterData$timeEnd[i]) &
        (linedataR$testName == clusterData$testName[i])
      Clusters = linedataR[Clusters,]
      # if(nrow(Clusters)>1){
      #   #-----------------------------------------------------------------------Update Graph Handle
      #   P = P +
      #     geom_ribbon(data = Clusters,
      #                 aes(x=timepoint, ymin = yMin, ymax = yMax),fill =  clusterFillColor, alpha = clusterFillAlpha)
      # }else{
      #   Clusters = rbind(Clusters,Clusters)
      #   Clusters$y[1] = Clusters$yMin[1]
      #   Clusters$y[2] = Clusters$yMax[2]
      #   Clusters = length(unique(linedata$timepoint))
      #   P = P +
      #     geom_line(data = Clusters,
      #                 aes(x=timepoint, y = y), size = 1, color =  clusterFillColor, alpha = clusterFillAlpha)
      # }

      if(nrow(Clusters)==1){
        wRibbon = mean(diff(unique(linedata$timepoint)))/2
        Clusters = rbind(Clusters,Clusters)
        Clusters$timepoint[2] = Clusters$timepoint[1]+wRibbon
      }
      #-----------------------------------------------------------------------Update Graph Handle
        P = P +
          geom_ribbon(data = Clusters,
                      aes(x=timepoint, ymin = yMin, ymax = yMax),fill =  clusterFillColor, alpha = clusterFillAlpha)

    }

  }
  #-----------------------------------------------------------------------Update Graph Handle
  P = P +
    geom_line(data = linedata,
              aes(x=timepoint, y=M, group=condition, color = condition),
              linewidth=1)

  if(!timeBinFlag){
    #-----------------------------------------------------------------------Update Graph Handle
    P = P +
      geom_ribbon(data = linedata,
                  aes(x=timepoint, ymin = M-SE, ymax = M+SE, fill = condition ),
                  alpha = gazePropRibbonAlpha)


  }else{
    #-----------------------------------------------------------------------Update Graph Handle
    P = P +
      geom_pointrange(data = linedata,
                      aes(x=timepoint, y=M, ymin=M-SE, ymax=M+SE, group=condition, color = condition),
                      alpha = pointAlpha, size = pointSize, fatten = pointFatten) +
      scale_x_continuous(breaks = 1:length(timeBinLabels) , labels=timeBinLabels)

  }
  #-----------------------------------------------------------------------Update Graph Handle
  P = P +
    facet_wrap(~testName,nrow = 1)+
    theme(panel.background = element_rect(fill = "transparent",colour = NA))+
    theme(plot.background = element_rect(fill = "transparent",colour = NA))+
    theme( axis.line = element_line(linewidth = 1, linetype = "solid"))+
    ylab(yLabel)+
	xlab(xLabel)

  if(showDataPointNumbers){
    P = P +
      geom_line(data = linedata,
                aes(x=timepoint, y=DataPercent, group=condition, color=condition),
                linetype="dashed", linewidth=1, alpha = 0.7, position = position_dodge(width = .7))+
      scale_y_continuous(sec.axis = sec_axis(~.*(1/secondAxisScale)*100, name=dataAxisLabel))
  }


  return(P)
}
