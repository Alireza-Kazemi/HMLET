#' Plot temporal gaze trends
#'
#' @param resultList can be a dataframe of the data that is already prepared by PrepareMLETData_MLET or a list that is the result of PermutationTest_MLET
#'
#' @export
PlotTemporalGazeTrends_MLET <- function(resultList, showDataPointNumbers = T,
                                        gazePropRibbonAlpha = .1,
                                        clusterFillColor = "#CC9933",
                                        clusterFillAlpha = .5,
                                        pointSize = 1,
                                        pointAlpha = 0.7,
                                        pointFatten = 3,
                                        testNameOrder = NULL,
                                        conditionOrder = NULL,
                                        onlySignificantClusters = T,
                                        clusterData = NULL){
  #-------------------Update Graph Handle
  P = ggplot()

  if(!is.data.frame(resultList)){
    # Permutation results are being plotted
    graphDat = resultList[[5]]
    clusterData = resultList[[1]]
    if(onlySignificantClusters){
      clusterData = clusterData[clusterData$pValue<0.05,]
    }

  }else{
    # only temporal trends are being plotted
    graphDat = resultList

  }

  linedata = as.data.frame(summarise(group_by(graphDat,
                                              testName, ID, timepoint, condition),
                                     Prop = mean(AOI, na.rm=T)))
  linedata = as.data.frame(summarise(group_by(linedata,
                                              testName, timepoint, condition),
                                     M = mean(Prop, na.rm=T),SD = sd(Prop, na.rm = T),N = n()))
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
      if(nrow(Clusters)>1){
        #-----------------------------------------------------------------------Update Graph Handle
        P = P +
          geom_ribbon(data = Clusters,
                      aes(x=timepoint, ymin = yMin, ymax = yMax),fill =  clusterFillColor, alpha = clusterFillAlpha)
      }else{
        Clusters = rbind(Clusters,Clusters)
        Clusters$y[1] = Clusters$yMin[1]
        Clusters$y[2] = Clusters$yMax[2]
        P = P +
          geom_line(data = Clusters,
                      aes(x=timepoint, y = y), size = 1, color =  clusterFillColor, alpha = clusterFillAlpha)
      }
    }

  }
  #-----------------------------------------------------------------------Update Graph Handle
  P = P +
    geom_line(data = linedata,
              aes(x=timepoint, y=M, group=condition, color = condition),
              size=1)

  if(is.numeric(linedata$timepoint)){
    #-----------------------------------------------------------------------Update Graph Handle
    P = P +
      geom_ribbon(data = linedata,
                  aes(x=timepoint, ymin = M-SE, ymax = M+SE, fill = condition ),
                  alpha = gazePropRibbonAlpha)
  }else{
    #-----------------------------------------------------------------------Update Graph Handle
    P = P +
      # geom_point(data = linedata,
      #            aes(x=timepoint, y=M, group=condition, color = condition))+
      geom_pointrange(data = linedata,
                      aes(x=timepoint, y=M, ymin=M-SE, ymax=M+SE, group=condition, color = condition),
                      alpha = pointAlpha, size = pointSize, fatten = pointFatten)

  }
  #-----------------------------------------------------------------------Update Graph Handle
  P = P +
    facet_wrap(~testName,nrow = 1)+
    theme(panel.background = element_rect(fill = "transparent",colour = NA))+
    theme(plot.background = element_rect(fill = "transparent",colour = NA))+
    theme( axis.line = element_line(size = 1, linetype = "solid"))+
    ylab("Gaze Proportion (%)")

  if(showDataPointNumbers){
    P = P +
      geom_line(data = linedata,
                aes(x=timepoint, y=DataPercent, group=condition, color=condition),
                linetype="dashed", size=1, alpha = 0.7, position = position_dodge(width = .7))+
      scale_y_continuous(sec.axis = sec_axis(~.*(1/secondAxisScale), name="Data Points (%)"))
  }


  return(P)
}
