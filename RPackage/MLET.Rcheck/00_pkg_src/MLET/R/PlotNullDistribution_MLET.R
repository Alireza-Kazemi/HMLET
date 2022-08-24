#' Plot Null Distribution
#'
#'
#'
#' @export
PlotNullDistribution_MLET <- function(resultList){
  graphDat = resultList[[2]]
  P = ggplot(graphDat, aes(x=NullDist)) +
      geom_histogram(aes(y = ..density..), binwidth = sd(graphDat$NullDist)/10, color="white")+
      geom_density(alpha = 0.5)

  # geom_vline(data = resClose[[1]], aes(xintercept = tStatistic))+
  return(P)
}
