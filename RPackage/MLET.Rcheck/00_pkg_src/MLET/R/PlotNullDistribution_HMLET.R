#' Plot Null Distribution
#'
#'
#'
#' @export
PlotNullDistribution_HMLET <- function(resultList){
  graphDat = resultList[[2]]
  P = ggplot(graphDat, aes(x = NullDist, fill = testName)) +
      geom_histogram(aes(y = ..density..), binwidth = sd(graphDat$NullDist)/10, color="white")+
      geom_density(alpha = 0.5) +
      facet_wrap(~testName, ncol = 1)

  # geom_vline(data = resClose[[1]], aes(xintercept = tStatistic))+
  return(P)
}
