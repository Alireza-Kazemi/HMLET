#' Plot Null Distribution.
#'
#' @param resultList list of the dataframes that is already prepared by PrepareMLETData_HMLET
#'                   or a list that is the result of PermutationTest_HMLET.
#' @return null distribution plot handle.
#'
#' @export
PlotNullDistribution_HMLET <- function(resultList){
  graphDat = resultList[[2]] # original: graphDat = resultList[[2]]
  P = ggplot(graphDat, aes(x = NullDist, fill = testName)) +
      geom_histogram(aes(y = after_stat(density)), binwidth = sd(graphDat$NullDist)/10, color="white")+
      geom_density(alpha = 0.5) +
      facet_wrap(~testName, ncol = 1)+
	#  geom_vline(data = resClose[[1]], aes(xintercept = tStatistic))+
	  theme_bw(base_family = "serif")+
	  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
			panel.background = element_blank())
  return(P)
}

