#' Plot Null Distribution.
#'
#' @param resultList list of the dataframes that is already prepared by PrepareMLETData_HMLET
#'                   or a list that is the result of PermutationTest_HMLET.
#' @param smoothingBandWidth  The smoothing bandwidth which determines the granularity of distribution estimation, defaults to sd/3
#' @return null distribution plot handle.
#' @import ggplot2
#'
#' @export
PlotNullDistribution_HMLET <- function(resultList, smoothingBandWidth = NULL){
  graphDat = resultList[[2]]
  P = ggplot(graphDat, aes(x = NullDist)) +
    geom_density(alpha = 0.5,bw =sd(graphDat$NullDist)/3, fill = "gray") +
    geom_histogram(aes(y = after_stat(density)), binwidth = sd(graphDat$NullDist)/3, alpha = .75)+
    facet_wrap(~testName, ncol = 1)+
    labs(title="Estimated Null Distribution",
         x ="Summed Statistics within Clusters",
         y = "Density")+
    guides(colour=guide_legend(title="Cluster Index"))+
    geom_vline(data = resultList[[1]], linetype="dashed",
               aes(xintercept = tStatistic,color = as.factor(index)))+
    theme_bw(base_family = "serif")+
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
          panel.background = element_blank())
  return(P)
}

