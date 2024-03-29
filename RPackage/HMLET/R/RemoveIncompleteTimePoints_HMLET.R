#' Remove Time points with missing conditions.
#' This function is currently only applicable for within participant manipulations.
#' @param data dataframe containing temporal data.
#' @return data with no missing conditions.
#'
#' @export
RemoveIncompletetimePoints_HMLET <- function(data){
	rmIdx = as.data.frame(dplyr::summarise(dplyr::group_by(data,ID,timePoint,condition,testName), prop = mean(AOI, na.rm=T)))
	rmIdx = reshape2::dcast(rmIdx, ID+timePoint+testName~condition, value.var = "prop")
	rmIdx = rmIdx[!complete.cases(rmIdx),]
	if(nrow(rmIdx)>0){
	  rmIdx = paste(rmIdx$ID,rmIdx$timePoint,rmIdx$testName,sep = "_")
	  dIdx = paste(data$ID,data$timePoint,data$testName,sep = "_")
	  rmDat = data[(dIdx %in% rmIdx),]
	  rmDat = dplyr::group_by(ID,timePoint,testName) %>%
	          dplyr::summarise(N=n()) %>%
	          dplyr::group_by(ID,testName) %>%
	          dplyr::summarise(N=mean(N,na.rm=T)) %>%
	          dplyr::group_by(testName) %>%
	          dplyr::summarise(N=mean(N,na.rm=T)) %>% as.data.frame()
	  data = data[!(dIdx %in% rmIdx),]
	  warning(paste("\n    >> In test data: ",unique(data$testName),
					"\n    >> Time points with missing conditions removed! (N =",
					length(dIdx[dIdx %in% rmIdx]),")", sep = ""))
	}

	#--> change to keep the t-value computation consistent with the threshold value for unbalanced data.
	rmIdx = unique(data[,c("ID","timePoint","testName")])
	rmIdx = as.data.frame(dplyr::summarise(dplyr::group_by(rmIdx,timePoint,testName), N = n()))
	rmIdx = rmIdx[rmIdx$N<=2,] # This is chosen arbitrarily to remove time points with only 1 participant data
	if(nrow(rmIdx)>0){
	  rmIdx = paste(rmIdx$timePoint,rmIdx$testName,sep = "_")
	  dIdx = paste(data$timePoint,data$testName,sep = "_")
	  data = data[!(dIdx %in% rmIdx),]
	  warning(paste("\n    >> In test data: ",unique(data$testName),
	                "\n    >> Time points with only 1 observation are removed! (N =",
	                length(dIdx[dIdx %in% rmIdx]),")", sep = ""))
	}
	#--<|
	return(data)
}
