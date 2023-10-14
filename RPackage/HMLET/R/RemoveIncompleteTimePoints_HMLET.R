#' Remove Time points with missing conditions.
#' This function is currently only applicable for within participant manipulations.
#' @param data dataframe containing temporal data.
#' @return data with no missing conditions.
#'
#' @export
RemoveIncompleteTimePoints_HMLET <- function(data){
	rmIdx = as.data.frame(dplyr::summarise(dplyr::group_by(data,ID,timepoint,condition,testName), prop = mean(AOI, na.rm=T)))
	rmIdx = reshape2::dcast(rmIdx, ID+timepoint+testName~condition, value.var = "prop")
	rmIdx = rmIdx[!complete.cases(rmIdx),]
	if(nrow(rmIdx)>0){
	  rmIdx = paste(rmIdx$ID,rmIdx$timepoint,rmIdx$testName,sep = "_")
	  dIdx = paste(data$ID,data$timepoint,data$testName,sep = "_")
	  data = data[!(dIdx %in% rmIdx),]
	  warning(paste("\n    >> In test data: ",unique(data$testName),
					"\n    >> Time points with missing conditions removed! (N =",
					length(dIdx[dIdx %in% rmIdx]),")", sep = ""))
	}

	#--> change to keep the t-value computation consistent with the threshold value for unbalanced data.
	rmIdx = unique(data[,c("ID","timepoint","testName")])
	rmIdx = as.data.frame(dplyr::summarise(dplyr::group_by(rmIdx,timepoint,testName), N = n()))
	rmIdx = rmIdx[rmIdx$N<=2,] # This is chosen arbitrarily to remove time points with only 1 participant data
	if(nrow(rmIdx)>0){
	  rmIdx = paste(rmIdx$timepoint,rmIdx$testName,sep = "_")
	  dIdx = paste(data$timepoint,data$testName,sep = "_")
	  data = data[!(dIdx %in% rmIdx),]
	  warning(paste("\n    >> In test data: ",unique(data$testName),
	                "\n    >> Time points with only 1 observation are removed! (N =",
	                length(dIdx[dIdx %in% rmIdx]),")", sep = ""))
	}
	#--<|
	return(data)
}
