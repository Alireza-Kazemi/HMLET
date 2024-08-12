#' AOI Extraction
#'
#' @description
#' AOIs can be computed in rectangular or elliptic forms.
#' This function uses gaze coordinates to categorize them into specified AOIs
#'
#' @details
#' The output is the same dataframe as input with one new column:
#' AOIs: Includes the name of the AOI at each time point.
#'
#' @param data long format dataframe containing temporal data.
#' @param GazeX string for column name that includes X coordinate of gazepoints
#' @param GazeY string for column name that includes Y coordinate of gazepoints
#' @param AOI_Names List of Strings for AOIs names
#' @param AOIs_Center_X List of numerical values for the horizontal coordinate of the center of all AOIs or list of strings indicating the column names in the input data that have these values
#' @param AOIs_Center_Y List of numerical values for the vertical coordinate of the center of all AOIs or list of strings indicating the column names in the input data that have these values
#' @param AOIs_Radius_X List of numerical values for width size/horizontal diameter of all AOIs or list of strings indicating the column names in the input data that have these values
#' @param AOIs_Radius_Y List of numerical values for height size/vertical diameter of all AOIs or list of strings indicating the column names in the input data that have these values.
#' @param ContentLabel Label for gaze points that are outside all of the AOIs, defaults to "Content"
#' @param EllipticAOI Binary value to identify whether elliptic AOIs should be used instead of rectangular ones, defaults to FALSE
#' @export
Preprocess_AOIExtraction_HMLET <- function(data, GazeX, GazeY,
                                           AOI_Names, AOIs_Center_X, AOIs_Center_Y,
                                           AOIs_Widths, AOIs_Heights,
                                           ContentLabel = "Content", EllipticAOI = FALSE){
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

  return(data)
}
