#' Generate Data
#'
#' @description
#' This function generates data to simulate a desired effect size at a specified time interval.
#' This data is generated only for testing and evaluation of the HMLET package
#' and is not valid for other purposes or research.
#'
#' @details
#' Data has two within participants' conditions: Cond1 and Cond2.
#' Trial level data for the number of participants will be generated with an effect
#' appearing in the specified time interval in which the two conditions are
#' different with the specified effect size
#'
#' @param trialNum Number of trials, defaults to 20.
#' @param subjectNum Number of subjects, defaults to 20.
#' @param timeMax Length of each trial in milliseconds, defaults to 1000.
#' @param samplingInterval Specify the expected sampling interval in milliseconds, defaults to 17.
#' @param effectInterval The expected time interval to simulate the effect, defaults to c(250,350).
#' @param effectSize Size of the effect
#' @param sdCond1 standard deviation for condition 1, defaults to 1.
#' @param sdCond2 standard deviation for condition 2, defaults to 1.
#' @param smoothingPar smoothing parameter to smooth the generated data based on spar parameter in smooth.spline(), defaults to 0.4
#' @export
GenerateRandomData_HMLET <- function(trialNum = 20, subjectNum = 20,
                                     timeMax = 1000, samplingInterval = 17,
                                     effectInterval = c(250,350), effectSize = .2,
                                     sdCond1 = 1,sdCond2 = 1, smoothingPar = .4 ){

  sdPooled = sqrt((sdCond1^2+sdCond2)/2)
  meanDiff = effectSize*sdPooled;
  time = seq(from = samplingInterval, to = timeMax, by = samplingInterval)
  sampleNum = length(time)


  Data = NULL
  for (ID in 1:subjectNum){
    for (trial in 1:trialNum/2){
      A = rnorm(n = sampleNum, mean = 0, sd = sdCond1)
      A =  A+effectSize/2*exp(-(time-mean(effectInterval))^2/diff(effectInterval)^2)
      A = smooth.spline(x = time,y = A, spar = smoothingPar)
      gazeMeasure  = A$y
      timePoint = time
      dataTrial = data.frame(timePoint, gazeMeasure)
      dataTrial$condition = "Cond1"
      dataTrial$trial = trial
      dataTrial$ID = ID
      Data = rbind(Data,dataTrial)
    }
    for (trials in ((trialNum/2+1):trialNum)){
      A = rnorm(n = sampleNum, mean = 0, sd = sdCond1)
      A =  A - effectSize/2*exp(-(time-mean(effectInterval))^2/diff(effectInterval)^2)
      A = smooth.spline(x = time,y = A, spar = smoothingPar)
      gazeMeasure  = A$y
      timePoint = time
      dataTrial = data.frame(timePoint, gazeMeasure)
      dataTrial$condition = "Cond2"
      dataTrial$trial = trial
      dataTrial$ID = ID
      Data = rbind(Data,dataTrial)
    }
  }

  # Data = PermutationTestDataPrep_HMLET(Data, ID = "ID",trial = "trial",
  #                                      timePoint = "timePoint",condition = "condition",
  #                                      gazeMeasure = "gazeMeasure",conditionLevels = c("Cond1","Cond2"))
  # PlotTimeSeries_HMLET(Data)

  return(data)
}
