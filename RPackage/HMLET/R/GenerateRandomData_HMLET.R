#' Generate unique random permutations
#' To be completed
#'
#'@export
GenerateRandomData_HMLET <- function(tMax = 20, effectOffset = 5, trialNum = 40, subjNum = 20,
                              effectSize = .2){
  effectSize = .2*.25;
  data = NULL

  # t = 1:tMax
  # for (sID in 1:subjNum){
  #   for (trials in 1:trialNum/2){
  #     A = round(rand(tMax,1)/2+.25+round(rand)*effectSize*exp(-(t-effectOffset).^2/4));
  #     plot(t,A,'b')
  #     AOI = cat(1,AOI,A);
  #     condition = cat(1,condition,repmat("C1",[tMax,1]));
  #     trial = cat(1,trial,repmat(trials,[tMax,1]));
  #     ID = cat(1,ID,repmat(sID,[tMax,1]));
  #     timePoint = cat(1,timePoint,t);
  #   }
  #   for (trials in ((trialNum/2+1):trialNum)){
  #     A = round(rand(tMax,1)/2+.25);
  #     plot(t,A,'r')
  #     AOI = cat(1,AOI,A);
  #     condition = cat(1,condition,repmat("C2",[tMax,1]));
  #     trial = cat(1,trial,repmat(trials,[tMax,1]));
  #     ID = cat(1,ID,repmat(sID,[tMax,1]));
  #     timePoint = cat(1,timePoint,t);
  #   }
  # }
  #

  # fix this functionm Later




  return(data)
}
