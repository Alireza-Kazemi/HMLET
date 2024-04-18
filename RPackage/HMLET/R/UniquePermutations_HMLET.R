#' Generate unique random permutations.
#'
#' @param listInput Arbitrary list.
#' @param uniqueLabels All possible Labels.
#' @param n An integer number indicating the number of permutations.
#'
#' @export

UniquePermutations_HMLET <- function(listInput, uniqueLabels, n = 1){


  ############# Note for improvement ##################
  # Later a mathematical function for generating indexed unique permutations
  # can be used to increase the performance


  labels = uniqueLabels
  L = length(listInput)
  #--------- Original labels
  # Convert original label list to an indexed list
  permLabels = rray::rray(as.numeric(factor(listInput)), dim = c(L,1))
  condNum = length(labels)

  if (n>factorial(condNum)^L){
    print(paste("Maximum number of unique ",
                "permutations is ",factorial(condNum)^L," while ",
                "samples is set to ", n,".",sep = ""))
    print(paste("Re-run the permutation test with samples <= ",factorial(condNum)^L,sep = ""))
    print("---------------------------------")
    stop("Error-> impossible number of permutations.
         ----------------------")
  }

  pb = txtProgressBar(min = 0, max = 1 , initial = 0, style = 3)
  for (i in 1:n){
    rep = 0
    repeat{
      newComb = sample(x = 1:factorial(condNum), size =  L, replace = T)
      newComb = rray::rray(newComb, dim = c(L,1))
      if(min(rray::rray_sum(abs(permLabels - newComb),axes = 1))>1){
        break
      }
      if(rep>1000){
        warning("Unique permutation hasn't been generated!(invalid results, re-try with smaller samples)")
        break
      }
      rep = rep+1
      # if(rep>0){print(paste("-----------------> Repeated in Index = ",i+1,sep = ""))}  # ------------------> Testing for repeat
    }
    permLabels = rray::rray_bind (permLabels,newComb, .axis = 2)
    setTxtProgressBar(pb,i/n)
  }
  # print(rep)                                              # ------------------> Debugging
  close(pb)
  permLabels = matrix(permLabels, nrow = L)
  permLabels = permLabels[, 2:ncol(permLabels)]  # remove the original labels

  #------------------------------------------> Debugging for duplicated permutation sample list
  # C = t(permLabels)
  # C = data.frame(apply(C, 1, function(row) {paste(row, collapse = "")}))
  # names(C) = c("V1")
  # C$index = 1:nrow(C)
  # C$UnikIdx =  as.numeric(factor(C$V1))
  # C = as.data.frame(mutate(group_by(C,V1), nRep = dplyr::n()))
  # C[C$nRep>1,]
  # ------------------------------------------>End
  return(matrix(labels[permLabels], nrow = L))
}
