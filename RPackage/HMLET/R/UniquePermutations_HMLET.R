#' Generate unique random permutations.
#'
#' @param listInput Arbitrary list.
#' @param n An integer number indicating the number of permutations.
#'
#' @export

UniquePermutations_HMLET <- function(listInput, n = 1){

  Labels = levels(factor(listInput))
  L = length(listInput)
  # Convert original label list to an indexed list
  permLabels = rray::rray(as.numeric(factor(listInput)), dim = c(L,1))
  condNum = length(Labels)

  ################################################### Compute subject-level unique labels
  #--------- Original labels
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
        warning("Unique permutation hasn't been generated!(invalid results)")
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
  # C = as.data.frame(mutate(group_by(C,V1), nRep = n()))
  # C[C$nRep>1,]
  # ------------------------------------------>End
  return(matrix(Labels[permLabels], nrow = L))
}
