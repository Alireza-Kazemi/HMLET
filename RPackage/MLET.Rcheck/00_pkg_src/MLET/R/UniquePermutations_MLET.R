#' Generate unique random permutations
#'
#' @param listInput Arbitrary list
#' @param n An integer number indicating the number of permutations
#'
#' @export

UniquePermutations_MLET <- function(listInput, n = 1){
  L = length(listInput)
  indexList0 = rray(1:L, dim = c(L, 1))
  indexList = indexList0

  pb = txtProgressBar(min = 0, max = 1 , initial = 0, style = 3)
  # rep = 0                                                   # ------------------> Debugging
  for (i in 1:n){
    rep = 0
    repeat{
      newComb = rray(sample(indexList0, L, replace = F), dim = c(L,1))
      if(min(rray_sum(abs(indexList - newComb),axes = 1))>1 | rep>5000){
        # print(rray_sum(abs(indexList - newComb),axes = 1))  # ------------------> Debugging
        # if(rep>0){print(rep)}  # ------------------> Testing for repeat
        break
      }
      rep = rep+1
    }
    indexList = rray_bind (indexList,newComb, .axis = 2)
    setTxtProgressBar(pb,i/n)
  }
  # print(rep)                                              # ------------------> Debugging
  close(pb)
  return(matrix(listInput[indexList],nrow = L))
}
