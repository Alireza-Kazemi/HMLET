#' ComputeSubjectLevelPerm_HMLET
#' Provides unique permututation indices within each subject.
#'
#' @param labels labels from the dataframe
#' @param n optional integer for number of permutations for resampling data
#' @return labels of unique permutation indices to use for subject level permutation test
#'@export
ComputeSubjectLevelPerm_HMLET <- function(labels, n = 1){
  condNum = length(unique(labels$condition))
  labelNew = unique(labels[,c("ID","timepoint")])
  listInput = sample(x = 1:factorial(condNum), size =  nrow(labelNew), replace = T)
  subjLevelPerms = UniquePermutations_HMLET(listInput, n-1)
  condList = levels(factor(labels$condition))
  condLevelPerms = permn(factorial(condNum))

  print("Converting labels:")
  pb = txtProgressBar(min = 0, max = 1 , initial = 0, style = 3)
  for (i in 1:n){
    labels <- cbind(labels,
                    map_dfr(as.list(subjLevelPerms[,i]),function(x){
                      data.frame(perm = c(condList[unlist(condLevelPerms[x])]))
                    }))
    setTxtProgressBar(pb,i/n)
  }
  close(pb)
  names(labels) = c(names(labels)[1:3],paste(names(labels)[-(1:3)],1:n,sep = ""))
  return(labels)
}
