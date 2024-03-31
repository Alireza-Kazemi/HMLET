#' Provides unique permutation indices within each subject.
#'
#' @param labels labels from the data frame.
#' @param n optional integer for number of permutations to resample data.
#' @return labels of unique permutation indices to use for subject level permutation test.
#'@export
ComputeSubjectLevelPerm_HMLET <- function(labels, n = 1){
  condNum = length(unique(labels$condition))
  L = length(unique(labels$ID))

  #------------> Check whether n number unique re-sampling is possible
  # In case of M conditions for L number of participants:
  # Within each participant we have M! enumerations and across participants we
  # have M!^L number of unique re-sampling.
  if (n>factorial(condNum)^L){
    print(paste("With ", L, "number of participants and ",
                condNum," number of conditions maximum number of unique",
                "permutations is ",factorial(condNum)^L," while currently ",
                "samples is set to ", n,".\n",
                "Re-run the permutation test with samples <=",factorial(condNum)^L,sep = ""))
    print("---------------------------------")
    stop("Error-> impossible number of permutations.
         ----------------------")
  }

  #--------- Original labels -> all are 1 corresponding to the first combination.
  condLevels = levels(factor(1:factorial(condNum)))
  subjLevelPerms = rep(x = 1, times =  L)
  subjLevelPerms = UniquePermutations_HMLET(subjLevelPerms, uniqueLabels = condLevels, n = samples)
  subjLevelPerms = as.data.frame(subjLevelPerms)
  subjLevelPerms$ID = unique(labels$ID)
  temp = unique(labels[,c("ID","timePoint")])
  subjLevelPerms = merge(temp,subjLevelPerms,by="ID")
## Old Version Buggy
#   subjLevelPerms = rray::rray(subjLevelPerms, dim = c(L,1))
#   L = length(subjLevelPerms)
#   print("Compute subject-level unique combination of conditions:")
#   pb = utils::txtProgressBar(min = 0, max = 1 , initial = 0, style = 3)
#   for (i in 1:n){
#     rep = 0
#     repeat{
#       newComb = sample(x = 1:factorial(condNum), size =  L, replace = T)
#       newComb = rray::rray(newComb, dim = c(L,1))
#       if(min(rray::rray_sum(abs(subjLevelPerms - newComb),axes = 1))>1){
#         break
#       }
#       if(rep>1000){
#         warning("Unique permutation hasn't been generated!(invalid results)")
#         break
#       }
#       rep = rep+1
#       # if(rep>0){print(paste("-----------------> Repeated in Index = ",i+1,sep = ""))}  # ------------------> Testing for repeat
#     }
#     subjLevelPerms = rray::rray_bind (subjLevelPerms,newComb, .axis = 2)
#     utils::setTxtProgressBar(pb,i/n)
#   }
#   # print(rep)                                              # ------------------> Debugging
#   close(pb)
#   subjLevelPerms = matrix(subjLevelPerms, nrow = L)
#   subjLevelPerms = subjLevelPerms[, 2:ncol(subjLevelPerms)]  # remove the original labels

  #------------------------------------------> Debugging for duplicated permutation sample list
  # C = t(subjLevelPerms)
  # C = data.frame(apply(C, 1, function(row) {paste(row, collapse = "")}))
  # names(C) = c("V1")
  # C$index = 1:nrow(C)
  # C$UnikIdx =  as.numeric(factor(C$V1))
  # C = as.data.frame(mutate(group_by(C,V1), nRep = n()))
  # C[C$nRep>1,]
  # ------------------------------------------>End

  ################################################### Compute condition-level unique labels
  condList = levels(factor(labels$condition))
  condLevelPerms = combinat::permn(factorial(condNum))
  print("Compute condition-level labels:")
  pb = utils::txtProgressBar(min = 0, max = 1 , initial = 0, style = 3)
  for (i in 1:n){
    # Old Version Buggy
    # labels <- cbind(labels,
    #                 purrr::map_dfr(as.list(as.numeric(subjLevelPerms[,i])),function(x){
    #                   data.frame(perm = c(condList[unlist(condLevelPerms[x])]))
    #                 }))
    labels <- cbind(labels,
                    purrr::map_dfr(as.list(as.numeric(subjLevelPerms[,paste("V",i,sep = "")])),
                                   function(x){
                                     data.frame(perm = c(condList[unlist(condLevelPerms[x])]))
                                   }))
    utils::setTxtProgressBar(pb,i/n)
  }
  close(pb)
  names(labels) = c(names(labels)[1:3],paste(names(labels)[-(1:3)],1:n,sep = ""))

  #------------------------------------------> Debugging for duplicated permutation sample list
  # C = t(permLabels)
  # C = data.frame(apply(C, 1, function(row) {paste(row, collapse = "")}))
  # names(C) = c("V1")
  # C$index = 1:nrow(C)
  # C$UnikIdx =  as.numeric(factor(C$V1))
  # C = as.data.frame(mutate(group_by(C,V1), nRep = n()))
  # C[C$nRep>1,]
  # ------------------------------------------>End

  return(labels)
}
