# -------------------> For one Across Subjects permutation test-------
dTest = dat[dat$ID %in% c("ha027","ha032","ha036"),]
dTest = dTest[as.numeric(as.character(dTest$timepoint))<300,]
resp_time = as.data.frame(summarise(group_by(dTest,ID,timepoint,condition), prop = mean(AOI, na.rm=T)))
labels = unique(resp_time[,c("ID","timepoint","condition")])


n = 100
set.seed(5)

condNum = length(unique(labels$condition))
################################################### Compute subject-level unique labels
#--------- Original labels
subjLevelPerms = sample(x = 1, size =  nrow(unique(labels[,c("ID","timepoint")])) , replace = T)
L = length(subjLevelPerms)
print("Compute subject-level unique labels:")
pb = txtProgressBar(min = 0, max = 1 , initial = 0, style = 3)
for (i in 1:n){
  rep = 0
  repeat{
    newComb = sample(x = 1:factorial(condNum), size =  L, replace = T)
    newComb = rray::rray(newComb, dim = c(L,1))
    if(min(rray::rray_sum(abs(subjLevelPerms - newComb),axes = 1))>1){
      break
    }
    if(rep>1000){
      warning("Unique permutation hasn't been generated!(invalid results)")
      break
    }
    rep = rep+1
    if(rep>0){print(paste("-----------------> Repeated in Index = ",i+1,sep = ""))}  # ------------------> Testing for repeat
  }
  subjLevelPerms = rray::rray_bind (subjLevelPerms,newComb, .axis = 2)
  setTxtProgressBar(pb,i/n)
}
# print(rep)                                              # ------------------> Debugging
close(pb)
subjLevelPerms = matrix(subjLevelPerms, nrow = L)
subjLevelPerms = subjLevelPerms[, 2:ncol(subjLevelPerms)]  # remove the original labels

#------------------------------------------> Debugging for duplicated permutation sample list
C = t(subjLevelPerms)
C = data.frame(apply(C, 1, function(row) {paste(row, collapse = "")}))
names(C) = c("V1")
C$index = 1:nrow(C)
C$UnikIdx =  as.numeric(factor(C$V1))
C = as.data.frame(mutate(group_by(C,V1), nRep = n()))
C[C$nRep>1,]
# ------------------------------------------>End

################################################### Compute condition-level unique labels
condList = levels(factor(labels$condition))
condLevelPerms = combinat::permn(factorial(condNum))
print("Compute condition-level unique labels:")
pb = txtProgressBar(min = 0, max = 1 , initial = 0, style = 3)
#------------------------------------------> Debugging for duplicated permutation sample list
A=NULL
# ------------------------------------------>End
for (i in 1:n){
  #------------------------------------------> Debugging for duplicated permutation sample list
  A = rbind(A,paste(purrr::map_dfr(as.list(subjLevelPerms[,i]),function(x){
                    data.frame(perm = c(condList[unlist(condLevelPerms[x])]))
                    })$perm, collapse = ""))
  # ------------------------------------------>End
  labels <- cbind(labels,
                  purrr::map_dfr(as.list(subjLevelPerms[,i]),function(x){
                    data.frame(perm = c(condList[unlist(condLevelPerms[x])]))
                  }))
  setTxtProgressBar(pb,i/n)
}
close(pb)
names(labels) = c(names(labels)[1:3],paste(names(labels)[-(1:3)],1:n,sep = ""))

#------------------------------------------> Debugging for duplicated permutation sample list
A = as.data.frame(A)
names(A) = c("V1")
length(unique(A$V1))
A$index = 1:nrow(A)
A$UnikIdx =  as.numeric(factor(A$V1))
A = as.data.frame(mutate(group_by(A,V1), nRep = n()))
A[A$nRep>1,]
# ------------------------------------------>End

#################################################################################
# -------------------> For one Within Subject permutation test-------
samples = 100
set.seed(5)

dTest = dat[dat$ID %in% c("ha027", "ha032", "ha036"),]
dTest = dTest[dTest$trial<20,]
labels = unique(dTest[,c("ID","trial","condition")])
labelsPerm = NULL
for (sID in unique(labels$ID)){
  labelTemp = labels[labels$ID==sID,]
  
  listInput = labelTemp[,"condition"]
  n = samples
  
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
      newComb = sample(x = 1:condNum, size =  L, replace = T)
      newComb = rray::rray(newComb, dim = c(L,1))
      if(min(rray::rray_sum(abs(permLabels - newComb),axes = 1))>1){
        break
      }
      if(rep>1000){
        warning("Unique permutation hasn't been generated!(invalid results)")
        break
      }
      rep = rep+1
      if(rep>0){print(paste("-----------------> Repeated in Index = ",i+1,sep = ""))}  # ------------------> Testing for repeat
    }
    permLabels = rray::rray_bind (permLabels,newComb, .axis = 2)
    setTxtProgressBar(pb,i/n)
  }
  # print(rep)                                              # ------------------> Debugging
  close(pb)
  permLabels = matrix(permLabels, nrow = L)
  permLabels = permLabels[, 2:ncol(permLabels)]  # remove the original labels
  
  #------------------------------------------> Debugging for duplicated permutation sample list
  C = t(permLabels)
  C = data.frame(apply(C, 1, function(row) {paste(row, collapse = "")}))
  names(C) = c("V1")
  C$index = 1:nrow(C)
  C$UnikIdx =  as.numeric(factor(C$V1))
  C = as.data.frame(mutate(group_by(C,V1), nRep = n()))
  C[C$nRep>1,]
  # ------------------------------------------>End
  L = matrix(Labels[permLabels], nrow = L)
  
  
  L = data.frame(L)
  names(L) = 1:ncol(L)
  labelTemp = cbind(labelTemp,L)
  names(labelTemp) = c(names(labelTemp)[1:3],paste("perm",names(labelTemp)[-(1:3)],sep = ""))
  labelsPerm = rbind(labelsPerm,labelTemp)
}
labels = merge(labels,labelsPerm, by = c("ID","trial","condition"))








