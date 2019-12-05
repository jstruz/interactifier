interactor<- function(data, predictors, outcome){
  #create each combo
  combos<-data.frame(combn(predictors, 2))
  
  #initialize pasted combos dataframe
  pastedcombos<-data.frame(1,dim(combos)[2])
  
  #paste combos together to input into model
  for (i in 1:dim(combos)[2]){
    pastedcombos[i]<-paste(combos[1,i],combos[2,i], sep='*')
  }
  
  #apply over the list of combos to test them all in seperate linear regressions
  sapply(pastedcombos, function(predictors) {
    formula <- as.formula(paste(outcome, "~", predictors))
    summs <- lm(formula, data = data)
    output1<-row.names(summary(summs)$coefficients[-1,])
    output2<-summary(summs)$coefficients[-1,4]
    output3<-cbind(output1,output2)
    ptable<-t(output3)[,3]
    return(ptable) 
  })
}