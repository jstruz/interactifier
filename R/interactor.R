##' @import Boston

interactor<- function(data, predictors, outcome){
  #create each combo
  combos<-combn(predictors, 2)
  
  #initialize pasted combos dataframe
  pastedcombos<-NULL
  
  #paste combos together to input into model
  for (i in 1:dim(combos)[2]){
    pastedcombos[i]<-paste(combos[1,i],combos[2,i], sep='*')
  }
  
#apply over the list of combos to test them all in seperate linear regressions
              sapply(pastedcombos, function(predictors) {
              formula <- as.formula(paste(outcome, "~", predictors))
              mods<- lm(formula, data = data)
              p_value<-summary(mods)$coefficients[4,4]
              return(p_value) 
            })
}
