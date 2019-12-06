corrector<-function(p_value, alpha=0.05){
  newalpha<-alpha/length(p_value)
  corrected_p<-data.frame(p_value[as.numeric(p_value)<newalpha])
  colnames(corrected_p)<-"Significant p-values"
  return(corrected_p)
}
