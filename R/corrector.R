corrector<-function(ptable, alpha=0.05){
  newalpha<-alpha/(dim(ptable)[2])
  whatiwant2<-ptable[,as.numeric(ptable[2,])<newalpha]
  return(whatiwant2)
}