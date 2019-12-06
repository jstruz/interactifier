\name{interactor}
\alias{interactor}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Finds interaction terms between 2-order predictor combinations
}
\description{
The interactor function is used to find the p-values for interaction terms between every two-order combination of input predictors, with each combination of predictors tested seperately against the specified outcome.
}
\usage{
interactor(data, predictors, outcome)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{data}{
The dataset you are using. This must be in the form of a dataframe.
}
  \item{predictors}{
The predictors for which you want to find interactions between. This must be a character vector that comprises all the names of predictor variables in the dataset. Example: predictors=c('var1','var2',...,'varx')
}
  \item{outcome}{
The outcome variable in the dataset which you which to regress each combination of predictors against. Example: outcome='outcomevariable'
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
%%  ~~who you are~~
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as
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
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
