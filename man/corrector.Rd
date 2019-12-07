\name{corrector}
\alias{corrector}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Outputs p-values below Bonferroni-corrected alpha
}
\description{
"Corrector" will take the p-value vector generated from the previous function "interactor", and compare all the p-values in this vector with the Bonferroni-corrected alpha value. If the p-value for the interaction term is significant after the comparison, these interaction terms with their p-values will be stored in a dataframe, which will then be returned.
}
\usage{
corrector(ptable, alpha = 0.05)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{ptable}{
A vector of interaction term p-values returned from the "interactor" function
}
  \item{alpha}{
The desired alpha level (significance level). The default is alpha=0.05
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
  \item{significantpvalues }{Returns a vector containing the p-values for the interaction terms for each 2-order predictor combination that are significant under Bonferroni correction}
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
INTERACTIONPVALS<-interactor(data=Boston,predictors=c('indus',"crim","zn","chas","nox","rm","age","dis","rad","tax","ptratio","black","lstat"),
                             outcome='medv')
SIGPVALS<-corrector(INTERACTIONPVALS)

## The function is currently defined as
corrector<-function(p_value, alpha=0.05){
  newalpha<-alpha/length(p_value)
  corrected_p<-data.frame(p_value[as.numeric(p_value)<newalpha])
  colnames(corrected_p)<-"Significant p-values"
  return(corrected_p)
}
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
