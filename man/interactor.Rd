\name{interactor}
\alias{interactor}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Finds interaction terms between 2-order predictor combinations
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
interactor(data, predictors, outcome)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{data}{
%%     ~~Describe \code{data} here~~
}
  \item{predictors}{
%%     ~~Describe \code{predictors} here~~
}
  \item{outcome}{
%%     ~~Describe \code{outcome} here~~
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
function (data, predictors, outcome) 
{
    combos <- data.frame(combn(predictors, 2))
    pastedcombos <- data.frame(1, dim(combos)[2])
    for (i in 1:dim(combos)[2]) {
        pastedcombos[i] <- paste(combos[1, i], combos[2, i], 
            sep = "*")
    }
    sapply(pastedcombos, function(predictors) {
        formula <- as.formula(paste(outcome, "~", predictors))
        summs <- lm(formula, data = data)
        output1 <- row.names(summary(summs)$coefficients[-1, 
            ])
        output2 <- summary(summs)$coefficients[-1, 4]
        output3 <- cbind(output1, output2)
        ptable <- t(output3)[, 3]
        return(ptable)
    })
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
