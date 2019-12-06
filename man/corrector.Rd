\name{corrector}
\alias{corrector}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Outputs p-values below Bonferroni-corrected alpha
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
corrector(ptable, alpha = 0.05)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{ptable}{
%%     ~~Describe \code{ptable} here~~
}
  \item{alpha}{
%%     ~~Describe \code{alpha} here~~
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
function (ptable, alpha = 0.05) 
{
    newalpha <- alpha/(dim(ptable)[2])
    whatiwant2 <- ptable[, as.numeric(ptable[2, ]) < newalpha]
    return(whatiwant2)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
