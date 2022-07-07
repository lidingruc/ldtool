\name{rec}
\alias{rec}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
recode var from sjmisc
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
rec(x, ..., rec, as.num = TRUE, var.label = NULL, val.labels = NULL, append = TRUE, suffix = "_r", to.factor = !as.num)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
%%     ~~Describe \code{x} here~~
}
  \item{\dots}{
%%     ~~Describe \code{\dots} here~~
}
  \item{rec}{
%%     ~~Describe \code{rec} here~~
}
  \item{as.num}{
%%     ~~Describe \code{as.num} here~~
}
  \item{var.label}{
%%     ~~Describe \code{var.label} here~~
}
  \item{val.labels}{
%%     ~~Describe \code{val.labels} here~~
}
  \item{append}{
%%     ~~Describe \code{append} here~~
}
  \item{suffix}{
%%     ~~Describe \code{suffix} here~~
}
  \item{to.factor}{
%%     ~~Describe \code{to.factor} here~~
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
function (x, ..., rec, as.num = TRUE, var.label = NULL, val.labels = NULL, 
    append = TRUE, suffix = "_r", to.factor = !as.num) 
{
    UseMethod("rec")
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory (show via RShowDoc("KEYWORDS")):
% \keyword{ ~kwd1 }
% \keyword{ ~kwd2 }
% Use only one keyword per line.
% For non-standard keywords, use \concept instead of \keyword:
% \concept{ ~cpt1 }
% \concept{ ~cpt2 }
% Use only one concept per line.
