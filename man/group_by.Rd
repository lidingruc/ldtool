\name{group_by}
\alias{group_by}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
group by from dplyr
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
group_by(.data, ..., .add = FALSE, .drop = group_by_drop_default(.data))
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{.data}{
%%     ~~Describe \code{.data} here~~
}
  \item{\dots}{
%%     ~~Describe \code{\dots} here~~
}
  \item{.add}{
%%     ~~Describe \code{.add} here~~
}
  \item{.drop}{
%%     ~~Describe \code{.drop} here~~
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
function (.data, ..., .add = FALSE, .drop = group_by_drop_default(.data)) 
{
    UseMethod("group_by")
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
