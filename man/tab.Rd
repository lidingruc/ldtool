\name{tab}
\alias{tab}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
column percent from expss
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
tab(cell_vars, col_vars = total(), row_vars = NULL, weight = NULL, subgroup = NULL, total_label = NULL, total_statistic = "u_cases", total_row_position = c("below", "above", "none"))
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{cell_vars}{
%%     ~~Describe \code{cell_vars} here~~
}
  \item{col_vars}{
%%     ~~Describe \code{col_vars} here~~
}
  \item{row_vars}{
%%     ~~Describe \code{row_vars} here~~
}
  \item{weight}{
%%     ~~Describe \code{weight} here~~
}
  \item{subgroup}{
%%     ~~Describe \code{subgroup} here~~
}
  \item{total_label}{
%%     ~~Describe \code{total_label} here~~
}
  \item{total_statistic}{
%%     ~~Describe \code{total_statistic} here~~
}
  \item{total_row_position}{
%%     ~~Describe \code{total_row_position} here~~
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
function (cell_vars, col_vars = total(), row_vars = NULL, weight = NULL, 
    subgroup = NULL, total_label = NULL, total_statistic = "u_cases", 
    total_row_position = c("below", "above", "none")) 
{
    str_cell_vars = expr_to_character(substitute(cell_vars))
    str_col_vars = expr_to_character(substitute(col_vars))
    str_row_vars = expr_to_character(substitute(row_vars))
    cell_vars = test_for_null_and_make_list(cell_vars, str_cell_vars)
    col_vars = test_for_null_and_make_list(col_vars, str_col_vars)
    if (!is.null(row_vars)) 
        row_vars = test_for_null_and_make_list(row_vars, str_row_vars)
    multi_cro(cell_vars = cell_vars, col_vars = col_vars, row_vars = row_vars, 
        weight = weight, subgroup = subgroup, total_label = total_label, 
        total_statistic = total_statistic, total_row_position = total_row_position, 
        stat_type = "cpct")
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
