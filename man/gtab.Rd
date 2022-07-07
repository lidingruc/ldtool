\name{gtab}
\alias{gtab}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
freq bar graph 
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
gtab(data, ..., title = "", weight.by = NULL, title.wtd.suffix = NULL, sort.frq = c("none", "asc", "desc"), type = c("bar", "dot", "histogram", "line", "density", "boxplot", "violin"), geom.size = NULL, geom.colors = "#336699", errorbar.color = "darkred", axis.title = NULL, axis.labels = NULL, xlim = NULL, ylim = NULL, wrap.title = 50, wrap.labels = 20, grid.breaks = NULL, expand.grid = FALSE, show.values = TRUE, show.n = TRUE, show.prc = TRUE, show.axis.values = TRUE, show.ci = FALSE, show.na = FALSE, show.mean = FALSE, show.mean.val = TRUE, show.sd = TRUE, drop.empty = TRUE, mean.line.type = 2, mean.line.size = 0.5, inner.box.width = 0.15, inner.box.dotsize = 3, normal.curve = FALSE, normal.curve.color = "red", normal.curve.size = 0.8, normal.curve.alpha = 0.4, auto.group = NULL, coord.flip = FALSE, vjust = "bottom", hjust = "center", y.offset = NULL)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{data}{
%%     ~~Describe \code{data} here~~
}
  \item{\dots}{
%%     ~~Describe \code{\dots} here~~
}
  \item{title}{
%%     ~~Describe \code{title} here~~
}
  \item{weight.by}{
%%     ~~Describe \code{weight.by} here~~
}
  \item{title.wtd.suffix}{
%%     ~~Describe \code{title.wtd.suffix} here~~
}
  \item{sort.frq}{
%%     ~~Describe \code{sort.frq} here~~
}
  \item{type}{
%%     ~~Describe \code{type} here~~
}
  \item{geom.size}{
%%     ~~Describe \code{geom.size} here~~
}
  \item{geom.colors}{
%%     ~~Describe \code{geom.colors} here~~
}
  \item{errorbar.color}{
%%     ~~Describe \code{errorbar.color} here~~
}
  \item{axis.title}{
%%     ~~Describe \code{axis.title} here~~
}
  \item{axis.labels}{
%%     ~~Describe \code{axis.labels} here~~
}
  \item{xlim}{
%%     ~~Describe \code{xlim} here~~
}
  \item{ylim}{
%%     ~~Describe \code{ylim} here~~
}
  \item{wrap.title}{
%%     ~~Describe \code{wrap.title} here~~
}
  \item{wrap.labels}{
%%     ~~Describe \code{wrap.labels} here~~
}
  \item{grid.breaks}{
%%     ~~Describe \code{grid.breaks} here~~
}
  \item{expand.grid}{
%%     ~~Describe \code{expand.grid} here~~
}
  \item{show.values}{
%%     ~~Describe \code{show.values} here~~
}
  \item{show.n}{
%%     ~~Describe \code{show.n} here~~
}
  \item{show.prc}{
%%     ~~Describe \code{show.prc} here~~
}
  \item{show.axis.values}{
%%     ~~Describe \code{show.axis.values} here~~
}
  \item{show.ci}{
%%     ~~Describe \code{show.ci} here~~
}
  \item{show.na}{
%%     ~~Describe \code{show.na} here~~
}
  \item{show.mean}{
%%     ~~Describe \code{show.mean} here~~
}
  \item{show.mean.val}{
%%     ~~Describe \code{show.mean.val} here~~
}
  \item{show.sd}{
%%     ~~Describe \code{show.sd} here~~
}
  \item{drop.empty}{
%%     ~~Describe \code{drop.empty} here~~
}
  \item{mean.line.type}{
%%     ~~Describe \code{mean.line.type} here~~
}
  \item{mean.line.size}{
%%     ~~Describe \code{mean.line.size} here~~
}
  \item{inner.box.width}{
%%     ~~Describe \code{inner.box.width} here~~
}
  \item{inner.box.dotsize}{
%%     ~~Describe \code{inner.box.dotsize} here~~
}
  \item{normal.curve}{
%%     ~~Describe \code{normal.curve} here~~
}
  \item{normal.curve.color}{
%%     ~~Describe \code{normal.curve.color} here~~
}
  \item{normal.curve.size}{
%%     ~~Describe \code{normal.curve.size} here~~
}
  \item{normal.curve.alpha}{
%%     ~~Describe \code{normal.curve.alpha} here~~
}
  \item{auto.group}{
%%     ~~Describe \code{auto.group} here~~
}
  \item{coord.flip}{
%%     ~~Describe \code{coord.flip} here~~
}
  \item{vjust}{
%%     ~~Describe \code{vjust} here~~
}
  \item{hjust}{
%%     ~~Describe \code{hjust} here~~
}
  \item{y.offset}{
%%     ~~Describe \code{y.offset} here~~
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
function (data, ..., title = "", weight.by = NULL, title.wtd.suffix = NULL, 
    sort.frq = c("none", "asc", "desc"), type = c("bar", "dot", 
        "histogram", "line", "density", "boxplot", "violin"), 
    geom.size = NULL, geom.colors = "#336699", errorbar.color = "darkred", 
    axis.title = NULL, axis.labels = NULL, xlim = NULL, ylim = NULL, 
    wrap.title = 50, wrap.labels = 20, grid.breaks = NULL, expand.grid = FALSE, 
    show.values = TRUE, show.n = TRUE, show.prc = TRUE, show.axis.values = TRUE, 
    show.ci = FALSE, show.na = FALSE, show.mean = FALSE, show.mean.val = TRUE, 
    show.sd = TRUE, drop.empty = TRUE, mean.line.type = 2, mean.line.size = 0.5, 
    inner.box.width = 0.15, inner.box.dotsize = 3, normal.curve = FALSE, 
    normal.curve.color = "red", normal.curve.size = 0.8, normal.curve.alpha = 0.4, 
    auto.group = NULL, coord.flip = FALSE, vjust = "bottom", 
    hjust = "center", y.offset = NULL) 
{
    type <- match.arg(type)
    sort.frq <- match.arg(sort.frq)
    plot_data <- get_dplyr_dot_data(data, dplyr::quos(...))
    if (!is.data.frame(plot_data)) {
        plot_data <- data.frame(plot_data, stringsAsFactors = FALSE)
        colnames(plot_data) <- deparse(substitute(data))
    }
    pl <- NULL
    if (inherits(plot_data, "grouped_df")) {
        grps <- get_grouped_data(plot_data)
        for (i in seq_len(nrow(grps))) {
            tmp <- sjlabelled::copy_labels(grps$data[[i]], data)
            tmp.title <- get_grouped_plottitle(plot_data, grps, 
                i, sep = "\n")
            plots <- lapply(colnames(tmp), function(.d) {
                plot_frq_helper(var.cnt = tmp[[.d]], title = tmp.title, 
                  weight.by = weight.by, title.wtd.suffix, sort.frq, 
                  type, geom.size, geom.colors, errorbar.color, 
                  axis.title, axis.labels, xlim, ylim, wrap.title, 
                  wrap.labels, grid.breaks, expand.grid, show.values, 
                  show.n, show.prc, show.axis.values, show.ci, 
                  show.na, show.mean, show.mean.val, show.sd, 
                  drop.empty, mean.line.type, mean.line.size, 
                  inner.box.width, inner.box.dotsize, normal.curve, 
                  normal.curve.color, normal.curve.size, normal.curve.alpha, 
                  auto.group, coord.flip, vjust, hjust, y.offset, 
                  var.name = .d)
            })
            pl <- c(pl, plots)
        }
    }
    else {
        pl <- lapply(colnames(plot_data), function(.d) {
            plot_frq_helper(var.cnt = plot_data[[.d]], title, 
                weight.by = weight.by, title.wtd.suffix, sort.frq, 
                type, geom.size, geom.colors, errorbar.color, 
                axis.title, axis.labels, xlim, ylim, wrap.title, 
                wrap.labels, grid.breaks, expand.grid, show.values, 
                show.n, show.prc, show.axis.values, show.ci, 
                show.na, show.mean, show.mean.val, show.sd, drop.empty, 
                mean.line.type, mean.line.size, inner.box.width, 
                inner.box.dotsize, normal.curve, normal.curve.color, 
                normal.curve.size, normal.curve.alpha, auto.group, 
                coord.flip, vjust, hjust, y.offset, var.name = .d)
        })
        if (length(pl) == 1) 
            pl <- pl[[1]]
    }
    pl
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
