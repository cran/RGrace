\name{log.ticks}
\alias{log10.ticks}
\alias{loge.ticks}
\title{"Logarithmic" tick marks.}
\description{
  These are utility functions which draws a tick marks at position with
  power of ten values. These functions are used in GUI text boxes as an
  easy way to calculate a position of ticks on "logarithmic" (note
  quotes and see Details!) axis.
}
\usage{
log10.ticks(range,tick.lab)
loge.ticks(range,tick.lab)
}

\arguments{
  \item{range}{
    a two-element vector with extent of axis.
  }
  \item{tick.lab}{
    is either "tick" or "label". In former case position of tick mark is
    calculated, in later - tick's label.
    }
}


\value{
  No.
}

\details{
  The character representation of these functions in the form they are
  written in Usage section can be passed as an element of \code{at}
  parameter to \code{\link{grid.plotarea}} function. The character strings is
  parsed and evaluated when the axis is to be redrawn in an environment
  with \code{.RANGE} and \code{.TICK.LAB} variables set appropriately
  and passed as input values (\code{range} and \code{tick.lab} respectively).
  
  To rule out possible misunderstanding - there is no such thing as
  logarithmic axis in RGrace. All axis are linear and this functions
  just draws "fake" ticks which can produce a "make-believe logarithmic"
  plots. To do so you have to plot log(MyData) and
  log10.ticks will draw ticks with labels which correspond to values of
  MyData. Note what the ranges of axis as shown in GUI controls nonetheless corresponds to
  values of log(MyData) so this is all very confusing. So it goes (c) K.Vonnegut
}

\references{
}
\seealso{
  \code{\link{figure}}, \code{\link{grid.plotarea}},
  \code{\link{panel}}, \code{\link{element}}
  
}
\examples{
figure()
x<-c(1,2,3,4,5)
y<-x^6
#draw log-log plot of x~y on current.Figure with freshly created panel with logarithmic tick marks
ggplot(log10(y), log10(x), panel=current.Figure$panel(
xticks=c("log10.ticks(RANGE,TICK.LAB)","Inf"), yticks=c("log10.ticks(RANGE,TICK.LAB)","Inf")))
}
\author{M.Kondrin}
\keyword{hplot}
\keyword{iplot}
\keyword{dynamic}
