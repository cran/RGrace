\name{BULLET}
\alias{BULLET}
\alias{bullet}
\alias{extplotmath}
\title{Dataline's representation in the textual annotation.}
\description{
  This is an utility function which converts positional numbers
  \code{\link{element}} and \code{\link{panel}} to a function of class "extplotmath"
  which is used inside \code{\link{grid.annotation}} to draw a graphical
  representation of this \code{\link{element}}. This function is used to
  draw "legend-like" text annotations and usually is a list element of
  \code{label} property of \code{\link{grid.annotation}} grob.
}
\usage{
BULLET(panel=1,element=1)
}

\arguments{
  \item{element,panel}{
    ordinal numbers of \code{element}'s data line from the
    \code{panel}'s panel in the current figure, whose graphical
    representation is to be added to the selected annotation.
  }
}


\value{
  Function call (of class "extplotmath" - just to make it somewhat
  special and different from expressions) to internal \code{grid.annotation}'s function
  \code{bullet(pch,size,gp)}, which is evaluated when the
  \code{grid.annotation} is to be redrawn. \code{bullet} draws a symbol
  \code{pch} with size \code{size} and grid graphical parameters (see
  \code{\link[grid]{gpar}}) \code{gp} at the "current position" inside
    the \code{grid.annotation}.   
}

\details{
  The next list element after \code{BULLET(...)} in the \code{label} property of
  \code{grid.annotation} is drawn just to the left of the dataline's
  icon (not at the next line which is the general rule). 
}

\references{
}
\seealso{
  \code{\link{figure}}, 
  \code{\link{grid.annotation}}, \code{\link{panel}}, \code{\link{element}}
  
}
\author{M.Kondrin}
\keyword{hplot}
\keyword{iplot}
\keyword{dynamic}
