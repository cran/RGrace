\name{panel}
\alias{panel}
\title{Axes and its elements}
\description{
  A function defined in \R-environment of class "figure" ("method" of "figure" object). \code{fig$panel()} creates new axes (i.e. tick's marks and labels, axes
  titles for left, bottom, right and top axes and  bounding box) in the
  plotting area of figure \code{fig}.
}
\section{Definition}{
\code{
panel( x=c(2,9), y=c(2,9), scale.X=c(-10,10), scale.Y=c(-10,10),
xticks=c("","Inf"), yticks=c("","Inf"), xlab=c("X Label",""),
ylab=c("YLabel", ""), ticks.in=TRUE, grill=FALSE,
Plabel=paste("Panel",panelsCount), GROB=NULL, select=0, update.GUI=T)
}
}

\arguments{
  \item{x,y}{
    location of axes in respect to parent figure's cell grid (see
    \code{\link{figure}}). Position specification is integer vector with
    first element - bottom,left corner position and the second - extent
    in the horizontal/vertical direction.    
  }
  \item{scale.X,scale.Y}{
    Minimum/maximum values of X and Y axes in native (i.e. data) units (see \code{\link[grid]{unit}}) 
  }
  \item{xticks,yticks}{
    Character vector with two components describing axis's
    ticks positions and ticks labels. First component is related to
    bottom X/left Y axis and the second to top X/bottom Y axis. 
  }
  \item{xlab,ylab}{
    Vectors with two components describing axis's
    title. Vector's component can be either character strings or
    expressions (in latter case axis title is formatted according to
    rules described in \code{\link[base]{plotmath}}). First component is related to
    bottom X/left Y axis and the second to top X/bottom Y axis. 
  }
  \item{ticks.in}{
    Direction of tick marks. By default they are drawn inside plot area.
  }
  \item{grill}{
    Draw or not gridlines on plot.
    }
  \item{Plabel}{
    Character string identifying the panel. It is not used internally and may
    have arbitrary value. Two panels in figure may have the same
    name. By default it is a concatenation of "Panel" string and the counter
    incrementing with every new panel added to the figure.
  }
  \item{GROB}{
    If GROB is not \code{NULL}, function \code{panel} just copies it in the
    output and redraws it on the \code{current.Figure}. All other parameters are ignored in this case.
    }
  \item{select}{
    If \code{select} is zero then new panel is created. If \code{select} is non-zero then
    function return \code{select}-th panel from figure's internal panel
    stack.
  }
  \item{update.GUI}{
    If \code{TRUE} widgets in figure are updated to reflect changes in
    panels properties. Setting to \code{FALSE} are useful only then you
    changes properties of several panels in a loop and don't want
    figure to be updated after every iteration. 
    }
    
}


\value{
  An R-environment  structure of class "panel" with following fields and methods:
  \item{element()}{
    method for adding new data line to panel or    
    accessing specific element inside panel (see \code{\link{element}}, \code{\link{grid.data}}).
  }
  \item{annotation()}{
    method for adding new text annotation to panel or    
    accessing specific annotation inside panel (see
    \code{\link{annotation}}, \code{\link[grid]{grid.text}}).
  }
  For a list of other fields and methods - do \code{ls(current.Figure$current.Panel)}.
}

\details{
  \code{panel()} has side effect of setting variable
  \code{current.Panel} inside parent figure's environment
  to \code{panel()}'s  return value. Although \code{xticks},
  \code{yticks} are character vectors they can contain code chunks that
  are parsed and evaluated when panel is redrawn (see details in \code{\link{grid.plotarea}}). 

  
}
\references{
}


\seealso{
  \code{\link{element}}, \code{\link{annotation}},
  \code{\link{grid.data}}, \code{\link[grid]{grid.text}}, 
  \code{\link[base]{assign}}, \code{\link[base]{get}}
  
}
\examples{
figure()$panel()
}
\author{M.Kondrin}
\keyword{hplot}
\keyword{iplot}
\keyword{dynamic}
