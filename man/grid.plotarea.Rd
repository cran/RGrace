\name{grid.plotarea}
\alias{grid.plotarea}
\title{Plot's frame.}
\description{
  This function draws fully rigged (i.e. with tick marks, tick labels,
  axis label and - optionally - with grid) left,right,bottom and top axis on the current graphic device.
}
\usage{
grid.plotarea(inward=FALSE,
grilled=FALSE,gp=gpar(lwd=2,fontsize=12,fontface=1),
at=c("","Inf","","Inf"),label=c("X Label","","Y Label", ""),
draw=TRUE, vp=NULL, title="")
}

\arguments{
  \item{at}{
    Character string containing the code used to calculate tick marks
    position and tick marks' labels. If \code{at} is empty, tick marks
    are evenly distributed along axis at position with "pretty numbers"
    (see \code{\link[grid]{grid.pretty}}). Otherwise \code{at} are
    parsed and evaluated twice in environment with two local variables
    \code{RANGE} and \code{TICK.LAB} set. \code{RANGE} contains the current
    range of the axis. \code{TICK.LAB} is set either to "tick" or
    "label". In the former case, if \code{at} string is evaluated to
    numerical vector, then this values are used as a positions of tick
    marks; in the latter case the value may be numerical or character
    vector which gives tick marks' labels (tips
    of the day: \code{at="Inf"} draws an empty axis without ticks and
    labels; \code{at="list(tick=0,label=\"Start point\")[[TICK.LAB]]"} draws
    only one tick at point zero with label "Start point";
    \code{at="log10.ticks(RANGE,TICK.LAB)"} makes automatic logariphmic axis with
    power of 10 labels).
  }
  \item{label}{
    Axis title. Can be either character string or expression. If entered
    in GUI text box in former case the value must be quoted, in the
    latter case it mustn't be quoted but should be wrapped in
    \code{expression} call.
  }
  \item{inward}{
    Direction of tick marks. If \code{TRUE} tick marks are drawn inward
    plotting area, otherwise - overwise.
  }
  \item{grilled}{
    To draw or not to draw grid lines. Grid are always drawn with 1pt wide
    dashed gray lines and \code{gp} parameter does not have effect on them.
    }
  \item{gp}{
    Graphic parameter of axis. Applies to all 4 plotarea's axis with their tick
    marks , tick labels and axis legends.
  }
  \item{vp,draw}{
    Standard grob's parameters - viewport to draw onto and does this
    function actually
    draw the graphic object or not.
  }
  \item{title}{
    Character string used for the plotarea identification. Something
    like a comment string stored with the grob.  	
  }
}


\value{
  Grob (GRaphic OBject, see \code{\link[grid]{grid.grob}}) of class "grid.plotarea".   
   }  
}

\details{
  Elements of vector parameters \code{label} and \code{at} corresponds
  to bottom,top,left and right axis. 
  }

\references{
}
\seealso{
  \code{\link[grid]{gpar}},  \code{\link[grid]{viewport}}
}
\author{M.Kondrin}
\keyword{hplot}
\keyword{iplot}
\keyword{dynamic}
