\name{grid.annotation}
\alias{grid.annotation}
\title{Multilined text}
\description{
  This is a functions that draws multilined text in grid's
  viewport. Beside plain text this function also handles mathematical
  expression and simple  graphical objects.
}
\usage{
grid.annotation(x=vp$xscale[[1]],y=vp$yscale[[2]],label="",
just=c("left","bottom"),rot=0,frame=FALSE,gp=gpar(lwd=2),
check.overlap=TRUE,vp=viewport(),draw=TRUE)
}

\arguments{
  \item{x,y}{
    Numeric vectors - coordinates of annotation's upper left corner.
  }
  \item{label}{
    Either a character string or a list whose elements are character
    strings (for plain text strings) or expressions (for mathematical
    expressions, see \link{plotmath}). Each element is a single line (
    mixing plain text and mathematics inside one line is not possible, use
    \code{expression()} in this case). It
    is also possible to draw simple grid graphic inside annotation, in
    that case list element have to be a function of class
    \code{extplotmath} which is evaluated when
    annotation is to be redrawn and inside this function actual drawing
    is done (see for example \code{\link{BULLET}})   
  }  
  \item{rot}{
    Angle in degrees between text base line and a horizontal line.
  }
  \item{frame}{
    Logical value. If \code{TRUE} a frame around annotation is draw.
    }
  \item{gp,draw,vp}{
    Standard parameters of grid objects (although with different default
    values). See
    \code{\link[grid]{gpar}}, \code{\link[grid]{viewport}}. 
  }
  \item{just}{
    Alignment of annotation text.
  }
  \item{check.overlap}{
    Check overlapping of annotation with figure edges. If
    \code{check.overlap=TRUE} and annotation indeed overlaps with figure margins
    then annotation is not shown.
  }

}


\value{
  Grob (GRaphic OBject, see \code{\link[grid]{grid.grob}}) of class "grid.annotation".  
   }  
}


\references{
}
\seealso{
  \code{\link[grid]{gpar}},  \code{\link[grid]{viewport}},
  \code{\link{grid.text}}.
  
  
}
\author{M.Kondrin}
\keyword{hplot}
\keyword{iplot}
\keyword{dynamic}
