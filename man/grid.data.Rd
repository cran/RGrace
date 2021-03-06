\name{grid.data}
\alias{grid.data}
\title{Draw Data Symbols and Line in a Grid Viewport}
\description{
  This function draws data points and lines connecting them on the
  current graphic device.
}
\usage{
grid.data(x=c(0, 1),y=c(0, 1), w=NULL, h=NULL, pch=as.integer(26),
size=unit(1, "char"), errstyle = "whiskers", gp=gpar(col="black",
fill="white", lty=1, lwd=1), title="", draw=TRUE, vp=NULL)
}

\arguments{
  \item{x,y}{
    Numeric vectors - coordinates of data points.
  }
  \item{w,h}{
    Numeric vectors or \code{NULL}s. If not \code{NULL} they describe an
    uncertainty in x,y data coordinates which is  represented on graphic
    according to \code{errstyle} parameter.
  }  
  \item{pch}{
    Symbol's shape. RGrace handles only symbols in the range 20-26
    (i.e. filled circles, squares, diamonds, up and down
    triangles). Then \code{pch=26} symbols are not drawn. 
  }
  \item{size,gp,draw,vp}{
    Standard parameters of grid objects (although with different default
    values). See
    \code{\link[grid]{gpar}}, \code{\link[grid]{grid.points}},
    \code{\link[grid]{grid.lines}}, \code{\link[grid]{viewport}}. 
  }
  \item{errstyle}{
    How data errors \code{w},\code{h} are represented on plot.
    Acceptable values:
    \begin{description}
    \item[\code{whiskers}] (the default). Errorbars with equal lengths in
    both directions.
    \item[\code{areafill}] Area is filled between the data curve and curve
    shifted by \code{c(w,h)} vector. There is special case. If both
    \code{w}, \code{h} are \code{NULL}s then area between data curve and
    x-axis is filled.
    \item[\code{arrows}] The arrows is drawn from the nodes ofdata curve
    in the direction of the vector \code{c(w,h)}. 
    \end{description}
    }
  \item{title}{
    Character string used for the data identification. Something
    like a comment string stored with the grob.  	
  }
}


\value{
  Grob (GRaphic OBject, see \code{\link[grid]{grid.grob}}) of class
  "grid.data".  Note what in contrast to the functions
  \code{grid.symbols} and \code{grid.lines} \code{x} and \code{y} arguments
  take as parameters numeric vectors (not unit objects !). \code{lty}
  (Line TYpe)
  field of \code{gp} argument applies only to line connecting data
  points, symbol's outline as well as error bars are always drawn with solid line (\code{lty=1}). 
   }  
}


\references{
}
\seealso{
  \code{\link[grid]{gpar}}, \code{\link[grid]{grid.points}}, \code{\link[grid]{viewport}},
  \code{\link[grid]{grid.lines}}
  
  
}
\author{M.Kondrin}
\keyword{hplot}
\keyword{iplot}
\keyword{dynamic}

