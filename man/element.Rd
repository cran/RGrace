\name{element}
\alias{element}
\title{Data line}
\description{
  A function defined in \R-environment of class "panel" (method of "panel" class). \code{panel$element()} creates new data line
  (i.e. symbols with connection line and symmetric error bars) inside \code{panel}.
}
\section{Definition}{
\code{element( y, x=seq(x), ...,
Elabel=paste(deparse(substitute(y)),deparse(substitute(x)),sep="~"),
select=0, rescale=TRUE, update.GUI=TRUE)
}
}

\arguments{
  \item{x,y}{
    coordinates of data points.
  }
  \item{...}{
    \code{...} arguments are directly passed to grid.data function (see
    \code{\link{grid.data}} for details).
  }
  \item{Elabel}{
    Character string identifying the data line. It is not used internally and may
    have arbitrary value. Two elements in panel may have the same
    name. By default it is a concatenation \code{x} and \code{y} "values".
  }
  \item{select}{
    If \code{select} is zero new data line is created. If \code{select} is non-zero then
    function return \code{select}-th element from panel's internal elements
    stack.
  }
  \item{rescale}{
    If \code{TRUE} parent panel's x/y scales are changed to make all
    elements in panel visible.
    }
  \item{update.GUI}{
    If \code{TRUE} widgets in figure are updated to reflect changes in
    elements properties. Setting to \code{FALSE} are useful only then you
    change properties of several elements in a loop and don't want
    figure to be updated after every iteration. 
    }
    
}


\value{
  Grob (GRaphic OBject, see \code{\link[grid]{grid.grob}}) of class "grid.data". Details
  about this class you can find in \code{\link{grid.data}}.
  }  
}


\references{
}
\seealso{
  \code{\link{panel}},
  \code{\link{grid.data}}
  
  
}
\examples{
figure()$panel()$element(c(1,2,3,4)^3)
}
\author{M.Kondrin}
\keyword{hplot}
\keyword{iplot}
\keyword{dynamic}
