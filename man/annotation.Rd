\name{annotation}
\alias{annotation}
\title{Text annotation}
\description{
  A function defined in \R-environment of "panel" class. \code{panel$annotation()} creates new
  multilined text annotation inside \code{panel}.
}
\section{Definition}{
\code{
 annotation(..., select=0, update.GUI=TRUE)
}
}

\arguments{
  \item{...}{
     \code{...} arguments are directly passed to underlying
\code{\link{grid.annotation}} function (position of annotation, its
content and graphical parameters)
  }
  \item{select}{
    If \code{select} is zero new annotation is created. If \code{select} is non-zero then
    function return \code{select}-th annotation from panel's internal annotations
    stack.
  }
  \item{update.GUI}{
    If \code{TRUE} widgets in figure are updated to reflect changes in
    annotations properties. Setting to \code{FALSE} are useful only then you
    change properties of several annotations in a loop and don't want
    figure to be updated after every iteration. 
    }
    
}


\value{
  Grob (GRaphic OBject, see \code{\link[grid]{grid.grob}}) of class "grid.annotation". Details
  about this class you can find in \code{\link{grid.annotation}}. 
}

\details{
  }

\references{
}
\seealso{
  \code{\link{panel}},
  \code{\link{grid.annotation}}
}
\examples{
figure()$panel()$annotation(label="test",x=0,y=0)
}
\author{M.Kondrin}
\keyword{hplot}
\keyword{iplot}
\keyword{dynamic}
