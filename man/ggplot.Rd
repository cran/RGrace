\name{ggplot}
\alias{ggplot}
\title{Front end to RGrace plotting routines.}
\description{
  Plot \code{y} vs. \code{x} data. The preferred way to do plotting. 
}

\usage{
ggplot(... ,panel=nearestPanel())
}

\arguments{
  \item{...}{
    arguments passed directly to \code{\link{grid.data}} function.
  }
  \item{panel}{
    \code{panel} to plot data onto. Function \code{nearestPanel} selects
    the most suitable panel to draw onto (usually the \code{current.Panel} in
    the \code{current.Figure}). If \code{is.null(current.Figure)} new
    figure are created, if \code{current.Figure} has no panels then new
    panel is created (see Details section in \code{\link{panel}} and \code{\link{figure}})
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
  \code{\link{panel}}, \code{\link{figure}},
  \code{\link{grid.data}}
  
  
}
\examples{
#open new figure
#as an alternative to the gtkDevice standard x11 device can be used (uncomment next 2 lines)
#figure<-.z.interface
#x11()
figure(cells=11)
#divide figure on two panels
current.Figure$panel( x=c(2,5), y=c(2,10), xlab=c("x",""), ylab=list(expression(x^2),""),
update.GUI=FALSE)->p1
current.Figure$panel(x=c(7,10),y=c(2,10),
xlab=list(expression(log(x)),""), ylab=list(expression(log(y)),""), update.GUI=TRUE)->p2
x<-c(1,2,3,4,5)
y<-x^2
#plot on the first panel
ggplot(x=x,y=y,pch=as.integer(22),
gp=gpar(fill="red",col="blue",lty=2,lwd=2), panel=p1, update.GUI=FALSE)
# plot on the second panel
ggplot(x=log(x),y=log(y), pch=as.integer(21),
gp=gpar(fill="yellow",col="green",lty=1,lwd=1), panel=p2, update.GUI=TRUE)
#annotate the whole figure
l2<-list("Sample plots:", BULLET(1,1), expression(x^2), BULLET(2,1),
expression(log(x)~"~"~log(y)), BULLET(1,2), "no such plot")
p1$annotation(x=x[1], y=y[1], label=l2, gp=gpar(col="red4",lwd=2),
rot=90, frame=TRUE, update.GUI=FALSE)

}
\author{M.Kondrin}
\keyword{hplot}
\keyword{iplot}
\keyword{dynamic}
