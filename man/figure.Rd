\name{figure}
\alias{figure}
\alias{figure.back}
\alias{.GTK2.interface}
\alias{.z.interface}
\title{Plotting window.}
\description{
  \code{figure()} opens new GUI window with plotting area (graphic
  device). \code{figure.back} and \code{.GTK.interface} are back-end and
  front-end procedures implementing RGrace
  functionality. \code{.z.interface} is another (primitive) realization
  of front-end function for RGrace.
}
\usage{
figure.back(width=640,height=480,cells=10)
figure(width=400, height=400, cells=10)
.GTK2.interface(width=400,height=400,cells=10)
.z.interface(width=1000,height=2000,cells=10)

}

\arguments{
  \item{width, height}{
    initial geometry of plotting area in pixels. Later it may be changed
    with window manager's resize operations.
  }
  \item{cells}{
    plotting area are divided by \code{cell x cell} grid. Panels
    inside figure are located in this grid's knots. So setting this
    parameter for higher values allows finer control of axes' placement.
  }
}



\value{
  An R-environment of class "figure" with following fields and methods:
  \item{panel()}{
  method for adding new panels(axes) to figure or
    accessing specific panel inside figure (see \code{\link{panel}}).
  }.
  For a list of other methods and fields - do \code{ls(current.Figure)}
}

\details{
  \code{figure.back} is a "abstract class" (or backend) where core functionality of
  RGrace is implemented. \code{.GTK.interface} is a GTK frontend to
  RGrace.  \code{figure} function is initially (at package loading) set 
  to \code{.GTK.interface}. RGrace design allows users
  easily implement their own frontend (for example with tcltk GUI and x11
  graphic device) and have all functionality of
  RGrace in the same time. \code{.z.interface} is a "dummy" interface which simply uses
  currently available graphic device as a plot area for RGrace. It is
  mainly intended for usage with non-interactive devices (like
  postscript, pdf etc.) when printing of gtkDevice's content is not
  applicable (for example, in the process of generating Sweave documents
  with RGrace graphic). Sample Sweave file with RGrace graphic can be found here \code{system.file("scripts","RGrace.Rnw",package="RGrace")}.
  For an example of typical interface class look into Examples section.

  \code{figure()} and friends has side effects of setting global variable \code{current.Figure}
  to \code{figure()}'s  return value and setting current graphic device
  to this window plotting area.
}
\references{
}
\seealso{
  \code{\link{panel}}
  
  \code{\link[base]{set}}
  
  \code{\link[base]{get}}
  
}
\examples{
#do not run
    .XXX.interface<-function(...)\{
    #create new device
    x11()
    #fig is local variable pointing to figure back-end
    fig <- figure.back(width=width,height=height,cells=cells)
    #Callback functions you have to define to let figure backend signal your
    #apllication when properties of panel,annotation or element has been
    #changed (to reflect them in GUI controls for example)
    fig$PANEL.CB <- panel.GUI
    fig$ANNOTATION.CB <- annotation.GUI
    fig$ELEMENT.CB <- element.GUI
    #and other code specific to this application
    ......
    \}
    
    #then redefine figure() function
    figure<-.XXX.interface
  }

\author{M.Kondrin}
\keyword{hplot}
\keyword{iplot}
\keyword{dynamic}
