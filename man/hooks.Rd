\name{hooks}
\alias{hooks}
\alias{callbacks}
\alias{on.select.element}
\alias{on.select.annotation}
\alias{on.select.region}
\alias{on.select.points}
\alias{figure.startup}
\alias{simple.calculus}
\alias{spline.data}
\alias{spline.points}
\alias{move.indexed}
\alias{move.indexed.y}
\alias{delete.in.range}
\alias{sort.data}
\alias{smooth.data}
\alias{lf.in.range}
\alias{default.hook}
\alias{default.annotation}
\alias{default.hooks}
\title{User-defined Hooks.}
\description{
  User defined hooks called whenever some mouse/keyboard
  events occured inside \code{\link{figure}}'s plotting area. Events are
  invoked by pressing left mouse button, dragging mouse pointer and
  releasing mouse button. RGrace
  called thess functions by name with up to four arguments set so this
  functions must be assigned in the global environment to the
  user-provided code. Which one of the
  function will be called depends on graphic interaction context
  (i.e. the item selected in "Graphic Interaction" menu in RGrace
  figure). Hooks are called after left mouse button is realeased or when
  keyboard "Enter" or "Escape" keys are hit (only in "Select Points" or
  "Select Zoom"
  context).
  \code{figure.startup} is called every time new figure is opened. This
  hook is primaraly intended to put some interface customization code in
  (for example to create user-specific gtk-menu in menubar).
}
\usage{
on.select.element<-function{reg,handle,index,data}{}
on.select.region<-function{reg,handle,index,data}{}
on.select.annotation<-function{reg,handle,index,data}{}
on.select.points<-function{reg,handle,index,data}{}
figure.startup<-function(figure){}
}

\arguments{
  \item{reg}{
    parameter with the meaning of the event's "region". This is a two-column array
    filled with  x-y coordinate pairs (number of rows may vary). For
    \code{on.select.points} \code{reg} has only one row with coordinates
    of the point where the mouse click has occured;
    overwise it contains the extremes of rectangular region 
    sweeped with cursor while left mouse button is being pressed
    (\code{reg} has two rows). For
    events triggered by keyboard this parameter is \code{NULL}.
  }
  \item{handle}{
    grob someway related to graphic event. For \code{on.select.element}
    this is the "grid.data" structure of element closest to the point
    where the left mouse button has been pressed. For
    \code{on.select.region} this is the same "grid.data" that has been
    previously selected by \code{on.select.data}. For
    \code{on.select.annotation} this is the "grid.annotation" structure of
    annotation closest to the point
    where the left mouse button has been pressed. For
    \code{on.select.points} this is always \code{NULL}.
  }
  \item{index}{
    index of "subelement" of graphic object somehow related to graphic
    event. For \code{on.select.element}
    this is the index of data point closest to the point
    where the left mouse button has been pressed. For
    \code{on.select.region} this is an integer vector with indices of 
    \code{handle} graphic object's points (in this context the grob is
    "grid.data") that are inside the rectangle sweeped with mouse
    cursor.
    For other contexts this parameter is irrelevant and is set to \code{NULL}
  }
  \item{data}{
    X-y coordinates of indexed points. Then passed to
    \code{on.select.points} this parameter is a stack of points selected so
    far. Hitting "Enter" on keyboard then selected context is "Select
    Points" first invokes \code{on.select.points} and then resets
    stack. Hitting "Esc" first deletes the last (topmost) point from the
    stack 
    and then calls \code{on.select.points}. In "Select Element" and
    "Select Region" contexts this is (respectively) coordinates of data points of
    selected curve or coordinates of dta points falling in selected range.
  }
  \item{figure}{
    Newly created figure's environment the \code{figure.startup()} function was called from.
    }
}


\details{
  RGrace are equipped with a number of predefined hooks. Default hooks
  for \code{on.select.data}, \code{on.select.region},
  \code{on.select.points} (\code{default.hook}) report their input parameters
	to global environment as
	\code{.REG},\code{.HANDLE},\code{.INDEX},\code{.DATA}
	matrices. Default hook for \code{on.select.annotation} (\code{default.annotation}) moves the
	annotation closest to the point where left mouse button was
	pressed to the point where the button was released. Beside these
	hooks there are some other which are quite handy to perform the
	most common operations with graphics. Note that  most of them require
	\code{stats} package which is  not imported automatically when
	RGrace is loaded: 
	\begin{itemize}
	\item{\code{spline.points}}{
	  - suited for \code{on.select.points}
	  to draw a smoothed spline through points user has selected (
	  it uses \code{\link[stats]{smooth.spline}} function)	  
	}
	\item{\code{spline.data}}{
	  - draws a smoothed spline through selected data curve	  
	}
	\item{\code{smooth.data}}{
	  - draws a smoothed curve through existing one (uses
	  \code{\link[stats]{ksmooth}} function)
	  
	  }
	\item{\code{delete.in.range}}{
	    - deletes selected points from selected data line.
	    }
	\item{\code{sort.data}}{
	      - sorts points in selected data line
	      according to theirs x-coordinates.
	    }
	 \item{\code{simple.calculus}}{
	      - simple arbitrary transformation
	      of x/y coordinates of the selected curve.
	      }   
	      \item{\code{move.indexed}}{
		- moves selected point in arbitrary direction
	      }
	      \item{\code{move.indexed.y}}{
		- the same as before, but only along y-axis
		}
	\item{\code{lf.in.range}}{
	  - draws a linear fit (calculated by
	  \code{\link[stats]{lm.fit}} function) through the
	points which falls in the selected range. Line fit parameters
	are returned in the global variable \code{.LF}.
      }
      	\end{itemize}

	Default hooks can be restored at any moment with command
	\code{default.hooks()}.
	Note that although hooks are defined globally, graph interaction
	context are figure-wide only. So if you are woking with several
	figures - they do share hooks, but can have different contexts.
	There is a way to change graphic context in R-scripts by
	assigning values to \code{.Interaction.Type} variable inside
	figure's environment (\code{assign(.Interaction.Type,"Select
	  Points",envir=fig$env)})  
  }  



\references{
}
\seealso{
  \code{\link{panel}}, \code{\link{figure}}

  
  
}
\examples{
#example of hook suitable for multi-gauss peak fitting.

#fit Gauss peaks with linear bias 
bias.gauss<-function(par,x,y){
  y1<-NULL
    for (i in 1:dim(par)[1]) {y1<-cbind(y1,exp(-(x-par[i,1])^2/par[i,2]^2))}
    Y<-cbind(y1,x,1)
    lf<-lm.fit(Y,y)
    return(list(lf=lf,Y=Y))
}

#wrapper for bias.gauss
fitfun<-function(par,x,y,dimensions){
  dim(par)<-dimensions
  i<-bias.gauss(par,x,y);
  sum(i$lf$residuals^2)
}

#the hook itself
#it is expected that .DATA global variable already holds 
#the data we want to fit. This means either on.select.element or
#on.select.region set to default.hook(). Then user should select points 
#where he would expect the peaks maxima to be located. 
#When done, hitting "Enter" key will start the fitting itself.
on.select.points <- function(reg,handle,index,data){
  #Test all peaks are selected.
  if (is.null(reg)){
    #first guess - not very smart
    par <- cbind(data[,1],data[,1]/100.)
    #optimize
    Parm<<-optim(par,fitfun,x=.DATA[,1],y=.DATA[,2],dimensions=dim(par));
    dim(Parm$par)<-dim(par)
    #calculate y-values of single peak components
    l1<-bias.gauss(Parm$par,x=.DATA[,1],y=.DATA[,2])
    #plot fitted curve - the peaks sum
    ggplot(x=.DATA[,1],y=l1$Y\%*\%l1$lf$coefficients,gp=gpar(col="red"))
    }    
}
  
}
\author{M.Kondrin}
\keyword{hplot}
\keyword{iplot}
\keyword{dynamic}
