default.hook <- function(reg,handle,index,data){
  .DATA<<-data
  .INDEX<<-index
  .HANDLE<<-handle
  .REG<<-reg
}

default.figure.startup<-function(...){
}

default.annotation <- function(reg,handle,index,data){
  grid.edit(handle$name,x=unit(reg[1,2],"native"),y=unit(reg[2,2],"native"),redraw=T)
}

delete.in.range<- function(reg,handle,index,datainreg){
   xd <- grid.get(handle$name)$x
   yd <- grid.get(handle$name)$y
   grid.edit(handle$name,x=xd[-index],y=yd[-index],redraw=T)
}

lf.in.range <- function(reg,handle,index,datainreg){
  library(stats)
  Y <- cbind(datainreg[,1],1)
  is.na(Y[,1])->l
  Y <- Y[!l,]
  x <- range(Y[,1])
  y <- datainreg[!l,2]
  x <- c(x[1]-(x[2]-x[1])/2,x[2]+(x[2]-x[1])/2)
  lf<-lm.fit(Y,y)
  .LF<<-lf$coefficients
  ggplot(x=x,y=cbind(x,1)%*%lf$coefficients,Elabel=paste(c("Ax+B: A=","B="),lf$coefficients,collapse=" ",sep=""),gp=gpar(col="blue"),rescale=F)
}

xsort.curve <- function(reg,handle,index,data){
   l <- sort(data[,1],index.return=TRUE)   
   grid.edit(handle$name,x=data[l$ix,1],y=data[l$ix,2],redraw=TRUE)
}

simple.calculus <- function(reg,handle,index,data){
  apply.cb<-function(...){
    X<-data[,1]
    Y<-data[,2]
    x1<-try(eval(parse(text=new.x.w$GetText())))
    y1<-try(eval(parse(text=new.y.w$GetText())))
    if(length(index)==length(X)){
      #select region case
      X<-grid.get(handle$name)$x
      Y<-grid.get(handle$name)$y
      X[index]<-x1
      Y[index]<-y1
      x1<-X
      y1<-Y
    }
    if (arng.w$GetActive()==TRUE){
      grid.edit(handle$name,x=x1,y=y1,redraw=FALSE)
      current.Figure$current.Panel$autorange()
    } else {
       grid.edit(handle$name,x=x1,y=y1,redraw=TRUE)
    }
  }
   rback.cb<-function(...){
    x1<-data[,1]
    y1<-data[,2]
    if(length(index)==length(x1)){
      #select region case
      X<-grid.get(handle$name)$x
      Y<-grid.get(handle$name)$y
      X[index]<-x1
      Y[index]<-y1
      x1<-X
      y1<-Y
    }
    if (arng.w$GetActive()==TRUE){
      grid.edit(handle$name,x=x1,y=y1,redraw=FALSE)
      current.Figure$current.Panel$autorange()
    } else {
       grid.edit(handle$name,x=x1,y=y1,redraw=TRUE)
    }
  }
  cancel.cb <- function(...){
    w$Destroy()
  }
  w<-gtkWindow()
  w$SetTitle("Edit X and Y values")
  w$Add(v <- gtkVBox(FALSE,6))
  
  v$Add(h <- gtkHBox(FALSE,2))
  v$SetChildPacking(h,FALSE,FALSE,0,0)
  h$Add(gtkLabel("X'<-f(X,Y)"))
  h$Add(new.x.w <- gtkEntry(10))
  new.x.w$SetText("X")

  v$Add(h <- gtkHBox(FALSE,2))
  v$SetChildPacking(h,FALSE,FALSE,0,0)
  h$Add(gtkLabel("Y'<-f(X,Y)"))
  h$Add(new.y.w <- gtkEntry(10))
  new.y.w$SetText("Y")

  v$Add(h <- gtkHBox(TRUE,1))
  v$SetChildPacking(h,FALSE,FALSE,0,0)
  h$Add(arng.w <- gtkCheckButton("Autorange?"))  
  arng.w$SetActive(TRUE)
  
  v$Add(h <- gtkHBox(TRUE,3))
  v$SetChildPacking(h,FALSE,FALSE,0,0)
  h$Add(apply.w <- gtkButton("Apply"))
  h$Add(rback.w <- gtkButton("Rollback"))
  h$Add(cancel.w <- gtkButton("Close"))

  apply.w$AddCallback("clicked",apply.cb)
  rback.w$AddCallback("clicked",rback.cb)
  cancel.w$AddCallback("clicked",cancel.cb)
  w$ShowAll()
}


move.indexed <- function(reg,handle,index,data){
  xd <- grid.get(handle$name)$x
  yd <- grid.get(handle$name)$y
  xd[index] <- xd[index]+reg[1,2]-reg[1,1]
  yd[index] <- yd[index]+reg[2,2]-reg[2,1]
  grid.edit(handle$name,x=xd,y=yd,redraw=TRUE)
}

move.indexed.y <- function(reg,handle,index,data){
  yd <- grid.get(handle$name)$y
  yd[index] <- yd[index]+reg[2,2]-reg[2,1]
  grid.edit(handle$name,y=yd,redraw=TRUE)
}

smooth.curve <- function(reg,handle,index,data){
  library(stats)
  ksmooth(x=data[,1],y=data[,2],kernel="normal",bandwidth=3)->z
  ggplot(x=z$x,y=z$y,Elabel=paste(handle$title,"smoothed"),gp=gpar(col="blue"),rescale=FALSE)
}
  
spline.curve <- function(reg,handle,index,data){
  library(stats)
  if(dim(data)[[1]]>4){
    z <- smooth.spline(x=data[,1],y=data[,2],spar=0.6,all.knots=T)
    sfun<-splinefun(z$x,z$y)
    xdata <- seq(range(data[,1])[[1]],range(data[,1])[[2]],length=250)
    ggplot(x=xdata,y=sfun(xdata),Elabel="Smooth spline",gp=gpar(col="red"),rescale=F)
  } 
}

spline.points <- function(reg,handle,index,data){
  library(stats)
  if (dim(data)[[1]]==1){
    .SPLINE<<-ggplot(x=data[1,1],y=data[1,2],Elabel=paste("Smooth spline for",handle$title),rescale=FALSE,gp=gpar(col="red"))
  }else{
    if(dim(data)[[1]]>4){
      z <- smooth.spline(x=data[,1],y=data[,2],spar=0.6,all.knots=T)
      sfun<-splinefun(z$x,z$y)
      xdata <- seq(range(data[,1])[[1]],range(data[,1])[[2]],length=250)
      grid.edit(.SPLINE$name,x=xdata,y=sfun(xdata))
    } else {
       grid.edit(.SPLINE$name,x=data[,1],y=data[,2])
    }
  }
}


default.hooks <- function(){
  on.select.annotation <<- default.annotation
  on.select.element <<- default.hook
  on.select.region <<- default.hook
  on.select.points <<- default.hook
}

