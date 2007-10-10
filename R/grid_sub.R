validGrobDetails.border <- function(x){
  x
}

drawDetails.border <- function(x, recording) {
if (!is.null(x$vp)){
  .Call.graphics("L_segments",unit(0,"npc"),unit(0,"npc"),unit(0,"npc"),unit(1,"npc"),NULL,PACKAGE="grid")
  .Call.graphics("L_segments",unit(1,"npc"),unit(0,"npc"),unit(1,"npc"),unit(1,"npc"),NULL,PACKAGE="grid")
  .Call.graphics("L_segments",unit(0,"npc"),unit(0,"npc"),unit(1,"npc"),unit(0,"npc"),NULL,PACKAGE="grid")
  .Call.graphics("L_segments",unit(0,"npc"),unit(1,"npc"),unit(1,"npc"),unit(1,"npc"),NULL,PACKAGE="grid")
}
}

grid.border <- function(gp=gpar(),name=NULL,draw=TRUE, vp=NULL) {
  r <- list(gp=gp, vp=vp)
  cl <- "border"
  z <- grob(gp=gp, cl=cl, vp=vp,name=name)
  if (draw) {
        grid.draw(z)
  }
  z
}



##################################
# Data (symbols+lines)  component#
##################################

validDetails.data <- function(x){
  if((length(x$x)==0)|(length(x$y)==0)){
    stop("x/y length must be not zero")
  }
  x
}


drawDetails.data<- function(x, recording) {
  if (!is.null(x$vp)){
    gp1 <- NULL
    switch(x$errstyle,
           whiskers={
             .draw.grid.lines(unit(x$x,"native"), unit(x$y,"native"))
             if (!is.null(x$gp)) {
               gp1<-x$gp
               gp1$lty<-1
               grid:::set.gpar(gp1)
             }
             if (!is.null(x$h)){
               z1 <- unit(x$y+x$h,"native")
               z2 <- unit(x$y-x$h,"native")
               h1 <- unit(x$x,"native")+unit(as.numeric(x$size)/2,"char")
               h2 <- unit(x$x,"native")-unit(as.numeric(x$size)/2,"char")
               .draw.grid.segments(unit(x$x,"native"),z1,unit(x$x,"native"),z2)
               .draw.grid.segments(h1,z1,h2,z1)
               .draw.grid.segments(h1,z2,h2,z2)
             }
             if (!is.null(x$w)){
               z1 <- unit(x$x+x$w,"native")
               z2 <- unit(x$x-x$w,"native")
               h1 <- unit(x$y,"native")+unit(as.numeric(x$size)/2,"char")
               h2 <- unit(x$y,"native")-unit(as.numeric(x$size)/2,"char")
               .draw.grid.segments(z1,unit(x$y,"native"),z2,unit(x$y,"native"))
               .draw.grid.segments(z1,h1,z1,h2)
               .draw.grid.segments(z2,h1,z2,h2)
             }
           },
           areafill={
             if (is.null(x$h)&is.null(x$w)){
               z1 <- c(x$x[length(x$x)],x$x[1])
               z2 <- c(x$vp$yscale[1],x$vp$yscale[1])
             } else{
               if (!is.null(x$h)){
                 z1 <- unit(x$y+x$h,"native")
               } else {
                 z1 <- unit(x$y,"native")
               }                
               if (!is.null(x$w)){
                 z2 <- unit(x$x+x$w,"native")
               } else {
                 z2 <- unit(x$x,"native")
               }
               
             }
             .Call.graphics("L_polygon", unit(c(x$x,z2[length(z2):1]),"native"), unit(c(x$y,z1[length(z1):1]),"native"), list(as.integer(1:(length(z2)+length(x$x)))),PACKAGE="grid")
           },
           arrows={
             .draw.grid.lines(unit(x$x,"native"), unit(x$y,"native"))
             if (!is.null(x$h)&!is.null(x$h)){
               gp1 <- NULL
               if (!is.null(x$gp)) {
                 gp1<-x$gp
                 gp1$lty<-1
                 grid:::set.gpar(gp1)
               }
               if (!is.null(x$h)){
                 z1 <- unit(x$y+x$h,"native")
               } else {
                 z1 <- unit(x$y,"native")
               }                
               if (!is.null(x$w)){
                 z2 <- unit(x$x+x$w,"native")
               } else {
                 z2 <- unit(x$x,"native")
               }               
               .Call.graphics("L_segments", unit(x$x,"native"), unit(x$y,"native"), z2, z1, arrow(10,unit(x$size,"char"),"last","closed"),PACKAGE="grid")
             }
           },
           ellipses={print(x$errstyle)}
           )
    if (x$pch<26) {
      .Call.graphics("L_points", unit(x$x,"native"), unit(x$y,"native"), x$pch, unit(x$size,"char"),PACKAGE="grid")
    }
    if (!is.null(gp1)) {
      grid:::set.gpar(x$gp)
    }

  }
}


grid.data <- function(x=c(0, 1),y=c(0, 1), w=NULL, h=NULL, pch=as.integer(26), size=unit(1, "char"), errstyle="whiskers", gp=gpar(col="black",fill="white",lty=1,lwd=1), title="", draw=TRUE, vp=NULL) {
  cl <- "data"
  z <- grob(title=title, x=x, y=y, h=h, w=w, pch=as.integer(pch), size=size, errstyle=errstyle, cl=cl, gp=gp, vp=vp)
  if (draw){
      grid.draw(z)
  }
  z
}



##################################################################
#Plotarea grob - axis with tick marks, tick labels,titles + frame#
##################################################################


editTicks <- function(x){
  if (!x$inward){
    dir <- 1
    offset <- 0
  } else {
    dir <- -1
    offset <- 0.5
  }
  lapply(1:4,function(z){
    at.exp <- parse(text=x$at[[z]])
    if (z%in%c(1,2)){
      range <- x$vp$xscale
    } else {
      range <- x$vp$yscale
    }
    if (!is.null(range)) {
      at.ticks <- eval(at.exp,list(RANGE=range,TICK.LAB="tick"))
      if (is.null(at.ticks)) { at.ticks <- grid.pretty(range) }
      at.labels <- eval(at.exp,list(RANGE=range,TICK.LAB="label"))
      if (is.null(at.labels)) {
        at.labels <- parse(text=gsub("e","%*%10^",as.character(at.ticks)))
      } else {
        at.labels<-as.character(at.labels)
      }
    
    } else {
      at.ticks <- Inf
      at.labels <- "Inf"
    }
    at.ticks <- unit(at.ticks,"native")
    switch(z,
         {            
           tick.y0 <- unit(0, "npc")
           tick.y1 <- unit(-.5*dir, "char")
           tick.x0 <- at.ticks
           tick.x1 <- at.ticks
           just <- c("center","top")
           rot <- 0
           label.x <- at.ticks
           label.y <- unit(-1+offset, "char")            
         },
         {
           tick.y0 <- unit(1, "npc")
           tick.y1 <- unit(1, "npc") + unit(.5*dir, "char")
           tick.x0 <- at.ticks
           tick.x1 <- at.ticks
           just <- c("center","bottom")
           rot <- 0
           label.x <- at.ticks
           label.y <- unit(1, "npc") + unit(1-offset, "char")
         },
         {
           tick.y0 <- at.ticks
           tick.y1 <- at.ticks
           tick.x0 <- unit(0, "npc")
           tick.x1 <- unit(-.5*dir, "char")
           just <- c("right","center")
           rot <- 0
           label.y <- at.ticks
           label.x <- unit(-1+offset, "char")
         },
         {
           tick.y0 <- at.ticks
           tick.y1 <- at.ticks
           tick.x0 <- unit(1, "npc")
           tick.x1 <- unit(1, "npc") + unit(.5*dir, "char")
           just <- c("left","center")
           rot <- 0
           label.y <- at.ticks
           label.x <- unit(1, "npc") + unit(1-offset, "char") 
         }
         )
  x <- editGrob(x,x0=tick.x0,y0=tick.y0,x1=tick.x1,y1=tick.y1,vp=x$vp,gPath=paste("ticks",z,sep=""),strict=TRUE)
  if (!x$grilled) {
    x <- editGrob(x,x0=unit(Inf,"npc"),y0=unit(0,"npc"),x1=unit(Inf,"npc"),y1=unit(1,"npc"),vp=x$vp,gPath=paste("setka",z,sep=""),strict=TRUE)
  } else {
  if(z%in%c(1,2)){
    x <- editGrob(x,x0=tick.x0,y0=unit(0,"npc"),x1=tick.x0,y1=unit(1,"npc"),vp=x$vp,gPath=paste("setka",z,sep=""),strict=TRUE)
  } else {
    x <- editGrob(x,x0=unit(0,"npc"),y0=tick.y0,x1=unit(1,"npc"),y1=tick.y0,vp=x$vp,gPath=paste("setka",z,sep=""),strict=TRUE)
  }
}
  x <- editGrob(x,label=at.labels, x=label.x, y=label.y, just=just, rot=rot,vp=x$vp, gPath=paste("labels",z,sep=""),strict=TRUE)
    switch (z,
          {
            x<<-editGrob(x,y=unit(-1.5,"char")+label.y,gPath=paste("tag",z,sep=""),vp=x$vp,strict=TRUE)
          },
          {
            x<<-editGrob(x,y=unit(1.5,"char")+label.y,gPath=paste("tag",z,sep=""),vp=x$vp,strict=TRUE)
          },
          {
            x<<-editGrob(x,x=-1*grobWidth(getGrob(x,"labels3",strict=TRUE))+unit(-0.5,"char")+label.x,gPath=paste("tag",z,sep=""),vp=x$vp,strict=TRUE)
          },
          {
            x<<-editGrob(x,x=grobWidth(getGrob(x,"labels4",strict=TRUE))+unit(0.5,"char")+label.x,gPath=paste("tag",z,sep=""),vp=x$vp,strict=TRUE)
          }
          )
  })
  x
}


editDetails.plotarea <- function(x, specs) {
  slot.names <- names(specs)
  if (match("label", slot.names, nomatch=0)) {
    sapply(c(1,2,3,4),function(type){
      x<<-editGrob(x,gPath=paste("tag",type,sep=""),label=x$label[[type]],strict=TRUE)
    })
  }
  if (match("vp", slot.names, nomatch=0)|match("at", slot.names, nomatch=0)|match("inward", slot.names, nomatch=0)|match("grilled", slot.names, nomatch=0)) {
     x <- editTicks(x)
#     if (match("vp", slot.names, nomatch=0)) {
#    x<<-editGrob(x,gPath="border",vp=x$vp,strict=T)
#  }
  }
  x
}

grid.plotarea <- function(inward=FALSE, grilled=FALSE, gp=gpar(lwd=2,fontsize=12,fontface=1),at=c("","Inf","","Inf"),label=c("X Label","","Y Label", ""),draw=TRUE, vp=NULL,title=""){
  Children <- sapply(list(1,2,3,4),function(type){
    Setka <- grid.segments(name=paste("setka",type,sep=""),gp=gpar(lty=2,lwd=1,col="grey"),vp=vp,draw=FALSE)
    Ticks <- grid.segments(name=paste("ticks",type,sep=""),gp=gpar(),vp=vp,draw=FALSE)
    Labels <- textGrob(name=paste("labels",type,sep=""),label="",gp=gpar(),vp=vp,check.overlap=FALSE)
    switch(type,
           {
             Tag <- textGrob(name=paste("tag",type,sep=""),label=label[[type]],y=unit(-1.5,"lines"),gp=gpar(),rot=0,check.overlap=TRUE,just=c("center","top"),vp=vp)
           },
           {
             Tag <- textGrob(name=paste("tag",type,sep=""),label=label[[type]],y=unit(1.5,"lines")+unit(1,"npc"),gp=gpar(),check.overlap=TRUE,rot=0,just=c("center","bottom"),vp=vp)
           },
           {
             Tag <- textGrob(name=paste("tag",type,sep=""),label=label[[type]],x=unit(-3,"lines"),gp=gpar(),check.overlap=TRUE,rot=90,just=c("center","bottom"),vp=vp)
           },
           {
             Tag <- textGrob(name=paste("tag",type,sep=""),label=label[[type]],x=unit(3,"lines")+unit(1,"npc"),gp=gpar(),check.overlap=TRUE,rot=90,just=c("center","top"),vp=vp)
           })
    return(gList(Setka,Ticks,Labels,Tag))
  })
  Children <- c(Children,gList(grid.rect(name="border",gp=gpar(),vp=vp,draw=FALSE)))
  class(Children)<- "gList"
  z <- gTree(at=at, label=label, grilled=grilled,inward=inward, children=Children, cl="plotarea", title=title,vp=vp ,gp=gp)
  z <- editTicks(z)
  sapply(c(1,2,3,4),function(type){
      z<<-editGrob(z,gPath=paste("tag",type,sep=""),vp=vp)
    })
  z<-editGrob(z,gPath="border",vp=vp)
  if(draw){
    grid.draw(z)
  }
  z 

}
##############################################
#Utility functions for drawing nonlinear axis#
##############################################

log10.ticks <- function(range,tick.lab){
  l<- round(range)
  lmajor <- seq(l[1],l[2])
  lminor <- NULL
  if (tick.lab=="tick"){
    return(lmajor)
  } else {
    return(as.character(10^lmajor))
  }
}

loge.ticks <- function(range,tick.lab){
  l<- round(range)*log10(e)
  lmajor <- seq(l[1],l[2])
  if (tick.lab=="tick"){
    return(lmajor)
  } else {
    return(c(as.character(10^lmajor),rep("",times=length(lminor))))
  }
}
