validGrobDetails.border <- function(x){
  x
}

drawDetails.border <- function(x, recording) {
#if (!is.null(x$vp)){
  .Call.graphics("L_segments",unit(0,"npc"),unit(0,"npc"),unit(0,"npc"),unit(1,"npc"),PACKAGE="grid")
  .Call.graphics("L_segments",unit(1,"npc"),unit(0,"npc"),unit(1,"npc"),unit(1,"npc"),PACKAGE="grid")
  .Call.graphics("L_segments",unit(0,"npc"),unit(0,"npc"),unit(1,"npc"),unit(0,"npc"),PACKAGE="grid")
  .Call.graphics("L_segments",unit(0,"npc"),unit(1,"npc"),unit(1,"npc"),unit(1,"npc"),PACKAGE="grid")
#}
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


drawDetails.data <- function(x, recording) {
  if (!is.null(x$vp)){
    .Call.graphics("L_lines", unit(x$x,"native"), unit(x$y,"native"),PACKAGE="grid")
    gp1 <- NULL
    if (!is.null(x$gp)) {
      gp1<-x$gp
      gp1$lty<-1
      grid:::set.gpar(gp1)
    }

    if (!is.null(x$h)){
      z1 <- unit(x$y+x$h/2,"native")
      z2 <- unit(x$y-x$h/2,"native")
      h1 <- unit(x$x,"native")+unit(as.numeric(x$size)/2,"char")
      h2 <- unit(x$x,"native")-unit(as.numeric(x$size)/2,"char")
      .Call.graphics("L_segments",unit(x$x,"native"),z1,unit(x$x,"native"),z2,PACKAGE="grid")
      .Call.graphics("L_segments",h1,z1,h2,z1,PACKAGE="grid")
      .Call.graphics("L_segments",h1,z2,h2,z2,PACKAGE="grid")
    }
    if (!is.null(x$w)){
      z1 <- unit(x$x+x$w/2,"native")
      z2 <- unit(x$x-x$w/2,"native")
      h1 <- unit(x$y,"native")+unit(as.numeric(x$size)/2,"char")
      h2 <- unit(x$y,"native")-unit(as.numeric(x$size)/2,"char")
      .Call.graphics("L_segments",z1,unit(x$y,"native"),z2,unit(x$y,"native"),PACKAGE="grid")
      .Call.graphics("L_segments",z1,h1,z1,h2,PACKAGE="grid")
      .Call.graphics("L_segments",z2,h1,z2,h2,PACKAGE="grid")
    }
    .Call.graphics("L_points", unit(x$x,"native"), unit(x$y,"native"), x$pch, unit(x$size,"char"),PACKAGE="grid")
    if (!is.null(gp1)) {
      grid:::set.gpar(x$gp)
    }
}
}

grid.data <- function(x=c(0, 1),y=c(0, 1), w=NULL, h=NULL, pch=as.integer(26), size=unit(1, "char"), gp=gpar(col="black",fill="white",lty=1,lwd=1), title="", draw=TRUE, vp=NULL) {
  cl <- "data"
  z <- grob(title=title, x=x, y=y, h=h, w=w, pch=as.integer(pch), size=size, cl=cl, gp=gp, vp=vp)
  if (draw){
      grid.draw(z)
  }
  z
}



################################################
#Scale grob - tick marks, tick labels and title#
################################################

drawDetails.scale <- function(x,recording=TRUE) {
# switch(x$type,
#         l={
#           .Call.graphics("L_segments",unit(0,"npc"),unit(0,"npc"),unit(0,"npc"),unit(1,"npc"))
#         },
#         r={
#           .Call.graphics("L_segments",unit(1,"npc"),unit(0,"npc"),unit(1,"npc"),unit(1,"npc"))
#         },
#         b={
#           .Call.graphics("L_segments",unit(0,"npc"),unit(0,"npc"),unit(1,"npc"),unit(0,"npc"))
#         },
#         t={
#           .Call.graphics("L_segments",unit(0,"npc"),unit(1,"npc"),unit(1,"npc"),unit(1,"npc"))
#         })
#  grid.draw(x$children$ticks, recording=F)
#  if (x$grilled) {
#    grid.draw(x$children$setka,recording=FALSE)
#    print(x$grilled)
#  }
##  grid.draw(x$children$tag,recording=F)
#  if (!is.null(x$children$labels))
#    grid.draw(x$children$labels, recording=FALSE)
}

editScaleProp <- function(x){
  at.exp <- parse(text=x$at)
  if (x$type%in%c("b","t")){
    range <- x$vp$xscale
  } else {
    range <- x$vp$yscale
  }
  if (!is.null(range)) {
    at.ticks <- eval(at.exp,list(RANGE=range,TICK.LAB="tick"))
    if (is.null(at.ticks)) { at.ticks <- grid.pretty(range) }
    at.labels <- eval(at.exp,list(RANGE=range,TICK.LAB="label"))
    if (is.null(at.labels)) { at.labels <- as.character(at.ticks)}
  } else {
    at.ticks <- Inf
    at.labels <- "Inf"
  }
  if (!x$inward){
    dir <- 1
    offset <- 0
  } else {
    dir <- -1
    offset <- 0.5
  }
  at.ticks <- unit(at.ticks,"native")
  switch(x$type,
         b={            
           tick.y0 <- unit(0, "npc")
           tick.y1 <- unit(-.5*dir, "lines")
           tick.x0 <- at.ticks
           tick.x1 <- at.ticks
           just <- "center"
           rot <- 0
           label.x <- at.ticks
           label.y <- unit(-1+offset, "lines")            
         },
         t={
           tick.y0 <- unit(1, "npc")
           tick.y1 <- unit(1, "npc") + unit(.5*dir, "lines")
           tick.x0 <- at.ticks
           tick.x1 <- at.ticks
           just <- "center"
           rot <- 0
           label.x <- at.ticks
           label.y <- unit(1, "npc") + unit(1-offset, "lines")
         },
         l= {
           tick.y0 <- at.ticks
           tick.y1 <- at.ticks
           tick.x0 <- unit(0, "npc")
           tick.x1 <- unit(-.5*dir, "lines")
           just <- "right"
           rot <- 0
           label.y <- at.ticks
           label.x <- unit(-1+offset, "lines")
         },
         r={
           tick.y0 <- at.ticks
           tick.y1 <- at.ticks
           tick.x0 <- unit(1, "npc")
           tick.x1 <- unit(1, "npc") + unit(.5*dir, "lines")
           just <- "left"
           rot <- 0
           label.y <- at.ticks
           label.x <- unit(1, "npc") + unit(1-offset, "lines") 
         }
         )
  x <- editGrob(x,x0=tick.x0,y0=tick.y0,x1=tick.x1,y1=tick.y1,vp=x$vp,gPath="ticks")
  if (!x$grilled) {
    x <- editGrob(x,x0=unit(Inf,"npc"),y0=unit(Inf,"npc"),x1=unit(Inf,"npc"),y1=unit(Inf,"npc"),vp=x$vp,gPath="setka")
  } else {
  if(x$type%in%c("t","b")){
    x <- editGrob(x,x0=tick.x0,y0=unit(0,"npc"),x1=tick.x0,y1=unit(1,"npc"),vp=x$vp,gPath="setka")
  } else {
    x <- editGrob(x,x0=unit(0,"npc"),y0=tick.y0,x1=unit(1,"npc"),y1=tick.y0,vp=x$vp,gPath="setka")
  }
}
  x <- editGrob(x,label=as.character(at.labels), x=label.x, y=label.y, just=just, rot=rot,vp=x$vp, gPath="labels")

  x
}

validGrobDetails.scale <- function(x){
  x
}

editDetails.scale <- function(x, specs) {
  slot.names <- names(specs)
  if (match("vp", slot.names, nomatch=0)|match("at", slot.names, nomatch=0)) {
    x <- editScaleProp(x)
    x <- editGrob(x,vp=x$vp,gPath="tag")
  }
  x
}

grid.scale <- function(at="", legend="", type="b", inward=FALSE, grilled=FALSE, gp=gpar(lwd=2,fontsize=12,fontface=1), draw=TRUE, vp=NULL) {
  if (is.null(legend)){
    switch(type,
           b={legend="X Label"},
           t={legend=""},
           l={legend="Y Label"},
           r={legend=""}
         )
  }
  setka <- grid.segments(name="setka",gp=gpar(lty=2,lwd=1,col="grey"),vp=vp,draw=FALSE)
  ticks <- grid.segments(name="ticks",gp=gp,draw=FALSE)
  labels <- textGrob(name="labels",label="",check.overlap=TRUE)
  switch(type,
         l={
           tag <- grid.text(name="tag",label=legend,x=unit(-3,"lines"),gp=gp,check.overlap=TRUE,rot=90,just=c("right","center"))
         },
         r={
           tag <- grid.text(name="tag",label=legend,x=unit(3,"lines")+unit(1,"npc"),gp=gp,check.overlap=TRUE,rot=90,just=c("left","center"))
         },
         b={
           tag <- textGrob(name="tag",label=legend,y=unit(-1.5,"lines"),gp=gp,rot=0,check.overlap=TRUE,just=c("center","top"))
         },
         t={
           tag <- textGrob(name="tag",label=legend,y=unit(1.5,"lines")+unit(1,"npc"),gp=gp,check.overlap=TRUE,rot=0,just=c("center","bottom"))
         })
  
  z <- gTree(at=at,name=NULL,grilled=grilled,inward=inward,  type=type,children=gList(setka, ticks, labels, tag), cl="scale", vp=vp ,gp=gp)
  z <- editScaleProp(z)
  grid.draw(z)
  z
}

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
      if (is.null(at.labels)) { at.labels <- as.character(at.ticks)}
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
  x <<- editGrob(x,x0=tick.x0,y0=tick.y0,x1=tick.x1,y1=tick.y1,vp=x$vp,gPath=paste("ticks",z,sep=""),strict=TRUE)
  if (!x$grilled) {
    x <<- editGrob(x,x0=unit(Inf,"npc"),y0=unit(0,"npc"),x1=unit(Inf,"npc"),y1=unit(1,"npc"),vp=x$vp,gPath=paste("setka",z,sep=""),strict=TRUE)
  } else {
  if(z%in%c(1,2)){
    x <<- editGrob(x,x0=tick.x0,y0=unit(0,"npc"),x1=tick.x0,y1=unit(1,"npc"),vp=x$vp,gPath=paste("setka",z,sep=""),strict=TRUE)
  } else {
    x <<- editGrob(x,x0=unit(0,"npc"),y0=tick.y0,x1=unit(1,"npc"),y1=tick.y0,vp=x$vp,gPath=paste("setka",z,sep=""),strict=TRUE)
  }
}
  x <<- editGrob(x,label=as.character(at.labels), x=label.x, y=label.y, just=just, rot=rot,vp=x$vp, gPath=paste("labels",z,sep=""),strict=TRUE)
    switch (z,
          {
            x<<-editGrob(x,y=unit(-1.5,"char")+label.y,gPath=paste("tag",z,sep=""),vp=x$vp,strict=TRUE)
          },
          {
            x<<-editGrob(x,y=unit(1.5,"char")+label.y,gPath=paste("tag",z,sep=""),vp=x$vp,strict=TRUE)
          },
          {
            x<<-editGrob(x,x=unit(-1,"strwidth",as.character(at.labels))+unit(-1.5,"char")+label.x,gPath=paste("tag",z,sep=""),vp=x$vp,strict=TRUE)
          },
          {
            x<<-editGrob(x,x=unit(1,"strwidth",as.character(at.labels))+unit(1.5,"char")+label.x,gPath=paste("tag",z,sep=""),vp=x$vp,strict=TRUE)
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
