if ((R.version$minor>0.1)&(R.version$major>=2)){
  .draw.grid.text<-function(xx,xpos,ypos,just,rot,overlap){
    .Call.graphics("L_text", xx, xpos, ypos,grid:::resolveHJust(just,NULL),grid:::resolveVJust(just,NULL), 0, overlap,PACKAGE="grid")
  }
} else {
  .draw.grid.text<-function(xx,xpos,ypos,just,rot,overlap){
    .Call.graphics("L_text", xx, xpos, ypos,just, 0, overlap,PACKAGE="grid")
  }
}	


preDrawDetails.annotation <- function(x) {
  v1 <- viewport(angle=x$rot+360,w=unit(0.1,"char"),h=unit(0.1,"char"),x=unit(x$x,"native"),y=unit(x$y,"native"),clip=FALSE)
  pushViewport(v1)}

postDrawDetails.annotation <- function(x){
   popViewport()
}

#validGrobDetails.annotation <- function(x){
#  x
#}

drawDetails.annotation <- function(x,recording){
    bullet <- function(pch=as.integer(26), size=unit(1, "char"), gp=gpar(col="black",fill="white",lty=1,lwd=1)){
    grid:::set.gpar(gp)
    .Call.graphics("L_segments",xpos,ypos-0.5*size,xpos+2*size,ypos-0.5*size,PACKAGE="grid")
    gp$lty<-1
    grid:::set.gpar(gp)
    .Call.graphics("L_points", xpos+size, ypos-0.5*size, pch, size,PACKAGE="grid")
    return(list(2*size,size))
  }
  dead.miner <- function(){
    ly<-0.2
    lx<-0.25
    gp=gpar(col="black",fill="white",lty=1,lwd=1)
    grid:::set.gpar(gp)
    xpos <- xpos+unit(0.5,"char")
    ypos <- ypos-unit(1,"char")
    try(.Call.graphics("L_circle",xpos+unit(lx*1,"char"),ypos+unit(lx*1,"char"),abs(unit(0.75,"char")),PACKAGE="grid"))
    try(.Call.graphics("L_segments",xpos+unit(lx*c(-0.5,-0.5,1.5,1.5),"char"),ypos+unit(ly*c(3,2,2,3),"char"),xpos+unit(lx*c(0.5,0.5,2.5,2.5),"char"),ypos+unit(ly*c(2,3,3,2),"char"),PACKAGE="grid"))
    try(.Call.graphics("L_lines",xpos+unit(lx*c(1.25,1.25,1.5,1.75,2,2),"char"),ypos+unit(ly*c(-0.325,-1,-1.75,-1.75,-1,-1),"char"),PACKAGE="grid"))
    try(.Call.graphics("L_lines",xpos+unit(lx*c(0,0.5,1,1.5,2),"char"),ypos+unit(ly*c(-1,-0.5,-0.25,-0.5,-1),"char"),PACKAGE="grid"))
    
     return(list(unit(1.5,"char"),unit(1.5,"char")))
  }
  ypos <- unit(0.5,"native")
  xwidth <- 0
  heighta <- -0.5
  width.b<-0
  height.b<-0
  lapply(x$label,function(xx){
    if (!inherits(xx,"extplotmath")){
      heighta <<- heighta+0.5+max(height.b,convertHeight(unit(1,"mystrheight",xx),"char",valueOnly=TRUE))
      xwidth<<-max(xwidth,width.b+convertWidth(unit(1,"mystrwidth",xx),"char",valueOnly=TRUE))
    }
    ypos<<- -1*unit((heighta+if(inherits(xx,"extplotmath")){ 0.5 } else { 0 }),"char")
    xpos<<- unit(width.b,"char")
    if (inherits(xx,"extplotmath")) {
      gp1 <- x$gp
      height.width<-eval(xx)
      grid:::set.gpar(gp1)
      width.b<<-width.b+convertWidth(height.width[[1]],"char",valueOnly=TRUE)+1
      height.b<<-max(height.b,convertWidth(height.width[[2]],"char",valueOnly=TRUE))
    } else {
	.draw.grid.text(xx, xpos, ypos,x$just, 0, x$check.overlap)
#      .Call.graphics("L_text", xx, xpos, ypos,x$just, 0, x$check.overlap,PACKAGE="grid")
      height.b<<-0
      width.b<<-0
    } 
  })
  
  if (x$frame) {
    .Call.graphics("L_lines",unit.c(unit(-0.5,"mychar"),unit(-0.5,"mychar"),unit(-0.5,"mychar")+unit(xwidth+1,"mychar"),unit(-0.5,"mychar")+unit(xwidth+1,"mychar"),unit(-0.5,"mychar")),unit.c(unit(0.5,"mychar"),unit(0.5,"mychar")+unit(-heighta-1,"mychar"),unit(0.5,"mychar")+unit(-heighta-1,"mychar"),unit(0.5,"mychar"),unit(0.5,"mychar")),PACKAGE="grid")
  }
}


grid.annotation <- function(x=vp$xscale[[1]],y=vp$yscale[[2]],label="",just=c("left","bottom"),rot=0,frame=FALSE,gp=gpar(lwd=2),check.overlap=TRUE,vp=viewport(),draw=TRUE){
  z <- grob(label=label, x=x, y=y, frame=frame, rot=rot, check.overlap=check.overlap,just=just, gp=gp, vp=vp, cl="annotation", draw=FALSE)
  grid.draw(z)
  z
}


