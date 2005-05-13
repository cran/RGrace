print.panel <- function(x,...){
  print(x$border$title)
}

print.figure <- function(x,...){
  print("Figure")
}

print.extplotmath<-function(x,...){
  print(comment(x))
}

BULLET<-function(panel=1,element=1){
  z <- substitute(dead.miner())
  panels<-current.Figure$panels
  if (panel<length(panels)+1){
    els<-panels[[panel]]$elements
    if (element<length(els)+1) {
      el <- els[[element]]
      z<-substitute(bullet(pch=a1,size=a2,gp=a3),list(a1=grid.get(el$name)$pch,a2=unit(grid.get(el$name)$size,"char"),a3=grid.get(el$name)$gp))
           
    } 
  }
                                        #create a function call to be evaluated then bullet is to be redrawn
                               #its comment is a command which was used to invoke this procedure. The comment is used to convert call back to string representation
  comment(z)<-paste("BULLET(panel=",panel,",element=",element,")")
  class(z) <- "extplotmath"
  return(z)
}

figure.back<- function(width=640,height=480,cells=10){

  set.active <- function(...){
    dev.set(DevID)
    current.Figure <<-parent.env(environment())
    class(current.Figure) <<- "figure"
    current.Figure
  }

  
  panel<-function(x=c(2,9),y=c(2,9),scale.X=c(-10,10),scale.Y=c(-10,10),xticks=c("","Inf"),yticks=c("","Inf"),xlab=c("X Label",""),ylab=c("Y Label", ""),ticks.in=TRUE,grill=FALSE,Plabel=paste("Panel",panelsCount),gp=gpar(lwd=2,fontsize=12,fontface=1),GROB=NULL,select=0,update.GUI=TRUE ){
    edit <- function(...,scale.X=NULL,scale.Y=NULL,x=NULL,y=NULL, update.GUI=TRUE){
      if (is.null(scale.X)&is.null(scale.Y)&is.null(x)&is.null(y)){
        grid.edit(border$name,...,redraw=TRUE,strict=TRUE)
      } else {
        v1<-grid.get(border$name)$vp
        if (is.null(scale.X)) {
          scale.X <- v1$xscale 
        }
        if (is.null(scale.Y)) {
          scale.Y <- v1$yscale 
        }
        if (is.null(y)) {
          y <- v1$layout.pos.row
        }
        if (is.null(x)) {
          x <- v1$layout.pos.col
        }
        v1<-viewport(layout.pos.row=y,layout.pos.col=x,xscale=scale.X,yscale=scale.Y,clip=TRUE)
        lapply(elements,function(x){grid.edit(x$name,vp=v1,redraw=FALSE,strict=TRUE)})
        v1$clip <- FALSE
        lapply(annotations,function(x){grid.edit(x$name,vp=v1,redraw=FALSE,strict=TRUE)})
        grid.edit(border$name,vp=v1,...,redraw=TRUE,strict=TRUE)
      }
      if (update.GUI){
        PANEL.CB(parent.env(environment()))
      }
    }
    
    element <- function(y,x=seq(y),...,Elabel=paste(deparse(substitute(y),width.cutoff = 500),deparse(substitute(x),width.cutoff = 500),sep="~"),select=0,rescale=TRUE,update.GUI=TRUE){
      if (select==0){
        set.active()       
        if (rescale){
          autorange(range(x),range(y))
        }
        v1 <- grid.get(border$name,strict=TRUE)$vp
        v1$clip <- TRUE
        z <- grid.data(x=x,y=y,...,title=Elabel,vp=v1)
        elements <<- append(elements,list(z))
        .Selected.Element<- length(elements)
      } else {
        
        z<-elements[[select]]
        .Selected.Element <<- select
      }
      .Index <<- 1
      if (update.GUI){
        try(ELEMENT.CB(parent.env(environment()),rescan=(select==0)))
      }
      return(z)
    }
    
    annotation <- function(...,select=0,update.GUI=TRUE){
      if (select==0){
        set.active()
        z <- grid.annotation(...,vp=grid.get(border$name,strict=TRUE)$vp)
        annotations <<- append(annotations,list(z))
        .Selected.Annotation <<- length(annotations) 
      } else {
        z<-annotations[[select]]
        .Selected.Annotation <<- select
      }
      if (update.GUI){
        try(ANNOTATION.CB(parent.env(environment()),rescan=(select==0)))
      }
      return(z)
    }
    
    autorange <- function( rngx=NULL, rngy=NULL) {    
      sapply(elements,function(z){
        Z <-grid.get(z$name,strict=TRUE)
        rngx<<-range(rngx,Z$x,na.rm=TRUE)
        rngy<<-range(rngy,Z$y,na.rm=TRUE)
      })
      edit(scale.X=rngx,scale.Y=rngy)
    }
    
    delete.element <- function(index=.Selected.Element,update.GUI=TRUE){
      set.active()
      grid.remove(elements[[index]]$name)
      elements[[index]] <<- NULL
      if (index == .Selected.Element ) {
        .Selected.Element<<-NULL
      }
      if (update.GUI){
        try(ELEMENT.CB(parent.env(environment()),rescan=TRUE))
      } 
    }
    
    delete.annot <- function(index=.Selected.Annotation,update.GUI=TRUE){      
      set.active()
      grid.remove(annotations[[index]]$name)
      annotations[[index]] <<- NULL
      if (index == .Selected.Annotation ) {
        .Selected.Annotation <<- NULL
      }
      if (update.GUI){
        try(ANNOTATION.CB(parent.env(environment()),rescan=TRUE))
      }
    }
    
    if (select==0) {
      set.active()
      if (is.null(GROB)){
        vp<-viewport(layout.pos.row=y,layout.pos.col=x,xscale=scale.X,yscale=scale.Y)
        border <- grid.plotarea(inward=ticks.in,grilled=grill,at=c(xticks,yticks),label=c(xlab,ylab),title=Plabel,gp=gp,vp=vp)
      } else {
        if ("plotarea"%in%class(GROB)) {
          border <- GROB
          vp <- GROB$vp
          grid.draw(border)
        }
      }

      panelsCount <<- panelsCount+1
      z<-environment()
      class(z) <- "panel"
      panels<<-append(panels,list(z))
#      current.Panel <<- panels[[length(panels)]]
      rm(xlab,ylab,xticks,yticks,scale.X,scale.Y,vp)
      elements <- list()
      annotations <- list()
      .Selected.Annotation <- 0
      .Selected.Element <- 0
      .Index <- 1
      .Pch<-21
      .Size<-1
      .Fill <- "white"
      .Col <- "black"
      .PointStack <- NULL
      .ZoomStack <- NULL
    } else {
      z <- panels[[select]]
    }
    current.Panel <<-z
    if (update.GUI){
      try(PANEL.CB(current.Panel,rescan=(select==0)))
    }
    return(z)
  }

  delete.panel <- function(index=which(sapply(panels,function(x){identical(x,current.Panel)})),update.GUI=TRUE){
    set.active()
    j <- panels[[index]]
    panels[[index]] <<- NULL
    lapply(j$elements,function(x){
      grid.remove(x$name,redraw=FALSE)
    })
    lapply(j$annotations,function(x){
      grid.remove(x$name,redraw=FALSE)
    })
    grid.remove(j$border$name) 
    if (update.GUI){
      try(PANEL.CB(parent.env(environment()),rescan=TRUE))
    }
  }

  clear <- function(){
    if (length(panels)>0){
      sapply(length(panels):1,function(x){
        delete.panel(x,update.GUI=FALSE)
      })
    }
    try(PANEL.CB(parent.env(environment()),rescan=TRUE))
  }
  
  save <- function(FileSel) {
    set.active()
    cat("",file=FileSel)
    sapply(panels,function(x){
      brd <- grid.get(x$border$name)      
      xpos <- brd$vp[["layout.pos.col"]]
      ypos <- brd$vp[["layout.pos.row"]]
      scX <- brd$vp[["xscale"]]
      scY <- brd$vp[["yscale"]]
      gp1 <- brd$gp
      panel.labels <- serialize(brd$label,connection=NULL,ascii=TRUE)
      dump(c("gp1","panel.labels"),file=FileSel,append=TRUE)
      cat("brd.lab<-unserialize(connection=panel.labels)",file=FileSel,fill=TRUE,append=TRUE)
      cat(paste("XPANEL<-current.Figure$panel(x=c(",paste(xpos,collapse=","),"),y=c(",paste(ypos,collapse=","),"),scale.X=c(",paste(scX,collapse=","),"),scale.Y=c(",paste(scY,collapse=","),"),Plabel=",deparse(brd$title,width.cutoff = 500),",xlab=brd.lab[1:2],ylab=brd.lab[3:4],xticks=c(",paste(deparse(brd$at[[1]],width.cutoff = 500),deparse(brd$at[[2]],width.cutoff = 500),sep=","),"),yticks=c(",paste(deparse(brd$at[[3]],width.cutoff=500),deparse(brd$at[[4]],width.cutoff = 500),sep=","),"),grill=",brd$grilled,",ticks.in=",brd$inward,"gp=gp1,update.GUI=FALSE)",sep=""),file=FileSel,fill=TRUE,append=TRUE)
      els <- x$elements
      lapply(els,function(pnt){
        z <- grid.get(pnt$name)
        x <- z$x
        y <- z$y
        w <- z$w
        h <- z$h
        gp1 <- z$gp
	pch1<-as.integer(z$pch)
        size1<-as.numeric(z$size)
        dump(c("x","y","w","h","gp1","pch1","size1"),append=TRUE,file=FileSel)
        cat(paste("XPANEL$element(x=x,y=y,w=w,h=h,pch=pch1,size=size1,gp=gp1,Elabel=",deparse(pnt$title,width.cutoff = 500),",rescale=FALSE,update.GUI=FALSE)",sep=""),file=FileSel,fill=TRUE,append=TRUE)
      })
      anns <- x$annotations
      lapply(anns,function(xx){
        a <- grid.get(xx$name)
        gp1 <- a$gp
        dump("gp1",file=FileSel,append=TRUE)
        lbl<-serialize(a$label,connection=NULL,ascii=TRUE)
        dump("lbl",file=FileSel,append=TRUE)
        cat("label<-unserialize(connection=lbl)",file=FileSel,fill=TRUE,append=TRUE)
        cat(paste("XPANEL$annotation(label=label,x=",as.numeric(a$x),",y=",as.numeric(a$y),",frame=",a$frame,",rot=",a$rot,",gp=gp1)", sep=""),file=FileSel,fill=TRUE,append=TRUE)
      })           
    })
    cat("current.Figure$PANEL.CB(XPANEL,rescan=TRUE)",file=FileSel,fill=TRUE,append=TRUE)
    cat("current.Figure$ELEMENT.CB(XPANEL,rescan=TRUE)",file=FileSel,fill=TRUE,append=TRUE)	
  }
  
  hardcopy <- function(l=c("PS","PNG","PDF","PDFwrite")){
    set.active()
    switch(l,
           PS=postscript("RPlots.ps"),
           PDF=pdf("RPlots.pdf"),
           PNG=png("RPlots.png"),
           PDFwrite=bitmap("RPlots.pdf",type="pdfwrite",width=11.69,height=8.62)
           )
    print.dev <- dev.cur()[[1]]
    v1<-viewport(w=1,h=1,layout=grid.layout(nrow=cells,ncol=cells),clip=T)
    grid.rect(width=unit(1,"npc")+unit(0.5,"inches"),height=unit(1,"npc")+unit(0.5,"inches"),gp=gpar(col=ps.options()$bg,fill=ps.options()$bg))
    pushViewport(v1)
    lapply(panels,function(z){
      dev.set(DevID)
      b <- grid.get(z$border$name)
      dev.set(print.dev)
      grid.draw(b)
      sapply(c(z$elements,z$annotations),function(x){
        dev.set(DevID)
        b <- grid.get(x$name)
        dev.set(print.dev)
        grid.draw(b)
      })
    })
    dev.off()
    dev.set(DevID)
  }

  kill.panel <- function(...){
    set.active()
    Z <- current.Panel
    ax <- grid.get(Z$border$name)
    d<-lapply(Z$elements,function(x) {
      el <- grid.get(x$name)
    })
    a<-lapply(Z$annotations,function(x) grid.get(x$name))
    .Kill.Structure<<-list("border"=ax,"elements"=d,"annot"=a)
  }
  
  yank <- function(...){
    set.active()
    if ("grob"%in%class(.Kill.Structure)){
      XX <- .Kill.Structure
      current.Panel$element(x=XX$x,y=XX$y,h=XX$h,w=XX$w,pch=XX$pch,size=XX$size,gp=XX$gp,Elabel=.Kill.Structure$title,rescale=FALSE)
    } else {
      Z <- .Kill.Structure$border
      l <- panel(x=Z$vp$layout.pos.col,y=Z$vp$layout.pos.row,scale.X=Z$vp$xscale,scale.Y=Z$vp$yscale,xticks=list(Z$at[[1]],Z$at[[2]]),yticks=list(Z$at[[3]],Z$at[[4]]),xlab=list(Z$label[[1]],Z$label[[2]]),ylab=list(Z$label[[3]],Z$label[[4]]),grill=Z$grilled,ticks.in=Z$inward,Plabel=Z$title)
      vp <- grid.get(l$border$name)$vp     
      vp$clip <- TRUE
      lapply(.Kill.Structure$elements,function(e){
        l$element(x=e$x,y=e$y,h=e$h,w=e$w,pch=e$pch,size=e$size,gp=e$gp,Elabel=e$title)
      })
      vp$clip <- FALSE
      lapply(.Kill.Structure$annot,function(a){
        l$annotation(label=a$label,x=a$x,y=a$y,rot=a$rot,frame=a$frame,gp=a$gp)
      })
    }
  }

  kill.element<-function(...){
    set.active()
    els <- current.Panel$elements
    ind <- current.Panel$.Selected.Element
    .Kill.Structure<<- grid.get(els[[ind]]$name)
  }

  
  apply.text.figure <- function(GP1){
    set.active()
    current.Figure$panels->p
    sapply(p,function(x){
      x$annotations->an
      sapply(an,function(x){
        if(!is.null(gp1 <- grid.get(x$name)$gp)){ GP1$col <- gp1$col }
        GP1$col <- gp1$col
        grid.edit(x$name,gp=GP1,redraw=FALSE)
      })
      grid.edit(x$border$name,gp=GP1)
#      sapply(c(1,2,3,4),function(x){
#        grid.edit(gPath(.ax$name,paste("tag",x,sep="")),gp=GP1,redraw=(x==4))
#      })
      
    })    
  }
  
  apply.text.panel <- function(GP1){
    set.active()
    current.Panel$annotations->an
    current.Panel$border ->.ax
    sapply(an,function(x){
      if(!is.null(gp1 <- grid.get(x$name)$gp)){ GP1$col <- gp1$col }        
      grid.edit(x$name,gp=GP1,redraw=FALSE)
    })
#    sapply(c(1,2,3,4),function(x){
#      grid.edit(gPath(.ax$name,paste("tag",x,sep="")),gp=GP1,redraw=FALSE)
#      grid.edit(gPath(.ax$name,paste("labels",x,sep="")),gp=GP1,redraw=(x==4))
#    })
    grid.edit(current.Panel$border$name,gp=GP1)
  }

  newlegend <- function(...){
    set.active()
    z <- which(sapply(panels,function(x){identical(x,current.Panel)}))
    els <- current.Panel$elements
    k<-1:length(els)
    legend<-NULL
    sapply(k,function(x){
      legend<<-append(legend,BULLET(z,x))
      legend<<-append(legend,els[[x]]$title)
    })
    current.Panel$annotation(label=legend)
  }

  PANEL.CB <- function(...){ }

  ANNOTATION.CB <- function(...){ }

  ELEMENT.CB <- function(...){ }

#  width <- width
#  height <- height
  cells <- cells
  DevID <- dev.cur()[[1]]
  Scale <- c(width,height)/cells
  panelsCount <- 1
  current.Panel <- NULL
  panels <- list()
  engine.display.list(FALSE)
  dev.control("inhibit")
  set.active()
  VP <- viewport(w=1,h=1,layout=grid.layout(nrow=cells,ncol=cells))
  pushViewport(VP)
  return(current.Figure)

}
