.onLoad <- function(lib,pkg){
                                        #.First.lib <- function(lib, pkg) {
#  library.dynam("RGrace", pkg, lib)
  require(RGtk)
  require(gtkDevice)
  current.Figure <<-NULL
  .Kill.Structure<<-NULL
  gtkSetLocale()
  require(grid)
  if (R.version$major>=2){
    require(grDevices)
  } else {
    require(graphics)
  }
  ps.options(width=1000,height=500,horizontal=TRUE,paper="a4")
  default.hooks()  
  figure()

}


  .Line.Names<-c("Line:No",
                  "Line:Solid",
                  "Line:Dashed",
                  "Line:Dotted",
                  "Line:Dash-dotted",
                  "Line:Long-dashed",
                  "Line:Short-long-dashed")

  .Symbol.Names<-c("Symbol: Open Square",
                    "Symbol: Open Circle",
                    "Symbol: Open UpTriangle",
                    "Symbol: +",
                    "Symbol: x",
                    "Symbol: Open Diamond",
                    "Symbol: Open DownTriangle",
                    "Symbol: Crossed Square",
                    "Symbol: *",
                    "Symbol: Crossed Diamond",
                    "Symbol: Crossed Circle",
                    "Symbol: Hexagramme",
                    "Symbol: Window",
                    "Symbol: Basketball",
                    "Symbol: Envelope",
                    "Symbol: Filled Square",
                    "Symbol: Filled Circle",
                    "Symbol: Filled UpTriangle",
                    "Symbol: Filled Diamond",
                    "Symbol: Filled Circle",
                    "Symbol: Dot",
                    "Symbol: Circle",
                    "Symbol: Square",
                    "Symbol: Diamond",
                    "Symbol: UpTriangle",
                    "Symbol: DownTriangle",
                    "Symbol: No"
                    )

.Face.Names <- c("plain",
                 "bold",
                 "italic",
                 "bold.italic",
                 "default")

.CS.INFO <- c("All Points",
                 "Selected Point",
                 "Points with the Same Values"
                 )


nearestPanel <- function(){
  if (!is.null(current.Figure)){
    nearest.panel <- current.Figure$current.Panel
    if (is.null(nearest.panel))
      nearest.panel <- current.Figure$panel()
  } else {
    nearest.panel <- figure()$panel()
  }

  
  return(nearest.panel)
}

ggplot <- function(...,panel=nearestPanel()) {
  return(panel$element(...))
}




.GTK.interface<- function(width=400,height=400,cells=10){
#  cells<-cells
#  width<-width
#  height<-height
  hPanedPos <- FALSE
  PlaceNewAnnotation <- FALSE
  FileSel <- NULL
  .Click.Inside.Area <- FALSE
  Interaction.Type <- "select.element1"

  rescan.elements <- function(panel){
    strings <- sapply(panel$elements,function(i){ i$title})
    if (!is.null(as.pairlist(strings))) {
      elements.w$SetPopdownStrings(strings)
    } else {
      elements.w$GetList()$ClearItems(0,-1)
    }

  }

  rescan.panels <- function(){
    strings<-sapply(fig$panels,function(i){
      if(!is.null(i)){
        vp<-grid.get(i$border$name)$vp
        paste(i$border$title," x=",paste(vp$layout.pos.col,collapse=" ")," y=",paste(vp$layout.pos.row,collapse=" "),collapse="")
      }
    })
    if (!is.null(as.pairlist(strings))) {
      combo1.w$SetPopdownStrings(strings)
    } else {
      combo1.w$GetList()$ClearItems(0,-1)
    }
  }


  element.GUI <- function(panel,rescan=FALSE) {
    if (rescan) {
      rescan.elements(panel)
    } else {
      index.w$SetAdjustment(gtkAdjustment(value=0,lower=1,upper=length(grid.get(panel$elements[[panel$.Selected.Element]]$name)$x),step.increment=1,page.increment=10,page.size=10))
      index.w$SetValue(panel$.Index)
    }
  }

  panel.GUI <- function(panel,rescan=FALSE){
    if (rescan) {
      rescan.panels()
    } else {
      Z <- grid.get(fig$current.Panel$border$name,strict=TRUE)
      vp <- Z$vp
      spinpanelx.w$SetValue(vp$layout.pos.col[1])
      spinpanely.w$SetValue(vp$layout.pos.row[1])
      spinpanelw.w$SetValue(vp$layout.pos.col[2]-vp$layout.pos.col[1])
      spinpanelh.w$SetValue(vp$layout.pos.row[2]-vp$layout.pos.row[1])
      apply(cbind(c(vp$xscale,vp$yscale),c("xrang1.w","xrang2.w","yrang1.w","yrang2.w")),1,function(x){
        get(x[2])$SetText(as.character(x[1]))
      })
      grill.w$SetActive(Z$grilled)
      ticksdir.w$SetActive(Z$inward)
      lapply(list(list(x1.w,x1label.w,1),list(x2.w,x2label.w,2),list(y1.w,y1label.w,3),list(y2.w,y2label.w,4)),function(x){
        x[[1]]$SetText(Z$at[x[[3]]])
        x[[2]]$SetText(deparse(Z$label[[x[[3]]]],width.cutoff=500))
      })
      rescan.elements(fig$current.Panel)
    }
  }
  
  annotation.GUI <- function(panel,rescan=FALSE){
    if (!is.null(panel$.Selected.Annotation)){
      grob <- panel$annotations[[panel$.Selected.Annotation]]
      annot.text.w$DeleteText(start=0,end=-1)
      annot.text.w$Insert(chars=paste(lapply(grid.get(grob$name)$label,function(x){if (inherits(x,"extplotmath")) { x} else { deparse(x,width.cutoff = 500)}} ),collapse="\n"))
      rotatetext.w$SetValue(grid.get(grob$name)$rot)
      text.frame.w$SetActive(grid.get(grob$name)$frame)
      gp1 <- grid.get(grob$name)$gp
      text.color.w$GetEntry()$SetText(ifelse(is.null(gp1$col),"default",gp1$col))
      text.face.w$GetEntry()$SetText(ifelse(is.null(gp1$fontface),"default",gp1$fontface))
      text.points.w$SetValue(ifelse(is.null(gp1$fontsize),0,gp1$fontsize))
    }
  }
  

  on.change.index <- function(...){
    fig$current.Panel$.Index <<- index.w$GetValueAsInt()
    c <-grid.get(fig$current.Panel$elements[[fig$current.Panel$.Selected.Element]]$name,strict=TRUE) 
    fig$current.Panel$.Pch<<-c$pch[[(fig$current.Panel$.Index-1)%%length(c$pch)+1]]
    symmenu.w$GetList()$SelectItem(fig$current.Panel$.Pch)
    fig$current.Panel$.Size <<-c$size[[(fig$current.Panel$.Index-1)%%length(c$size)+1]]
    symsize.w$SetValue(as.numeric(fig$current.Panel$.Size))
    z <- c$gp
    if (is.null(z$fill)){z$fill <- "default"}
    if (is.null(z$col)){z$col <- "default"}
    if (is.null(z$lwd)){z$lwd <- 0}
    if (is.null(z$lty)){z$lty <- 0}
    fig$current.Panel$.Fill<<-z$fill[[(fig$current.Panel$.Index-1)%%length(z$fill)+1]]
    fillcol.w$GetEntry()$SetText(fig$current.Panel$.Fill)
    linemenu.w$GetList()$SelectItem(z$lty)
    linsize.w$SetValue(z$lwd)
    fig$current.Panel$.Col<<-z$col[[(fig$current.Panel$.Index-1)%%length(z$col)+1]]
    linecol.w$GetEntry()$SetText(fig$current.Panel$.Col)
    xdata.w$SetText(as.character(c$x[[fig$current.Panel$.Index]]))
    ydata.w$SetText(as.character(c$y[[fig$current.Panel$.Index]]))
    wdata.w$SetText(ifelse(is.null(f <- c$w[[(fig$current.Panel$.Index-1)%%length(c$w)+1]]),"",as.character(f)))
    hdata.w$SetText(ifelse(is.null(f <- c$h[[(fig$current.Panel$.Index-1)%%length(c$h)+1]]),"",as.character(f)))
  }

  stroke <- function(win,arg){
    if (.Click.Inside.Area){
      .C("draw_rubber_box",win,as.integer(xb),as.integer(yb),PACKAGE="RGrace")
      xb<<-as.double(gdkEventMotionGetX(arg))
      yb<<-as.double(gdkEventMotionGetY(arg))
      .C("draw_rubber_box",win,as.integer(xb),as.integer(yb),PACKAGE="RGrace")
    }
  }

  hit.key <- function(win,arg){
    fig$set.active()
    z <- gdkEventKeyGetKeyval(arg)
    switch(Interaction.Type,
           select.points1={
             if (z==65293) {
                                        #Enter
               try(on.select.points(NULL,NULL,NULL,fig$current.Panel$.PointStack))
               fig$current.Panel$.PointStack <<- NULL
             }
             if (z==65307){
                                        #Esc
               if (length(fig$current.Panel$.PointStack)==0){
                 fig$current.Panel$PointStack<<-matrix(fig$current.Panel$.PointStack[-1,],ncol=2)
                 try(on.select.points(NULL,NULL,-1,fig$current.Panel$.PointStack))
               }
             }               
           },
           select.zoom1={
             if (z==65307){
                                        #Esc
               if (length(fig$current.Panel$.ZoomStack)!=0){
                 a <- fig$current.Panel$.ZoomStack[1,]
                 fig$current.Panel$.ZoomStack<<-matrix(fig$current.Panel$.ZoomStack[-1,],ncol=4)                 
                 apply(cbind(a,c("xrang1.w","xrang2.w","yrang1.w","yrang2.w")),1,function(x){get(x[2])$SetText(as.character(x[1]))})
                 fig$current.Panel$edit(scale.X=a[1:2],scale.Y=a[3:4])
               }                  
             }
           })
  }

  start.stroke<-function(win, arg) {
    .Click.Inside.Area<<-TRUE
    fig$set.active()
    xa<<-as.double(gdkEventButtonGetX(arg))
    ya<<-as.double(gdkEventButtonGetY(arg))
    xb <<- xa+1
    yb <<- ya+1
    .C("draw_rubber_box",win,as.integer(xa),as.integer(ya),PACKAGE="RGrace")
    .C("draw_rubber_box",win,as.integer(xb),as.integer(yb),PACKAGE="RGrace")
  }

  end.stroke<-function(win, arg) {
    if(!.Click.Inside.Area) return(NULL)
    .Click.Inside.Area <<- FALSE 
    .C("draw_rubber_box",win,as.integer(xb),as.integer(yb),PACKAGE="RGrace")
    .C("draw_rubber_box",win,as.integer(xa),as.integer(ya),PACKAGE="RGrace")
    xb<-as.double(gdkEventButtonGetX(arg))
    yb<-as.double(gdkEventButtonGetY(arg))
    vp<-grid.get(fig$current.Panel$border$name,strict=TRUE)$vp
    xx<-(c(xa,xb)/fig$Scale[1]-vp$layout.pos.col[1]+1)*(vp$xscale[2]-vp$xscale[1])/(vp$layout.pos.col[2]-vp$layout.pos.col[1]+1)+vp$xscale[1]
    yy<-(-c(ya,yb)/fig$Scale[2]+vp$layout.pos.row[2])*(vp$yscale[2]-vp$yscale[1])/(vp$layout.pos.row[2]-vp$layout.pos.row[1]+1)+vp$yscale[1]
    if (PlaceNewAnnotation) {
      new.annot.w$SetActive(FALSE)
      PlaceNewAnnotation <<- FALSE        
      fig$current.Panel$annotation(label=eval(parse(text=paste("list(",gsub("\n",",",annot.text.w$GetChars(start=0,end=-1)),")",sep=""))),x=xx[1],y=yy[1],rot=rotatetext.w$GetValueAsInt(),frame=as.numeric(text.frame.w$GetActive()),gp=annot.get.gp.GUI(),update.GUI=TRUE)
      return()
    }
    switch(Interaction.Type,
           select.element1={
             l1 <- sapply(fig$current.Panel$elements,function(el){
               hh <- (as.numeric(grid.get(el$name)$x)-xx[1])^2/(vp$xscale[2]-vp$xscale[1])^2+(as.numeric(grid.get(el$name)$y)-yy[1])^2/(vp$yscale[2]-vp$yscale[1])^2
               index <- which.min(hh)
               list(index=index,value=hh[index])
             })
             indx<- which.min(l1["value",])
             if (l1["value",indx]>0.02) {return()}
             fig$current.Panel$.Selected.Element <<- indx
             elements.w$GetList()$SelectItem(fig$current.Panel$.Selected.Element-1)
             fig$current.Panel$.Index <<- l1["index",indx][[1]]
             index.w$SetValue(fig$current.Panel$.Index)
             dataline <- fig$current.Panel$elements[[fig$current.Panel$.Selected.Element]]
             try(on.select.element(rbind(xx,yy),dataline,fig$current.Panel$.Index,cbind(as.numeric(grid.get(dataline$name)$x),as.numeric(grid.get(dataline$name)$y))))               
           },
           select.points1={
             win$GrabFocus()
             fig$current.Panel$.PointStack <<- rbind(c(xx[2],yy[2]),fig$current.Panel$.PointStack)
             try(on.select.points(c(xx[2],yy[2]),NULL,NULL,fig$current.Panel$.PointStack))
           },
           select.annotation1={
             l1 <- sapply(fig$current.Panel$annotations,function(ann){
               (as.numeric(grid.get(ann$name)$x)-xx[1])^2/(vp$xscale[2]-vp$xscale[1])^2+(as.numeric(grid.get(ann$name)$y)-yy[1])^2/(vp$yscale[2]-vp$yscale[1])^2
             })
             indx <- which.min(l1)
             if (l1[[indx]]>0.02) {return()}
             fig$current.Panel$annotation(select=indx)
             if (!is.null(fig$current.Panel$.Selected.Annotation)){
               try(on.select.annotation(rbind(xx,yy),fig$current.Panel$annotations[[fig$current.Panel$.Selected.Annotation]],NULL,NULL))
             }
           },
           select.region1={
             dataline <- fig$current.Panel$elements[[fig$current.Panel$.Selected.Element]]
             xlinedata <- as.numeric(grid.get(dataline$name)$x)
             ylinedata <- as.numeric(grid.get(dataline$name)$y)
             indexes <- which((xlinedata<max(xx))&(xlinedata>min(xx))&(ylinedata<max(yy))&(ylinedata>min(yy)))
             fig$current.Panel$.Index<<-indexes[1]
             index.w$SetValue(fig$current.Panel$.Index)
             try(on.select.region(rbind(c(min(xx),max(xx)),c(min(yy),max(yy))),dataline,indexes,cbind(xlinedata[indexes],ylinedata[indexes])))
           },
           select.zoom1={
             win$GrabFocus()
             fig$current.Panel$.ZoomStack<<-rbind(c(vp$xscale,vp$yscale),fig$current.Panel$.ZoomStack)
             apply(cbind(c(sort(xx),sort(yy)),c("xrang1.w","xrang2.w","yrang1.w","yrang2.w")),1,function(x){get(x[2])$SetText(as.character(x[1]))})
             fig$current.Panel$edit(scale.X=c(min(xx),max(xx)),scale.Y=c(min(yy),max(yy)))
           }
           )
  }


  autorange <- function(...) {
    fig$current.Panel$autorange()
  }

  on.button1.clicked<-function(...) {
    x <- spinpanelx.w$GetValueAsInt()
    y <- spinpanely.w$GetValueAsInt()
    xa <- spinpanelw.w$GetValueAsInt()+x
    ya <- spinpanelh.w$GetValueAsInt()+y
    rang <- sapply(c("xrang1.w","xrang2.w","yrang1.w","yrang2.w"),function(x){
      if(is.na(j <- as.double(get(x)$GetText()))) NULL else j
    })
    fig$current.Panel$edit(label=list(eval(parse(text=x1label.w$GetText())),eval(parse(text=x2label.w$GetText())),eval(parse(text=y1label.w$GetText())),eval(parse(text=y2label.w$GetText()))),at=list(x1.w$GetText(),x2.w$GetText(),y1.w$GetText(),y2.w$GetText()),grilled=grill.w$GetActive(),inward=ticksdir.w$GetActive(),y=c(y,ya),x=c(x,xa),scale.X=rang[1:2],scale.Y=rang[3:4])
  }

   on.sel.element<-function(List, Item) {
    fig$current.Panel$element(select=List$ChildPosition(Item)+1)
  }

  on.delete.element <- function(...){
    fig$current.Panel$delete.element()
  }
    
  on.apply.annot <- function(...) {
    fig$set.active()
    if (!is.null(fig$current.Panel$.Selected.Annotation)) {
      grid.edit(fig$current.Panel$annotations[[fig$current.Panel$.Selected.Annotation]]$name,label=eval(parse(text=paste("list(",gsub("\n",",",annot.text.w$GetChars(start=0,end=-1)),")",sep=""))),rot=rotatetext.w$GetValueAsInt(),frame=text.frame.w$GetActive(),gp=annot.get.gp.GUI())
    }
  }

  on.delete.annot <- function(...){
    fig$current.Panel$delete.annot()
  }

  do.apply.element <- function(...){
    try(fig$set.active())
    points <- fig$current.Panel$elements[[fig$current.Panel$.Selected.Element]]
    .Index <- fig$current.Panel$.Index
    c <- grid.get(points$name,strict=TRUE)
    pch <- c$pch
    size <- c$size
    GP <- c$gp

    switch(which(.CS.INFO==cs.info.w$GetEntry()$GetText()),
           {
             index <- seq(along=pch)
             index1 <- seq(along=size)
             index2 <- seq(along=GP$col)
             index3 <- seq(along=GP$fill)
           },
           {
             index <- (.Index-1)%%length(pch)+1
             index1 <- (.Index-1)%%length(size)+1
             index2 <- (.Index-1)%%length(GP$col)+1
             index3 <- (.Index-1)%%length(GP$fill)+1
           },
           {
             index <- which(pch==fig$current.Panel$.Pch)
             index1 <- which(as.numeric(size)==as.numeric(fig$current.Panel$.Size))               
             index2 <- which(GP$col==fig$current.Panel$.Col)
             index3 <- which(GP$fill==fig$current.Panel$.Fill)
           }
           )
    if (is.null(GP$col)){ index2 <- 1}
    if (is.null(GP$fill)){ index3 <- 1}
    pch[index] <- which(.Symbol.Names==symmenu.w$GetEntry()$GetText())-1
    fig$current.Panel$.Pch <<- pch[index[1]]
    size[index1] <- symsize.w$GetValueAsFloat()
    fig$current.Panel$.Size<<-size[index1[1]]
    GP$lty <- which(.Line.Names==linemenu.w$GetEntry()$GetText())-1
    if((k <- linsize.w$GetValueAsFloat())!=0) GP$lwd <- k
    if((k <- linecol.w$GetEntry()$GetText())!="default") {
      GP$col[index2] <- k
    } else {
      
        GP$col[index2] <- NULL
      
    }
    fig$current.Panel$.Col<<- GP$col[index2[1]]
    if((k <- fillcol.w$GetEntry()$GetText())!="default") {
      GP$fill[index3] <- k
    } else {
        GP$fill[index3] <- NULL
    }
    fig$current.Panel$.Fill<<-GP$fill[index3[1]]
    if (!is.na(k <- as.numeric(wdata.w$GetText()))){
      c$w[(fig$current.Panel$.Index-1)%%length(c$w)+1] <- k
    }
    if (!is.na(k <- as.numeric(hdata.w$GetText()))){
      c$h[(fig$current.Panel$.Index-1)%%length(c$h)+1] <- k
    }
    if (!is.na(k <- as.numeric(xdata.w$GetText()))){
      c$x[fig$current.Panel$.Index] <- k
    }
    if (!is.na(k <- as.numeric(ydata.w$GetText()))){
      c$y[fig$current.Panel$.Index] <- k
    }
    grid.edit(points$name,x=c$x,y=c$y,w=c$w,h=c$h,pch=as.integer(pch),size=size,gp=GP,strict=TRUE)      
  }

  select.interaction <- function(z){
    Interaction.Type<<-z$GetName()
  }
  
  on.select.panel<-function(List, Item) {   
    fig$panel(select=gtkListChildPosition(List,Item)+1)
  }

  on.delete.panel <- function(...){
    fig$delete.panel()
  }

  on.new.annot.clicked <- function(...){
    if (!PlaceNewAnnotation) {PlaceNewAnnotation<<-TRUE}
  }

  on.save1.activate <- function(...){
    if(is.null(FileSel)) on.saveas1.activate()
    else fig$save(FileSel)
  }
  
  on.saveas1.activate <- function(...){
    F1.w <<- gtkFileSelection("Save Figure")
    F1.w$GetOkButton()$AddCallback("clicked",FS.ok.button2.clicked)
    F1.w$GetCancelButton()$AddCallback("clicked",FS.cancel.button2.clicked)
    F1.w$Show()
  }

  on.open1.activate <- function(...){
    F1.w <<- gtkFileSelection("Open Figure")
    F1.w$GetOkButton()$AddCallback("clicked",FO.ok.button1.clicked)
    F1.w$GetCancelButton()$AddCallback("clicked",FS.cancel.button2.clicked)
    F1.w$Show()
  }

  FS.ok.button2.clicked <- function(...){
    FileSel <<- F1.w$GetFilename()
    F1.w$Destroy()
    fig$save(FileSel)
  }

  FS.cancel.button2.clicked <- function(...) {
    F1.w$Destroy()
  }

  FO.ok.button1.clicked <- function(...){
    fig$set.active()
    FS<- F1.w$GetFilename()
    F1.w$Destroy()
    if (file.exists(FS)){
      FileSel<<-FS
      fig$clear()
      panels <<-list()
      source(FileSel,local=TRUE)
    }
  }

  upld.file.Action <- function(...){
    
     ok.button.clicked <- function(...){
       commit <- function(df){
          write.table(df,file=FS,row.names=FALSE,col.names=FALSE)
       }
      FS<- F1.w$GetFilename()
      F1.w$Destroy()
      e <- fig$current.Panel$elements[[ fig$current.Panel$.Selected.Element]]
      wisard.out(grid.get(e$name),commit)
    }

    fig$set.active()
    F1.w <- gtkFileSelection("Write Data File",show=FALSE)
    F1.w$GetOkButton()$AddCallback("clicked",ok.button.clicked)
    F1.w$GetCancelButton()$AddCallback("clicked",function(...){ F1.w$Destroy()})
    F1.w$Show()
  }
  
  downld.file.Action<- function(...){
    
    ok.button.clicked <- function(...){
      FS<- F1.w$GetFilename()
      F1.w$Destroy()
      if (file.exists(FS)){
        pp <- read.table(FS)
        wisard.in(pp,FS)
      }
    }

    fig$set.active()
    F1.w <- gtkFileSelection("Open Data File",show=FALSE)
    F1.w$GetOkButton()$AddCallback("clicked",ok.button.clicked)
    F1.w$GetCancelButton()$AddCallback("clicked",function(...){ F1.w$Destroy()})
    F1.w$Show()
  }

  
  button3.clicked <- function(...){
    fig$panel()
  }

  on.slide.panel <- function(win,arg){
    hPanedPos <<- !hPanedPos
    gtkPanedSetPosition(win,-1*as.numeric(hPanedPos))
  }
  
  on.resize <- function(w1,w2){
    fig$set.active()
    class(w2)<-"GdkEventConfigure"
    fig$Scale <<- c(gdkEventConfigureGetWidth(w2),gdkEventConfigureGetHeight(w2))/fig$cells
  }
  
  on.ggRDev.destroy <- function(...){
    if (!is.null(current.Figure)) {
      if (identical(current.Figure,fig)) {
        current.Figure<<-NULL
      }
    }
    ggRDev.w$Destroy()
  }
  
  on.print1 <- function(z){
    fig$hardcopy(z$GetName())
  }

  on.kill.panel1 <- function(...){
    fig$kill.panel()
  }
  
  on.yank1 <- function(...){
    fig$yank()
  }

  on.kill.element1<-function(...){
    fig$kill.element()
  }

  annot.get.gp.GUI <- function(with.color=TRUE){
    GP <- gpar(lwd=2,fontsize=if(k <- text.points.w$GetValueAsInt()){k}else{NULL},fontface=if((j <- text.face.w$GetEntry()$GetText())!="default"){j}else{NULL})
    if (with.color) { if((k <- text.color.w$GetEntry()$GetText())!="default"){GP$col <- k}}
    return(GP)
  }

  on.apply.text.figure <- function(...){
    GP1 <- annot.get.gp.GUI(FALSE)
    fig$apply.text.figure(GP1)
  }
  
  on.apply.text.panel <- function(...){
    GP1 <- annot.get.gp.GUI(FALSE)
    fig$apply.text.panel(GP1)
  }

  on.newlegend <- function(...){
    fig$newlegend()
  }  

  ggRDev.w <- gtkWindow()
  ggRDev.w$Add(hpaned1<-gtkHPaned())
  hpaned1$SetPosition(0)
  hpaned1$Add1(vbox1<-gtkVBox())
  drawingarea1.w<-gtkDrawingArea()
  drawingarea1.w$SetEvents(c("button1-motion-mask","button-press-mask","button-release-mask","key-press-mask","enter-notify-mask","leave-notify-mask","focus-change-mask"))
  drawingarea1.w[["can_focus"]] <- TRUE
  hpaned1$Add2(drawingarea1.w)

  
  vbox1$Add(hbox1<-gtkHBox(2))
  hbox1$Add(menubar1<-gtkMenuBar())
  hbox1$Add(inter1<-gtkOptionMenu())
  vbox1$SetChildPacking(hbox1,FALSE,FALSE,0)
  vbox1$Add(nb1<-gtkNotebook())
  vbox1$SetHomogeneous(FALSE)

  nb1$AppendPage(v2<-gtkVBox(2),gtkLabel("Panel"))
  v2$SetHomogeneous(FALSE)
  v2$Add(hb1<-gtkHBox(4))
  v2$SetChildPacking(hb1,FALSE,FALSE,0)

  hb1$SetHomogeneous(TRUE)
  hb1$Add(button1.w <- gtkButton("Apply"))
  hb1$Add(del.panel.w <- gtkButton("Delete"))
  del.panel.w$AddCallback("clicked",on.delete.panel)
  hb1$Add(new.panel.w <- gtkButton("Add New"))
  new.panel.w$AddCallback("clicked",button3.clicked)
  hb1$Add(autorngbtn.w <- gtkButton("Auto Range"))

  sapply(list(button1.w,new.panel.w,del.panel.w,autorngbtn.w),function(x){hb1$SetChildPacking(x,TRUE,TRUE,0)})

  v2$Add(combo1.w<-gtkCombo())
  v2$Add(hb2<-gtkHBox(2))
  v2$SetChildPacking(combo1.w,FALSE,FALSE,0)
  v2$SetChildPacking(hb2,FALSE,FALSE,0)

  hb2$SetHomogeneous(FALSE)
  hb2$Add(x <- gtkLabel("X Range"))
  hb2$Add(v<-gtkVBox(2))

  v$Add(xrang1.w<-gtkEntry())
  v$Add(xrang2.w<-gtkEntry())

  v2$Add(hb2<-gtkHBox(2))
  v2$SetChildPacking(hb2,FALSE,FALSE,0)

  hb2$SetHomogeneous(FALSE)
  hb2$Add(gtkLabel("Y Range"))
  hb2$Add(v<-gtkVBox(2))
  v$Add(yrang1.w<-gtkEntry())
  v$Add(yrang2.w<-gtkEntry())

  v2$Add(nb2<-gtkNotebook())
  v2$SetChildPacking(nb2,FALSE,FALSE,0)
  nb2$AppendPage(v<-gtkVBox(4),gtkLabel("Bottom"))
  v$Add(gtkLabel("Ticks"))
  v$Add(x1.w <- gtkEntry())
  v$Add(gtkLabel("Label"))
  v$Add(x1label.w <- gtkEntry())

  nb2$AppendPage(v<-gtkVBox(4),gtkLabel("Top"))
  v$Add(gtkLabel("Ticks"))
  v$Add(x2.w <- gtkEntry())
  v$Add(gtkLabel("Label"))
  v$Add(x2label.w <- gtkEntry())

  nb2$AppendPage(v<-gtkVBox(4),gtkLabel("Left"))
  v$Add(gtkLabel("Ticks"))
  v$Add(y1.w <- gtkEntry())
  v$Add(gtkLabel("Label"))
  v$Add(y1label.w <- gtkEntry())

  nb2$AppendPage(v<-gtkVBox(4),gtkLabel("Right"))
  v$Add(gtkLabel("Ticks"))
  v$Add(y2.w <- gtkEntry())
  v$Add(gtkLabel("Label"))
  v$Add(y2label.w <- gtkEntry())

  v2$Add(hb2<-gtkHBox(2))
  v2$SetChildPacking(hb2,FALSE,FALSE,0)
  hb2$SetHomogeneous(FALSE)
  hb2$Add(grill.w <- gtkCheckButton("Grilled?"))
  hb2$Add(ticksdir.w <- gtkCheckButton("Ticks in?"))
  v2$Add(f<-gtkFrame("Panel Placement"))
  v2$SetChildPacking(f,FALSE,FALSE,0)
  f$Add(v<-gtkVBox(2))
  v$Add(hb<-gtkHBox(4))
  hb$Add(gtkLabel("X"))
  hb$Add(spinpanelx.w <- gtkSpinButton(climb=1,digits=0))
  hb$Add(gtkLabel("Width"))
  hb$Add(spinpanelw.w <- gtkSpinButton(climb=1,digits=0))
  v$Add(hb<-gtkHBox(4))
  hb$Add(gtkLabel("Y"))
  hb$Add(spinpanely.w <- gtkSpinButton(climb=1,digits=0))
  hb$Add(gtkLabel("Height"))
  hb$Add(spinpanelh.w <- gtkSpinButton(climb=1,digits=0))

  nb1$AppendPage(v2<-gtkVBox(2),gtkLabel("Element"))
  v2$SetHomogeneous(FALSE)
  v2$SetSpacing(0)
  v2$Add(hb1<-gtkHBox(4))
  hb1$SetHomogeneous(TRUE)
  hb1$Add(element.apply.w <- gtkButton("Apply"))
  hb1$Add(del.element.w <- gtkButton("Delete"))

  sapply(list(element.apply.w,del.element.w),function(x){hb1$SetChildPacking(child=x,expand=TRUE,fill=TRUE,0)})

  v2$SetChildPacking(hb1,FALSE,FALSE,0)
  v2$Add(elements.w<-gtkCombo())
  v2$SetChildPacking(elements.w,FALSE,FALSE,0)
  v2$Add(symmenu.w<-gtkCombo())
  v2$SetChildPacking(symmenu.w,FALSE,FALSE,0)
  symmenu.w$SetPopdownStrings(.Symbol.Names)
  symmenu.w$GetList()$SetSelectionMode(1)
  v2$Add(linemenu.w<-gtkCombo())
  v2$SetChildPacking(linemenu.w,FALSE,FALSE,0)
  linemenu.w$SetPopdownStrings(.Line.Names)
  linemenu.w$GetList()$SetSelectionMode(1)
  v2$Add(h<-gtkHBox(2))
  v2$SetChildPacking(h,FALSE,FALSE,0)
  h$Add(v<-gtkVBox(2))
  v$Add(gtkLabel("Symbol Size"))
  v$Add(symsize.w <- gtkSpinButton(adjustment=gtkAdjustment(value=1,lower=0,upper=10,step.increment=0.5,page.increment=1,page.size=1),climb=10,digits=1))
  h$Add(v<-gtkVBox(2))
  v$Add(gtkLabel("Outline Width"))
  v$Add(linsize.w <- gtkSpinButton(adjustment=gtkAdjustment(value=1,lower=0,upper=10,step.increment=0.5,page.increment=1,page.size=1),climb=10,digits=1))
  v2$Add(x <- gtkLabel("Fill Color"))
  v2$SetChildPacking(x,FALSE,FALSE,0)
  v2$Add(fillcol.w <- gtkCombo())
  v2$SetChildPacking(fillcol.w,FALSE,FALSE,0)
  v2$Add(x <- gtkLabel("Line Color"))
  v2$SetChildPacking(x,FALSE,FALSE,0)
  v2$Add(linecol.w <- gtkCombo())
  v2$SetChildPacking(linecol.w,FALSE,FALSE,0)
  
  v2$Add(f<-gtkFrame("Data"))
  v2$SetChildPacking(f,FALSE,FALSE,0)

  f$Add(h <- gtkHBox(2))
  h$SetHomogeneous(FALSE)
  
  h$Add(va <- gtkVBox(4))
  h$Add(vb <- gtkVBox(4))
  va$Add(gtkLabel("Index:"))
  vb$Add(index.w <- gtkSpinButton(adjustment=gtkAdjustment(value=1,lower=1,upper=1,step.increment=1,page.increment=10,page.size=10),climb=10,digits=0))

  va$Add(gtkLabel("X"))
  vb$Add(xdata.w <- gtkEntry())

  va$Add(gtkLabel("Y"))
  vb$Add(ydata.w <- gtkEntry())

  va$Add(gtkLabel("W"))
  vb$Add(wdata.w <- gtkEntry())

  va$Add(gtkLabel("H"))
  vb$Add(hdata.w <- gtkEntry())
  
  v2$Add(l <- gtkLabel("Apply Color/Symbol Values to:"))
  v2$SetChildPacking(l,FALSE,FALSE,0)
  v2$Add(cs.info.w <- gtkCombo())
  v2$SetChildPacking(cs.info.w,FALSE,FALSE,0)
  cs.info.w$SetPopdownStrings(.CS.INFO)
    
  nb1$AppendPage(v2<-gtkVBox(2),gtkLabel("Annotation"))
  v2$SetHomogeneous(FALSE)
  v2$SetSpacing(0)
  v2$Add(hb1<-gtkHBox(4))
  v2$SetChildPacking(hb1,FALSE,FALSE,0)
  hb1$SetHomogeneous(TRUE)
  hb1$Add(new.annot.w <- gtkToggleButton("Place New"))
  new.annot.w$AddCallback("clicked",on.new.annot.clicked)
  hb1$Add(applyannot.w  <- gtkButton("Apply"))
  hb1$Add(deleteannot.w <- gtkButton("Delete"))
  hb1$Add(newlegend.w <- gtkButton("Legend"))
  newlegend.w$AddCallback("clicked",on.newlegend)

  sapply(list(new.annot.w,applyannot.w,deleteannot.w,newlegend.w),function(x){hb1$SetChildPacking(child=x,expand=TRUE,fill=TRUE,0)})

  v2$Add(ss<-gtkScrolledWindow())
  ss$SetPolicy("never","always")
  ss$Add(annot.text.w<-gtkText())
  annot.text.w$SetEditable(TRUE)
  ss$SetVadjustment(annot.text.w$GetVadj())

  v2$Add(f <- gtkFrame("Text Properies"))
  v2$SetChildPacking(f,FALSE,FALSE,0)
  f$Add(v <- gtkVBox(3))
  v$Add(h <- gtkHBox(2))
  h$Add(gtkLabel("Points (0=default)"))
  h$Add(text.points.w <- gtkSpinButton(adjustment=gtkAdjustment(value=0,lower=0,upper=30,step.increment=1,page.increment=5,page.size=5),climb=10,digits=0))
  v$Add(h <- gtkHBox(2))
  h$Add(gtkLabel("Face"))
  h$Add(text.face.w <- gtkCombo())
  text.face.w$SetPopdownStrings(.Face.Names)
  v$Add(h <- gtkHBox(2))
  h$Add(apply.text.figure.w <- gtkButton("Apply to Figure"))
  apply.text.figure.w$AddCallback("clicked",on.apply.text.figure)
  h$Add(apply.text.panel.w <- gtkButton("Apply to Panel"))
  apply.text.panel.w$AddCallback("clicked",on.apply.text.panel)

  v2$Add(x<-gtkLabel("Color"))
  v2$SetChildPacking(x,FALSE,FALSE,0)
  v2$Add(text.color.w <- gtkCombo())
  v2$SetChildPacking(text.color.w,FALSE,FALSE,0)
  v2$Add(x <- gtkLabel("Rotation"))
  v2$SetChildPacking(x,FALSE,FALSE,0)
  v2$Add(rotatetext.w <- gtkSpinButton(adjustment=gtkAdjustment(value=0,lower=-180,upper=180,step.increment=15,page.increment=90,page.size=90),climb.rate=1,digits=0))
  v2$SetChildPacking(rotatetext.w,FALSE,FALSE,0)
  v2$Add(text.frame.w <- gtkCheckButton("Framed?"))
  v2$SetChildPacking(text.frame.w,FALSE,FALSE,0)

  menubar1$Add(file1<-gtkMenuItem("File"))
  file1$SetSubmenu(file.sub <- gtkMenu())
  file.sub$Append(save <- gtkMenuItem("Save"))
  save$AddCallback("activate",on.save1.activate)
  file.sub$Append(save.as.1 <- gtkMenuItem("Save As"))
  save.as.1$AddCallback("activate",on.saveas1.activate)
  file.sub$Append(open1 <- gtkMenuItem("Open"))
  open1$AddCallback("activate",on.open1.activate)
  file.sub$Append(separator <- gtkMenuItem())
  file.sub$Append(downld.file <- gtkMenuItem("Download From File"))
  downld.file$AddCallback("activate",downld.file.Action)
  file.sub$Append(upld.file <- gtkMenuItem("Upload To File"))
  upld.file$AddCallback("activate",upld.file.Action)
  file.sub$Append(separator <- gtkMenuItem())
  file.sub$Append(print1 <- gtkMenuItem("Print"))
  print1$SetSubmenu(print.dev<-gtkMenu())
  sapply(c("PS","PDF","PNG","PDFwrite"),function(x){
    print.dev$Append(w <- gtkMenuItem(x))
    w$SetName(x)
    w$AddCallback("activate",on.print1)
  })
  file.sub$Append(separator <- gtkMenuItem())
  file.sub$Append(quit <- gtkMenuItem("Quit"))
  ggRDev.w$AddCallback("delete-event",on.ggRDev.destroy)
  quit$AddCallback("activate",on.ggRDev.destroy)

  menubar1$Add(edit1<-gtkMenuItem("Edit"))
  edit1$SetSubmenu(edit1.sub <- gtkMenu())
  edit1.sub$Append(copy.element1 <- gtkMenuItem("Copy Element"))
  copy.element1$AddCallback("activate",on.kill.element1)
  edit1.sub$Append(copy.panel1 <- gtkMenuItem("Copy Panel"))
  copy.panel1$AddCallback("activate",on.kill.panel1)
  edit1.sub$Append(paste1 <- gtkMenuItem("Paste"))
  paste1$AddCallback("activate",on.yank1)

  inter1$SetMenu(inter1.sub <- gtkMenu())
  inter1.sub$Append(select.element1 <- gtkMenuItem("Select Element"))
  inter1.sub$Append(select.annotation1 <- gtkMenuItem("Select Annotation"))
  inter1.sub$Append(select.region1 <- gtkMenuItem("Select Region"))
  inter1.sub$Append(select.points1 <- gtkMenuItem("Select Points"))
  inter1.sub$Append(select.zoom1 <- gtkMenuItem("Select Zoom"))

  lapply(c("select.element1","select.zoom1","select.annotation1","select.region1","select.points1","select.zoom1"),
         function(x){
           get(x)$AddCallback("activate",select.interaction)
           get(x)$SetName(x)
         })

  inter1$SetHistory(0)
  
  button1.w$AddCallback("clicked",on.button1.clicked)
  autorngbtn.w$AddCallback("clicked",autorange)
  elements.w$GetList()$AddCallback("select-child",on.sel.element)
  element.apply.w$AddCallback("clicked",do.apply.element)
  del.element.w$AddCallback("clicked",on.delete.element)
  applyannot.w$AddCallback("clicked",on.apply.annot)
  deleteannot.w$AddCallback("clicked",on.delete.annot)
  index.w$AddCallback("changed",on.change.index)

  lapply(c(spinpanelx.w,spinpanely.w,spinpanelw.w,spinpanelh.w),
         function(x){x$SetAdjustment(gtkAdjustment(1,1,cells,1,1,0))})
#  drawingarea1.w$SetUsize(width,height)
  drawingarea1.w$Size(width,height)
  gdkFlush()
  asGtkDevice(drawingarea1.w,width=width,height=height,pointsize=12)  

  drawingarea1.w$AddCallback("motion_notify_event",stroke)
  drawingarea1.w$AddCallback("configure_event",on.resize)
  drawingarea1.w$AddCallback("button-press-event",start.stroke)
  drawingarea1.w$AddCallback("button-release-event",end.stroke)
  drawingarea1.w$AddCallback("key-press-event",hit.key)

  fig <- figure.back(width=width,height=height,cells=cells)
  fig$PANEL.CB <- panel.GUI
  fig$ANNOTATION.CB <- annotation.GUI
  fig$ELEMENT.CB <- element.GUI
  combo1.w$GetList()$AddCallback( "select-child",on.select.panel)
  colorlist <- c(palette(),"default")
  fillcol.w$SetPopdownStrings(colorlist)
  fillcol.w$GetList()$SetSelectionMode(1)
  linecol.w$SetPopdownStrings(colorlist)
  linecol.w$GetList()$SetSelectionMode(1)
  text.color.w$SetPopdownStrings(colorlist)
  text.color.w$GetList()$SelectItem(which(colorlist=="default")-1)
  try(figure.startup(environment()))
  gdkFlush()
  ggRDev.w$ShowAll()
#  fig$set.active()
  return(fig)
}

.z.interface <- function(width=1000,height=2000,cells=10){
  redraw <- function(...){
#    v()
  }
#  start.view()
  fig <- figure.back(width=width,height=height,cells=cells)
  fig$PANEL.CB <- redraw
  fig$ANNOTATION.CB <- redraw
  fig$ELEMENT.CB <- redraw
  return(fig)

}

figure <- .GTK.interface


