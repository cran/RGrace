.onLoad <- function(lib, pkg) {
  require(RGtk2)
#  require(gtkDevice2)
  require(cairoDevice)
  current.Figure <<-NULL
  .Kill.Structure<<-NULL
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

gtkComboBoxEntrySetPopdownStrings<-function(object,strings){
  object$SetModel(rGtkDataFrame(as.data.frame(strings)))
  if (length(strings)>0) {
    if(object$GetTextColumn()!=0){
      object$SetTextColumn(0)
      object$SetActive(0)
    }
  } 
}


GetOkButton <- function(obj){
  if ("GtkFileChooserDialog"%in%class(obj)){
    obj$GetChildren()[[1]]$GetChildren()[[2]]$GetChildren()[[1]]
  }
  if ("GtkFileSelection"%in%class(obj)){
    obj$GetOkButton()
  }
}

GetCancelButton <- function(obj){
 if ("GtkFileChooserDialog"%in%class(obj)){
    obj$GetChildren()[[1]]$GetChildren()[[2]]$GetChildren()[[2]]
  }
  if ("GtkFileSelection"%in%class(obj)){
    obj$GetCancelButton()
  }
}


tolocale<-function(x){
  return(iconv(x,"UTF8",""))
}

toutf<-function(x){
  return(iconv(x,"","UTF8"))
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

.Line.Names<-c("Line:No",
                  "Line:Solid",
                  "Line:Dashed",
                  "Line:Dotted",
                  "Line:Dash-dotted",
                  "Line:Long-dashed",
                  "Line:Short-long-dashed")

.Face.Names <- c("plain",
                 "bold",
                 "italic",
                 "bold.italic",
                 "default")

.CS.INFO <- c("All Points",
                 "Selected Point",
                 "Points with the Same Values"
                 )

.Err.Style <- c("whiskers",
                "areafill",
                "arrows")


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


.GTK2.interface<- function(width=400,height=400,cells=10){
#  cells<-cells
#  width<-width
#  height<-height
  hPanedPos <- FALSE
  PlaceNewAnnotation <- FALSE
  FileSel <- NULL
  .Click.Inside.Area <- FALSE
  Interaction.Type <- "select.element1"
  if (!exists("figure.startup"))
    figure.startup <<- default.figure.startup
  
  gladewidget<-function(name,tree=XML){
    w<-gladeXMLGetWidget(tree,name)
    class(w)<-gtkObjectGetClasses(w,check=F)
    w
  }

  
  rescan.elements <- function(panel){
    strings <- sapply(panel$elements,function(i){ i$title})
    if (!is.null(as.pairlist(strings))) {
      elements.w$SetPopdownStrings(strings)
#      elements.w$SetActive(0)
    } else {
      elements.w$SetPopdownStrings(NULL)
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
#      combo1.w$SetActive(0)
    } else {
      combo1.w$SetPopdownStrings(NULL)
    }
    VB.w$GetWindow()$ProcessUpdates(T); VB.w$QueueDraw()	
  }


  element.GUI <- function(panel,rescan=FALSE) {
    if (rescan) {
      rescan.elements(panel)
    } else {
      z <- grid.get(panel$elements[[panel$.Selected.Element]]$name)
      index.w$SetAdjustment(gtkAdjustment(value=0,lower=1,upper=length(z$x),step.incr=1,page.incr=10,page.size=10))
      errstyle.w$GetChildren()[[1]]$SetText(z$errstyle)
      index.w$SetValue(panel$.Index)
      on.change.index()
    }
    VB.w$GetWindow()$ProcessUpdates(T); VB.w$QueueDraw()
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
         x[[2]]$SetText(toutf(deparse(Z$label[[x[[3]]]],width.cutoff=500)))
      })
      rescan.elements(fig$current.Panel)
    }
	VB.w$GetWindow()$ProcessUpdates(T); VB.w$QueueDraw()
  }
  
  annotation.GUI <- function(panel,rescan=FALSE){
    if (!is.null(panel$.Selected.Annotation)){
      grob <- panel$annotations[[panel$.Selected.Annotation]]
      annot.text.w$SetText(toutf(paste(lapply(grid.get(grob$name)$label,function(x){if (inherits(x,"extplotmath")) { x} else { deparse(x,width.cutoff = 500)}} ),collapse="\n")))
      rotatetext.w$SetValue(grid.get(grob$name)$rot)
      text.frame.w$SetActive(grid.get(grob$name)$frame)
      gp1 <- grid.get(grob$name)$gp
      text.color.w$GetChildren()[[1]]$SetText(ifelse(is.null(gp1$col),"default",gp1$col))
      text.face.w$GetChildren()[[1]]$SetText(ifelse(is.null(gp1$fontface),"default",gp1$fontface))
      text.points.w$SetValue(ifelse(is.null(gp1$fontsize),0,gp1$fontsize))
    }
	VB.w$Show()
  }
  

  on.change.index <- function(...){
    fig$current.Panel$.Index <<- index.w$GetValueAsInt()
    c <-grid.get(fig$current.Panel$elements[[fig$current.Panel$.Selected.Element]]$name,strict=TRUE) 
    fig$current.Panel$.Pch<<-c$pch[[(fig$current.Panel$.Index-1)%%length(c$pch)+1]]
    symmenu.w$SetActive(fig$current.Panel$.Pch)
    fig$current.Panel$.Size <<-c$size[[(fig$current.Panel$.Index-1)%%length(c$size)+1]]
    symsize.w$SetValue(as.numeric(fig$current.Panel$.Size))
    z <- c$gp
    if (is.null(z$fill)){z$fill <- "default"}
    if (is.null(z$col)){z$col <- "default"}
    if (is.null(z$lwd)){z$lwd <- 0}
    if (is.null(z$lty)){z$lty <- 0}
    fig$current.Panel$.Fill<<-z$fill[[(fig$current.Panel$.Index-1)%%length(z$fill)+1]]
    fillcol.w$GetChildren()[[1]]$SetText(fig$current.Panel$.Fill)
    linemenu.w$SetActive(z$lty)
    linsize.w$SetValue(z$lwd)
    fig$current.Panel$.Col<<-z$col[[(fig$current.Panel$.Index-1)%%length(z$col)+1]]
    linecol.w$GetChildren()[[1]]$SetText(fig$current.Panel$.Col)
    xdata.w$SetText(as.character(c$x[[fig$current.Panel$.Index]]))
    ydata.w$SetText(as.character(c$y[[fig$current.Panel$.Index]]))
    wdata.w$SetText(ifelse(is.null(f <- c$w[[(fig$current.Panel$.Index-1)%%length(c$w)+1]]),"",as.character(f)))
    hdata.w$SetText(ifelse(is.null(f <- c$h[[(fig$current.Panel$.Index-1)%%length(c$h)+1]]),"",as.character(f)))
  }

  stroke <- function(win,arg){
    if (.Click.Inside.Area){
      crosshair(as.integer(xb),as.integer(yb))
      xb<<-as.double(gdkEventMotionGetX(arg))
      yb<<-as.double(gdkEventMotionGetY(arg))
      crosshair(as.integer(xb),as.integer(yb))

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
    crosshair(as.integer(xa),as.integer(ya))
    crosshair(as.integer(xb),as.integer(yb))
  }

  end.stroke<-function(win, arg) {
    if(!.Click.Inside.Area) return(NULL)
    .Click.Inside.Area <<- FALSE 
    crosshair(as.integer(xa),as.integer(ya))
    crosshair(as.integer(xb),as.integer(yb))
    xb<-as.double(gdkEventButtonGetX(arg))
    yb<-as.double(gdkEventButtonGetY(arg))
    vp<-grid.get(fig$current.Panel$border$name,strict=TRUE)$vp
    xx<-(c(xa,xb)/fig$Scale[1]-vp$layout.pos.col[1]+1)*(vp$xscale[2]-vp$xscale[1])/(vp$layout.pos.col[2]-vp$layout.pos.col[1]+1)+vp$xscale[1]
    yy<-(-c(ya,yb)/fig$Scale[2]+vp$layout.pos.row[2])*(vp$yscale[2]-vp$yscale[1])/(vp$layout.pos.row[2]-vp$layout.pos.row[1]+1)+vp$yscale[1]
    if (PlaceNewAnnotation) {
      new.annot.w$SetActive(FALSE)
      PlaceNewAnnotation <<- FALSE        
      fig$current.Panel$annotation(label=eval(parse(text=paste("list(",gsub("\n",",",tolocale(annot.text.w["text"])),")",sep=""))),x=xx[1],y=yy[1],rot=rotatetext.w$GetValueAsInt(),frame=as.numeric(text.frame.w$GetActive()),gp=annot.get.gp.GUI(),update.GUI=TRUE)
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
             elements.w$SetActive(fig$current.Panel$.Selected.Element-1)
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
    fig$current.Panel$edit(label=list(eval(parse(text=tolocale(x1label.w$GetText()))),eval(parse(text=tolocale(x2label.w$GetText()))),eval(parse(text=tolocale(y1label.w$GetText()))),eval(parse(text=tolocale(y2label.w$GetText())))),at=list(tolocale(x1.w$GetText()),tolocale(x2.w$GetText()),tolocale(y1.w$GetText()),tolocale(y2.w$GetText())),grilled=grill.w$GetActive(),inward=ticksdir.w$GetActive(),y=c(y,ya),x=c(x,xa),scale.X=rang[1:2],scale.Y=rang[3:4])
  }

   on.sel.element<-function(List) {
     fig$current.Panel$element(select=List$GetActive()+1)
  }

  on.delete.element <- function(...){
    fig$current.Panel$delete.element()
  }
    
  on.apply.annot <- function(...) {
    fig$set.active()
    if (!is.null(fig$current.Panel$.Selected.Annotation)) {
      grid.edit(fig$current.Panel$annotations[[fig$current.Panel$.Selected.Annotation]]$name,label=eval(parse(text=paste("list(",gsub("\n",",",tolocale(annot.text.w["text"])),")",sep=""))),rot=rotatetext.w$GetValueAsInt(),frame=text.frame.w$GetActive(),gp=annot.get.gp.GUI())
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

    switch(cs.info.w$GetActive()+1,
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
    pch[index] <- symmenu.w$GetActive()
    fig$current.Panel$.Pch <<- pch[index[1]]
    size[index1] <- symsize.w$GetValue()
    fig$current.Panel$.Size<<-size[index1[1]]
    GP$lty <- linemenu.w$GetActive()
    if((k <- linsize.w$GetValue())!=0) GP$lwd <- k
    if((k <- linecol.w$GetActiveText())!="default") {
      GP$col[index2] <- k
    } else {
        GP$col[index2] <- NULL
    }
    fig$current.Panel$.Col<<- GP$col[index2[1]]
    if((k <- fillcol.w$GetActiveText())!="default") {
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
    es<-errstyle.w$GetChildren()[[1]]$GetText()
    grid.edit(points$name,x=c$x,y=c$y,w=c$w,h=c$h,pch=as.integer(pch),size=size,gp=GP,errstyle=es,strict=TRUE)      
  }

  select.interaction <- function(z){    
    Interaction.Type<<-z$GetName()
  }
  
  on.select.panel<-function(List) {   
    fig$panel(select=List$GetActive()+1)
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
#    F1.w <<- gtkFileChooserDialog(parent=NULL,title="Save Figure",action="save","gtk-cancel",GtkResponseType["cancel"],"gtk-save", GtkResponseType["accept"])
    F1.w <<-  gtkFileSelection("Save Figure")
    F1.w$SetFilename(paste(getwd(),"/",sep=""))
    GetOkButton(F1.w)$AddCallback("clicked",FS.ok.button2.clicked)
    GetCancelButton(F1.w)$AddCallback("clicked",FS.cancel.button2.clicked)
    F1.w$Show()
  }

  on.open1.activate <- function(...){
#    F1.w <<- gtkFileChooserDialog(parent=NULL,title="Open Figure",action="open","gtk-cancel",GtkResponseType["cancel"],"gtk-open", GtkResponseType["accept"])
    F1.w <<-  gtkFileSelection("Open Figure")
    F1.w$SetFilename(paste(getwd(),"/",sep=""))
    GetOkButton(F1.w)$AddCallback("clicked",FO.ok.button1.clicked)
    GetCancelButton(F1.w)$AddCallback("clicked",FS.cancel.button2.clicked)
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
#      fig$clear()
#      panels <<-list()
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
      wisard.out(grid.get(e$name))$COMMIT.CB<-commit
    }

    fig$set.active()
    #F1.w <- gtkFileChooserDialog(parent=NULL,title="Write Data File",action="save","gtk-cancel",GtkResponseType["cancel"],"gtk-save", GtkResponseType["accept"])
     F1.w <-  gtkFileSelection("Write Data File")
    F1.w$SetFilename(paste(getwd(),"/",sep=""))
    GetOkButton(F1.w)$AddCallback("clicked",ok.button.clicked)
    GetCancelButton(F1.w)$AddCallback("clicked",function(...){ F1.w$Destroy()})
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
#    F1.w <-  gtkFileChooserDialog(parent=NULL,title="Open Data File",action="open","gtk-cancel",GtkResponseType["cancel"],"gtk-open", GtkResponseType["accept"])
    F1.w <-  gtkFileSelection("Open Data File")
    F1.w$SetFilename(paste(getwd(),"/",sep=""))
    GetOkButton(F1.w)$AddCallback("clicked",ok.button.clicked)
    GetCancelButton(F1.w)$AddCallback("clicked",function(...){ F1.w$Destroy()})
    F1.w$Show()
  }

  
  button3.clicked <- function(...){
    fig$panel()
  }

  on.slide.panel <- function(win,arg){

    
                                        #    hPanedPos <<- !hPanedPos
#    gtkPanedSetPosition(win,-1*as.numeric(hPanedPos))
  }
  
  on.resize <- function(w1,w2){
    fig$set.active()
    class(w2)<-"GdkEventConfigure"
    fig$Scale <<- c(gdkEventConfigureGetWidth(w2),gdkEventConfigureGetHeight(w2))/fig$cells
    return(TRUE)
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
    GP <- gpar(lwd=2,fontsize=if(k <- text.points.w$GetValueAsInt()){k}else{NULL},fontface=if((j <- text.face.w$GetChildren()[[1]]$GetText())!="default"){j}else{NULL})
    if (with.color) { if((k <- text.color.w$GetChildren()[[1]]$GetText())!="default"){GP$col <- k}}
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
  
  COL<-FALSE

  collapse<-function(...){
    if(COL){
      VB.w$Show()
      COL<<-FALSE
    } else {
      VB.w$Hide()
      COL<<-TRUE
     }
  }
     
  XML <- gladeXMLNew(system.file("ggplot.glade",package="RGrace"))
  sapply(c("spinpanely","spinpanelx","cs.info","spinpanelw","spinpanelh","drawingarea1","combo1","fillcol","linecol","elements","ticksdir","x1label","x2label","y1label","y2label","x1","y1","x2","y2","grill","xrang1","xrang2","yrang1","yrang2","symmenu","linemenu","symsize","linsize","new.annot","ggRDev","index","wdata","xdata","ydata","hdata","rotatetext","text.points","text.face","text.color","text.frame","VB","errstyle"),function(x){assign(paste(x,".w",sep=""),gladewidget(x),envir=parent.env(environment()))})
  gladewidget("annot.text")->a
  gtkTextBuffer()->annot.text.w
 

  a$SetBuffer(annot.text.w)
  drawingarea1.w$SetSizeRequest(width,height)
  lapply(c(spinpanelx.w,spinpanely.w,spinpanelw.w,spinpanelh.w),
         function(x){x$SetAdjustment(gtkAdjustment(1,1,cells,1,1,0))})
  cs.info.w$SetPopdownStrings(.CS.INFO)
#  cs.info.w$SetActive(0)
  symmenu.w$SetPopdownStrings(.Symbol.Names)
  linemenu.w$SetPopdownStrings(.Line.Names)
  colorlist <- c(palette(),"default")
  fillcol.w$SetPopdownStrings(colorlist)
  linecol.w$SetPopdownStrings(colorlist)
  text.color.w$SetPopdownStrings(colorlist)
  text.color.w$SetActive(which(colorlist=="default")-1)
  text.face.w$SetPopdownStrings(.Face.Names)
  errstyle.w$SetPopdownStrings(.Err.Style)
  gdkFlush()
  asCairoDevice(drawingarea1.w)
  N<-gdkGC(drawingarea1.w$GetWindow())
  N$SetFunction(GdkFunction["xor"])
#  N$SetForeground(c(1024,0,0,0))
  N$SetRgbFgColor(c(0,65000,0))
  N$SetLineAttributes(1,GdkLineStyle["solid"],GdkCapStyle["butt"],GdkJoinStyle["miter"])
  N$SetFill(GdkFill["solid"])

  crosshair<-function(x,y){
    w<-drawingarea1.w$GetWindow()
    gdkDrawLine(w,N,x,0,x,10000)
    gdkDrawLine(w,N,0,y,10000,y)
  }

#  try(gladeXMLAutoconnect(XML,do.connect))
  try(gladeXMLSignalAutoconnectFull(XML,function(handler,obj,signal,...){
    try(obj$AddCallback(signal,get(handler)))
  },"TheEnd"))
  fig <- figure.back(width=width,height=height,cells=cells)
  fig$PANEL.CB <- panel.GUI
  fig$ANNOTATION.CB <- annotation.GUI
  fig$ELEMENT.CB <- element.GUI
  try(figure.startup(environment()))
  gdkFlush()
  ggRDev.w$ShowAll()
#  fig$set.active()
	
  return(fig)
}

figure <- .GTK2.interface
