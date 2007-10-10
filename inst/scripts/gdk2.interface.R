.GTK2.interface.gdk <- function(width=400,height=400,cells=10){
  .GTK2.interface(width,height,cells)
  dev.off()
  environment(current.Figure$ELEMENT.CB)->z
  z$gladewidget("hpaned1")->hp
  z$drawingarea1.w<-gtkDrawingArea()
  z$drawingarea1.w["name"]<-"drawingarea1"
  hp$Add(z$drawingarea1.w)
  library(gtkDevice2)
  asGtkDevice(z$drawingarea1.w,width,height,12)
  N<-gdkGC(z$drawingarea1.w$GetWindow())
  N$SetFunction(GdkFunction["xor"])
  N$SetRgbFgColor(c(0,65000,0))
  N$SetLineAttributes(1,GdkLineStyle["solid"],GdkCapStyle["butt"],GdkJoinStyle["miter"])
  N$SetFill(GdkFill["solid"])
  z$drawingarea1.w$AddCallback("configure-event",z$on.resize)
  z$drawingarea1.w$AddCallback("button-press-event",z$start.stroke)
  z$drawingarea1.w$AddCallback("button-release-event",z$end.stroke)
  z$drawingarea1.w$AddCallback("motion-notify-event",z$stroke)
  z$drawingarea1.w$AddCallback("key-press-event",z$stroke)
  current.Figure$DevID <- dev.cur()
  z$fig <- figure.back(width=width,height=height,cells=cells)
  z$fig$PANEL.CB <- z$panel.GUI
  z$fig$ANNOTATION.CB <- z$annotation.GUI
  z$fig$ELEMENT.CB <- z$element.GUI
  return(z$fig)
}

figure <-.GTK2.interface.gdk 
