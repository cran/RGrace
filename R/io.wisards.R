wisard.in <- function(frame,frame.name){
  select.role <- function(List,Item){
    role <- .Roles[gtkListChildPosition(List,Item)+1]
    z <- gsub("rolelist","stretch.w",List["name"],extended=T)
    get(z)->check
    if (role%in%c("Color","Symbol","Size")){
      check["sensitive"] <- T
    } else {
      check["sensitive"] <- F
      check$SetActive(F)
    }
  }
  ok.clicked <- function(...){
    to.col.index <- function(z){
      col.scale <- (length(palette())-1)/diff(range(abs(draw.frame$Color)))
      col.start <- min(abs(draw.frame$Color))
      palette()[1+as.integer((abs(draw.frame$Color)-col.start)*col.scale)]
    }
    z <- ls(envir=parent.env(environment()))
#    print(z)
    fr.env<-attach(frame)
    f <- grep("\\.role\\.w",z,extended=TRUE,value=TRUE)
    f <- gsub("\\.role\\.w","",f,extended=TRUE)
    draw.frame <- frame
    draw.frame$X <- seq(along=draw.frame[[1]])
    comment(draw.frame$X) <- "seq"
    for (i in f){
      get(paste(i,".role.w",sep=""))->z
      .Roles[z$GetActive()+1]->l
      if(l=="None"){
        draw.frame[[i]]<-NULL
        next
      }
      z2 <- get(paste(i,".name.w",sep=""))
      p <- eval(parse(text=z2$GetText()),envir=fr.env)
      draw.frame[[l]] <- p

      comment(draw.frame[[l]])<-z2$GetText()
#      draw.frame[[i]] <- NULL
      
    }
    Draw.Frame<<-draw.frame
    f <- grep("^Y[[:digit:]]*",names(draw.frame),extended=T,value=T)
    frame.name <- basename(frame.name)
    for (i in f){
      ggplot(y=draw.frame[[i]],x=draw.frame$X,w=draw.frame$ErrX,h=draw.frame[[paste("Err",i,sep="")]],pch=if(is.null(draw.frame$Symbol)){22}else{as.integer(draw.frame$Symbol)%%26},size=if(is.null(draw.frame$Size)){0.5}else{as.numeric(abs(draw.frame$Size))},gp=if(is.null(draw.frame$Color)){gpar(fill=palette()[1])}else{gpar(fill=to.col.index(draw.frame$Color))},Elabel=paste(frame.name,"[",comment(draw.frame[[i]]),"]~",frame.name,"[",comment(draw.frame$X),"]",sep=""))
    }
  }
  on.close <- function(...){
    w$Destroy()
  }
  w <- gtkWindow()
  w$Add(v <- gtkVBox(FALSE))
  
  v$Add(h <- gtkHBox(TRUE))
  v$SetChildPacking(h,FALSE,FALSE,0,as.integer(0))
  h$Add(gtkLabel("Column"))
  h$Add(gtkLabel("Role"))
#  h$Add(gtkLabel("Options"))
  k <- length(names(frame))
  .Roles <- c("X",paste("Y",1:k,sep=""),"ErrX",paste("ErrY",1:k,sep=""),"Color","Symbol","Size","None")
  l<-1
  for (i in names(frame)){
    v$Add(h <- gtkHBox(TRUE))
    v$SetChildPacking(h,FALSE,FALSE,2,as.integer(0))
    h$Add(z <- gtkEntry())
    z$SetText(i)
    h$Add(z1 <- gtkComboBoxNewText())
    sapply(.Roles,function(x){z1$AppendText(x)})
    z1["name"] <- paste(i,".rolelist",sep="")
    switch(l,
           {
             z1$SetActive(0)
             l<-l+1
           },
           {
             z1$SetActive(1)
             l <- l+1
           },
           {z1$SetActive(length(.Roles)-1)})
#    h$Add(z2 <- gtkCheckButton("Stretch?"))
#    z2[["sensitive"]] <- FALSE
    assign(paste(i,".name.w",sep=""),z)
    assign(paste(i,".role.w",sep=""),z1)
#    assign(paste(i,".stretch.w",sep=""),z2)
#    z1$GetList()$AddCallback("select-child",select.role)
  }
  v$Add(h <- gtkHBox(FALSE))
  v$SetChildPacking(h,FALSE,FALSE,0,as.integer(0))
  h$Add(ok.b <- gtkButton("Apply"))
  h$Add(cancel.b <- gtkButton("Close"))
  ok.b["can-default"]<-T
  cancel.b["can-default"]<-T
  ok.b["has-default"]<-T
  ok.b$AddCallback("clicked",ok.clicked)
  cancel.b$AddCallback("clicked",on.close)
  w$AddCallback("delete-event",on.close)
  w$ShowAll()
  return(environment())
}

wisard.out <- function(curve,var.names=NULL,var.units=NULL){
  COMMIT.CB<-function(df){return(df)}
  ok.clicked <- function(...){
    l <- data.frame()
    for (i in c("X","Y","W","H","Size","Symbol","Color") ){
      get(paste(i,".include.w",sep=""))->z1
      if (z1$GetActive()){
        get(paste(i,".val.w",sep=""))->z
        p <- eval(parse(text=z$GetText()))
        if(!is.null(var.units)){
          comment(p) <- get(paste(i,".unit.w",sep=""))$GetText()
        }
        p<-data.frame(p)
        if (is.null(var.names)){
          i1 <- i
        } else {
          i1 <- get(paste(i,".name.w",sep=""))$GetText()
          if (i1=="NULL"|i1==""|i1=="null"){
            i1 <- i
          }
        }
        names(p)<-i1
        l<-c(l,p)
       }     
    }
    COMMIT.CB(l)
    w$Destroy()
  }
  X <- curve$x
  Y <- curve$y
  W <- curve$w
  H <- curve$h
  Size <- curve$size
  Symbol <- curve$pch
  Color <- ordered(curve$gp$fill,levels=palette())
  levels(Color) <- seq(along=palette())
  Color <- as.numeric(Color)
  w <- gtkWindow()
  w$Add(v <- gtkVBox(FALSE))
  
  v$Add(h <- gtkHBox(TRUE))
  v$SetChildPacking(h,FALSE,FALSE,2,as.integer(0))
  h$Add(gtkLabel("Data"))
  h$Add(gtkLabel("Include?"))
  if (!is.null(var.names)){
    h$Add(gtkLabel("Col. Name"))
    if (!is.null(var.units)){
      h$Add(gtkLabel("Meas. Unit"))
    }
  }
  for (i in c("X","Y","W","H","Size","Symbol","Color")){
    v$Add(h <- gtkHBox(TRUE))
    v$SetChildPacking(h,FALSE,FALSE,2,as.integer(0))
    h$Add(z <- gtkEntry())
    z$SetText(i)
    h$Add(z2 <- gtkCheckButton(""))
    switch(i,
           X={z2$SetActive(TRUE)},
           Y={z2$SetActive(TRUE)},
           {z2$SetActive(FALSE)})
    assign(paste(i,".val.w",sep=""),z)
    assign(paste(i,".include.w",sep=""),z2)
    if (!is.null(var.names)){
      h$Add(z3 <- gtkEntry())
      z3$SetText(ifelse(is.null(var.names[[i]]),"",var.names[[i]]))
      assign(paste(i,".name.w",sep=""),z3)
      if(!is.null(var.units)){
        h$Add(z4 <- gtkEntry())
        z4$SetText(ifelse(is.null(var.units[[i]]),"",var.units[[i]]))
        assign(paste(i,".unit.w",sep=""),z4)
      }
    }
  }
  v$Add(h <- gtkHBox(FALSE))
  v$SetChildPacking(h,FALSE,FALSE,2,as.integer(0))
  h$Add(ok.b <- gtkButton("OK"))
  h$Add(cancel.b <- gtkButton("Cancel"))
  ok.b["can-default"]<-T
  cancel.b["can-default"]<-T
  ok.b["has-default"]<-T
  ok.b$AddCallback("clicked",ok.clicked)
  cancel.b$AddCallback("clicked",function(...){ w$Destroy() })
  w$AddCallback("delete-event",function(...){ w$Destroy() })
  w$ShowAll()
  return(environment())
}
