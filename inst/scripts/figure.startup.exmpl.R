hooks.user.menu <- function(fig){
  all.hooks <- list(element=c("default.hook","simple.calculus","delete.in.range","lf.in.range","xsort.curve","move.indexed","move.indexed.y","smooth.curve","spline.curve"),
                    region=c("default.hook","simple.calculus","delete.in.range","lf.in.range","xsort.curve","smooth.curve","spline.curve"),
                    points=c("default.hook","spline.points"))

  check.hook <- function(Item){
    my.name <- Item["name"]
    my.hooks <- all.hooks[[my.name]]
    for (j in my.hooks){
      z1 <- get(paste(my.name,".",j,".w",sep=""))
      if(identical(get(paste("on.select.",my.name,sep="")),get(j))){
        z1$Select()
      } else {
        z1$Deselect()
      } 
    }
                                        #    element.sub$Popup()
  }

  assign.hook <- function(Item){
    p <- strsplit(Item["name"],"\\.")
    hook.name <-  paste("on.select.",p[[1]][1],sep="")
    callback.name <- paste(p[[1]][-1],collapse=".")
    assign(hook.name,get(callback.name),envir=.GlobalEnv)
                                        #    Item$SetActive(FALSE)
  }

  Z <- fig$gladewidget("menubar1")
  Z$Add(hooks<-gtkMenuItem("Hooks"))
  hooks$SetSubmenu(hooks.sub <- gtkMenu())
  sapply(names(all.hooks),function(xxx){
    hooks.sub$Append(hook.sub <- gtkMenuItem(paste("On",xxx,"select")))
    hook.sub["name"] <- xxx
    hook.sub$SetSubmenu(z.sub <- gtkMenu())
    hook.sub$AddCallback("activate",check.hook)
    for (i in all.hooks[[xxx]]){
      z.sub$Append(z1 <- gtkMenuItem(i))
      z1["name"] <- paste(xxx,".",i,sep="")
      assign(paste(xxx,".",i,".w",sep=""),z1,envir=parent.env(environment()))
                                        #     z1$SetShowToggle(TRUE)
      z1$AddCallback("activate",assign.hook)
    }
  })
}

