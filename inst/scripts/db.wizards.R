library(DBI)
library(RMySQL)

lablog.append <- function(df){
}

lablog.find <- function(out=FALSE){
  md <- MySQL()
  mc <- dbConnect(md)
  .zzz <- try(mysqlInsertId(mc))
  if(class(.zzz)=="try-error"){
    mysqlInsertId <- function(conn){
      return(dbGetQuery(conn,"select last_insert_id()"))
    }
  }
  frame <- NULL
  selectedEntry<<-NULL
  selectedGroup<<-NULL
  search.db <- function(...){
    qr <- NULL
    if ((z <- sid.w$GetText())!=""){
      qr <- c(qr,paste("SAMPLES.sid like","\'",z,"\'",sep=""))
    }
    if ((z <- composition.w$GetText())!=""){
      qr <- c(qr,paste("SAMPLES.composition like","\'",z,"\'",sep=""))
    }
    if ((z <- type.w$GetText())!=""){
      qr <- c(qr,paste("JOURNAL.type like \'",z,"\'",sep=""))
    }
    if ((z <- name.w$GetText())!=""){
      qr <- c(qr,paste("PARS.parname like \'",z,"\'",sep=""))
    }
   if ((z <- unit.w$GetText())!=""){
      qr <- c(qr,paste("PARS.parunit like \'",z,"\'",sep=""))
    } 
    if ((z <- other.w$GetText())!=""){
      qr <- c(qr,z)
    }
    qr <- paste(qr,collapse=" and ")
    if (qr!=""){
      qr <- paste("and",qr)
    }

    frame <<- dbGetQuery(mc,statement=paste("select SAMPLES.*,JOURNAL.expid,JOURNAL.type,JOURNAL.date,PARS.pargroup,PARS.parid,concat(PARS.parname,'[0] = ',PARVALUES.value,' ',PARS.parunit) from SAMPLES natural left join JOURNAL natural left join PARS natural left join PARVALUES where (PARVALUES.tuple=0 or PARVALUES.tuple is null)",qr,"order by SAMPLES.sample,JOURNAL.expid,PARS.pargroup,PARS.parid"))
    selectedGroup<<-NULL
    selectedEntry<<-NULL
    selectedSample <<- ""
    c.w$Clear()
    c.w$Freeze()
    for (i in 1:dim(frame)[1]){
      c.w$Append(frame[i,])
    }
    c.w$Thaw()
  }
  frame.stat <- function(w, r, c, ev, selected=TRUE) {
    selectedGroup<<-frame$pargroup[r+1]
    selectedEntry<<-frame$expid[r+1]
    selectedSample<<-frame$sid[r+1]
    frame.stat <<- dbGetQuery(mc,paste("select concat(parname,'(',parunit,')[',PARS.parid,']'),count(value),min(value),max(value),avg(value),std(value) from PARS natural left join PARVALUES where pargroup=",frame$pargroup[r+1],"group by PARS.parid"))
    stat.w$Clear()
    stat.w$Freeze()
    for (i in 1:dim(frame.stat)[1]){
      stat.w$Append(frame.stat[i,])
    }
    stat.w$Thaw()
  }
  set.pars <- function(...){
    commit.to.db <- function(df){
      if (is.null(selectedEntry)){
        selectedSample <<- sid.w$GetText()
        dbGetQuery(mc,paste("insert into SAMPLES set sample=null,sid=\'",sid.w$GetText(),"\',composition=\'",composition.w$GetText(),"\'",sep=""))
        mysqlInsertId(mc)->j
        dbGetQuery(mc,paste("insert into JOURNAL set sample=",j,",expid=null,type=\"",type.w$GetText(),"\",date=now()",sep=""))
        selectedEntry <<- mysqlInsertId(mc)
      }
      dbGetQuery(mc,"insert into PARGROUPS set pargroup=null")
      selectedGroup<<-mysqlInsertId(mc)
      for (i in seq(along=df)){
        i1 <- comment(df[[i]])
        i2 <- names(df)[[i]]
        dbGetQuery(mc,paste("insert into PARS set expid=",selectedEntry,",pargroup=",selectedGroup,",parid=null,parname=\'",i2,"\',parunit=\'",i1,"\'",sep=""))
        mysqlInsertId(mc)->j
        vals <- paste(j,seq(along=df[[i]])-1,df[[i]],sep=",",collapse="),(")
        dbGetQuery(mc,paste("insert into PARVALUES values(",vals,")",sep=""))
      }
    }
    e<-RGrace:::wisard.out(grid.get(current.Figure$current.Panel$elements[[current.Figure$current.Panel$.Selected.Element]]$name),commit.to.db,var.names=list(),var.units=list())
  }
  get.frame <- function(...){
    if (is.null(selectedGroup)){
      search.db()

      if (length(unique(frame$pargroup)!=1)){
        return()
      }
      selectedSample<<-frame$sid[1]
      selectedEntry<<-frame$expid[1]
      selectedGroup<<-frame$pargroup[1]
    }
    z <- dbGetQuery(mc,paste("select parid,parname,parunit from PARS where pargroup=",selectedGroup,sep=""))
    print(z)
    g<-NULL
    for (i in 1:dim(z)[1]){
      k<-dbGetQuery(mc,paste("select value from PARVALUES where parid=",z[["parid"]][i],sep=""))
      names(k) <- z[["parname"]][i]
      comment(k) <- z[["parunit"]][i]
      if (is.null(g)){
        g <- k
      } else {
        g<-data.frame(g,k)
      }
    }
    assign(paste(".",selectedSample,sep=""),g,envir=parent.env(environment()))
    on.close()
    RGrace:::wisard.in(g,frame.name=selectedSample)
  }
  on.close <- function(...){
    dbDisconnect(mc)
    w$Destroy()
  }
  w <- gtkWindow()

  w$SetTitle(ifelse(out,"Upload To Lablog","Download from LabLog"))
  w$Add(v <- gtkVBox(FALSE,6))
  
  v$Add(h <- gtkHBox(TRUE,4))
  v$SetChildPacking(h,FALSE,FALSE,0)
  h$Add(gtkLabel("SampleID:"))
  h$Add(sid.w <- gtkEntry(10))
  h$Add(gtkLabel("Composition:"))
  h$Add(composition.w <- gtkEntry(10))

  v$Add(h <- gtkHBox(TRUE,4))
  v$SetChildPacking(h,FALSE,FALSE,0)
  h$Add(gtkLabel("ExpType:"))
  h$Add(type.w <- gtkEntry(10))
  h$Add(gtkLabel("Parname:"))
  h$Add(name.w <- gtkEntry(10))

  v$Add(h <- gtkHBox(TRUE,4))
  v$SetChildPacking(h,FALSE,FALSE,0)
  h$Add(gtkLabel("Parunit:"))
  h$Add(unit.w <- gtkEntry(10))
  h$Add(gtkLabel("Other:"))
  h$Add(other.w <- gtkEntry(10))

  v$Add(h <- gtkHBox(TRUE,3))
  v$SetChildPacking(h,FALSE,FALSE,0)
  h$Add(find.w <- gtkButton("Find"))
  h$Add(get.w <- gtkButton("Apply"))
  h$Add(cancel.w <- gtkButton("Close"))
  v$Add(sw<-gtkScrolledWindow())
  sw$Add(c.w<-gtkCList(9,c("Sample #","  SID  ","Comosition","Exp #", " Type ", "    Date   ","Frame #","Par #","Parname[Unit]=Value[0]")))
  v$Add(sw2<-gtkScrolledWindow())
  sw2$Add(stat.w<-gtkCList(6,c("   Parameter   ","  Length  ","    Min   ","    Max   ", "   Average  ", "    STD   ")))
  if (out){
    get.w$AddCallback("clicked",set.pars)
  } else {
    get.w$AddCallback("clicked",get.frame)
  }
  find.w$AddCallback("clicked",search.db)
  c.w$AddCallback("select_row", frame.stat)
  w$AddCallback("delete-event",on.close)
  cancel.w$AddCallback("clicked",on.close)
  return(environment())
}


db.user.menu <- function(fig){
  z1.Action <- function(...){
    fig$fig$set.active()
    l <- lablog.find(FALSE)
  }
  z2.Action <- function(...){
    fig$fig$set.active()
    l <- lablog.find(TRUE)
  }
  z1 <- gtkMenuItem("Download From LabLog")
  z2 <- gtkMenuItem("Upload To LabLog")
  z <- fig$file.sub
  z$Insert(z2,4)
  z$Insert(z1,4)
  z1$AddCallback("activate",z1.Action)
  z2$AddCallback("activate",z2.Action)
}
