library(methods)
library(DBI)
library(RMySQL)

mysqlInsertId <- function(obj, ...){
  if(!isIdCurrent(obj))
    stop(paste("expired", class(obj), deparse(substitute(obj))))
  info <- .Call("RS_MySQL_insertid", as(obj, "integer"), PACKAGE = "RMySQL")
  return(info$iid)
}


lablog.find <- function(out=FALSE){
  md <- MySQL()
  mc <- dbConnect(md)
  TableSchema <- dbGetInfo(mc)$dbname
  frame <- NULL
  selectedEntry<-NULL
  selectedGroup<-NULL
  selectedSample <- ""
  DF<-NULL

  COMMIT.CB<-function(...){}

 
  gladewidget2<-function(name,tree=XML.db){
    w<-gladeXMLGetWidget(tree,name)
    class(w)<-gtkObjectGetClasses(w,check=F)
    w
  }

  search.db <- function(...){
    qr <- NULL
    if ((z <- sid.w$GetText())!=""){
      qr <- c(qr,paste("SAMPLES.sid like \'",z,"\'",sep=""))
    }
    if ((z <- composition.w$GetText())!=""){
      qr <- c(qr,paste("SAMPLES.composition like \'",z,"\'",sep=""))
    }
    if ((z <- type.w$GetText())!=""){
      qr <- c(qr,paste("JOURNAL.type like \'",z,"\'",sep=""))
    }
    if ((z <- name.w$GetText())!=""){
      qr <- c(qr,paste("PARAMS.parname like \'",z,"\'",sep=""))
    }
    if ((z <- unit.w$GetText())!=""){
      qr <- c(qr,paste("PARAMS.parunit like \'",z,"\'",sep=""))
    }
    if ((z <- parvalue.w$GetText())!=""){
      qr <- c(qr,paste("PARAMS.value ",z,sep=""))
    }
    if ((z <- date.w$GetText())!=""){
      qr <- c(qr,paste("JOURNAL.date  ",z,sep=""))
    }
    if ((z <- frame.pref.w$GetText())!=""){
      qr <- c(qr,paste("FRAMES.prefix like \'",z,"\'",sep=""))
    }
    if ((z <- col.name.w$GetText())!=""){
      qr <- c(qr,paste("information_schema.columns.column_name like \'",z,"\'",sep=""))
    }
    if ((z <- other.w$GetText())!=""){
      qr <- c(qr,z)
    }
    qr <- paste(qr,collapse=" and ")
    if (qr!=""){
      qr <- paste("and",qr)
    }
    statement<-paste("select SAMPLES.*,JOURNAL.expid,JOURNAL.type,JOURNAL.date,group_concat(distinct concat(if(PARAMS.parname is null, \"\" , PARAMS.parname),\" = \",if(PARAMS.value is null, \"\" , PARAMS.value),\" \", if(PARAMS.parunit is null, \"\" , PARAMS.parunit)) order by PARAMS.parname separator \"\n\") as param,concat(FRAMES.prefix,FRAMES.frameid) as tablename,if(FRAMES.prefix='','',group_concat(distinct concat(information_schema.columns.column_name,\"(\",information_schema.columns.column_comment,\")\") order by information_schema.columns.ordinal_position separator \"\n\")) as 'raw data' from SAMPLES ignore index (sid) join JOURNAL using (sample) join (FRAMES left join PARAMS using (frameid) left join information_schema.columns on (information_schema.columns.table_name=concat(FRAMES.prefix,FRAMES.frameid))) on (FRAMES.expid=JOURNAL.expid) where (information_schema.columns.table_schema='",TableSchema,"' or FRAMES.prefix='') ",qr," group by information_schema.columns.table_name,FRAMES.frameid order by SAMPLES.sample,JOURNAL.expid,FRAMES.frameid",sep="")

    frame <<- dbGetQuery(mc,statement)
#    frame <<- dbGetQuery(mc,statement=paste("select SAMPLES.*,JOURNAL.expid,JOURNAL.type,JOURNAL.date,concat(if(PARAMS.parname is null, \"\" , PARAMS.parname),\" = \",if(PARAMS.value is null, \"\" , PARAMS.value),\" \", if(PARAMS.parunit is null, \"\" , PARAMS.parunit)) as param,concat(FRAMES.prefix,FRAMES.frameid) as tablename,\' \' as 'raw data' from SAMPLES join JOURNAL using (sample) join FRAMES using (expid) join PARAMS using (frameid)  where true ",qr," group by FRAMES.frameid,param order by SAMPLES.sample,JOURNAL.expid,FRAMES.frameid,param"))

    v1<-factor(frame[,1])
    frame[10]<<-ifelse(as.logical(cumsum(c(1,as.integer(v1[1:length(v1)-1]!=v1[2:length(v1)])))%%2),"white","grey90")
    v1<-factor(frame[,4])
    frame[11]<<-ifelse(as.logical(cumsum(c(1,as.integer(v1[1:length(v1)-1]!=v1[2:length(v1)])))%%2),"white","grey90")
    v1<-factor(frame[,8])
    frame[12]<<-ifelse(as.logical(cumsum(c(1,as.integer(v1[1:length(v1)-1]!=v1[2:length(v1)])))%%2),"white","grey90")
    frame[13]<<- ifelse(as.logical(seq(along=frame[,7])%%2),"white","grey90")
    selectedGroup<<-NULL
    selectedEntry<<-NULL
    selectedSample <<- ""
    mode(frame$sample) <- "integer"
    mode(frame$expid) <- "integer"
    tv.c.w$SetModel(rGtkDataFrame(frame))
  }

  frame.stat <- function(tv) {
    gtkTreePathGetIndices(tv$GetCursor()$path)[1]->r
    tv$GetCursor()$focus.column$GetData("ColRole")->c
    if (c>8) {
      sample.e.w$SetValue(frame$sample[r+1])
      sid.e.w$SetText(frame$sid[r+1])
      compos.e.w$SetText(frame$composition[r+1])

    }
    if (c > 9){
      expid.e.w$SetValue(frame$expid[r+1])
      type.e.w$SetText(frame$type[r+1])
      date.e.w$SetText(frame$date[r+1])
    }
    if (c > 10){
      frameid.e.w$SetValue(as.integer(sub("[[:alpha:]]*([[:digit:]]*)$","\\1",frame$tablename[r+1],extended=T,fixed=F)))
      prefix.e.w$SetText(sub("([[:alpha:]]*)[[:digit:]]*$","\\1",frame$tablename[r+1],extended=T,fixed=F))
      parameters.e.b$SetText(frame$param[r+1])
    }
    selectedGroup<<-frame$tablename[r+1]
    selectedEntry<<-frame$expid[r+1]
    selectedSample<<-frame$sid[r+1]
    tr<-dbGetQuery(mc,paste("select table_rows from information_schema.tables where table_name='",selectedGroup,"'",sep=""))
    tc<-dbGetQuery(mc,paste("select column_name,column_comment from information_schema.columns where table_name='",selectedGroup,"'",sep=""))
    zz <- lapply(1:length(tc[[1]]),function(i){
      paste("select '",paste(tc[[1]][i],"(",tc[[2]][i],")"),"',max(",tc[[1]][i],"),min(",tc[[1]][i],"),avg(",tc[[1]][i],"),std(",tc[[1]][i],"),(select ",tc[[1]][i]," from ",selectedGroup," where ituple=1),(select ",tc[[1]][i]," from ",selectedGroup," where ituple=",tr,") from ",selectedGroup)
    })
    frame.stat <- dbGetQuery(mc,paste("(",paste(zz,collapse=")union("),")"))
    tv.stat.w$SetModel(rGtkDataFrame(frame.stat))
  }

  set.pars <- function(...){
    check.empty <- function(obj,test="",default="null",quot=""){
      return(ifelse((j <- obj$GetText())==test,default,paste(quot,j,quot,sep="")))
    }
    dbGetQuery(mc,paste("replace into SAMPLES set sample=",check.empty(sample.e.w,"0"),",sid=\'",check.empty(sid.e.w),"\',composition=\'",check.empty(compos.e.w),"\'",sep=""))
    mysqlInsertId(mc)->j
    dbGetQuery(mc,paste("replace into JOURNAL set sample=",j,",expid=",check.empty(expid.e.w,"0"),",type=\'",check.empty(type.e.w),"\',date=",check.empty(date.e.w,"","now()","\'"),sep=""))
    j<- mysqlInsertId(mc)
    dbGetQuery(mc,paste("replace into FRAMES set expid=",j,",prefix=\'",pref <- check.empty(prefix.e.w,"",ifelse(is.null(DF),"","exp")),"\',frameid=",check.empty(frameid.e.w,"0"),sep=""))
    j<- mysqlInsertId(mc)
    selectedGroup <<- paste(pref,j,sep="")
    print("OK")
    if ((z <- parameters.e.b["text"])!=""){
      
      dbGetQuery(mc,paste("delete from PARAMS where frameid=",j))
      for (ll in strsplit(z,"\n")[[1]]) {
        z1 <- sub("([[:alnum:]]*)[[:blank:]]*=[[:blank:]]*([[:digit:].Ee+-]*)[[:blank:]]*([[:alnum:]]*).*","\\1 \\3 \\2",ll,extended=T,fixed=F)
        z1 <- strsplit(z1," ")[[1]]
        if (z1[1]!=""){
          print(z1[1])
          print(z1[2])
          print(z1[3])
          dbGetQuery(mc,paste("replace into PARAMS set parname=\'",z1[1],"\',parunit=\'",z1[2],"\',value=",ifelse(z1[3]=="","null",z1[3]),",frameid=",j,sep=""))
        }
      }      
    }
    if (!is.null(DF)){
      dbGetQuery(mc,paste("drop table if exists ",selectedGroup,sep=""))
      dbWriteTable(mc,selectedGroup,DF,overwrite=TRUE,row.names=TRUE)
      dbGetQuery(mc,paste(paste("alter table",selectedGroup,"change column row_names ituple int not null auto_increment primary key, "),paste("change column ",make.db.names(mc,names(DF))," ",make.db.names(mc,names(DF))," double default 0 comment \'", sapply(DF,function(x){ifelse(is.null(z <- comment(x)),"",z)}),"\'",collapse=",",sep="")))
      frameid.e.w$SetValue(j)
      prefix.e.w$SetText(pref)
      DF<<-NULL
    }
  }

  get.frame <- function(...){
    if (is.null(selectedGroup)){
      search.db()
      selectedSample<<-frame$sid[1]
      selectedEntry<<-frame$expid[1]
      selectedGroup<<-frame$tablename[1]
    }
    DF<<- dbReadTable(mc,selectedGroup,row.names="ituple")
    z1<-dbGetQuery(mc,paste("show full columns from",selectedGroup))
    comment(DF)<<-paste(selectedGroup)
    for (i in 1:dim(DF)[2]){
      comment(DF[i]) <- z1[["Comment"]][i+1]
    }
    on.close()
    COMMIT.CB(DF)    
  }
  on.close <- function(...){
    try(dbDisconnect(mc))
    try(dbUnloadDriver(md))
    window2.w$Destroy()
  }
  if (out){
    get.or.set<-set.pars
  } else {
    get.or.set<-get.frame
  }

  del.view.cb <- function(...){
    z<-"Not yet ready"
    (gtkMessageDialogNew(window2.w,GtkMessageType["warning"],"modal",GtkButtonsType["yes-no"],z))$Run()
  }

  XML.db <- gladeXMLNew(system.file("db.interface.glade",package="RGrace"))
#  XML.db <- gladeXMLNew("/usr/local/src/CRAN/RGorace/inst/db.interface.glade")
  sapply(c("sample.e","sid.e","compos.e","expid.e","type.e","date.e","parameters.e","frameid.e","prefix.e","tv.c","tv.stat","get","sid","composition","type","name","unit","other","parvalue","frame.pref","date","col.name","window2"),function(x){assign(paste(x,".w",sep=""),gladewidget2(x,XML.db),envir=parent.env(environment()))})
  parameters.e.w$SetBuffer(parameters.e.b <- gtkTextBuffer())
  h1<-cbind(c("Sample #","  SID  ","Comosition","Exp #", " Type ", "    Date   ","Parname = Val Unit","Frame","Columns","col1","col2","col3","col4"),c("integer","character","character","integer","character","character","character","character","character","character","character","character","character")) #
  tv.c.w$SetModel(c.w<-gtkListStoreNew(h1[,2]))
  for (i in 1:9){
    col<-12
    if (i%in%1:3){col<-9}
    else if (i%in%4:6){col<-10}
    else if (i%in%8:9){col<-11}
    tv.c.w$AppendColumn(zz <- gtkTreeViewColumnNewWithAttributes(h1[i,1],gtkCellRendererTextNew(),text=i-1,background=col))
    zz$SetData("ColRole",col)
  }
  h2<- cbind(c("   Parameter   ","    Max   ","    Min   ", "   Average  ", "    STD   "," First ", " Last "),c("character","numeric","numeric","numeric","numeric","numeric","numeric"))
  tv.stat.w$SetModel(stat.w<-gtkListStoreNew(h2[,2]))
  for (i in seq(along=h2[,1])){
    tv.stat.w$AppendColumn(gtkTreeViewColumnNewWithAttributes(h2[i,1],gtkCellRendererTextNew(),text=i-1))   
  }
  try(gladeXMLSignalAutoconnectFull(XML.db,function(handler,obj,signal,...){try(obj$AddCallback(signal,get(handler)))},"TheEnd"))
  if (out){
    window2.w$SetTitle("Upload to LabLog")
  } else {
    window2.w$SetTitle("Download from LabLog")
  }
  window2.w$ShowAll()
  return(environment())
}


db.user.menu <- function(fig){

  z1.Action <- function(...){
    fig$fig$set.active()
    lablog.find(FALSE)->z
    z$COMMIT.CB<-function(df){
      print("OK")
      RGrace:::wisard.in(df,frame.name=comment(df))
    }
  }

  z2.Action <- function(...){
    fig$fig$set.active()
    RGrace:::wisard.out(grid.get(current.Figure$current.Panel$elements[[current.Figure$current.Panel$.Selected.Element]]$name),var.names=list(),var.units=list())->z
    z$COMMIT.CB<-function(df){
      dd<-lablog.find(TRUE)
      dd$DF<-df
    }
  }

  z1 <- gtkMenuItem("Download From LabLog")
  z2 <- gtkMenuItem("Upload To LabLog")
  z <- fig$gladewidget("file1_menu1")
  z$Insert(z2,4)
  z$Insert(z1,4)
  z1$AddCallback("activate",z1.Action)
  z2$AddCallback("activate",z2.Action)
}
