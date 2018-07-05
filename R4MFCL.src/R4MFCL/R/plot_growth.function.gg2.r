#' @export
plot_growth.function.gg2 <- function(rep=tmp.rep,YLIM=c(0,210),
  xlab="Age (quarters)",alpha=0.6,plot=TRUE,only.mean=FALSE,model.names="Default",
  legend.position="bottom")
{
  require(ggplot2)
  require(magrittr)
  require(dplyr)
  require(tidyr)
  textlab="Length (cm)"
  # growth curve with variation
  if(length(model.names)==1){
  nages <- if(is.null(rep$version)){rep$nAges
           }else{
              ifelse(length(rep$nAges)>1,rep$nAges[1],rep$nAges)
           }
  nSp<-ifelse(is.null(rep$nSp),1,rep$nSp)

  ##length at age
  lth <-if(nSp==1){as.data.frame(rep$mean.LatAge)}else{as.data.frame(t(rep$mean.LatAge))}


  ##sd at length
  lthsd <-if(nSp==1){
    as.data.frame(rep$sd.LatAge)
  }else{
    as.data.frame(t(rep$sd.LatAge))
  }
  colnames(lthsd)<-colnames(lth)<-if(nSp==1){
    "Aggregated"
  }else{
    c("Male","Female")
  }
  lth$age<-1:nages
  lthsd$age<-1:nages
  lthrange <- seq(from=0,length=max(YLIM))
  lth.long<-lth %>%gather(key=Gender,value=meanLength,-age)
  plt<-lth.long %>% ggplot()
  if(nSp==1){
    plt<-plt+geom_line(aes(x=age,y=meanLength))
    plt<-plt+geom_point(aes(x=age,y=meanLength),size=2,color="black")
  }else{
    plt<-plt+geom_line(aes(x=age,y=meanLength,colour=Gender))
    plt<-plt+geom_point(aes(x=age,y=meanLength, colour=Gender),size=2)
  }
  plt<-plt+ylim(YLIM[1],YLIM[2])
   plt<-plt+guides(colour=guide_legend(title=NULL,legend.position="bottom"))
  plt<-plt+xlab(xlab)+ylab(textlab)
 if(!only.mean){
  positions<-list()
  if(nSp==1){
    for (i in 1:nages){
#      browser()
      a <- dnorm(lthrange, lth[i,"Aggregated"], lthsd[i,"Aggregated"])
      #trim the range
      lthrange2 <- lthrange[a > 0.0004]
      a <- a[a > 0.0004]
      a <- a/max(a*1.1)
      positions[[i]]<-data.frame(age=i,x=c(a+i,rep(i, length(lthrange2))),y=c(lthrange2, rev(lthrange2)))
    }
    positions2<-do.call("rbind",positions)
    plt<-plt+geom_polygon(data=positions2,aes(x=x,y=y,group=age),
      color="grey",alpha=alpha)
  }else{
    cols<-c("black","red")
    for(j in 1:nSp){
      for (i in 1:nages){
        a <- dnorm(lthrange, lth[i,j], lthsd[i,j])
        #trim the range
        lthrange2 <- lthrange[a > 0.0004]
        a <- a[a > 0.0004]
        a <- a/max(a*1.1)
        positions[[i]]<-data.frame(age=i,x=c(a+i,rep(i, length(lthrange2))),y=c(lthrange2, rev(lthrange2)))
      }
      positions2<-do.call("rbind",positions)
      positions2$Gender<-c("Male","Female")[j]
      plt<-plt+geom_polygon(data=positions2,
                  aes(x=x,y=y,group=age,colour=Gender,fill=Gender),
                  alpha=alpha)
    }
  }
  }
  plt<-plt+guides(fill=guide_legend(title=NULL,position=legend.position))
  }else{ # Multi model
    for(i in 1:length(rep)){
      rep[[i]]$.model.name<-model.names[i]
    }
    nages <-sapply(rep,function(x){
                    if(is.null(x$version)){
                      x$nAges
                    }else{
                      ifelse(length(x$nAges)>1,x$nAges[1],x$nAges)
                    }})
    nSp<-sapply(rep,function(x){ifelse(is.null(x$nSp),1,x$nSp)})
  ##length at age
    lth <-lapply(rep,function(x){
      lth0<-if(is.null(x$nSp) |x$nSp==1){
        as.data.frame(x$mean.LatAge)
      }else{
        as.data.frame(t(x$mean.LatAge))
      }
      #cat("L102\n");browser()
      colnames(lth0)<-paste0(x$.model.name,if(x$nSp==1){"_Aggregated"}else{c("_Male","_Female")})
      lth0$age<-if(x$nSp==1){1:nages}else{1:nages[1]}
      return(lth0)
    })

  ##sd at length
    lthsd <-lapply(rep,function(x){
      lthsd0<-if(is.null(x$nSp) |x$nSp==1){
        as.data.frame(x$sd.LatAge)
      }else{
        as.data.frame(t(x$sd.LatAge))
      }
      colnames(lthsd0)<-paste0(x$.model.name,if(x$nSp==1){"_Aggregated"}else{c("_Male","_Female")})
      lthsd0$age<-if(x$nSp==1){1:nages}else{1:nages[1]}
      return(lthsd0)
  })
  cat("117\n") #;browser()
  lthrange <- seq(from=0,length=max(YLIM))
  lth.long<-lth %>% lapply(.,function(x){x%>%gather(key=Gender,value=meanLength,-age)->out;return(out)})
  lth.long<- do.call("rbind",lth.long)
  lth.long %>% ggplot() ->plt
  if(length(nSp)==1 & nSp==1 | length(model.names)==1){
    cat("L123\n")#;browser()
    plt<-plt+geom_line(aes(x=age,y=meanLength))
    plt<-plt+geom_point(aes(x=age,y=meanLength),size=2,
      color="black")
  }else{
   cat("148\n")#;browser()
    plt<-plt+geom_line(aes(x=age,y=meanLength,colour=Gender))
    plt<-plt+geom_point(aes(x=age,y=meanLength, colour=Gender),size=2)
  }
  plt<-plt+ylim(YLIM[1],YLIM[2])
#  plt<-plt+guides(fill=guide_legend(title=NULL))
   plt<-plt+guides(colour=guide_legend(title=NULL))+theme(legend.position=legend.position)
  plt<-plt+xlab(xlab)+ylab(textlab)
 # browser()
 if(!only.mean){
  positions<-list()
  if(nSp==1){
    for (i in 1:nages){
#      browser()
      a <- dnorm(lthrange, lth[i,"Aggregated"], lthsd[i,"Aggregated"])
      #trim the range
      lthrange2 <- lthrange[a > 0.0004]
      a <- a[a > 0.0004]
      a <- a/max(a*1.1)
      positions[[i]]<-data.frame(age=i,x=c(a+i,rep(i, length(lthrange2))),y=c(lthrange2, rev(lthrange2)))
    }
    positions2<-do.call("rbind",positions)
    plt<-plt+geom_polygon(data=positions2,aes(x=x,y=y,group=age),
      color="grey",alpha=alpha)
  }else{
    cols<-c("black","red")
    for(j in 1:nSp){
      for (i in 1:nages){
        a <- dnorm(lthrange, lth[i,j], lthsd[i,j])
        #trim the range
        lthrange2 <- lthrange[a > 0.0004]
        a <- a[a > 0.0004]
        a <- a/max(a*1.1)
        positions[[i]]<-data.frame(age=i,x=c(a+i,rep(i, length(lthrange2))),y=c(lthrange2, rev(lthrange2)))
      }
      positions2<-do.call("rbind",positions)
      positions2$Gender<-c("Male","Female")[j]
      plt<-plt+geom_polygon(data=positions2,
                  aes(x=x,y=y,group=age,colour=Gender,fill=Gender),
                  alpha=alpha)
    }
  }
  }
  plt<-plt+guides(fill=guide_legend(title=NULL,legend.position=legend.position))+scale_color_hue(name="",labels=model.names)
  }
#  browser()
  if(plot)print(plt)
  return(invisible(plt))
}