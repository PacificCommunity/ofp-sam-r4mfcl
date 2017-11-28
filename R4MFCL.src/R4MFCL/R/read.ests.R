 #' @importFrom magrittr "%>%"
 read.ests <- function(rep.obj,ests="C:/assessments/alb/2008/6_area/28.splitgr3/ests.rep",x=1,verbose=FALSE)
{
# Simon D Hoyle 24/6/09
# Based on do.critical.calcs
# Update  YT 20170716
#require(magrittr)
myrep <- rep.obj

# Dimensioning stuff
Nages <- myrep$nAges
Nfish <- myrep$nFisheries
#Nyears <- (myrep$nTimes/myrep$nRecs.yr)-1
Nyears <- (myrep$nTimes/myrep$nRecs.yr)
Fyear <- myrep$Year1

# Biological stuff
lenatage <- myrep$mean.LatAge
wtatage <- myrep$mean.WatAge

a <- readLines(ests)
line1 <- grep("Predicted catch by fishery by year and age", a)

# Determine number of lines to skip
#start.lines <- seq(from=line1,length=Nfish,by=(Nyears+x)) + 1:Nfish
start.lines <-grep("^Fishery",a)[grep("^Fishery",a)>line1][1:Nfish]+1
Nages1<-if(is.null(myrep$nSp) || myrep$nSp==1){Nages}else{Nages[1]}
catage <- array(NA,dim=c(Nyears,Nages1,Nfish),dimnames=list(seq(from=Fyear,length=Nyears,by=1),c(1:Nages1),c(1:Nfish)))
if(verbose)cat("L27;")
#browser()
# create an array with all the catch at age data
  for(i in 1:Nfish)
  {

    ## dat <- scan(ests, skip=start.lines[i], nmax=Nyears*Nages)
    dat <-a[start.lines[i]+1:Nyears-1] %>% sapply(.,trimws) %>% strsplit(.,split=" +") %>% sapply(.,"as.numeric") %>% t()
    dimnames(dat)<-NULL
#    browser()
    catage[,,i] <- matrix(dat,nrow=Nyears,ncol=Nages1,byrow=FALSE)
#    browser()
  }

start.lines <- grep("fishery  realiz. realiz. ratio      predicted     observed   observed    effort      number of", a)

return(list(
    catage = catage))
}
