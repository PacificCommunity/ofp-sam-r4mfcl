read_tag_rep <- function(frqfile=basefrq,tagrepfile="temporary_tag_report",tagfile="skj.tag",
                year1=1972, rm.prelims=FALSE) {           
##========================================================
##  Read tag data from temporary_tag_reportt into an R object consisting
## of two lists, one for predicted and one for observed recaps at
## age. Within each list there is a series of sub-lists, each containing
## a vector of recaps-at-age. Each sub-list corresponds to a particular
## group, region, fishery, capture date combination. That information is
## contained in a data frame along with other auxiliary information,
## including release information, consisting of region, data and tagging
## programme name. Thus rows in the data frame correspond to sub-lists
## within the predicted and observed lists. Here are the columns of the
## data frame:
##    grp  : num  relese group
##    creg : num  capture region
##    fry  : num  fishery
##    cyr  : num  capture year
##    cmo  : num  capture month
##    rreg : num  release region
##    ryr  : num  release year
##    rmo  : num  release month
##    prog : chr  tagging programme id
##    rtime: num  release time in decimal years
##    ctime: num  capture time in decimal years
##    t2rec: num  time at large
##    orec : num  sum of observed recoveries (sum of corresponding sub-list)
##    prec : num  sum of predicted recoveries (sum of corresponding sub-list)
##========================================================

getnums <- function(string){
##========================================================
## Extracts numbers out of mixed numbers and other text
##========================================================
  tt <- as.numeric(unlist(strsplit(string,"[^0-9.]+")))
  tt <- tt[!is.na(tt)]
  tt
}

scanText <- function (string, what = character(0), ...) 
{
  tc <- textConnection(string)
  result <- scan(tc, what = what, quiet = TRUE, ...)
  close(tc)
  return(result)
}

  ## get text of tag report
  aa <- readLines(tagrepfile)
  
  if(is.character(frqfile)) frqfile <- read.frq(frqfile)
  yr1 <- frqfile$struct$yr1
  if(is.character(tagfile)) tagfile <- read.tag(tagfile)

  tmp.pt <- grep("predicted recapture in region",aa)

  if(rm.prelims) aa <- aa[(tmp.pt[1]-1):length(aa)]

  ## establish pointers to different record types
  grp.pt <- grep("release group ", aa)
  reg.pt <- grep("predicted recapture in region",aa)
  cap.pt <- grep("fishery",aa)

  ncap <- length(cap.pt) # no. capture records
 
  ## get groups and regions
  gpp <- as.numeric(cut(cap.pt,grp.pt))
  rpp <- as.numeric(cut(cap.pt,reg.pt))
  gpp[is.na(gpp)] <- max(gpp,na.rm=T)+1
  rpp[is.na(rpp)] <- max(rpp,na.rm=T)+1

  ##  make data frame of group and region for each cap record
  grp <- numeric(length(grp.pt))
  reg <- numeric(length(reg.pt))
  for(i in 1:length(grp.pt)) grp[i] <- getnums(aa[grp.pt[i]])
  for(i in 1:length(reg.pt)) reg[i] <- getnums(aa[reg.pt[i]])
  xx <- data.frame(grp=grp[gpp],creg=reg[rpp])

  ## add fishery cap yr and cap month
  yy <- matrix(nrow=nrow(xx),ncol=3)
  colnames(yy) <- c("fry","cyr","cmo")
  for(i in 1:nrow(yy)) yy[i,] <- getnums(aa[cap.pt[i]])
  xx <- cbind(xx,yy)
  xx$cyr <- xx$cyr+year1-1
  
  ## get other auxiliary info
  reldat <- tagfile$rel[xx$grp,]
  colnames(reldat) <- c("rreg","ryr","rmo")
  xx <- cbind(xx,reldat)
  xx$ryr <- tagfile$rel$y[xx$grp]
  xx$rmo <- tagfile$rel$m[xx$grp]
  xx$prog <- tagfile$tagprog[xx$grp]
  xx$rtime <- with(xx,ryr+rmo/12)
  xx$ctime <- with(xx,cyr+cmo/12)
  xx$t2rec <- with(xx,(cyr*12+cmo-ryr*12-rmo)/3+1)
  xx$rtime <- with(xx,ryr+rmo/12)
  xx$ctime <- with(xx,cyr+cmo/12)
  
  ## get text of pred and obs capture data in 2 col matrix
  tt <- which(!((1:length(aa))%in%c(grp.pt,reg.pt,cap.pt)))
  captxt <- matrix(aa[tt],ncol=2,byrow=TRUE)
  
  ##  make lists of pred and obs recap-by-size vectors 
  predtag <- obstag <- list()
  for(i in 1:nrow(captxt)) {
    predtag[[i]] <- as.numeric(scanText(captxt[i,1]))
    obstag[[i]] <- as.numeric(scanText(captxt[i,2]))
  }

  ## add sum over sizes to auxiliary info
  xx$orec <- sapply(obstag,sum)
  xx$prec <- sapply(predtag,sum)

  list(auxdat=xx,predtag=predtag,obstag=obstag)
}
