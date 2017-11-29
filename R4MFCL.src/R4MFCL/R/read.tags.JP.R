#' @importFrom utils count.fields
 read.tags.JP <-
function(tagfile,reg_areas,fisheries) {
# Read Japanese tag data for skj assessment
# SDH 2010
  b <- count.fields(tagfile, skip=0)

  a <- readLines(tagfile)
  a <- a[-grep("#",a)]
  a <- gsub("PL_DW",1.001,a)
  a <- gsub("PL_OS",1.002,a)
  a <- gsub("PS",1.003,a)

  hd <- as.numeric(unlist(strsplit(a[1],"[[:space:]]+"))[-1])
  hd <- data.frame(nrel=hd[1],l1=hd[2],nint=hd[3],iwd=hd[4])
  nrecov.grp <- as.numeric(unlist(strsplit(a[2],"[[:space:]]+"))[-1])

  rel <- strsplit(a[b==6],"[[:space:]]+")
  rel <- t(matrix(unlist(rel),nrow=7)[c(2:7),])
  class(rel) <- "numeric"
  colnames(rel) <- c("lat","lattype","long","longtype","y","m")
  rel <- data.frame(rel)

  rel.lens <- strsplit(a[b==45],"[[:space:]]+")
  rel.lens <- t(matrix(unlist(rel.lens),nrow=46)[c(2:46),])
  class(rel.lens) <- "numeric"

  cc <- c(0,1)[(b==45) + 1]
  grp <- cumsum(cc)[b==9]

  rel.recov <- strsplit(a[b==9],"[[:space:]]+")
  rel.recov <- t(matrix(unlist(rel.recov),nrow=10)[c(2:10),])
  class(rel.recov) <- "numeric"
  colnames(rel.recov) <- c("len","fsh","yr","m","lat","lattype","long","longtype","n")
  rel.recov <- data.frame(rel.recov)
  rel.recov <- cbind(rel.recov,grp)

  rel$lat <- rel$lat * c(1,-1)[rel$lattype]
  rel$long[rel$longtype==2] <- 360 - rel$long[rel$longtype==2]
  rel.recov$lat <- rel.recov$lat * c(1,-1)[rel.recov$lattype]
  rel.recov$long[rel.recov$longtype==2] <- 360 - rel.recov$long[rel.recov$longtype==2]

  rel$reg <- 9
  rel.recov$reg <- 9

#  for(i in 1:length(rel$lat)) rel[i,]$reg <- reg_areas[reg_areas$lat==rel[i,]$lat & reg_areas$long==rel[i,]$long,]$reg
  
  rel$reg <- reg_areas[match(paste(rel$lat,rel$long),paste(reg_areas$lat, reg_areas$long)),]$reg
  rel.recov$reg <- reg_areas[match(paste(rel.recov$lat,rel.recov$long),paste(reg_areas$lat, reg_areas$long)),]$reg
#  for(i in 1:length(rel.recov$lat)) rel.recov[i,]$reg <- reg_areas[reg_areas$lat==rel.recov[i,]$lat & reg_areas$lon==rel.recov[i,]$long,]$reg

  cutrel <- (1:length(rel$reg))[rel$reg==9 | rel$y > 2008]
  rel <- rel[-cutrel,]
  nrecov.grp <- nrecov.grp[-cutrel]
  rel.lens <- rel.lens[-cutrel,]
  rel.recov <- rel.recov[(rel.recov$grp %in% cutrel)==F,]
  oldgrp <- 1:hd$nrel
  for (i in cutrel) oldgrp[i:length(oldgrp)] <- oldgrp[i:length(oldgrp)] - 1
  rel.recov$grp <- oldgrp[rel.recov$grp]
  hd$nrel <- length(rel$reg)

  nrecov.grp <- as.vector(table(factor(rel.recov$grp,levels=1:hd$nrel)))

  rel.recov$gear <- ""
  rel.recov$gear[rel.recov$fsh==1.001] <- "PL_DW"
  rel.recov$gear[rel.recov$fsh==1.002] <- "PL_OS"
  rel.recov$gear[rel.recov$fsh==1.003] <- "PS"

  rel$lat <- rel$long <- NULL
  rel$lattype <- rel$longtype <- NULL
  rel.recov$lat <- rel.recov$long <- NULL
  rel.recov$lattype <- rel.recov$longtype <- rel.recov$fsh <- NULL
  return(list(hd=hd,nrecov.grp=nrecov.grp,rel=rel,rel.lens=rel.lens,rel.recov=rel.recov))
}
