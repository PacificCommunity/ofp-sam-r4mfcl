plot.catch.maps <- function(lims=c(100,260,-40,45), add.reg=TRUE, eez.path="I:/assessments/Pop dy modeling/MFCL/R functions/EZNEW2.TXT",
                                  reg.x=list(R1=c(140,170,170,140,140),
                                             R2=c(170,190,190,170,170)),
                                  reg.y=list(R1=c(0,0,20,20,0),
                                             R2=c(0,0,20,20,0)), RSCALE=0.02, gear.keep=setNames(c("firebrick3","yellow2","lawngreen"), c("L","S","Z")),
                             sp_code="bet_mt", dat=read.csv("BET15/catch5X5.csv",header=T), year.lims=c(2004,2013),
                             txt.col="yellow", adj.y = c(0,10,0,0,0), adj.x = c(0,0,0,0,0)) {

  # need these libraries
require(maps)
require(mapdata)
require(scales)

  eez <- read.table(eez.path)
  plot(1,1, yaxt="n", xaxt="n", type="n", xlim=c(lims[1]+10,lims[2]-10), ylim=c(lims[3]+5,lims[4]-5), ylab="", xlab="", bg="lightblue")
  polygon(c(lims[1],lims[2],lims[2],lims[1]), c(lims[3],lims[3],lims[4],lims[4]), col=alpha("lightblue",0.5))
  polygon(eez[,1], eez[,2], lwd=1, col="white")
  lines(eez[,1], eez[,2], lwd=1, col=grey(0.3))
  map('world2Hires',  yaxt="n", xaxt="n", add=T, resolution=1)
  map('world2Hires',  region = c("USA","Hawaii","Mexico","Japan","China","South Korea","North Korea","Philippines","Vietnam","Laos","Taiwan","Fiji",
                                 "Vanuatu", "Malaysia", "Australia", "New Zealand", "Indonesia", "New Caledonia", "Papua New Guinea", "Solomon Islands",
                                 "Peru", "Costa Rica", "Chile", "Ecuador", "Colombia", "Panama", "Nicaragua", "Belize", "Honduras", "Guatemala", "El Salvador",
                                 "Argentina", "Venezuela", "Brazil", "Bolivia","USSR","Monglolia","Canada","Cuba"), fill=T, add=T, yaxt="n", xaxt="n", col=grey(0.85))
  box(lwd=2)
  
# Plot the region boundaries if wanted  
  if(add.reg) lapply(1:length(reg.x), function(x) polygon(reg.x[[x]], reg.y[[x]], lwd=2))



# Add axes and labels
  axis(1, at=seq(lims[1],lims[2],by=10), labels=F)
  axis(2, at=seq(lims[3],lims[4],by=5), labels=F)
  latseq <- seq(lims[3]+10,lims[4]-10,by=10) ;latseq2 <- as.character(latseq)
  lonseq <- seq(lims[1]+20,lims[2]-20,by=20) ;lonseq2 <- as.character(lonseq)
  latseq2[latseq < 0] <- paste(abs(latseq[latseq < 0]),"S",sep="")
  latseq2[latseq > 0] <- paste(latseq[latseq > 0],"N",sep="")
  lonseq2[lonseq < 180] <- paste(lonseq2[lonseq < 180],"E",sep="")
  lonseq2[lonseq > 180] <- paste(360-lonseq[lonseq > 180],"W",sep="")
  axis(2, at=latseq, labels=latseq2, cex.axis=0.75, las=1)
  axis(1, at=lonseq, labels=lonseq2, cex.axis=0.75)

  add.catch(dat=dat, year.lims=year.lims, gear.keep=gear.keep, sp_code=sp_code, RSCALE=RSCALE)

# Plot the region names if wanted  
  if(add.reg) lapply(1:length(reg.x), function(x) text(mean(reg.x[[x]]) + adj.x[x], mean(reg.y[[x]]) + adj.y[x], x, col=txt.col, cex=1.6, font=2))

}


addpie <- function (x, labels = names(x), edges = 200, radius = 0.8, density = NULL,
                    angle = 45, col = NULL, border = NULL, lty = NULL, main = NULL, lat = 0, long =0, rscale = 1,
                    ...)
{
  
  radius <- sqrt(sum(x)/3.148) * rscale
  x <- c(0, cumsum(x)/sum(x))
  dx <- diff(x)

  nx <- length(dx)
  if (is.null(col))
    col <- if (is.null(density))
      c("white", "lightblue", "mistyrose", "lightcyan",
        "lavender", "cornsilk")
  else par("fg")
  col <- rep(col, length = nx)
  border <- rep(border, length = nx)
  lty <- rep(lty, length = nx)
  angle <- rep(angle, length = nx)
  density <- rep(density, length = nx)
  for (i in 1:nx) {
    n <- max(2, floor(edges * dx[i]))
    t2p <- 2 * pi * seq(x[i], x[i + 1], length = n)
    xc <- c(cos(t2p), 0) * radius
    yc <- c(sin(t2p), 0) * radius
    polygon(xc + long, yc + lat, density = density[i], angle = angle[i],
            border = border[i], col = col[i], lty = lty[i])
    t2p <- 2 * pi * mean(x[i + 0:1])
    xc <- cos(t2p) * radius
    yc <- sin(t2p) * radius
    if (!is.na(lab <- labels[i]) && lab != "") {
      text(1 * (xc+long), 1 * (yc+lat), lab, cex=0.75, xpd = TRUE, adj = ifelse(xc <
                                                                                  0, 1, 0), ...)
    }
  }
  invisible(NULL)
}


add.catch <- function(dat=read.csv("BET15/catch5X5.csv",header=T), year.lims=c(2004,2013), gear.keep = setNames(c("firebrick3","yellow2","lawngreen"), c("L","S","Z")),
                      sp_code="bet_mt", RSCALE=0.01)
{

# Manipulate data into the right format
  dat <- dat[dat$yy >= year.lims[1] & dat$yy <= year.lims[2],]
  dat <- aggregate(x=dat[,sp_code], by=list(dat$gear, paste(dat$lond,dat$latd)), FUN=sum)
  
  dat$lond <- as.numeric(sub(' .*','',dat$Group.2))
  dat$latd = as.numeric(sub('.* ','',dat$Group.2))

  dat$gear <- as.character(dat$Group.1)
  dat$gear <- ifelse(dat$gear %in% names(gear.keep), dat$gear, "Z")
  #dat[!(dat$gear %in% names(gear.keep)),]$gear <- "Z"
  
# adds pies of catch by gear to a plot of the model regions
  mat <- tapply(dat[,"x"], list(dat$Group.2, dat$gear), sum)
  mat <- ifelse(is.na(mat) == T, 0, mat)
  mat <- ifelse(mat == 0, 0.01, mat)
  index <- sort(unique(dat$Group.2))
  x <- dat$lond[match(index, dat$Group.2)]
  y <- dat$latd[match(index, dat$Group.2)]

  for (i in 1:length(x)){
    addpie(mat[i,], lat=y[i], long=x[i], rscale = RSCALE, col = gear.keep, labels = NA, edges=400)
  }
}




