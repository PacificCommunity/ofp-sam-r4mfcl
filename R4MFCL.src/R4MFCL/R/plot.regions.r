#' Plot regions for assessment
# ' @importFrom RColorBrewer
#' 
#' @import mapdata 

plot.regions <- function(col.map='wheat3', col.regions='lightyellow', col.labels='black'){

#require(RColorBrewer)
#require(mapdata)

add.continents.NDC <- function(..., lonlim=c(-180,360), latlim=c(-90,90), new.map=FALSE) {

  if(new.map) {
    par(mfrow=c(1,1))
    plot(1,type="n",xaxs="i",yaxs="i",xlim=lonlim, ylim=latlim, ann=FALSE, axes=FALSE)
  }

  # EEZ land masses to keep for map --

  eez2keep <- c("American Samoa", "Australia","Bhutan",
                "Canada","China","Cook Islands","Cuba","Fiji", "French Polynesia",
                "Guam", "Haiti","Hawaii", "Indonesia","Japan",
                "Kiribati", "Korea","Malaysia","Marshall Islands", "Mexico", "Micronesia","Mongolia",
                "Myanmar","Nauru", "New Caledonia", "New Zealand", "Niue", "Northern Mariana Islands",
                "Palau", "Papua New Guinea", "Philippines", "Samoa", "Solomon Islands",
                "Tokelau", "Tonga", "Tuvalu", "USA", "USSR", "Vanuatu",
                "Panama","Chile","Argentina",
                "Belize","Nicaragua","Ecuador","Honduras","Costa Rica",
                "Colombia", "Uruguay", "Brazil", "Peru", "Guatemala",
                "Venezuela","Bolivia","Paraguay","Dominica")


  data(world2HiresMapEnv,envir = environment()) # loads dataset with landlines
  spc.region <- map('world2Hires', namesonly=T, ylim=c(-45,50), plot=F)

  # Keep SPC countries + Indonesia (to show with PNG)
  geo <- lapply(eez2keep, function(ee) spc.region[grep(ee,spc.region)])

  # ... add land for SPC countries (defined in object "geo")
  map('world2Hires', regions=unlist(geo), add=T,wrap=TRUE,fill=TRUE, ...)

 }


# Convert to transparent colors
col2transp <- function(col,tlev=0.5) {

sa <- lapply(col, function(cc) col2rgb(cc)/255)
s2 <- sapply(sa,function(s1) rgb(s1[1],s1[2],s1[3],alpha=tlev))
return(s2)
}

# Check if graphic device is of correct size, else opens one
check.dev.size <- function(ww,hh,use.prop=FALSE) {

  if(hh>7.5 & use.prop) {
    rt <- ww/hh
    hh <- 7.5
    ww <- hh*rt
  }
  if(dev.cur()==1){ dev.new(width=ww,height=hh)
  } 
  else {
    ds <- dev.size()
    if(round(ds[1],2)!=round(ww,2)| round(ds[2],2)!=round(hh,2)) {
      dev.off(); dev.new(width=ww,height=hh)
    } 
  }
}




switch.coords <- function(x) {

  num.x <- as.numeric(gsub("([0-9]*).*","\\1",x))
  if(grepl("E|N",x)) {
        num.x
  } 
  else if(grepl("W",x)) {
    360 - num.x
  } 
  else {
    -num.x
  }
}

get.reg <- function(wreg) {

    lon.test <- reg.index$lon %between% c(limits.reg[wreg,c("lon.min","lon.max")])
    lat.test <- reg.index$lat %between% c(limits.reg[wreg,c("lat.min","lat.max")])

    wreg * lon.test * lat.test
}


limits.reg <- data.frame(lon.min=c(120,170,140,170,140,170,110,140,NA, 210,210,240),
                         lon.max=c(170,210,170,210,170,210,140,155,NA, 280,240,290),
                         lat.min=c(20,20,-10,-10,-40,-40,-10,-10,NA,10,-40,-40),
                         lat.max=c(50,50,20,20,-10,-10,20,0,NA,50,10,10))

limits.reg[81,] <- c(155, 160, -10, -5)




reg.index <- expand.grid(lon=0.5 + (105:310),
                         lat=0.5 + (-56:55))
# can't do this as %between% function not recognised in get.reg 
#check.reg <- sapply(c(1:12, 81), get.reg) # loop through regions to check if pos
check.reg <- reg.index


reg.index$reg <- apply(check.reg, 1, max, na.rm=TRUE)
                                        # (keep max number, this works because regions within smaller regions have higher numbers)
                                        #... but if they are all zeroes it returns a '1' so:

reg.index$reg[reg.index$reg==81] <- 8 # region 8 playing hard to get
row.names(reg.index) <- paste(reg.index$lon, reg.index$lat, sep="_") # set index as cell id

reg.mat <- tapply(reg.index$reg, as.list(reg.index[,c("lon","lat")]), unique)
reg.ll <- lapply(dimnames(reg.mat), as.numeric)




## BET region coordinates from mufdat
mufdat.reg <- list(B1=c("20N", "50N", "120E", "170E"), #ok
                   B2=c("20N", "50N", "170E", "150W"),
                   B32=c("10S", "20N", "160E", "170E"),
                   B33=c("05S", "0", "155E", "160E"),
                   B34=c("0", "20N", "140E", "160E"),
                   B4=c("10S","20N","170E","150W"),
                   B51=c("20S","15S","140E","150E"),
                   B52=c("15S","10S","140E","155E"),
                   B53=c("40S","10S","155E","170E"),
                   B54=c("40S","20S","140E","155E"),
                   B55=c("20S","15S","150E","155E"),
                   B6=c("40S","10S","170E","150W"),
                   B71=c("10S","20N","110E","130E"),
                   B72=c("10S","0","130E","140E"),
                   B81=c("10S","0","140E","155E"),
                   B82=c("10S","05S","155E","160E"),
                   B10=c("10N","50N","150W","80W"),
                   B11=c("40S","10N","150W","120W"),
                   B12=c("40S","10N","120W","80W"))

#check.dev.size(9.5, 7.25)
windows(12,8)
par(mfrow=c(1,1),omi=rep(0,4),mai=c(0.75,0.75,0.5,0.5),family="HersheySans")
colpal <- colorRampPalette(brewer.pal(8, "Blues"))
colv <- col2transp(c(NA, colpal(12)))[c(1,8,5,11,6,4,12,6,8,8,12,8,10)]
image(reg.ll$lon, reg.ll$lat, reg.mat, las=1, asp=1,
      breaks=seq(-0.5,12.5), col=colv,
      xlab="", ylab="")
box()

mufdat.poly <- lapply(mufdat.reg, function(mr) sapply(mr, switch.coords))
make.poly <- function(x, ...) polygon(x[c(3,3,4,4)],x[c(1,2,2,1)], ...)
reg.lab <- function(x, ...) text(mean(mufdat.poly[[x]][3:4]), mean(mufdat.poly[[x]][1:2]), x, ...)
reg.lab.2 <- function(x, ...) text(mean(unlist(limits.reg[x,1:2])),
                                   mean(unlist(limits.reg[x,3:4])), x,
                                   vfont=c("sans serif","bold"),...)

dmm <- sapply(1:12, function(x) reg.lab.2(x, col=col.labels, cex=2))


big.reg.list <- lapply(1:12, function(lr) list(x=limits.reg[lr,c(1,2,2,1)], y=limits.reg[lr,c(3,3,4,4)]))
big.reg.list[[8]]$x <- c(140,160,160,155,155,140)
big.reg.list[[8]]$y <- c(-10,-10,-5,-5,0,0)



region.grid <- function(x, ...) dmm <- sapply(big.reg.list, polygon, ...)
region.grid(lwd=2, col=col.regions)
add.continents.NDC(col=col2transp(col.map),border=NA)
#sapply(1:12, function(x) reg.lab.2(x, col=col.labels, cex=2))
#dev.copy2pdf(file="BET-region-map-2015.pdf")

#reg.map.lim <- list("1"=list(xl=c(120,170)+0.5, yl=c(18,52)+0.5),
#                    "4"=list(xl=c(168,212)+0.5, yl=c(-12,22)+0.5),
#                    "5"=list(xl=c(140,170)+0.5, yl=c(-41,-10)+0.5))

}