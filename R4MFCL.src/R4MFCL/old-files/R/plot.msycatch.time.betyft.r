plot.msycatch.time.betyft <- function(legpos="left",catchrep=basecat,plotrep=read.rep(baserep),lfish=gears[[1]],sfish=gears[[2]],
                                      ofish=gears[[3]],pfish=gears[[4]],msyfile=basemsy,specs=gearspecs,spp="skj")
{

##plots total catch by gear vs MSY for 
# SJH 16/7/2011 -- catch rather than proportions

#catchrep <- "P:/bigeye/2009/Model-runs/stepwise/run10/catch.rep"
#repfile <- tmp.rep
nfish <- plotrep$nFisheries
nyr <- plotrep$nTimes
year1 <- plotrep$Year1
tsteps <- plotrep$nRecs.yr
year <- trunc(seq(year1,length=nyr,by=1/tsteps))

# read in - split up by fishery type and sum - aggregate by year
pos1 <- grep("# Catch by year",readLines(catchrep))
catch <- matrix(scan(catchrep,skip=pos1,nlines=nfish),byrow=T,nrow=nfish,ncol=nyr)
ll.catch <- apply(catch[lfish,],2,sum)
ll.catch <- aggregate(ll.catch,list(year),sum)[,2]
ps.catch <- apply(catch[sfish,],2,sum)
ps.catch <- aggregate(ps.catch,list(year),sum)[,2]
ot.catch <- apply(catch[ofish,],2,sum)
ot.catch <- aggregate(ot.catch,list(year),sum)[,2]
    if(!spp == "skj")
    {
        ann.catch <- cbind(ll.catch,ps.catch,ot.catch)
    } else {
        pl.catch <- apply(catch[pfish,],2,sum)
        pl.catch <- aggregate(pl.catch,list(year),sum)[,2]
        ann.catch <- cbind(ll.catch,ps.catch,ot.catch,pl.catch)
    }
ann.prop.catch <- ann.catch/apply(ann.catch,1,sum)
#browser()
#mat3 <- ann.prop.catch
mat3 <- ann.catch/1000
years <- unique(year)
#browser()
# MSY
msy <- read.table(msyfile,header=T)
msy2 <- msy$MSY/1000
# For y-limits
maxcatch <- max(c(apply(mat3,1,sum)),msy2)

par(mar=c(5,5,2,5))
plot(1,1, ylim=c(0,maxcatch), xlim=range(years), axes=F,ylab="Catch by gear / MSY (000 mt)", xlab="",type="n")
polygon(c(years, rev(years)), c(rep(0,length(years)), rev(mat3[,1])), col=specs$cls[specs$code=="L"])
a <- rev(mat3[,1])
b <- a + rev(mat3[,2])
polygon(c(years, rev(years)), c(rev(a), b), col=specs$cls[specs$code=="S"])

if(spp == "skj")
{

a <- a + rev(mat3[,2])
b <- a + rev(mat3[,4])
polygon(c(years, rev(years)), c(rev(a), b), col=specs$cls[specs$code=="P"])


a <- a + rev(mat3[,4])
b <- a + rev(mat3[,3])
polygon(c(years, rev(years)), c(rev(a), b), col=specs$cls[specs$code=="Z"])

labels <- c("Purse Seine", "Pole & Line", "Other")
legend(legpos, legend=rev(labels), fill=rev(c(specs$cls[specs$code=="L"], specs$cls[specs$code=="S"], specs$cls[specs$code=="P"], specs$cls[specs$code=="Z"])), bg="white",bty="n")
} else {

a <- a + rev(mat3[,2])
b <- a + rev(mat3[,3])
polygon(c(years, rev(years)), c(rev(a), b), col=specs$cls[specs$code=="Z"])

labels <- c("Longline", "Purse Seine", "Other")
legend(legpos, legend=rev(labels), fill=rev(c(specs$cls[specs$code=="L"], specs$cls[specs$code=="S"], specs$cls[specs$code=="Z"])), bg="white",bty="n")
}

axis(1)
axis(2,las=1)
lines(years[-length(years)], msy2, lwd=4, col="red")
box()
}
