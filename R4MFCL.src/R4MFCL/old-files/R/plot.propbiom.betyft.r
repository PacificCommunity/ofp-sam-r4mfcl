plot.propbiom.betyft <- function(parfile=basepar,repfile=read.rep(baserep),specs=regionspecs,sp="bet")
{
regions <- repfile$nReg
nage <- repfile$nAges
qtrs <- repfile$nRecs.yr # should use mpy from frq file!
linesget <- regions*nage*qtrs
pos1 <- grep("# movement matrices",readLines(parfile))
mat <- as.matrix(read.table(parfile, skip=pos1, nrows= linesget))
matq <- array(NA, dim=c(regions*nage, regions, qtrs))
a <- regions*nage
matq[,,1] <- mat[1:a,1:regions]
matq[,,2] <- mat[(a+1):(2*a),1:regions]
matq[,,3] <- mat[(2*a+1):(3*a),1:regions]
matq[,,4] <- mat[(3*a+1):(4*a),1:regions]

##constant with respect to age
matq <- matq[1:regions,,]

###matrix for populations in from region into region
#over 100 timesteps
tstep <- 100
qstep <- rep(1:4, tstep/4)
pop <- array(0, c(tstep, nage, regions, regions))
###create populations
totrec <- 100000000
##proportion of recruitment by region
#recprop <- c(0.1,0.1,0.5,0.4, 0.05,0.05)
##calculate recruitment proportions from the average of recruitment for the entire time period
##get recruitment serie from plot.rep
# Number of time periods
#linesget <- 228

#pos1 <- grep("# movement matrices",readLines(parfile))
#mat <- as.matrix(read.table(parfile, skip=pos1, nrows= linesget))



#recmat <- as.matrix(read.table("D:\\mfcl\\bet\\2008\\WCPO\\final\\run4\\plot-13new.par.rep", skip=3287, nrows= linesget))
recmat <- repfile$Recruitment
rec <- apply(recmat, 2, mean)
recprop <- rec/sum(rec)
##first age class
      for (i in 1:regions){
      	pop[,1,i,i] <- rep(totrec*recprop[i], tstep)
      }

##need to include this from plot.rep file
#natmort <- rep(0, nage)

natmort <- repfile$MatAge

#for each region (topregion)
    for(topregion in 1:regions){
    nagesum <- 0
    #for each timestep
        for(tint in 2:tstep){
        #for each age class
            for(age in 2:nage){
                for(j in 1:regions){
                #for each region - move fish in agex from region y to region x - kill fish with M
                regionx <- j
                    for (i in 1:regions){
                    	regiony <- i
                    	nagsum <- 0

                    	nagesum <-  pop[tint-1,age-1,regiony,topregion] * matq[regionx,regiony,qstep[tint-1]]
                    	##apply natural mortality

                    	nagesum <- nagesum * exp(-natmort[age])
                    	#print(nagesum)
                    	pop[tint,age,regionx, topregion] <- pop[tint, age, regionx, topregion] + nagesum
                    }
                }
            }
        }
    }

pop[1:20, 1:10,,1]


###convert number at age to weight at age
wtage <- repfile$mean.WatAge

#use last 10 time periods as a summary
popwt <- array(0, c(1, nage, regions, regions))
    for(j in 1:regions){
        for (i in 1:regions){
        popwt[1,1:nage,i, j]  <- apply(pop[90:100,1:nage,i,j], 2, sum) * wtage
        }
    }

##compile total biomass in each region (cols) by source region (rows)
matsum <- matrix(NA, regions, regions)
    for (i in 1:regions){
    a <- popwt[1,1:nage,i,]
    matsum[,i] <- apply(a,2,sum)/sum(a)
    }

cols <- specs$cls[specs[[paste(sp,"col",sep="")]]]

win.graph()
layout(matrix(c(1,2),2,1,byrow=TRUE), c(20,20), c(20,5), TRUE)
layout.show(2)
par(mar=c(2.5,3,2,2))
barplot(matsum, col=cols, names.arg= paste("Reg", 1:regions),las=1)
mtext(side=2, "Proportion of biomass by source region", line=2.5)
a <- barplot(matsum, col=rainbow(6), plot=F)
#dumby plot to plot legend
par(mar=c(2,3,0,2))
plot(0,0, ylim=c(0,1), xlim=c(0,(max(a)+a[1]/2)), type="n", ylab="", xlab="", yaxt="n", xaxt="n",xpd=T, bty="n")
points(a, rep(1,length(a)), pch=15, cex=4, col=cols,xpd=T, bty="n")
}

