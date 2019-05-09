compare.frq <- function(file1,file2,fm="all",plotname,fdesc="",lwd=2,what=rep(TRUE,3))
{
##=====================================================================================
## Based on code by Adam Langley
## fm is the fisheries to compare, in two rows
## Change 9-Jun-09 SDH added option to read in fishery description - see example below
## fdesc <- data.frame(num=1:30,
##       method=c("L","L","L","L","L","L","L","L","L","L","L","L","L","L","L","L","L",
##                "L","L","L","L","L","L","L","T","T","D","D","L","L"),
##       flag=c("JP","KR","TWDW","AU","NC","FJ","OT","JP","KR","TWDW","AS/WS","TO","PF",
##               "OT","JP","KR","TWDW","AU","NZ","OT","JP","KR","TWDW","OT","ALL","ALL",
##               "ALL","ALL","ALL","ALL"),
##       reg=c(1,1,1,1,1,1,1,2,2,2,2,2,2,2,3,3,3,3,3,3,4,4,4,4,3,4,3,4,5,6))
## Changed 31-11-2010 SDH to sort frq on yr & mon, + simplify
## if file1 is character, assume it's a file name, otherwise assube it's a frq object
##  same for file2
##    PK June 2011
## Changed 31-11-2010 SDH to sort frq on yr & mon, + simplify
## Changed 26-06-2011 NMD to handle missing fisheries in one of the two frq objects,
##  and prompts if differing frq versions
##=====================================================================================

fdesc <- as.matrix(fdesc)
# Check frq files and number of fisheries
#first file
if(is.character(file1)) frq1 <- read.frq(file1)
else frq1 <- file1
mat <- as.data.frame(frq1$mat)[,1:6]
mat$tstep <- mat[,1] + mat[,2]/12

#second file
if(is.character(file2)) frq2 <- read.frq(file2)
else frq2 <- file2
mat2 <- as.data.frame(frq2$mat)[,1:6]
mat2$tstep <- mat2[,1] + mat2[,2]/12
# Get version number of frq file to determine first column for checking data
vsn1 <- frq1$struct$te
vsn2 <- frq2$struct$te
if(vsn1 != vsn2){
  print("Freq files input are different versions!")
  break()
}
col1 <- ifelse(vsn1 < 6, 7, 8)          # First column for length frequencies

# Check numbers of fisheries in each frq object and add missing fisheries if necessary
pltfrq1flg <- 0
pltfrq2flg <- 0
fadd <- 0
frq1n <- frq1$struct$nf
frq2n <- frq2$struct$nf
if(frq1n != frq2n){
  nfdif <- abs(frq1n - frq2n)
  if(frq1n > frq2n){    # add dummy fisheries to frq2
    fadd <- seq(from = frq2n+1, to = frq1n, by = 1)
    tmp <- frq1$mat[frq1$mat[,4] %in% fadd,]        # copy missing fisheries from frq1
    tmp[,c((col1-2),(col1-1))] <- 0                # zero catch and effort
    tmp[,c(col1,(col1+1))] <- -1
    tmp[,c((col1+2):dim(frq2$mat)[2])] <- 0
    frq2$mat <- rbind(frq2$mat,tmp)
    pltfrq2flg <- 1
  } else {    # add dummy fisheries to frq1
    fadd <- seq(from = frq1n+1, to = frq2n, by = 1)
    tmp <- frq2$mat[frq2$mat[,4] %in% fadd,]        # copy missing fisheries from frq2
    tmp[,c((col1-2),(col1-1))] <- 0                # zero catch and effort
    tmp[,c(col1,(col1+1))] <- -1
    tmp[,c((col1+2):dim(frq1$mat)[2])] <- 0
    frq1$mat <- rbind(frq1$mat,tmp)
    pltfrq1flg <- 1
  }
}

mat <- as.data.frame(frq1$mat)[,1:6]
mat$tstep <- mat[,1] + mat[,2]/12

mat2 <- as.data.frame(frq2$mat)[,1:6]
mat2$tstep <- mat2[,1] + mat2[,2]/12

##fisheries
fish1 <- sort(unique(mat[,4]))
fish2 <- sort(unique(mat2[,4]))

if(is.null(dim(fm)[2])==T) fm <- rbind(sort(unique(mat[,4])),sort(unique(mat2[,4])))
#
for(i in 1:dim(fm)[2]){
  #i <- 1
    par(mfrow=c(3,1), mar=c(2,4,2,2))
  ##catch
  #browser()
  a1 <- mat[mat[,4] == fish1[fm[1,i]],]
  a2 <- mat2[mat2[,4] == fish2[fm[2,i]],]
  a1 <- a1[order(a1[,1],a1[,2]),]
  a2 <- a2[order(a2[,1],a2[,2]),]
  ymax <- max(a1[,5], a2[,5])
  if(what[1]){
    plot(a1$tstep, a1[,5], type="l", xlim=range(c(mat[,1],mat2[,1])),ylim=c(-1,ymax),lwd=lwd, ylab="Catch", xlab="")
    lines(a2$tstep, a2[,5], lty=2, col=2,lwd=lwd)
    legend(min(a1$tstep),(0.9*ymax),legend=(c("frq1","frq2")),col=c(1,2),lty=c(1,2))
    ti <- paste("Plot",i,"Fishery", fish1[fm[1,i]], "& Fishery", fish2[fm[2,i]])
    if (dim(fdesc)[1]>1) { ti <- paste(c("Fishery", i, "-",fdesc[i,2:4]),collapse=" ") }

    if(!i %in% fadd){          # frq files have same number of fisheries
      mtext(side=3,line=0.5, text=ti)
    } else {            # add note to plot for missing fisheries
      if(pltfrq1flg){
        mtext(side=3, line=0.5, text=paste(ti," - missing in frq1",sep=""))
      }
      if(pltfrq2flg){
        mtext(side=3, line=0.5, text=paste(ti," - missing in frq2",sep=""))
      }
    }
  }
  ##effort
  if(what[2]) {
    ymax <- max(a1[,6], a2[,6])
    plot(a1$tstep, a1[,6], type="l", xlim=range(c(mat[,1],mat2[,1])),ylim=c(-1,ymax),lwd=lwd, ylab="Effort", xlab="")
    lines(a2$tstep, a2[,6], lty=2, col=2)
  }
  ##CPUE
  if(what[3]) {
    cpue1 <- a1[,5]/a1[,6]
    cpue2 <- a2[,5]/a2[,6]
    cpue1[is.na(cpue1)] <- 0; cpue2[is.na(cpue2)] <- 0
    plot(a1$tstep, cpue1, type="l", xlim=range(c(mat[,1],mat2[,1])),ylim=c(0,max (cpue1, cpue2)),lwd=lwd, ylab="CPUE", xlab="")
    lines(a2$tstep, cpue2, lty=2, col=2)
  }
    if(!missing(plotname)) {
      savePlot(paste(plotname,"_plot_",formatC(i,width=2,flag=0),".png",sep=""),type="png")
    }
}
# End of Function
}
##################################################################################
# The MIT License
#
# Copyright (c) 2009 Secretariat of the Pacific Community
#
# Permission is hereby granted, free of charge, to any person obtaining a copy of this
# software and associated documentation files (the "Software"), to deal in the Software
# without restriction, including without limitation the rights to use, copy, modify, merge,
# publish, distribute, sublicense, and/or sell copies of the Software, and to permit persons
# to whom the Software is furnished to do so, subject to the following conditions:
#
# The above copyright notice and this permission notice shall be included in all copies
# or substantial portions of the Software.
#
# THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED,
# INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR
# PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE
# FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR
# OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER
# DEALINGS IN THE SOFTWARE.
