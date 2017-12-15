#' Calculate and store time series of benchmarks for kobe plot (and majuro plot)
#'
#' @param rootdir rootdir
#' @param basedir basedir
#' @param mfclexe mfclexe
#' @param mfclfiles mfclfiles
#' @param runfiles runfiles
#' @param msy.start msy.start
#' @param msy.finish msy.finish
#' @param years years
#' @param mfclcmd command arguments of mfcl
#' @param lyr last year
#' @param save.rep of rep files be retained
#' @export
run.kobe <- function(rootdir="C:/bet/2014/assessment/Setup",
                     basedir="C:/bet/2014/assessment/Model_runs/2014s/Run238",
                     mfclexe="~/bin/dev15072017/mfclo",
                     mfclfiles=c("mfcl.cfg"),
                     runfiles=c("bet.frq","bet.tag","10.par"),
                     msy.start=seq(from=236,to=4,by=-4),
                     msy.finish=seq(from=232,to=0,by=-4),
                     years =seq(1952,2010),
                     mfclcmd ="mfclo64 bet.frq 10.par junk.par",
                     lyr = 2015,save.rep=FALSE)
{
#tmp.dir <- getwd()
msy.times <- cbind(msy.start,msy.finish)
    wd.org<-getwd()
    on.exit(setwd(wd.org))
    basedir<-normalizePath(basedir)
    rundir <- normalizePath(paste(basedir,"kobe",sep="/")) 
    # normalizePath helps to obtain absolute path from relative path
    dir.create(rundir)
    setwd(rundir)   # Set working directory for running MFCL

    # copy across MFCL execution files
    file.copy(mfclexe,to=rundir,overwrite=T)
#    cat("L22\n");browser()
    file.copy(paste(basedir,mfclfiles,sep="/"),to=rundir,overwrite=T)
#    file.copy(paste(basedir,mfclfiles,sep="/"),to=rundir,overwrite=T)
    # copy across bet.frq,bet.tag, and 11.par
    file.copy(paste(basedir,runfiles,sep="/"),to=rundir,overwrite=T)

    mat <- matrix(NA, nrow(msy.times), 12)

        for(k in 1:nrow(msy.times))
        #for(k in 1:5)
        {
          
          time1 <- msy.times[k,1]
          time2 <- msy.times[k,2]
          runcmd<-paste(mfclcmd, "-switch 8 1 1 1 1 188 0 1 189 0 1 187 0 1 186 0 -999 55 1 2 148", time1, "2 155", time2, sep=" ")
          cat("runcmd=",runcmd,"\n")
        system(runcmd, show.output.on.console = F, invisible = T, wait=TRUE)
        #}


        a <- get.outcomes(file.rep = "plot-junk.par.rep", file.par = "junk.par",catch.rep = "catch.rep", nofish = TRUE,
                          nofishp = c(11,1), lateyr = lyr)
             if(save.rep)system(paste("mv plot-junk.par.rep plot-junk",k,".par.rep",collapse=""))
        mat[k,1] <- years[k]
        mat[k,2] <- time1
        mat[k,3] <- time2
        mat[k,4] <- round(a$Fcurr.Fmsy,3)
        mat[k,5] <- a$MSY
        mat[k,6] <- round(a$Bcurr.Bmsy,3)
        mat[k,7] <- round(a$SBcurr.SBmsy,3)
        mat[k,8] <- round(a$Fmsy,3)
        mat[k,9] <- round(a$Bmsy.B0,3)
        mat[k,10] <- round(a$SBmsy.SB0,3)
        mat[k,11] <- round(a$SBcurr.SBF0,3)
        mat[k,12] <- round(a$SBlatest.SBF0,3)
        }
    out <- as.data.frame(mat)
    names(out) <- c("year","time1","time2","FFmsy","MSY","BBmsy","SBSBmsy","Fmsy","BmsyB0","SBmsySB0","SBcurrSBF0","SBlateSBF0")
    write.table(out, "MSYoutput.txt")
    setwd(rootdir)
}