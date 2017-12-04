#' 
#'
#' @export
run.kobe <- function(
rootdir=  "C:/bet/2014/assessment/Setup",
basedir="C:/bet/2014/assessment/Model_runs/2014s/Run238",
mfclfiles=c("mfclo64.exe","mfcl.cfg"),
runfiles=c("bet.frq","bet.tag","10.par"),
msy.start=seq(from=236,to=4,by=-4),
msy.finish=seq(from=232,to=0,by=-4),
years =seq(1952,2010),
mfclcmd ="mfclo64 bet.frq 10.par junk.par" )
{
#tmp.dir <- getwd()
msy.times <- cbind(msy.start,msy.finish)

    rundir <- paste(basedir,"kobe",sep="/")
    dir.create(rundir)
    setwd(rundir)   # Set working directory for running MFCL

    # copy across MFCL execution files
    file.copy(paste(basedir,mfclfiles,sep="/"),to=rundir,overwrite=T)
    # copy across bet.frq,bet.tag, and 11.par
    file.copy(paste(basedir,runfiles,sep="/"),to=rundir,overwrite=T)

    mat <- matrix(NA, nrow(msy.times), 10)

        for(k in 1:nrow(msy.times))
        #for(k in 1:5)
        {
        time1 <- msy.times[k,1]
        time2 <- msy.times[k,2]
        system(paste(mfclcmd,"-switch 8 1 1 1 1 188 0 1 189 0 1 187 0 1 186 0 -999 55 1 2 148",time1,"2 155", time2,sep=" "),show.output.on.console = F,invisible = T,wait=TRUE)
        #}


        a <- get.outcomes.2014("plot-junk.par.rep","junk.par","catch.rep")
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
        }
    out <- as.data.frame(mat)
    names(out) <- c("year","time1","time2","FFmsy","MSY","BBmsy","SBSBmsy","Fmsy","BmsyB0","SBmsySB0")
    write.table(out, "MSYoutput.txt")
    setwd(rootdir)
}