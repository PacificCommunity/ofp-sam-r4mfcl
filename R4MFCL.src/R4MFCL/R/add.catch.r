#' @importFrom utils read.csv
add.catch <- function(alb=read.csv("I:/assessments/bigeye/2011/working/CATCH_5X5_2000-2010.TXT",header=T),
species="bet",groupvar="gear",COLS=c("forestgreen","firebrick3","dodgerblue2","yellow2"),RSCALE=0.01)
{
# adds pies of catch by gear to a plot of the model regions
alb$GEAR <- substring(as.character(alb[,groupvar]),1,1)
alb$index <- paste(alb$lond, alb$latd)
sp_code <- paste(species,"_mt",sep="")

mat <- tapply(alb[,sp_code], list(alb$index, alb$GEAR), sum)
#mat <- tapply(alb$BET_MT, list(alb$index, alb$GEAR), sum)
mat <- ifelse(is.na(mat) == T, 0, mat)
mat <- ifelse(mat == 0, 0.01, mat)
index <- sort(unique(alb$index))
x <- alb$lond[match(index,alb$index)]
y <- alb$latd[match(index,alb$index)]
#x <- as.numeric(substring(index, 1,4))
#y <- as.numeric(substring(index, 7,20))
    for (i in 1:length(x)){
    addpie(mat[i,], lat=y[i], long=x[i], rscale = RSCALE, col = COLS, labels = NA, edges=400)
    #addpie(mat[i,], lat=y[i], long=x[i], rscale = 0.03, col = c("blue", "lightgray", "darkorange"), labels = NA, edges=400)
    }
}
