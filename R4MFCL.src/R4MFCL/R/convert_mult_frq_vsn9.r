# Convert multi-spp/sex .frq file from version 8 to version 9
source("C:/Nick/MFCL/Testing/R_toolbox/read.mult.frq.R")
source("C:/Nick/MFCL/Testing/R_toolbox/write.mult.frq.R")

#wkdir <- c("C:/Nick/Linux64VM_sync/VM64_compltn/Testing/MTHRD/devvsn7_mult_spp/")

wkdir <- c("C:/Nick/MFCL/sppsex/Testing/2017-08-11_sngl_eval_mult_sex/devvsn7_mult_sex/")

setwd(wkdir)
#
#fln <- c("mult5.frq")
fln <- c("mult_eqsex.frq")
dat <- read.mult.frq(fln)
#
# Change data structure flag for version of .frq to 9
dat$struct$te <- 9
#
# Insert replicate columns for data aggregation flags sppagg_1 sppagg_2
# for LF and WF data
mat <- dat$mat
head(mat)
tmp <- mat[,c("sppagg_1","sppagg_2")]
tmplf <- tmp
dimnames(tmplf)[[2]] <- c("sppagglf_1","sppagglf_2")
tmpwf <- tmp
dimnames(tmpwf)[[2]] <- c("sppaggwf_1","sppaggwf_2")
mat2 <- cbind(mat[,c(1:8)],tmplf,tmpwf,mat[,c(9:dim(mat)[2])])
head(mat2)
# re-number fisheries data columns
dimnames(mat2)[[2]][c(16:dim(mat2)[2])] <- as.character(c(16:dim(mat2)[2]))
#
# Restore into .frq object
dat$mat <- mat2
#
# Write to file
frqfile <- c("mult_eqsex_vsn9.frq")
frq.obj <- dat
write.mult.frq(frqfile,frq.obj)
#
################################################################################
