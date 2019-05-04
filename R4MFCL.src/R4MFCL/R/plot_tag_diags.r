#' Function to make diagnostic plots for tag
#'
#' @importFrom lattice xyplot

plot_tag_diags <- function(tmptagrepfile, parfile, type='logscale'){

  if(!(type %in% c('logscale','residuals')))
    stop("type arg must be either 'logscale' or 'residuals'")

  tagz <- tmptagrepfile
  tagz$auxdat$id <- paste(tagz$auxdat$rreg, tagz$auxdat$creg, sep='-')
  lyout <- unlist(lapply(list(tagz$auxdat$creg, tagz$auxdat$rreg), function(x){length(unique(x))}))
  tgage <- parfile$afl[96] + 1

  res <- data.frame(id = rep(colnames(tapply(tagz$auxdat$prec, list(tagz$auxdat$t2rec, tagz$auxdat$id), sum)), each=tgage),
                  period    = rep(1:tgage, length(unique(tagz$auxdat$id))),
                  predicted = as.vector(tapply(tagz$auxdat$prec, list(tagz$auxdat$t2rec, tagz$auxdat$id), sum)),
                  observed  = as.vector(tapply(tagz$auxdat$orec, list(tagz$auxdat$t2rec, tagz$auxdat$id), sum)))
  res$observed[res$observed==0] <- NA
  res$resids    <- res$observed - res$predicted
  res$resids.sd <- rep(tapply(res$resids, res$id, sd, na.rm=T), each=tgage)
  res$pearson.r <- res$resids/res$resids.sd

  windows(width=14, height=11)

  logscale <- function(res, lyt){
    xyplot(observed+predicted~period|id, data=res,
           type=c('p','l'), pch=c(19,NA), lty=c(0,1), col=c('black', 'black'), ylab='Recaptures (log)', xlab='Period',
           scales=list(y=list(log=10, relation='free', equispaced.log=FALSE)), as.table=TRUE, layout=lyt)
  }

  residuals <- function(res, lyt) {
    pfun <- function(x,y,...){
      panel.xyplot(x,y,...)
      panel.abline(h=0, col='grey')
    }
    xyplot(pearson.r~period|id, data=res, type='p', panel=pfun, as.table=T,
           ylab='Pearson Residuals', xlab='Period', layout=lyt)
  }

  switch(type,
         logscale =logscale(res, lyout),
         residuals=residuals(res, lyout))
}



