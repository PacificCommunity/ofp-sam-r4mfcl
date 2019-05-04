#' convert version 4 frq to version 6 frq
#' @param a frq file object that has previously been read in
#'
#' @author Simon D Hoyle
#' @export
convert.frq.ver6 <- function(a) {
##======================================================================
# By Simon D Hoyle 2009
##======================================================================
  if(a$struct$te==6) return(a)
  a$mat <- cbind(a$mat[,1:6],rep(-1,dim(a$mat)[1]),a$mat[,7:dim(a$mat)[2]])
  a$struct$age_inds <- c(0,-1)
  a$reg$seas_reg_flags <- matrix(1,ncol=a$struct$nreg,nrow=a$struct$tc)
  a$struct$te <- 6
  a$version <- 6
  return(a)
}
