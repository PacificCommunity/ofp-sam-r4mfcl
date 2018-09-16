#' Function to read *.tag file
#' @param tagfile file name of *.tag file
#' @export
read.tag <- function(tagfile) {
  # by Simon Hoyle June 2008
  # SDH 10 Jul 2010 stop attaching hd
  # - include comment labels for Tag_program
  #
  op <- options(warn=-1)
  ff <- readLines(tagfile)
  options(op)
  ff <- gsub("\032","",ff)   # to remove mysterious ^z
  writeLines(ff,"erasethisfile")
  a <- scan("erasethisfile", nlines=0, comment.char="#")
  b <- readLines("erasethisfile")
  file.remove("erasethisfile")
  loc <- grep("Tag_program",b)
  d <- b[loc]
  hd <- data.frame(nrel=a[1],l1=a[2],nint=a[3],iwd=a[4])
  if(length(d) > 0) tagprog <- substring(d,first=nchar(d)-3) else tagprog <- rep("    ",hd$nrel)  
  st <- 5 ; nd<- st + hd$nrel -1
  nrecov.grp <- a[st:nd]
  prog <- rep("",hd$nrel)
  rel <- data.frame(reg=rep(0,hd$nrel),y=rep(0,hd$nrel),m=rep(0,hd$nrel))
  rel.lens <- matrix(nrow=hd$nrel,ncol=hd$nint)
  trecov <- sum(nrecov.grp)
  rel.recov <- data.frame(grp=rep(0,trecov),len=rep(0,trecov),
        fsh=rep(0,trecov),yr=rep(0,trecov),m=rep(0,trecov),n=rep(0,trecov))
  rcount <- 1
  for (g in 1:hd$nrel)
  {
    st <- nd+1 ; nd <- st + 2
    rel[g,] <- a[st:nd]
    st <- nd+1 ; nd <- st + hd$nint - 1
    rel.lens[g,] <- a[st:nd]
    if (nrecov.grp[g] > 0) {
      for (r in 1:nrecov.grp[g])
      {
        st <- nd + 1 ; nd <- st + 4
        rel.recov[rcount,1] <- g
        rel.recov[rcount,2:6] <- a[st:nd]
        rcount <- rcount + 1
      }
    }
  }
  return(list(hd=hd,nrecov.grp=nrecov.grp,tagprog=tagprog,rel=rel,rel.lens=rel.lens,rel.recov=rel.recov))
}
