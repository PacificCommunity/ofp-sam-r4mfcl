read.ini <- function(ini.file) 
##============================================================================
## by Simon Hoyle June 2008
##  revised, PK June 2011
##============================================================================
{
  a <- readLines(ini.file)
  a <- a[nchar(a)>=1]     #get rid of blank lines 
  hpts <- grep("^#",a)
  ini.obj <- list()
 
  pos <- grep("version number",a,ignore.case=T)+1
  if(length(pos)==0) ini.obj$version=0
  else ini.obj$version <- scanText(a[pos],what=0)
       
  pos <- grep("# tag fish rep *$",a,ignore.case=T)+1
  if(length(pos)>0) {
    p2 <- hpts[hpts>pos][1]-1
    ini.obj$tag.fish.rep <- matrix(scanText(a[pos:p2],what=0),byrow=TRUE,nrow=p2+1-pos)
  }
    
  pos <- grep("# tag fish rep group flags",a,ignore.case=T)+1
  if(length(pos)>0) {
    p2 <- hpts[hpts>pos][1]-1
    ini.obj$grpflags <- matrix(scanText(a[pos:p2],what=0),byrow=TRUE,nrow=p2+1-pos)
  }
    
  pos <- grep("# tag_fish_rep active flags",a,ignore.case=T)+1
  if(length(pos)>0) {
    p2 <- hpts[hpts>pos][1]-1
    ini.obj$activeflags <- matrix(scanText(a[pos:p2],what=0),byrow=TRUE,nrow=p2+1-pos)
  }
    
  pos <- grep("# tag_fish_rep target",a,ignore.case=T)+1
  if(length(pos)>0) {
    p2 <- hpts[hpts>pos][1]-1
    ini.obj$reptarget <- matrix(scanText(a[pos:p2],what=0),byrow=TRUE,nrow=p2+1-pos)
  }
    
  pos <- grep("# tag_fish_rep penalty",a,ignore.case=T)+1
  if(length(pos)>0) {
    p2 <- hpts[hpts>pos][1]-1
    ini.obj$reppenalty <- matrix(scanText(a[pos:p2],what=0),byrow=TRUE,nrow=p2+1-pos)
  }
    
  pos <- grep("# sv\\(29\\)",a)+1
  if(length(pos)>0) { if(!(pos%in%hpts)) {
    ini.obj$sv29 <- scanText(a[pos],what=0)
    a[pos] <- paste("#",a[pos]) # to keep it from being chosen below
  }}
 
  pos <- grep("# number of age classes",a,ignore.case=T)+1
  ini.obj$nages <- as.numeric(a[pos])
  
  pos <- grep("# MATURITY AT AGE",a,ignore.case=T)+1
  ini.obj$mat <- scanText(a[pos],what=0)
  
  pos <- grep("# natural mortality",a,ignore.case=T)+1
  ini.obj$M <- scanText(a[pos],what=0)
  
  pos <- grep("# move",a,ignore.case=T)+1
  ini.obj$movemap <- scanText(a[pos],what=0)
  if(ini.obj$movemap[1]==0)
  {
    ini.obj$diffcoffs <- 0
  } else {
    pos <- grep("# diffusion coffs",a,ignore.case=T)+1
    p2 <- hpts[hpts>pos][1]-1
    ini.obj$diffcoffs <- matrix(scanText(a[pos:p2],what=0),byrow=TRUE,nrow=p2+1-pos)
  }
  
  pos <- grep("# age_pars",a,ignore.case=T)+1
  if(length(pos)>0) {
    p2 <- hpts[hpts>pos][1]-1
    ini.obj$age_pars <- matrix(scanText(a[pos:p2],what=0),byrow=TRUE,nrow=p2+1-pos)
  }
  
  pos <- grep("# recruitment",a,ignore.case=T)+1
  ini.obj$recbyreg <- scanText(a[pos],what=0)
  
  b <- scanText(a[(hpts[hpts>pos][1]):length(a)],what=0, comment.char="#")
  ini.obj$VBLmin   <- b[1:3]
  ini.obj$VBLmax   <- b[4:6]
  ini.obj$VBK      <- b[7:9]
  ini.obj$LW       <- b[10:11]
  ini.obj$sdLatA   <- b[12:14]
  ini.obj$Ldep_sd  <- b[15:17]
  ini.obj$Nmeanconstr <- b[18:length(b)]

  return(ini.obj)
}

