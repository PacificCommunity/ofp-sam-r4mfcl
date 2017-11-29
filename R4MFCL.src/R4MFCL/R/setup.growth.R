 setup.growth <-
function(rungrp,VBopt) {
#  attach(VBopt);on.exit(detach(VBopt))
  with(VBopt,{
  if(fixVB) {
    rungrp$doitall <- change.flag(rungrp$doitall,1,flagnum=14,newval=0)
  } else {
    rungrp$doitall <- change.flag(rungrp$doitall,1,flagnum=14,newval=1)
  }
  rungrp$ini$VBLmax <- c(startVB[2],90,130)
  rungrp$ini$VBK <- c(startVB[3],0.05,0.5)
#  detach(VBopt)
  })
  return(rungrp)
}
