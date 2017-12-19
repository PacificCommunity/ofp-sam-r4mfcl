 make.projection.betyft.frq <-
function(frq.in=base.frq,fish=1:24,years=10)
{
#### Takes a frq file and creates an new bit that can be manipulated and tacked on to
# an exisiting frq file
# takes into account the frq file version
# only works for quarterly time steps, e.g. BET/YFT at the moment
# it is then attached to the bottom and then output using write.frq
# SJH 30/06/09
ver <- frq.in$struct$te
data <- frq.in$mat

# do the first fishery separate
      xxx <- data[data[,4]==fish[1],1:6]
      last.year <- max(xxx[,1])

      dat.out <- data.frame(
      years=sort(rep(seq(from=last.year+1, length=years,by=1),4)),
      quarts=rep(c(2,5,8,11),years),
      weeks=rep(1,length=4*years),
      fishery=rep(fish[1],length=4*years),
      catch=rep(xxx[,5][xxx[,1]==last.year],years),
      effort=rep(xxx[,6][xxx[,1]==last.year],years))


      for(i in 2:length(fish))
      {
      xxx <- data[data[,4]==fish[i],1:6]
      last.year <- max(xxx[,1])

      new.data <- data.frame(
      years=sort(rep(seq(from=last.year+1, length=years,by=1),4)),
      quarts=rep(c(2,5,8,11),years),
      weeks=rep(1,length=4*years),
      fishery=rep(fish[i],length=4*years),
      catch=rep(xxx[,5][xxx[,1]==last.year],years),
      effort=rep(xxx[,6][xxx[,1]==last.year],years))
      dat.out <- rbind(dat.out,new.data)
      }
      
# now add on the extra columns
output <- matrix(0,nrow=dim(dat.out)[1],ncol=dim(data)[2])
output[,1:6] <- as.matrix(dat.out)
#browser()
      if(ver>=6)
      {
      # we need a -1 for the effort dev weight
      output[,7:9] <- -1
      }
      else
      {
      # just missing length and weight data
      output[,7:8] <- -1
      }
return(output)
}
