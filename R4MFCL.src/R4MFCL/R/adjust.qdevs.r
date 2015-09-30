adjust.qdevs <- function(parfile,projrep,psfish=14:17,SCALE=1,FY=2010)
{
#SJH 4/10/2012 7:33:23 PM
# This is the first attempt to reduce the impact of qdevs in teh final year of the model
# We go into the projection parfile and make the 'fix'
# the compexity of this fix will change through time
#
# CHALLENGES
# We need to have run a projection first to get the fishing events
# The qdevs are in the rem part of teh R par object
# we need to get the flags to line stuff up properly
# note that the rows in the fishing events won't be the same as the rows for qdevs (for BET and YFT)
# SJH 10/13/2012 4:27:16 PM - need to get the realizations for q groups go to projpar code
# If the Purse seine fisheries have shared q-devs then this code will not work


#SP$sqrep <- read.rep("plot-sq.par.rep")
# Find out when the fishing events occur - grab from the dummy projection repfile
x <-projrep$Rlz.t.fsh[,-1] # take off the first column now to make it line up
xx <- projrep$nRlz.fsh 
# need to identify the fisheries to look at and find their catchability groups
qrows <- parfile$ffl[psfish,29]
# number of groups
nqgrps <- max(parfile$ffl[,29])
# number of fishing realizations per group
nqgrpreal <- rep(NA,nqgrps)
for(i in 1:nqgrps)
{
tmp <- unique(as.vector(x[parfile$ffl[,29]==i,]))
nqgrpreal[i] <- length(tmp[!is.na(tmp)])
}



#
# get the grouped qdevs from the parfile and create a matrix
tmp.rem <- parfile$rem
pos1 <- grep("# The grouped_catch_dev_coffs flag",tmp.rem)
pos2 <- grep("# Objective function value",tmp.rem)
y <- tmp.rem[(pos1+3):(pos2-2)]
# when to make the change in qdev
fchange <- FY+1+(1/8)

#browser()

    grp.qdev <- matrix(nrow = length(nqgrpreal), ncol = max(nqgrpreal))
    for (i in 1:length(nqgrpreal)) {
    #print(i)
       grp.qdev[i, 1:(nqgrpreal[i])] <- as.numeric(unlist(strsplit(y[i], split = "[[:blank:]]+"))[-1])
    }

# now go and do the correction for the purse seine fisheries
    for(i in 1:length(psfish))
    {
    #identify the devs from the final year
    tmp <- grp.qdev[qrows[i],(trunc(x[psfish[i],]) %in% FY)]
    #browser()
    #make a change -- a negative one in the first quarter of the projections
    tmp2 <- x[psfish[i],]
    grp.qdev[qrows[i],(tmp2==fchange & !is.na(tmp2))] <- -1*SCALE*sum(tmp)

    # put it back in the y object
    jnk <-  grp.qdev[qrows[i],!is.na(grp.qdev[qrows[i],])]
    #browser()
    y[qrows[i]] <- paste(jnk,collapse=" ")
    }
# now to put the qdevs back in the rem
tmp.rem[(pos1+3):(pos2-2)] <- y
parfile$rem <- tmp.rem
return(parfile)
}

#cbind(grp.qdev[qrows[i],],x[psfish[i],])
