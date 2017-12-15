 reconstruct.frq.size <-
function(data=data,FISH=1,
# By Shelton J Harley 2009
LF.FILE="P:/yft/2009/Data Preparation/size data/LLlendataR1.txt",
WT.FILE="P:/yft/2009/Data Preparation/size data/LLwtdataR1.txt")
{
# then do the matching
fish <- FISH
fishery2 <- data[data[,4] == fish,]
fishindex <- paste(fishery2[,1], round(fishery2[,2]/4,0) + 1)
lf <- read.table(LF.FILE)
lfindex <- paste(lf[,1], lf[,2])
a <- match(fishindex,lfindex)
wt <- read.table(WT.FILE)
wtindex <- paste(wt[,1], wt[,2])
b <- match(fishindex,wtindex)

# lets wipe out all existing data first
fishery2[,7:301] <- 0

for(i in 1:nrow(fishery2))
{
    # No length data
    if(is.na(a[i])==T)
    {
    fishery2[i,7] <- -1

        # No weight data either
        if(is.na(b[i])==T)
        {
        fishery2[i,8] <- -1
        }
        else
        {
        # No length, but some weight data
        fishery2[i,8:207] <- as.vector(unlist(wt[b[i],3:202]))
        }
    }

    else
    {
    # Length data
    fishery2[i,7:101] <- as.vector(unlist(lf[a[i],3:97]))

        # Length, but no weight
        if(is.na(b[i])==T)
        {
        fishery2[i,102] <- -1
        }
        else
        {
        # No length, but some weight data
        fishery2[i,102:301] <- as.vector(unlist(wt[b[i],3:202]))
        }
     }
}
return(fishery2)
}
