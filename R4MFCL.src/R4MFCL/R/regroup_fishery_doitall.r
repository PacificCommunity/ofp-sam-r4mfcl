regroup_fishery_doitall <- function(doitall,f,flag,newgrps) {
    ## by Matthew Vincent May 2019
    ## Takes all fisheries in f and changes the group they are in
    a = doitall
    ## Check to make sure that f and newgrps are same length
    if(length(f) != length(newgrps)){stop("length of f is not equal to newgrps")}
    if(length(flag)!=1){stop("Only a single flag replacement at one time")}
    for (i in 1:length(f)) {
        mt = paste(-f[i],"[[:blank:]]+",flag,"[[:blank:]]+[[:digit:]]+",sep="")
        rp = paste(-f[i],flag,newgrps[i],sep=" ")
        a = gsub(mt,rp,a)
    }
    return(a)
}
