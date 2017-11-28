#' SJDM - Changed it to a ggplot as was easier to control and far less code
#' @importFrom ggplot2 ggplot
#' @importFrom ggplot2 geom_bar facet_wrap xlab ylab
#' @importFrom reshape2 melt
plot.selectivity.betyft <- function(repfile=read.rep("final-rp.glm.rep"), fleetlabs=BET_fleet$fnames)
{
#require(reshape2)
#require(ggplot2)

    Ncols = ceiling(length(fleetlabs)/9)   # Seems to be best if 9 or less rows of fisheries
    sel <- repfile$SelAtAge                # Matrix of selectivities at age - one row per fishery

    sel <- as.data.frame(t(sel))           # Convert to data.frame and ensure that fisheries are columns not rows
      names(sel) <- fleetlabs              # Give corect fishery labels
        sel$bin <- 1:length(sel[,1])       # Creat an x-axis variable - age-classes

    plot.dat <- melt(sel, id=c("bin")); names(plot.dat)[2:3] <- c("Fishery","Coefficient")   # Format data into the shape required for ggplot e.g. long formate for all fisheries

    # Produce and print plot
    p <- ggplot(plot.dat, aes(x=bin, y=Coefficient)) + geom_bar(stat="identity", colour="black", fill="black")
      p <- p + facet_wrap(~ Fishery, ncol=Ncols) #+ theme(strip.text.x = element_text(size=8))
        p <- p + xlab("Age-class (quarters)") + ylab("Selectivity coefficient")
    print(p)
    
}
