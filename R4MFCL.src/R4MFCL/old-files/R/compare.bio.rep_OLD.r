compare.bio.rep_OLD <- function (vrep = c(""), vpar = c(""), vcrep = c(""), modnms = c(""),
    plotfol = c(""), plotnm = "")
{
    if (length(vrep) > 7) {
        print("No. models to compare > 7!!")
        (break)()
    }
    rslt <- c("a_rslt", "b_rslt", "c_rslt", "d_rslt", "e_rslt",
        "f_rslt", "g_rslt")
    pars <- c("a_par", "b_par", "c_par", "d_par", "e_par", "f_par",
        "g_par")
    lst.rslt <- list()
    lst.pars <- list()
    for (i in 1:length(vrep)) {
        lst.rslt[[i]] <- read.rep(vrep[i])
        lst.pars[[i]] <- read.par(vpar[i])
    }
    names(lst.rslt) <- rslt[c(1:length(vrep))]
    names(lst.pars) <- pars[c(1:length(vrep))]
    mods <- c("a", "b", "c", "d", "e", "f", "g")
    mods <- mods[c(1:length(vrep))]
    modnames <- modnms
    plotfol <- plotfol
    ymax <- 0
    nmax <- 0
    nmodyrs <- vector(mode = "numeric", length = length(mods))
    for (i in 1:length(mods)) {
        yy <- rowSums(lst.rslt[[i]][["TotBiomass"]])
        ymax <- max(max(yy), ymax)
        nmax <- max(length(yy), nmax)
        nmodyrs[i] <- length(yy)
    }
    ymat <- matrix(NA, nrow = nmax, ncol = length(mods))
    for (i in 1:length(mods)) {
        ymat[c(1:nmodyrs[i]), i] <- rowSums(lst.rslt[[i]][["TotBiomass"]])
    }
    yy <- ymat[, 1]
    plot(1:length(yy), yy, type = "n", lty = 1, lwd = 1, main = "Comparison total biomass",
        xlab = "Time interval", ylab = "Biomass (mt)", ylim = c(0,
            ymax))
    for (i in 1:length(mods)) {
        lines(1:length(ymat[, 1]), ymat[, i], type = "l", lty = i,
            lwd = 1, col = i)
    }
    legend(0, ymax, legend = modnames, lty = c(1:length(mods)),
        col = c(1:length(mods)), lwd = 3)
    filnm <- paste(plotfol, "/", plotnm, "_biom_comprsn", sep = "")
    savePlot(filnm, type = "png")
    ymax <- 0
    for (i in 1:length(mods)) {
        yy <- lst.rslt[[i]][["mean.LatAge"]]
        yy_sd <- lst.rslt[[i]][["sd.LatAge"]]
        ymax <- max(max(yy + yy_sd), ymax)
    }
    yy <- lst.rslt[[1]][["mean.LatAge"]]
    yy_sd <- lst.rslt[[1]][["sd.LatAge"]]
    plot(1:length(yy), yy, type = "n", lty = 1, lwd = 1, main = "Comparison growth curves (+/- 1sd)",
        xlab = "Age", ylab = "Length (cm)", ylim = c(0, ymax))
    for (i in 1:length(mods)) {
        yy <- lst.rslt[[i]][["mean.LatAge"]]
        yy_sd <- lst.rslt[[i]][["sd.LatAge"]]
        lines(1:length(yy), yy, type = "l", lty = 1, lwd = 3,
            col = i)
        lines(1:length(yy), yy + yy_sd, type = "l", lty = 3,
            lwd = 3, col = i)
        lines(1:length(yy), yy - yy_sd, type = "l", lty = 3,
            lwd = 3, col = i)
    }
    legend(0, ymax, legend = modnames, lty = 1, col = c(1:length(mods)),
        lwd = 3)
    filnm <- paste(plotfol, "/", plotnm, "_growth_comprsn", sep = "")
    savePlot(filnm, type = "png")
    like_tbl <- data.frame(Mods = mods, Like = rep(NA, length = length(mods)))
    for (i in 1:length(mods)) {
        like_tbl[i, "Like"] <- lst.pars[[i]][["obj"]]
    }
    filnm <- paste(plotfol, "/", plotnm, "_likl_comprsn.txt",
        sep = "")
    write.table(like_tbl, file = filnm, quote = FALSE, row.names = FALSE)
    mod_outs <- list()
    for (i in 1:length(mods)) {
        mod_outs[[i]] <- get.outcomes.2014(vrep[i], vpar[i],
            vcrep[i])
    }
    names(mod_outs) <- modnames
    outputs <- c(names(mod_outs[[1]]))
    comprsn_outs <- data.frame(Qnts = outputs, dummy = rep(NA,
        length = length(outputs)))
    for (i in 1:length(mods)) {
        comprsn_outs[, (i + 1)] <- unlist(mod_outs[[i]])
    }
    names(comprsn_outs)[2:(length(mods) + 1)] <- modnames
    comprsn_pcntg_outs <- comprsn_outs
    comprsn_pcntg_outs[, c(2:(length(mods) + 1))] <- comprsn_pcntg_outs[,
        c(2:(length(mods) + 1))]/comprsn_outs[, 2]
    filnm <- paste(plotfol, "/", plotnm, "_comprsn_outs.txt",
        sep = "")
    write.table(comprsn_outs, file = filnm, quote = FALSE, row.names = FALSE)
    filnm <- paste(plotfol, "/", plotnm, "_comprsn_pcntg_outs.txt",
        sep = "")
    write.table(comprsn_pcntg_outs, file = filnm, quote = FALSE,
        row.names = FALSE)
}