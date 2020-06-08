plot.tag.reporting.rates = function(parf = read.par("ALB15/12.par"), grp.names = paste0("G",1:14), new.style=TRUE,
                                xmargin=6, nls=4, trunc=0.9){

  require(scales)

  # There is the option of two figures - the old bar one and a newer one that shows the prior more explicitely...

  if(!new.style)
  {
    taggrp <- sort(unique(as.vector(parf$trpgpfl)))
    tagests <- tapply(as.vector(parf$trpfl),as.vector(parf$trpgpfl),mean)
    tagpriors <- tapply(as.vector(parf$treptarg)/100,as.vector(parf$trpgpfl),mean)
    tagpens <- tapply(as.vector(parf$treppen),as.vector(parf$trpgpfl),mean)
    tagrepgrps <- data.frame(RR=tagests, target = tagpriors, cv = 1/(2*sqrt(tagpens)), grp = taggrp, nms = grp.names)

    nfish <- dim(tagrepgrps$grp)[2]
    estrr <- tagrepgrps$RR

    # get priors
    mean <- tagrepgrps$target
    sd <- tagrepgrps$cv
    prior <- data.frame(mean=tagrepgrps$target,lcb=mean-1.96*sd,ucb=mean+1.96*sd)

    par(mar=c(xmargin,5,.5,.5)+.1,lwd=.5)

    plot(c(1,length(estrr)),c(0,1),type='n',ann=F,axes=F)

    dx <- .3
    x <- seq(estrr)
    for(i in x) {
      y1 <- max(0,prior$lcb[i])
      y2 <- min(0.9,prior$ucb[i])
      polygon(c(rep(i-dx,2),rep(i+dx,2)),c(y1,rep(y2,2),y1),
              border="white",col="LightSteelBlue")
    }

    points(x,prior$mean,pch=18,cex=2,col="white")
    points(x,estrr,pch=19,cex=2)

    axis(2,lwd=.1,cex.axis=1.2,las=2)
    title(ylab="Tag reporting rate",cex.lab=1.5)
    labs <- tagrepgrps$nms
    axis(1,lwd=.1,at=x,labels=labs,las=2,cex.axis=.9)

    box()

    legend(1,1.04,pch=c(NA,19),lty=c(1,NA),lwd=5,col=c("LightSteelBlue",'black'),cex=1.5,legend=c("Prior","Estimate"),y.intersp=1.5, bty="n", ncol=2)
    legend(1,1.04,pch=c(18,19),lty=c(0,NA),lwd=5,col=c("white",'black'),cex=1.5,legend=c("Prior","Estimate"),y.intersp=1.5, bty="n", ncol=2)

  } else {

    theme_set(theme_bw())

    grp.ind <- match(sort(unique(as.vector(parf$trpgpfl))), as.vector(parf$trpgpfl))

    rep.sums <- data.frame(Grp = sort(unique(as.vector(parf$trpgpfl))),
                           Ests = as.vector(parf$trpfl)[grp.ind],
                           PriorMu = as.vector(parf$treptarg)[grp.ind]/100,
                           PriorSig = 1/sqrt(2*as.vector(parf$treppen)[grp.ind]),
                           Gnm = grp.names)

    rep.norm <- expand.grid(Grp = rep.sums$Grp, xax = seq(0.01,trunc,0.001))
    rep.norm$pdens <- dnorm(rep.norm$xax, mean = rep.sums$PriorMu[rep.norm$Grp], sd = rep.sums$PriorSig[rep.norm$Grp])
    rep.norm$Ests <- rep.sums$Ests[rep.norm$Grp]
    rep.norm$Gnm <- factor(grp.names[rep.norm$Grp], levels = grp.names)

    pl <- ggplot(rep.norm, aes(x=xax, y=pdens))  + geom_line(size=0.7) +
                 facet_wrap(~ Gnm, ncol=nls, scales="free_y") + scale_x_continuous(limits = c(0,1), breaks = c(.25,.75)) +
                 theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
                 xlab("Tag group") + ylab("Tag reporting rate") +
                 geom_vline(aes(xintercept = Ests), size=1, colour="red") +
                 geom_vline(aes(xintercept = trunc), size=0.7, colour=alpha("blue",0.4), linetype="longdash") +
                 scale_y_continuous(limits = c(0,NA))

    print(pl)
  }
}












