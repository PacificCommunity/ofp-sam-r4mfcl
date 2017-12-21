#' plots quarterly movements on a map, currently not age-specific but can add that if we start allowing age-specific movement
#' 
#' @param reg.keep reg.keep
#' @param mov.dat.path mov.dat.path
#' @param repfile  string file name of plot.rep
#' @param parfile file name of par file
#'
#' @importFrom ggplot2 ggplot geom_path geom_text geom_segment
#' @importFrom ggmap get_map ggmap
#' @importFrom grid pushViewport viewport
plot.quarterly.movement.map = function(reg.keep = c('BR1','BR2','BR31','BR4','BR5','BR6','BR7','BR8','BR9'),
                                       mov.dat.path = 'L:/bet/2014/assessment/Results/Sam/MovementInfo.txt',
                                       repfile,
                                       parfile)
{
# SJDM 27/06/14 - plots quarterly movements on a map, currently not age-specific but can add that if we start allowing age-specific movement

#require(ggplot2)
#library(ggmap)
#require(grid)

setInternet2(TRUE)

     reg.names = data.frame(x=c(145,180,160,180), y=c(30,30,-25,-25), r=c('1','2','5','6'))
     reg.bounds.x = list(BR1 = c(120,170,170,120,120),
                         BR2 = c(170,210,210,170,170),
                         BR3 = c(120,170,170,120,120),
                         BR4 = c(170,210,210,170,170),
                         BR5 = c(140,170,170,140,140),
                         BR6 = c(170,210,210,170,170),
                         BR7 = c(110,140,140,110,110),
                         BR8 = c(140,160,160,155,155,140,140),
                         BR9 = c(140,150,150,140,140),
                         BR31 = c(140,155,155,160,160,170,170,140,140),
                         BR72 = c(130,140,140,130,130),
                         BR3A = c(110,130,130,110,110),
                         BR37 = c(130,140,140,130,130),
                         BR5A = c(140,170,170,140,140,150,150,140,140),
                         SR1 = c(120,210,210,120,120),
                         SR2 = c(110,170,170,110,110),
                         SR3 = c(170,210,210,170,170),
                         SR21 = c(140,155,155,160,160,170,170,140,140),
                         SR5 = c(140,160,160,155,155,140,140),
                         SR4 = c(110,140,140,110,110),
                         SR41 = c(110,140,140,130,130,110,110),
                         SR42 = c(130,140,140,130,130),
                         SR22 = c(120,170,170,120,120))

     reg.bounds.y = list(BR1 = c(20,20,50,50,20),
                         BR2 = c(20,20,50,50,20),
                         BR3 = c(-10,-10,20,20,-10),
                         BR4 = c(-10,-10,20,20,-10),
                         BR5 = c(-40,-40,-10,-10,-40),
                         BR6 = c(-40,-40,-10,-10,-40),
                         BR7 = c(-10,-10,20,20,-10),
                         BR8 = c(-10,-10,-5,-5,0,0,-10),
                         BR9 = c(-20,-20,-15,-15,-20),
                         BR31 = c(0,0,-5,-5,-10,-10,20,20,0),
                         BR72 = c(-10,-10,0,0,-10),
                         BR3A = c(-10,-10,20,20,-10),
                         BR37 = c(0,0,20,20,0),
                         BR5A = c(-40,-40,-10,-10,-15,-15,-20,-20,-40),
                         SR1 = c(20,20,50,50,20),
                         SR2 = c(-20,-20,20,20,-20),
                         SR3 = c(-20,-20,20,20,-20),
                         SR21 = c(0,0,-5,-5,-20,-20,20,20,0),
                         SR5 = c(-20,-20,-5,-5,0,0,-20),
                         SR4 = c(-20,-20,20,20,-20),
                         SR41 = c(-20,-20,0,0,20,20,-20),
                         SR42 = c(0,0,20,20,0),
                         SR22 = c(-20,-20,20,20,-20))

     reg.txt = data.frame(reg = c('BR1','BR2','BR31','BR4','BR5','BR6','BR7','BR8','BR9','SR1','SR2','SR3','SR4','SR5', 'SR22', 'BR3'),
                            x = c( 160,  200,   160,  200,  160,  200,  130,  150,  145,  170,  160,  200,  120,  150,   160,    160),
                            y = c( 40,   40,    15,   15,   -20,  -20,  15,   -5,   -19.5, 40,  10,   10,   10,   -15,   10,     15),
                            r = c( '1',  '2',   '3',  '4',  '5',  '6',  '7',  '8',  '9',   '1', '2',  '3',  '4',  '5',   '2',    '3'))

      reg.map = get_map(location = c(160,5), zoom = 3, maptype = 'satellite')# maptype = 'roadmap')
      p = ggmap(reg.map, fullpage=TRUE)

      for(i in reg.keep)
      {
          dat = data.frame(x=reg.bounds.x[[i]],y=reg.bounds.y[[i]])
          dat.txt = reg.txt[reg.txt$reg == i,]
          p = p + geom_path(data=dat, mapping=aes(x=x, y=y, colour='red'), show_guide=FALSE)
          p = p + geom_text(data=dat.txt, aes(x=x, y=y, label=r, colour='red'), hjust=0, vjust=0, size=3, show_guide=FALSE)
      }

#      p = p + theme(legend.position="none")
      
      dat = read.table(file=mov.dat.path, header=TRUE)   # I made these manually, was a lot of fun

#by age class (40)
#by quarter (4)
jnk <- read.rep(repfile)

regions <- jnk$nReg
nage <- jnk$nAges
qtrs <- jnk$nRecs.yr # should use mpy from frq file!
linesget <- regions*nage*qtrs
# movement coefficients
pos1 <- grep("# movement matrices",readLines(parfile))

mat <- as.matrix(read.table(parfile, skip=pos1, nrows= linesget))
matq <- array(NA, dim=c(regions*nage, regions, qtrs))
a <- regions*nage
matq[,,1] <- mat[1:a,1:regions]
matq[,,2] <- mat[(a+1):(2*a),1:regions]
matq[,,3] <- mat[(2*a+1):(3*a),1:regions]
matq[,,4] <- mat[(3*a+1):(4*a),1:regions]
assign("movements",matq,pos=1)

 dat$Q1 = as.vector(t(matq[1:regions,1:regions,1]))
 dat$Q2 = as.vector(t(matq[1:regions,1:regions,2]))
 dat$Q3 = as.vector(t(matq[1:regions,1:regions,3]))
 dat$Q4 = as.vector(t(matq[1:regions,1:regions,4]))
 dat$endLon = dat$Lon + dat$LonDir*5#dat$LonDir*dat$Q1*100
 dat$endLat = dat$Lat + dat$LatDir*5#dat$LatDir*dat$Q1*100
 dat$arrowSize1 = 0.5 + 10*log(1+dat$Q1)
 dat$arrowSize2 = 0.5 + 10*log(1+dat$Q2)
 dat$arrowSize3 = 0.5 + 10*log(1+dat$Q3)
 dat$arrowSize4 = 0.5 + 10*log(1+dat$Q4)
 
 p1 <- p + geom_segment(data=dat, aes(x=endLon, y=endLat, xend=Lon, yend=Lat), arrow=arrow(ends="first", length = unit(0.15,"cm")), colour='red', size=dat$arrowSize1)
 p2 <- p + geom_segment(data=dat, aes(x=endLon, y=endLat, xend=Lon, yend=Lat), arrow=arrow(ends="first", length = unit(0.15,"cm")), colour='red', size=dat$arrowSize2)
 p3 <- p + geom_segment(data=dat, aes(x=endLon, y=endLat, xend=Lon, yend=Lat), arrow=arrow(ends="first", length = unit(0.15,"cm")), colour='red', size=dat$arrowSize3)
 p4 <- p + geom_segment(data=dat, aes(x=endLon, y=endLat, xend=Lon, yend=Lat), arrow=arrow(ends="first", length = unit(0.15,"cm")), colour='red', size=dat$arrowSize4)


windows(13,15)

pushViewport(viewport(layout = grid.layout(2,2)))
                print(p1, vp = viewport(layout.pos.row=1, layout.pos.col=1))
                print(p2, vp = viewport(layout.pos.row=1, layout.pos.col=2))
                print(p3, vp = viewport(layout.pos.row=2, layout.pos.col=1))
                print(p4, vp = viewport(layout.pos.row=2, layout.pos.col=2))
}








