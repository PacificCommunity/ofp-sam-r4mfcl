#' SJDM 20/06/2014
#' A function to map the stock assessment boundaries for BET/YFT and SKJ
#' The corners of all the subregions (as specified in mufdager) are listed below
#' and the function call determines which ones are displayed
#' @importFrom ggplot2 geom_polygon geom_path geom_text theme xlab ylab
#' @importFrom ggmap get_map
#'
# ' @importFrom scales

plot.map.region.boundaries = function(reg.keep, reg.highlight, linesize = c(2,6), cols = "white")
{
#    require(scales)
#    require(ggplot2)
#   require(ggmap)

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
#      p = ggmap(reg.map, fullpage=TRUE)
      p = ggmap(reg.map)

      if(reg.highlight == 'NULL')
      {
          print('Example Plot')
      } else
      {
          for(i in reg.highlight)
          {
              dat = data.frame(x=reg.bounds.x[[i]],y=reg.bounds.y[[i]])
              p = p + geom_polygon(data=dat, mapping=aes(x=x, y=y, fill='red', alpha=1/2))
          }
      }

      for(i in reg.keep)
      {
          dat = data.frame(x=reg.bounds.x[[i]],y=reg.bounds.y[[i]])
          dat.txt = reg.txt[reg.txt$reg == i,]
          p = p + geom_path(data=dat, mapping=aes(x=x, y=y), colour=cols, size=linesize[1])
          p = p + geom_text(data=dat.txt, aes(x=x, y=y, label=r), colour=cols, hjust=0, vjust=0, size=linesize[2])
      }

      p = p + theme(legend.position="none")
      p = p + xlab("") + ylab("")
      p
}





















