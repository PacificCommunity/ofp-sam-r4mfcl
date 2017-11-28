#' @importFrom RODBC odbcDriverConnect sqlTables odbcGetInfo sqlQuery
 load.LFdata <-
function(species="ALB",gear="L") {
  # By Simon D Hoyle 2008
#  library(RODBC)
#  channel <- odbcDriverConnect("DSN=Visual FoxPro Database;UID=;PWD=;SourceDB=G:\\Tuna_dbs\\LFreq_old\\dbf\\lfreq.DBC;SourceType=DBC;Exclusive=No;BackgroundFetch=Yes;Collate=Machine;Null=Yes;Deleted=Yes;")
  channel <- odbcDriverConnect("DSN=Visual FoxPro Database;UID=;PWD=;SourceDB=G:\\Tuna_dbs\\LWFreq\\dbf\\lfreq.DBC;SourceType=DBC;Exclusive=No;BackgroundFetch=Yes;Collate=Machine;Null=Yes;Deleted=Yes;")
  sqlTables(channel)
  odbcGetInfo(channel)

  #get all LL ALB LF records
  lfd <- sqlQuery(channel, paste("select * from lf_master where sp_id = '",species,"' and gr = '",gear,"'",sep=""), max=0)

  lfd$origin_id <- as.character(lfd$origin_id)
  lfd$flag_id <- as.factor(as.character(lfd$flag_id))

  # set up minlat and minlong
  lfd$minlat <- as.numeric(substring(lfd$lat_short, 1, 2))
  lfd$minlat <- ifelse(substring(lfd$lat_short, 3, 3) == "S", -1 * lfd$minlat, lfd$minlat)
  lfd$minlong <- as.numeric(substring(lfd$lon_short, 1, 3))
  lfd$minlong <- ifelse(substring(lfd$lon_short, 4, 4) == "W", 360-lfd$minlon, lfd$minlon)
  lfd$centlat <- lfd$minlat + 2.5
  lfd$centlong <- lfd$minlong + 2.5

  if(species=="ALB") {
  # label lfd with regions and subregions
  lfd <- lfd[lfd$centlat > -50,]
  lfd <- lfd[lfd$centlat < 0,]
  lfd <- lfd[lfd$centlong > 140,]
#  lfd <- lfd[lfd$centlong < 250,]
  lfd$reg <- NA
  lfd$reg[lfd$centlat >= -25 & lfd$centlong < 180] <- 1
  lfd$reg[lfd$centlat >= -25 & lfd$centlong >= 180 & lfd$centlong < 250] <- 2
  lfd$reg[lfd$centlat < -25 & lfd$centlong < 180] <- 3
  lfd$reg[lfd$centlat < -25 & lfd$centlong >= 180 & lfd$centlong < 250] <- 4
  lfd$reg[lfd$centlat > -25 & lfd$centlong >= 250] <- 5
  lfd$reg[lfd$centlat < -25 & lfd$centlong >= 250] <- 6
  }
  
  lfd$latlon <- interaction(lfd$centlat,lfd$centlong, sep=" ")
  lfd$latlon10 <- interaction(trunc(lfd$centlat / 10) * 10, trunc(lfd$centlong / 10) * 10, sep=" ")
  lfd$latlon20 <- interaction(trunc(lfd$centlat / 10) * 10, trunc(lfd$centlong / 20) * 20, sep=" ")

  # get rid of bad data
  lfd <- lfd[lfd$qtr != 0,]
  lfd <- lfd[is.na(lfd$freq) == F,]
  lfd <- lfd[lfd$mon != 0,]

  # get rid of irrelevant data
  # lfd <- lfd[lfd$flag_id %in% c("KR","JP","TW"),]
  return(lfd)
}
