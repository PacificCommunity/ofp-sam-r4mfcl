#' @import RSQLite

get.flags <- function(flags){

#  require(RSQLite)

  # connect to the sqlite database
  drv  <- dbDriver("SQLite")
  con  <- dbConnect(drv, dbname=system.file("extdata", "flags.sqlite", package="R4MFCL"))

  # put flag string into usable format
  nflags <- as.numeric(unlist(strsplit(flags, split=" ")))[1]
  flags_df <- as.data.frame(matrix(as.numeric(unlist(strsplit(flags, split=" ")))[-1], byrow=T, nrow=nflags,
                                   dimnames=list(c(as.character(1:nflags)),c('type','number','value'))))

  # extract flag descriptions from database tables
  for(i in 1:nrow(flags_df)){
     if(is.element(flags_df[i,'type'], c(1,2,-999,-9999)))
        table_id <- switch(EXP=as.character(flags_df[i,'type']), '1'='parest_flags', '2'='age_flags', '-999'='fish_flags', '-9999'='tag_flags')
     if(is.element(flags_df[i,'type'], c(-1:-99)))
        table_id <- 'fish_flags'
     if(flags_df[i,'type'] < -999) 
       table_id <- 'tag_flags'   
     if(flags_df[i,'type'] < -9999) 
       table_id <- 'region_flags'     
     res   <- dbGetQuery(con, paste('SELECT Description FROM', table_id ,'WHERE (((FlagNumber) In (', format(flags_df[i,2]),')));', sep=" "))
     flags_df[i,'Description'] <- res
  }
  dbDisconnect(con)
  return(flags_df)
}

#get.flags("7 1 1 1 2 190 0 2 191 0 2 148 4 2 155 0 -999 55 1 -9 56 1")

################################################################################
## code to create the flags database

#library(RSQLite)

## create new database
#con <- dbConnect(SQLite(), 'C:\\temp\\flags.sqlite')

## connect to existing database
#drv <- dbDriver("SQLite")
#con <- dbConnect(drv, 'C:\\temp\\flags.sqlite')

#dbWriteTable(con, "parest_flags", read.csv("C:\\temp\\parest.csv", header=T))
#dbWriteTable(con, "age_flags",    read.csv("C:\\temp\\age.csv", header=T))
#dbWriteTable(con, "fish_flags",   read.csv("C:\\temp\\fish.csv", header=T))
#dbWriteTable(con, "tag_flags",    read.csv("C:\\temp\\tag.csv", header=T))
#dbWriteTable(con, "region_flags", read.csv("C:\\temp\\region.csv", header=T))

#dbDisconnect(con)

