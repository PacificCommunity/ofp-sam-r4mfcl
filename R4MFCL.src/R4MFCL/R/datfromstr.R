# datfromstr <-
#function(datstring) {
  # SDH 4/2/09
#  return(as.numeric(unlist(strsplit(datstring,split="[[:blank:]]+"))[-1]))
#  }
# YT 2018/10/17 
#' @importFrom magrittr "%>%"
datfromstr<-function (datstring,transpose=FALSE)
  {
 #   print(datstring)
 #   return(as.numeric(unlist(strsplit(trimws(datstring), split = "[[:blank:]]+"))))  # trimws is avalable from  R >=3.2.0
    out<-if(length(datstring)>1){
      datstring %>% sapply("trimws") %>% strsplit(split = "[[:blank:]]+") %>% 
      	lapply(.,"as.numeric") %>%
      	{
      		if(all(sapply(.,length) == max(sapply(.,length))))
      			do.call("rbind",.)
      		else
      			# http://r.789695.n4.nabble.com/Convert-quot-ragged-quot-list-to-matrix-td895283.html
      			matrix(unlist(lapply(., '[', 1:max(sapply(., length)))), nrow = length(.), byrow = TRUE) 
      	} %>% 'colnames<-'(NULL) %>% 'rownames<-'(NULL) %>% {if(transpose)t(.)else .} 
    }else{
      datstring %>% trimws() %>% strsplit(split = "[[:blank:]]+") %>% "[["(1) %>% as.numeric()
    }
    return(out)
  }
