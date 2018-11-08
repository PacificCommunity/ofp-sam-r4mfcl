convert.latlon <- function(lon=x,lat=y,In=1,Out=1)
{
# Code from PW
#Where 'xlat' and 'xlon' are the latitude/longitude for 1-degx1-deg at the south-west corner, the following produces the point (decimal) at the centre the 1-degx1-deg

    if(In==1)
    {
    NLat <-  (as.numeric(substring(lat,1,2))+ifelse(substring(lat,nchar(lat),nchar(lat))=='S',-0.5,0.5))*ifelse(substring(lat,nchar(lat),nchar(lat))=='S',-1,1)
    NLon <-   abs(as.numeric(substring(lon,1,3))+ifelse(substring(lon,nchar(lon),nchar(lon))=='W',-0.5,0.5)+ifelse(substring(lon,nchar(lon),nchar(lon))=='W',-360,0))
    }
#Where 'xlat' and 'xlon' are the latitude/longitude for 5-degx5-deg at the south-west corner, the following produces the point (decimal) at the centre the 5-degx5-deg
    if(In==5)
    {
    NLat <-  (as.numeric(substring(lat,1,2))+ifelse(substring(lat,nchar(lat),nchar(lat))=='S',-2.5,2.5))*ifelse(substring(lat,nchar(lat),nchar(lat))=='S',-1,1)
    NLon <-   abs(as.numeric(substring(lon,1,3))+ifelse(substring(lon,nchar(lon),nchar(lon))=='W',-2.5,2.5)+ifelse(substring(lon,nchar(lon),nchar(lon))=='W',-360,0))
    }

#To get from NLat1 to NLat5
     if(In==1 & Out==5)
     {
     NLat = floor( NLat / 5) * 5 +2.5
     NLon = floor( NLon / 5) * 5 +2.5
     }
return(data.frame(lon=NLon,lat=NLat))
}
