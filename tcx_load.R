library(XML)
library(dplyr)

## read in .tcx file
raw_data = xmlParse("sample.tcx")

## pull out latititude, longitude and altitude data
lat_lon = xmlToDataFrame(nodes <- getNodeSet(raw_data, "//ns:Position", "ns"), 
                                                c("numeric","numeric"))
alt = xmlToDataFrame(nodes <- getNodeSet(raw_data, "//s:AltitudeMeters", "s"), 
                                            "numeric")

## combine and rename
track_data <- cbind(lat_lon,alt) %>% 
                rename(lat = LatitudeDegrees, 
                        lon = LongitudeDegrees, 
                        ele = text)
