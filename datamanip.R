setwd("SDAIapp")
shapes_to_filter <- rgdal::readOGR("boundary/LA/LA.shp") # rgdal import to 'Spatial Object'
shapes_to_filter <- spTransform(shapes_to_filter, CRS("+init=epsg:4326"))
councilnames<-readr::read_csv("boundary/councilnames.csv")
shapes_to_filter<-merge(shapes_to_filter, councilnames, by="NAME")

## add in the earnings
library(dplyr)
earnings<- readr::read_csv("macroeconomy/earnings.csv")
floss<- readr::read_csv("macroeconomy/financialloss.csv")

# Earnings
LAallearnings<-earnings %>%
  tidyr::spread(., DateCode, Value) %>%
  dplyr::filter(Gender=="All", `Population Group`=="Residence Based")%>%
  dplyr::rename(., Council = FeatureCode) %>%
  dplyr::select(-one_of(c("1998", "1999", "2000", "Gender", "Population Group"
                          , "Units", "Measurement"))) %>%
  tidyr::gather(., key=Year, value=Value,-Council) %>%
  dplyr::mutate(Year = as.factor(Year)) %>%
  na.omit() 

# financial loss
LAfinanloss<-earnings %>%
  tidyr::spread(., DateCode, Value) %>%
  dplyr::filter(Gender=="All", `Population Group`=="Residence Based")%>%
  dplyr::rename(., Council = FeatureCode) %>%
  dplyr::select(-one_of(c("1998", "1999", "2000", "Gender", "Population Group"
                          , "Units", "Measurement"))) %>%
  tidyr::gather(., key=Year, value=Value,-Council) %>%
  dplyr::mutate(Year = as.factor(Year)) %>%
  na.omit() 

# merge
shapes_to_filter<-merge(shapes_to_filter, LAallearnings, by="Council", duplicateGeoms = TRUE)
