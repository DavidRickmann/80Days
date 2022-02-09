
#prepare the route map


#load libraries
library(sf)
library(readxl)
library(tidyverse)
library(leaflet)
library(rgdal)
library(readr)
library(googleway)
Route <- read_excel("Route.xlsx")

#tidy up data
df2 <- Route %>%
  select(LocationOrder, lon,lat,City, LocationName, RoutingPoint, polyline, traveltype, Events) %>%
  distinct(LocationOrder, lon = as.numeric(lon),lat = as.numeric(lat), City, LocationName, RoutingPoint, polyline, traveltype, Events) %>%
  mutate(popup = paste0("<h1>",City,"</h1>","<br/>",
                        "<b>",LocationName,"</b>","<br/>",
                        lon,"/",lat,"<br/>",
                        Events
  ))



#draw points
map <- leaflet(df2 %>% filter(RoutingPoint == 0)) %>%
  addProviderTiles("CartoDB.Positron", options=providerTileOptions(noWrap = TRUE)) %>%
  addCircleMarkers(lng = ~lon, lat = ~lat, popup = ~popup)



for(p in 1:(nrow(df2)-1)){
  print(p)
  pl1 <- df2 %>% filter(LocationOrder == p) %>% select(polyline, traveltype)
  if(pl1$polyline == "skip"){
    next
  }
  #this is absurd! but SOMETHING! is replacing the // in the encoded polylines with a ////
  #so, fuck it. I'm just gonna replace it straight back again,
  pl2 <- gsub("\\\\", "\\", pl1$polyline, fixed = TRUE)
  traveltype <- case_when(
    pl1$traveltype == "ship"        ~ "red",
    pl1$traveltype == "train"        ~ "blue",
    pl1$traveltype == "drive"        ~ "green",
    pl1$traveltype == "walk"        ~ "grey",
    TRUE ~ "orange"
    )

  #decoding the encoded polylines gives me longtitudes greater than 180, which is annoying. So a quick fix.
  route <- decode_pl(pl2) %>% mutate(lon = if_else(lon > 180, lon - 360, lon)    )  %>% select(lon, lat)
  coords <- list()
  coords[[1]] <- route

  sfg <- lapply( coords, function(x) sf::st_linestring( x = as.matrix(x) ) )
  path <- sf::st_sfc( sfg, crs = 4326 )

  map <- addPolylines(map, data = path, color = traveltype)

}

map








