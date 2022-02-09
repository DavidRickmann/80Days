#Mark on the map progress made at various points.


#automate this to plot out a list of points.



#load libraries ====
library(sf)
library(readxl)
library(tidyverse)
library(leaflet)
library(rgdal)
library(readr)
library(googleway)
library(geosphere)
library(googlePolylines)
library(sf)
library(lwgeom)

#prep functions ====
add_distance_marker <- function(distance, journey){

  df2 <- journey
  desired_distance <- units::set_units(distance, "m")
  pointflag <- 0

  for(p in 1:(nrow(df2)-1)){
    print(p)
    pl1 <- df2 %>% filter(LocationOrder == p) %>% select(polyline, traveltype)
    if(pl1$polyline == "skip"){
      next
    }
    #this is absurd! but SOMETHING! is replacing the // in the encoded polylines with a ////
    #so, fuck it. I'm just gonna replace it straight back again,
    pl2 <- gsub("\\\\", "\\", pl1$polyline, fixed = TRUE)

    #decoding the encoded polylines gives me longtitudes greater than 180, which is annoying. So a quick fix.
    route <- decode_pl(pl2) %>% mutate(lon = if_else(lon > 180, lon - 360, lon)    )  %>% select(lon, lat)
    coords <- list()
    coords[[1]] <- route

    sfg <- lapply( coords, function(x) sf::st_linestring( x = as.matrix(x) ) )
    path <- sf::st_sfc( sfg, crs = 4326 )

    pathlength <- st_length(path)

    if (desired_distance > pathlength){

      desired_distance <- desired_distance - pathlength

    } else
    {
      if(pointflag == 1) {} else {
        ratio <- desired_distance / pathlength
        (pt <- st_linesubstring(path, from = 0, to = ratio) %>% st_endpoint())


        pointflag <- 1
        break
      }

    }

  }
  return(pt)


}


#import route ====
Route <- read_excel("Route.xlsx")

#tidy up data ====
df2 <- Route %>%
  select(LocationOrder, lon,lat,City, LocationName, RoutingPoint, polyline, traveltype, Events) %>%
  #drop_na()  %>%
  distinct(LocationOrder, lon = as.numeric(lon),lat = as.numeric(lat), City, LocationName, RoutingPoint, polyline, traveltype, Events) %>%
  mutate(popup = paste0("<h1>",City,"</h1>","<br/>",
                        "<b>",LocationName,"</b>","<br/>",
                        #lon,"/",lat,"<br/>",
                        Events
  ))


#set up map ====
map <- leaflet(df2 %>% filter(RoutingPoint == 0)) %>%
  addProviderTiles("CartoDB.Positron", options=providerTileOptions(noWrap = TRUE))

#%>%
 # addCircleMarkers(lng = ~lon, lat = ~lat, popup = ~popup)



#draw route up to furthest point reached
desired_distance <- units::set_units(3980000, "m")
pointflag <- 0
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


  pathlength <- st_length(path)

  if (desired_distance > pathlength){
    desired_distance <- desired_distance - pathlength
    map <- addPolylines(map, data = path, color = traveltype)

  } else {
    if(pointflag == 1) {}
      else {
      ratio <- desired_distance / pathlength
      (pt <- st_linesubstring(path, from = 0, to = ratio) %>% st_endpoint())
      subpath <- st_linesubstring(path, from = 0, to = ratio)
      map <- addPolylines(map, data = subpath, color = traveltype)
      pointflag <- 1
      break
      }
  }
}

#add on distance points


map <- addMarkers(map, data = add_distance_marker(3980000, df2), popup = paste0("<h2>Fogg and Passepartout</h2><br>Bound for Suez, onboard the steamer “Mongolia,” belonging to the Peninsular and Oriental
Company, built of iron, of two thousand eight hundred tons burden, and
five hundred horse-power. <br> 06/02/2022"))





map











