#A little bit or script to grab poly lines for route segments


library(googleway)

## not specifying the api will add the key as your 'default'
key <- "ADD A Google Key here"
set_key(key = key)
google_keys()


Route <- read_excel("Route.xlsx")

p <- #choose route leg here

a <-  as.numeric(Route[Route$LocationOrder == p,  ]$lat)
b <-  as.numeric(Route[Route$LocationOrder == p,  ]$lon)
c <-  as.numeric(Route[Route$LocationOrder == p+1,  ]$lat)
d <-  as.numeric(Route[Route$LocationOrder == p+1,  ]$lon)

print(paste0("Row ",p,":",a,"/",b))

#pl <- df$routes$overview_polyline$points

df <- google_directions(origin = "c(a,b)",
                        destination = "c(c,d)",
                        #mode = c("driving"),
                       # mode = c("walking"),
                       #ode = c("bicycling"),
                        mode = c("transit"),
                        key = key,
                      #transit_routing_preference = "fewer_transfers",
                     #transit_mode = "train",
                      # departure_time =  Sys.time() + (12 * 60 * 60),
                        simplify = TRUE)


pl <- direction_polyline(df)
print(pl)

route <- decode_pl(pl)
testmap <- leaflet() %>% addTiles()

#draw links
for(i in 1:nrow(route)-1){
  #for(i in 1:193){
  testmap <- addPolylines(testmap, lat = c(route[i,]$lat,route[i+1,]$lat),
                      lng = c(route[i,]$lon,route[i+1,]$lon))
}
testmap
