library(geojsonio)
data <- geojson_read("Routes/dubpool2.geojson", parse = TRUE)

data <- as_tibble(data$features$geometry$coordinates[[1]])

colnames(data) <- c("lon","lat")




#pl <- encode_pl(route$lat,route$lon)
pl <- encode_pl(data$lat,data$lon)

pl
route <- decode_pl(pl)
testmap <- leaflet() %>% addTiles()

#draw links
for(i in 1:nrow(route)-1){
  testmap <- addPolylines(testmap, lat = c(route[i,]$lat,route[i+1,]$lat),
                          lng = c(route[i,]$lon,route[i+1,]$lon))
}
testmap
