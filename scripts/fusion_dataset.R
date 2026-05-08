## Collecte gtfs
library(readr)
library(dplyr)
library(tidyverse)


stops<-read_csv('./data/raw/gtfs/stops.txt')
head(stops,10)
stop_clean <- stops|>select(stop_id,stop_name,stop_lat,stop_lon)
head(stop_clean)
path_gtfs<-"./data/raw/gtfs/"
routes<-read_csv(paste0(path_gtfs,"routes.txt"))
stop_times<-read_csv(paste0(path_gtfs,"stop_times.txt"))
trips<-read_csv(paste0(path_gtfs,"trips.txt"))
agency<-read_csv(paste0(path_gtfs,"agency.txt"))

reseau_mobilite<-routes|>left_join(agency ,by = "agency_id") |> inner_join(trips,by = "route_id") |> inner_join(stop_times, by = "trip_id") |> inner_join(stops, by = "stop_id")
reseau_final<-reseau_mobilite |> select(agency_name,route_long_name,trip_id,stop_sequence,stop_name,stop_lat,stop_lon) |> arrange(route_long_name,trip_id,stop_sequence) |> as_tibble()
print(reseau_final)


saveRDS(reseau_final,"./data/raw/gtfs/reseau_gtfs.rds")