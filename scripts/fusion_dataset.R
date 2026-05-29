library(sf)
library(tidyverse)

file_geo <- "data/raw/osm/communes_abidjan_13.rds"
file_stats <- "data/processed/stats_communes_2021.csv"
file_gtfs <- "data/raw/gtfs/reseau_gtfs.rds"
file_output <- "data/processed/mobilite_dataset_final.rds"

if (!all(file.exists(c(file_geo, file_stats, file_gtfs)))) {
  stop("Erreur : Fichiers sources manquants.")
}

geo_communes <- readRDS(file_geo)
stats_pop <- read_csv(file_stats, show_col_types = FALSE)
gtfs_net <- readRDS(file_gtfs)

names(stats_pop) <- names(stats_pop) |> 
  str_replace_all("[:punct:]|[:symbol:]", " ") |> 
  str_squish() |> 
  str_replace_all(" ", "_")

col_commune <- names(stats_pop)[1]

master_data <- geo_communes |>
  left_join(stats_pop, by = setNames(col_commune, "name")) |>
  st_as_sf()

gtfs_sf <- gtfs_net |>
  distinct(stop_name, .keep_all = TRUE) |>
  st_as_sf(coords = c("stop_lon", "stop_lat"), crs = 4326)

arrets_par_commune <- st_join(gtfs_sf, master_data, join = st_intersects) |>
  group_by(name) |>
  summarise(nb_arrets = n()) |>
  st_drop_geometry()

col_pop <- names(stats_pop)[str_detect(names(stats_pop), "(?i)population")][1]

master_final <- master_data |>
  left_join(arrets_par_commune, by = "name") |>
  mutate(
    nb_arrets = replace_na(nb_arrets, 0),
    accessibilite_score = (nb_arrets / !!sym(col_pop)) * 10000
  )

print(master_final |> select(name, nb_arrets, accessibilite_score))

if (!dir.exists("data/processed")) dir.create("data/processed", recursive = TRUE)
saveRDS(master_final, file_output)