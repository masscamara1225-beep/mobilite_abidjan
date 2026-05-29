library(dplyr)
library(igraph)
library(readr)
library(stringr)
library(purrr)
library(sf)
library(tibble)

# ==============================================================================
# 08_graphe.R — Graphe inter-communes avec flux TomTom + GTFS
# Lit depuis data/processed/flux_enrichi.csv (deja enrichi par 06)
# ==============================================================================

# 1. CHARGEMENT
tomtom <- read_csv("data/processed/flux_enrichi.csv", show_col_types = FALSE,
                   col_types = cols(timestamp = col_character(), date = col_character()))
gtfs   <- readRDS("data/raw/gtfs/reseau_gtfs.rds")
master <- readRDS("data/processed/mobilite_dataset_final.rds")

cat(sprintf("TomTom : %d lignes | %d axes\n", nrow(tomtom), n_distinct(tomtom$id_axe)))

noeuds_df <- master |>
  sf::st_drop_geometry() |>
  select(name, population = population_2021) |>
  mutate(population = replace_na(population, median(population, na.rm = TRUE)))

# 2. ARETES TOMTOM
aretes_tomtom <- tomtom |>
  mutate(
    commune_dep = str_split(commune, "/") |> map_chr(1) |> str_trim(),
    commune_dep = str_replace_all(commune_dep,
                                  c("Adjame" = "Adjamé", "Attecoube" = "Attécoubé", "Port-Bouet" = "Port-Bouët"))
  ) |>
  filter(commune_dep != destination) |>
  group_by(from = commune_dep, to = destination) |>
  summarise(
    flux        = n(),
    vitesse_moy = mean(vitesse_kmh, na.rm = TRUE),
    indice_moy  = mean(indice_cong, na.rm = TRUE),
    .groups     = "drop"
  ) |>
  mutate(type = "tomtom", nb_lignes = NA_integer_)

cat(sprintf("Aretes TomTom : %d\n", nrow(aretes_tomtom)))

# 3. ARETES GTFS
gtfs_sf <- gtfs |>
  distinct(stop_name, stop_lat, stop_lon) |>
  st_as_sf(coords = c("stop_lon", "stop_lat"), crs = 4326)

arrets_communes <- st_join(gtfs_sf, master |> select(name),
                           join = st_within) |>
  st_drop_geometry() |>
  distinct(stop_name, name)

gtfs_avec_commune <- gtfs |>
  left_join(arrets_communes, by = "stop_name",
            relationship = "many-to-many") |>
  distinct(trip_id, stop_sequence, route_long_name, name) |>
  filter(!is.na(name))

aretes_gtfs <- gtfs_avec_commune |>
  arrange(trip_id, stop_sequence) |>
  group_by(trip_id) |>
  mutate(commune_suivante = lead(name)) |>
  ungroup() |>
  filter(!is.na(commune_suivante), name != commune_suivante) |>
  group_by(from = name, to = commune_suivante) |>
  summarise(
    flux      = n(),
    nb_lignes = n_distinct(route_long_name),
    .groups   = "drop"
  ) |>
  mutate(type = "gtfs", vitesse_moy = NA_real_, indice_moy = NA_real_)

# Seuil GTFS assoupli a 50 lignes pour connecter plus de communes
aretes_gtfs_filtre <- aretes_gtfs |> filter(nb_lignes >= 50)
cat(sprintf("Aretes GTFS (>= 50 lignes) : %d\n", nrow(aretes_gtfs_filtre)))

# 4. FUSION
aretes_final <- bind_rows(aretes_tomtom, aretes_gtfs_filtre) |>
  group_by(from, to) |>
  summarise(
    flux        = sum(flux, na.rm = TRUE),
    nb_lignes   = sum(nb_lignes, na.rm = TRUE),
    vitesse_moy = mean(vitesse_moy, na.rm = TRUE),
    indice_moy  = mean(indice_moy, na.rm = TRUE),
    type        = if_else(any(type == "tomtom"), "tomtom+gtfs", "gtfs"),
    .groups     = "drop"
  )

cat(sprintf("Total aretes finales : %d\n", nrow(aretes_final)))

# 5. GRAPHE
g <- graph_from_data_frame(d = aretes_final, vertices = noeuds_df, directed = TRUE)
V(g)$size  <- log(V(g)$population + 1) * 3
V(g)$label <- V(g)$name
E(g)$width <- E(g)$flux / max(E(g)$flux, na.rm = TRUE) * 8
E(g)$color <- ifelse(E(g)$type == "tomtom+gtfs", "#F47920", "#999999")

# 6. METRIQUES
metriques <- tibble(
  commune      = V(g)$name,
  betweenness  = betweenness(g, directed = TRUE, normalized = TRUE),
  closeness    = closeness(g, mode = "all", normalized = TRUE),
  degre        = degree(g, mode = "all"),
  flux_entrant = degree(g, mode = "in"),
  flux_sortant = degree(g, mode = "out")
) |> arrange(desc(betweenness))

print(metriques)

# 7. LOUVAIN
g_undir <- as_undirected(g, mode = "collapse",
                         edge.attr.comb = list(flux = "sum", weight = "sum",
                                               nb_lignes = "sum", type = "first",
                                               vitesse_moy = "mean", indice_moy = "mean",
                                               width = "mean", color = "first"))
E(g_undir)$weight <- E(g_undir)$flux
comm <- cluster_louvain(g_undir, weights = E(g_undir)$weight)
mod  <- modularity(comm)
# Assigner groupes aux noeuds du graphe principal
groupes    <- membership(comm)
V(g)$group <- sapply(V(g)$name, function(n) {
  if (n %in% names(groupes)) as.integer(groupes[n]) else 0L
})

cat(sprintf("Modularite Louvain : %.3f\n", mod))
cat(sprintf("Communautes : %d\n", length(unique(membership(comm)))))
# 8. SAUVEGARDE
saveRDS(
  list(graphe = g, metriques = metriques, modularite = mod, communautes = comm),
  "data/processed/graphe_communes.rds"
)
cat("graphe_communes.rds sauvegarde.\n")