library(dplyr)
library(httr)
library(jsonlite)
library(readr)
library(lubridate)

# ==============================================================================
# 03_collecte_complete.R
# Collecte TomTom — TOUS les axes en un seul job
# 42 axes par session (20 principaux + 10 complement + 12 retour)
# Sauvegarde par type dans des fichiers separes par jour
# ==============================================================================

# ------------------------------------------------------------------------------
# 1. AXES PRINCIPAUX (20 axes)
# ------------------------------------------------------------------------------
axes_principaux <- tibble(
  id_axe = paste0("AX_", sprintf("%02d", 1:20)),
  nom_axe = c(
    "Pont HKB (Henri Konan Bedie)",
    "Pont De Gaulle",
    "Pont Felix Houphouet-Boigny",
    "Boulevard VGE (Giscard d Estaing)",
    "5eme Pont acces Saint-Jean",
    "Marche Bellville (Bvd Marseille)",
    "Rond-point Treichville",
    "Gare Lagunaire Bvd Lagunaire",
    "Carrefour de l Indenie (Adjame)",
    "Adjame Mosquee (Bvd Nangui Abrogoua)",
    "Echangeur de la Ferraille (Adjame)",
    "Feux Autoroute Nord entree Adjame",
    "Carrefour Universite (Cocody)",
    "2 Plateaux Vallon (Bvd Mitterrand)",
    "Abobo Gare Mairie (Route Abobo)",
    "Gare Yopougon Autoroute Yopougon",
    "Clinique Fraternite (Bvd Fraternite)",
    "Carrefour Jatak (Route de Dabou)",
    "Lycee Bingerville (Route Bingerville)",
    "Rond-point Marcory (Bvd de la Paix)"
  ),
  commune = c(
    "Cocody/Marcory", "Plateau/Treichville", "Plateau/Treichville",
    "Plateau/Port-Bouet", "Cocody/Plateau", "Treichville",
    "Treichville", "Plateau/Treichville", "Adjame", "Adjame",
    "Adjame", "Adjame", "Cocody", "Cocody", "Abobo",
    "Yopougon", "Yopougon", "Yopougon", "Bingerville", "Marcory"
  ),
  destination = c(
    "Plateau","Plateau","Plateau","Plateau","Plateau",
    "Plateau","Plateau","Plateau","Plateau","Plateau",
    "Plateau","Plateau","Cocody","Cocody","Plateau",
    "Plateau","Plateau","Plateau","Cocody","Plateau"
  ),
  lat_dep = c(
    5.329775,5.317516,5.328028,5.284092,5.335149,
    5.305870,5.307293,5.312868,5.341379,5.354097,
    5.308686,5.299899,5.334383,5.362127,5.420996,
    5.369048,5.349303,5.321877,5.357895,5.303988
  ),
  lon_dep = c(
    -3.981814,-4.016406,-4.017425,-3.960922,-3.996117,
    -3.998069,-4.011812,-4.010077,-4.020257,-4.026506,
    -3.964053,-3.998254,-3.938384,-3.989620,-4.019010,
    -3.999911,-4.021832,-4.103545,-3.890668,-4.003239
  ),
  lat_arr = c(
    5.3167,5.3167,5.3167,5.3167,5.3167,
    5.3167,5.3167,5.3167,5.3167,5.3167,
    5.3167,5.3167,5.3523,5.3523,5.3167,
    5.3167,5.3167,5.3167,5.3523,5.3167
  ),
  lon_arr = c(
    -4.0234,-4.0234,-4.0234,-4.0234,-4.0234,
    -4.0234,-4.0234,-4.0234,-4.0234,-4.0234,
    -4.0234,-4.0234,-3.9895,-3.9895,-4.0234,
    -4.0234,-4.0234,-4.0234,-3.9895,-4.0234
  ),
  vitesse_libre_ref = c(
    80,60,60,100,80,50,40,60,
    50,55,50,100,60,60,90,100,
    50,90,70,50
  ),
  type = "principal"
)

# ------------------------------------------------------------------------------
# 2. AXES COMPLEMENT 
# ------------------------------------------------------------------------------
axes_complement <- tibble(
  id_axe = paste0("AX_", sprintf("%02d", 21:30)),
  nom_axe = c(
    "Grand Carrefour Koumassi (VGE)",
    "Boulevard du Caire Koumassi",
    "Boulevard FHB Port-Bouet",
    "Aeroport FHB Port-Bouet",
    "4eme Pont cote Attecoube",
    "Marche Attecoube",
    "Mairie Anyama",
    "Marche Anyama",
    "Carrefour Jacqueville Songon",
    "Marche Bagnon Songon"
  ),
  commune = c(
    "Koumassi","Koumassi",
    "Port-Bouet","Port-Bouet",
    "Attecoube","Attecoube",
    "Anyama","Anyama",
    "Songon","Songon"
  ),
  destination = rep("Plateau", 10),
  lat_dep = c(
    5.288328,5.292830,
    5.289733,5.259251,
    5.329149,5.348717,
    5.507137,5.496204,
    5.321191,5.340265
  ),
  lon_dep = c(
    -3.970538,-3.961058,
    -3.981778,-3.940046,
    -4.051204,-4.034145,
    -4.053472,-4.050669,
    -4.231602,-4.109843
  ),
  lat_arr = rep(5.3167, 10),
  lon_arr = rep(-4.0234, 10),
  vitesse_libre_ref = c(80,50,80,60,70,40,90,50,90,50),
  type = "complement"
)

# ------------------------------------------------------------------------------
# 3. AXES RETOUR 
# ------------------------------------------------------------------------------
axes_retour <- tibble(
  id_axe = paste0("AX_R", sprintf("%02d", 1:12)),
  nom_axe = c(
    "Retour Plateau -> Abobo (Gare Mairie)",
    "Retour Plateau -> Yopougon (Gare Autoroute)",
    "Retour Plateau -> Yopougon (Clinique Fraternite)",
    "Retour Plateau -> Yopougon (Carrefour Jatak)",
    "Retour Plateau -> Marcory (Rond-point)",
    "Retour Cocody -> Bingerville (Lycee)",
    "Retour Cocody -> Yopougon (Carrefour Universite)",
    "Retour Cocody -> Abobo (2 Plateaux)",
    "Yopougon (Gare) -> Marcory (Zone 4)",
    "Yopougon (Jatak) -> Marcory (Zone 4)",
    "Abobo -> Cocody (Carrefour Universite)",
    "Abobo -> Marcory (Zone 4)"
  ),
  commune = c(
    "Plateau","Plateau","Plateau","Plateau","Plateau",
    "Cocody","Cocody","Cocody",
    "Yopougon","Yopougon",
    "Abobo","Abobo"
  ),
  destination = c(
    "Abobo","Yopougon","Yopougon","Yopougon","Marcory",
    "Bingerville","Yopougon","Abobo",
    "Marcory","Marcory",
    "Cocody","Marcory"
  ),
  lat_dep = c(
    5.3167,5.3167,5.3167,5.3167,5.3167,
    5.3523,5.3523,5.3523,
    5.369048,5.321877,
    5.420996,5.420996
  ),
  lon_dep = c(
    -4.0234,-4.0234,-4.0234,-4.0234,-4.0234,
    -3.9895,-3.9895,-3.9895,
    -3.999911,-4.103545,
    -4.019010,-4.019010
  ),
  lat_arr = c(
    5.420996,5.369048,5.349303,5.321877,5.303988,
    5.357895,5.369048,5.420996,
    5.303988,5.303988,
    5.334383,5.303988
  ),
  lon_arr = c(
    -4.019010,-3.999911,-4.021832,-4.103545,-4.003239,
    -3.890668,-3.999911,-4.019010,
    -4.003239,-4.003239,
    -3.938384,-4.003239
  ),
  vitesse_libre_ref = c(
    90,100,50,90,50,
    70,100,90,
    50,90,
    90,50
  ),
  type = "retour"
)

# ------------------------------------------------------------------------------
# 4. FUSION — tous les axes
# ------------------------------------------------------------------------------
axes_tous <- bind_rows(axes_principaux, axes_complement, axes_retour)
cat(sprintf("Total axes : %d\n", nrow(axes_tous)))

# ------------------------------------------------------------------------------
# 5. FONCTIONS
# ------------------------------------------------------------------------------
get_tomtom_flow <- function(lat_dep, lon_dep, lat_arr, lon_arr, key) {
  url <- paste0(
    "https://api.tomtom.com/routing/1/calculateRoute/",
    lat_dep, ",", lon_dep, ":", lat_arr, ",", lon_arr,
    "/json?key=", key, "&traffic=true&travelMode=car"
  )
  reponse <- GET(url, timeout(10))
  if (status_code(reponse) == 200) {
    contenu <- fromJSON(content(reponse, "text", encoding = "UTF-8"))
    route   <- contenu$routes$summary
    tibble(distance_m  = route$lengthInMeters,
           duree_sec   = route$travelTimeInSeconds,
           vitesse_kmh = round((route$lengthInMeters / route$travelTimeInSeconds) * 3.6, 1))
  } else {
    warning(paste("Erreur HTTP:", status_code(reponse)))
    NULL
  }
}

classer_congestion <- function(vitesse, vitesse_ref) {
  indice <- min(vitesse / vitesse_ref, 1.0)
  case_when(
    indice >= 0.80 ~ "Fluide",
    indice >= 0.50 ~ "Modere",
    indice >= 0.30 ~ "Congestionne",
    TRUE           ~ "Bloque"
  )
}

# ------------------------------------------------------------------------------
# 6. SAUVEGARDE 
# ------------------------------------------------------------------------------
sauvegarder_collecte <- function(df) {
  dir.create("data/raw/tomtom", recursive = TRUE, showWarnings = FALSE)
  date_str <- format(Sys.Date(), "%Y-%m-%d")
  
  for (t in unique(df$type)) {
    df_t   <- df |> filter(type == t)
    f_jour <- paste0("data/raw/tomtom/flux_", t, "_", date_str, ".csv")
    write_csv(df_t, f_jour,
              append    = file.exists(f_jour),
              col_names = !file.exists(f_jour))
    cat(sprintf("✅ %d lignes [%s] -> %s\n", nrow(df_t), t, f_jour))
  }
}

# ------------------------------------------------------------------------------
# 7. COLLECTE
# ------------------------------------------------------------------------------
collecter_trafic <- function() {
  cat(sprintf("\nCollecte %s a %sh%s\n",
              format(Sys.Date(), "%d/%m/%Y"),
              hour(Sys.time()),
              formatC(minute(Sys.time()), width = 2, flag = "0")))
  
  resultats <- list()
  for (i in seq_len(nrow(axes_tous))) {
    axe <- axes_tous[i, ]
    cat(sprintf("[%02d/%02d] %-50s... ", i, nrow(axes_tous), axe$nom_axe))
    mesure <- get_tomtom_flow(
      axe$lat_dep, axe$lon_dep,
      axe$lat_arr, axe$lon_arr,
      Sys.getenv("TOMTOM_KEY")
    )
    if (!is.null(mesure)) {
      resultats[[i]] <- tibble(
        id_axe            = axe$id_axe,
        nom_axe           = axe$nom_axe,
        commune           = axe$commune,
        destination       = axe$destination,
        lat_dep           = axe$lat_dep,
        lon_dep           = axe$lon_dep,
        vitesse_kmh       = mesure$vitesse_kmh,
        vitesse_libre_ref = axe$vitesse_libre_ref,
        indice_cong       = round(min(mesure$vitesse_kmh / axe$vitesse_libre_ref, 1.0), 3),
        niveau_cong       = classer_congestion(mesure$vitesse_kmh, axe$vitesse_libre_ref),
        distance_m        = mesure$distance_m,
        duree_sec         = mesure$duree_sec,
        date              = as.character(Sys.Date()),
        heure             = hour(Sys.time()),
        jour              = as.character(wday(Sys.time(), label = TRUE, abbr = FALSE)),
        timestamp         = format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
        type              = axe$type
      )
      cat(sprintf("%s km/h - %s\n", mesure$vitesse_kmh, resultats[[i]]$niveau_cong))
    } else {
      cat("Echec\n")
    }
    Sys.sleep(1)
  }
  
  df <- bind_rows(resultats)
  if (nrow(df) > 0) sauvegarder_collecte(df)
  df
}

# ------------------------------------------------------------------------------
# 8. 
# ------------------------------------------------------------------------------
lancer_collecte_auto <- function(nb_heures = 168) {
  cat(sprintf("Debut collecte complete — %d heures (%d jours) — %d axes\n",
              nb_heures, nb_heures / 24, nrow(axes_tous)))
  for (i in seq_len(nb_heures)) {
    cat(sprintf("\n=== Session %d/%d ===\n", i, nb_heures))
    collecter_trafic()
    if (i < nb_heures) Sys.sleep(1800)
  }
  cat("Collecte complete terminee.\n")
}

lancer_collecte_auto(nb_heures = 168)