library(dplyr)
library(httr)
library(jsonlite)
library(readr)
library(lubridate)

# ==============================================================================
# 03c_collecte_retour.R

# ==============================================================================

# ------------------------------------------------------------------------------
# 1. AXES RETOUR — inverses des axes residentiels existants
# ------------------------------------------------------------------------------
axes_abidjan <- tibble(
  id_axe = paste0("AX_R", sprintf("%02d", 1:12)),
  nom_axe = c(
    # Retours depuis Plateau
    "Retour Plateau -> Abobo (Gare Mairie)",
    "Retour Plateau -> Yopougon (Gare Autoroute)",
    "Retour Plateau -> Yopougon (Clinique Fraternite)",
    "Retour Plateau -> Yopougon (Carrefour Jatak)",
    "Retour Plateau -> Marcory (Rond-point)",
    # Retours depuis Cocody
    "Retour Cocody -> Bingerville (Lycee)",
    "Retour Cocody -> Yopougon (via Carrefour Universite)",
    "Retour Cocody -> Abobo (via 2 Plateaux)",
    # Nouvelles paires manquantes
    "Yopougon (Gare) -> Marcory (Zone 4)",
    "Yopougon (Jatak) -> Marcory (Zone 4)",
    "Abobo -> Cocody (Carrefour Universite)",
    "Abobo -> Marcory (Zone 4)"
  ),
  commune = c(
    "Plateau", "Plateau", "Plateau", "Plateau", "Plateau",
    "Cocody", "Cocody", "Cocody",
    "Yopougon", "Yopougon",
    "Abobo", "Abobo"
  ),
  destination = c(
    "Abobo", "Yopougon", "Yopougon", "Yopougon", "Marcory",
    "Bingerville", "Yopougon", "Abobo",
    "Marcory", "Marcory",
    "Cocody", "Marcory"
  ),
  # Points de depart = anciens points d'arrivee (centre)
  lat_dep = c(
    5.3167, 5.3167, 5.3167, 5.3167, 5.3167,  # depuis Plateau
    5.3523, 5.3523, 5.3523,                    # depuis Cocody
    5.369048, 5.321877,                         # depuis Yopougon
    5.420996, 5.420996                          # depuis Abobo
  ),
  lon_dep = c(
    -4.0234, -4.0234, -4.0234, -4.0234, -4.0234,
    -3.9895, -3.9895, -3.9895,
    -3.999911, -4.103545,
    -4.019010, -4.019010
  ),
  # Points d'arrivee = anciens points de depart (commune)
  lat_arr = c(
    5.420996,  # Abobo Gare
    5.369048,  # Gare Yopougon
    5.349303,  # Clinique Fraternite
    5.321877,  # Carrefour Jatak
    5.303988,  # Marcory
    5.357895,  # Lycee Bingerville
    5.369048,  # Yopougon
    5.420996,  # Abobo
    5.303988,  # Marcory
    5.303988,  # Marcory
    5.334383,  # Cocody Universite
    5.303988   # Marcory
  ),
  lon_arr = c(
    -4.019010,
    -3.999911,
    -4.021832,
    -4.103545,
    -4.003239,
    -3.890668,
    -3.999911,
    -4.019010,
    -4.003239,
    -4.003239,
    -3.938384,
    -4.003239
  ),
  vitesse_libre_ref = c(
    90, 100, 50, 90, 50,
    70, 100, 90,
    50, 90,
    90, 50
  )
)

# ------------------------------------------------------------------------------
# 2. FONCTIONS (identiques au script principal)
# ------------------------------------------------------------------------------
get_tomtom_flow <- function(lat_dep, lon_dep, lat_arr, lon_arr, key) {
  url <- paste0(
    "https://api.tomtom.com/routing/1/calculateRoute/",
    lat_dep, ",", lon_dep, ":", lat_arr, ",", lon_arr,
    "/json?key=", key,
    "&traffic=true&travelMode=car"
  )
  reponse <- GET(url, timeout(10))
  if (status_code(reponse) == 200) {
    contenu <- fromJSON(content(reponse, "text", encoding = "UTF-8"))
    route   <- contenu$routes$summary
    tibble(
      distance_m  = route$lengthInMeters,
      duree_sec   = route$travelTimeInSeconds,
      vitesse_kmh = round((route$lengthInMeters / route$travelTimeInSeconds) * 3.6, 1)
    )
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
# 3. SAUVEGARDE — fichier par jour, ne touche pas flux_cumul.csv
# ------------------------------------------------------------------------------
sauvegarder_collecte <- function(df) {
  dir.create("data/raw/tomtom", recursive = TRUE, showWarnings = FALSE)
  f_jour <- paste0("data/raw/tomtom/flux_retour_",
                   format(Sys.Date(), "%Y-%m-%d"), ".csv")
  write_csv(df, f_jour,
            append    = file.exists(f_jour),
            col_names = !file.exists(f_jour))
  cat(sprintf("✅ %d lignes -> %s\n", nrow(df), f_jour))
}

# ------------------------------------------------------------------------------
# 4. COLLECTE
# ------------------------------------------------------------------------------
collecter_trafic <- function() {
  cat(sprintf("\nCollecte retour %s a %sh%s\n",
              format(Sys.Date(), "%d/%m/%Y"),
              hour(Sys.time()),
              formatC(minute(Sys.time()), width = 2, flag = "0")))
  resultats <- list()
  for (i in seq_len(nrow(axes_abidjan))) {
    axe <- axes_abidjan[i, ]
    cat(sprintf("[%02d/%02d] %-50s... ", i, nrow(axes_abidjan), axe$nom_axe))
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
        sens              = "retour"
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
# 5. LANCEMENT 
# ------------------------------------------------------------------------------
lancer_collecte_auto <- function(nb_heures = 168) {
  cat(sprintf("Debut collecte retour — %d heures (%d jours)\n",
              nb_heures, nb_heures / 24))
  for (i in seq_len(nb_heures)) {
    cat(sprintf("\n--- Session %d/%d ---\n", i, nb_heures))
    collecter_trafic()
    if (i < nb_heures) Sys.sleep(1800)
  }
  cat("Collecte retour terminee.\n")
}

lancer_collecte_auto(nb_heures = 168)