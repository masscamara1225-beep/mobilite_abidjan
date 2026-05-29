library(dplyr)
library(httr)
library(jsonlite)
library(readr)
library(lubridate)


# ------------------------------------------------------------------------------
# 1. NOUVEAUX AXES
# ------------------------------------------------------------------------------
axes_abidjan <- tibble(
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
    "Koumassi", "Koumassi",
    "Port-Bouet", "Port-Bouet",
    "Attecoube", "Attecoube",
    "Anyama", "Anyama",
    "Songon", "Songon"
  ),
  lat_dep = c(
    5.288328, 5.292830,
    5.289733, 5.259251,
    5.329149, 5.348717,
    5.507137, 5.496204,
    5.321191, 5.340265
  ),
  lon_dep = c(
    -3.970538, -3.961058,
    -3.981778, -3.940046,
    -4.051204, -4.034145,
    -4.053472, -4.050669,
    -4.231602, -4.109843
  ),
  lat_arr = rep(5.3167, 10),
  lon_arr = rep(-4.0234, 10),
  destination = rep("Plateau", 10),
  vitesse_libre_ref = c(
    80, 50,   # Koumassi
    80, 60,   # Port-Bouet
    70, 40,   # Attecoube
    90, 50,   # Anyama
    90, 50    # Songon
  )
)

# ------------------------------------------------------------------------------
# 2. FONCTIONS
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
# 3. SAUVEGARDE — 
# ------------------------------------------------------------------------------

sauvegarder_collecte <- function(df) {
  dir.create("data/raw/tomtom", recursive = TRUE, showWarnings = FALSE)
  
  # Fichier du jour pour les axes complémentaires
  f_jour <- paste0(
    "data/raw/tomtom/flux_complement_",
    format(Sys.Date(), "%Y-%m-%d"),
    ".csv"
  )
  
  write_csv(df, f_jour,
            append    = file.exists(f_jour),
            col_names = !file.exists(f_jour))
  
  cat(sprintf("✅ %d lignes sauvegardées -> %s\n", nrow(df), f_jour))
}

# ------------------------------------------------------------------------------
# 4. COLLECTE
# ------------------------------------------------------------------------------

collecter_trafic <- function() {
  cat(sprintf("\nCollecte du %s a %sh%s\n",
              format(Sys.Date(), "%d/%m/%Y"),
              hour(Sys.time()),
              formatC(minute(Sys.time()), width = 2, flag = "0")))
  
  resultats <- list()
  
  for (i in seq_len(nrow(axes_abidjan))) {
    axe <- axes_abidjan[i, ]
    cat(sprintf("[%02d/10] %-40s... ", i, axe$nom_axe))
    
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
        indice_cong       = round(
          min(mesure$vitesse_kmh / axe$vitesse_libre_ref, 1.0), 3
        ),
        niveau_cong       = classer_congestion(
          mesure$vitesse_kmh, axe$vitesse_libre_ref
        ),
        distance_m        = mesure$distance_m,
        duree_sec         = mesure$duree_sec,
        date              = as.character(Sys.Date()),
        heure             = hour(Sys.time()),
        jour              = as.character(
          wday(Sys.time(), label = TRUE, abbr = FALSE)
        ),
        timestamp         = format(Sys.time(), "%Y-%m-%d %H:%M:%S")
      )
      cat(sprintf("%s km/h - %s\n",
                  mesure$vitesse_kmh,
                  resultats[[i]]$niveau_cong))
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

# ------------------------------------------------------------------------------

lancer_collecte_auto <- function(nb_heures = 168) {
  cat(sprintf(
    "Debut collecte complementaire — %d heures prevues (%d jours)\n",
    nb_heures, nb_heures / 24
  ))
  
  for (i in seq_len(nb_heures)) {
    cat(sprintf("\n--- Session %d/%d ---\n", i, nb_heures))
    collecter_trafic()
    if (i < nb_heures) Sys.sleep(1800)  # 30 minutes
  }
  
  cat("Collecte complementaire terminee.\n")
}

lancer_collecte_auto(nb_heures = 168)