library(dplyr)
library(httr)
library(jsonlite)
library(readr)
library(lubridate)

api_key <- Sys.getenv("TOMTOM_KEY")

axes_abidjan <- tibble(
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
  lat_dep = c(
    5.329775, 5.317516, 5.328028, 5.284092, 5.335149,
    5.305870, 5.307293, 5.312868, 5.341379, 5.354097,
    5.308686, 5.299899, 5.334383, 5.362127, 5.420996,
    5.369048, 5.349303, 5.321877, 5.357895, 5.303988
  ),
  lon_dep = c(
    -3.981814, -4.016406, -4.017425, -3.960922, -3.996117,
    -3.998069, -4.011812, -4.010077, -4.020257, -4.026506,
    -3.964053, -3.998254, -3.938384, -3.989620, -4.019010,
    -3.999911, -4.021832, -4.103545, -3.890668, -4.003239
  ),
  lat_arr = c(
    5.3167, 5.3167, 5.3167, 5.3167, 5.3167,
    5.3167, 5.3167, 5.3167,
    5.3167, 5.3167, 5.3167, 5.3167,
    5.3523, 5.3523,
    5.3167,
    5.3167, 5.3167, 5.3167,
    5.3523,
    5.3167
  ),
  lon_arr = c(
    -4.0234, -4.0234, -4.0234, -4.0234, -4.0234,
    -4.0234, -4.0234, -4.0234,
    -4.0234, -4.0234, -4.0234, -4.0234,
    -3.9895, -3.9895,
    -4.0234,
    -4.0234, -4.0234, -4.0234,
    -3.9895,
    -4.0234
  ),
  destination = c(
    "Plateau", "Plateau", "Plateau", "Plateau", "Plateau",
    "Plateau", "Plateau", "Plateau",
    "Plateau", "Plateau", "Plateau", "Plateau",
    "Cocody", "Cocody",
    "Plateau",
    "Plateau", "Plateau", "Plateau",
    "Cocody",
    "Plateau"
  ),
  vitesse_libre_ref = c(
    80, 60, 60, 100, 80, 50, 40, 60,
    50, 55, 50, 100, 60, 60, 90, 100,
    50, 90, 70, 50
  )
)

get_tomtom_flow <- function(lat_dep, lon_dep, lat_arr, lon_arr, key) {
  url <- paste0(
    "https://api.tomtom.com/routing/1/calculateRoute/",
    lat_dep, ",", lon_dep, ":", lat_arr, ",", lon_arr,
    "/json",
    "?key=", key,
    "&traffic=true",
    "&travelMode=car"
  )
  reponse <- GET(url, timeout(10))
  if (status_code(reponse) == 200) {
    contenu <- fromJSON(content(reponse, "text", encoding = "UTF-8"))
    route <- contenu$routes$summary
    distance_m <- route$lengthInMeters
    duree_sec <- route$travelTimeInSeconds
    vitesse_kmh <- round((distance_m / duree_sec) * 3.6, 1)
    return(tibble(
      distance_m = distance_m,
      duree_sec = duree_sec,
      vitesse_kmh = vitesse_kmh
    ))
  } else {
    warning(paste("Erreur HTTP:", status_code(reponse)))
    return(NULL)
  }
}

classer_congestion <- function(vitesse, vitesse_ref) {
  indice <- min(vitesse / vitesse_ref, 1.0)
  case_when(
    indice >= 0.80 ~ "Fluide",
    indice >= 0.50 ~ "Modere",
    indice >= 0.30 ~ "Congestionne",
    TRUE ~ "Bloque"
  )
}

sauvegarder_collecte <- function(df) {
  dir.create("data/raw/tomtom", recursive = TRUE, showWarnings = FALSE)
  f_jour <- paste0("data/raw/tomtom/flux_", format(Sys.Date(), "%Y-%m-%d"), ".csv")
  f_cumul <- "data/raw/tomtom/flux_cumul.csv"
  write_csv(df, f_jour,
            append = file.exists(f_jour),
            col_names = !file.exists(f_jour)
  )
  write_csv(df, f_cumul,
            append = file.exists(f_cumul),
            col_names = !file.exists(f_cumul)
  )
  cat(sprintf("Sauvegarde : %d lignes -> %s\n", nrow(df), f_jour))
}

collecter_trafic <- function() {
  cat(sprintf("\nCollecte du %s a %sh%s\n",
              format(Sys.Date(), "%d/%m/%Y"),
              hour(Sys.time()),
              minute(Sys.time())
  ))
  resultats <- list()
  for (i in seq_len(nrow(axes_abidjan))) {
    axe <- axes_abidjan[i, ]
    cat(sprintf("[%02d/%02d] %-40s... ", i, nrow(axes_abidjan), axe$nom_axe))
    mesure <- get_tomtom_flow(
      axe$lat_dep, axe$lon_dep,
      axe$lat_arr, axe$lon_arr,
      Sys.getenv("TOMTOM_KEY")
    )
    if (!is.null(mesure)) {
      resultats[[i]] <- tibble(
        id_axe = axe$id_axe,
        nom_axe = axe$nom_axe,
        commune = axe$commune,
        destination = axe$destination,
        lat_dep = axe$lat_dep,
        lon_dep = axe$lon_dep,
        vitesse_kmh = mesure$vitesse_kmh,
        vitesse_libre_ref = axe$vitesse_libre_ref,
        indice_cong = round(min(mesure$vitesse_kmh / axe$vitesse_libre_ref, 1.0), 3),
        niveau_cong = classer_congestion(mesure$vitesse_kmh, axe$vitesse_libre_ref),
        distance_m = mesure$distance_m,
        duree_sec = mesure$duree_sec,
        date = as.character(Sys.Date()),
        heure = hour(Sys.time()),
        jour = as.character(wday(Sys.time(), label = TRUE, abbr = FALSE)),
        timestamp = format(Sys.time(), "%Y-%m-%d %H:%M:%S")
      )
      cat(sprintf("%s km/h - %s\n",
                  mesure$vitesse_kmh,
                  resultats[[i]]$niveau_cong
      ))
    } else {
      cat("Echec\n")
    }
    Sys.sleep(1)
  }
  df <- bind_rows(resultats)
  sauvegarder_collecte(df)
  df
}

lancer_collecte_auto <- function(nb_heures = 240) {
  cat(sprintf("Debut collecte automatique — %d heures prevues\n", nb_heures))
  for (i in seq_len(nb_heures)) {
    cat(sprintf("\n--- Session %d/%d ---\n", i, nb_heures))
    collecter_trafic()
    if (i < nb_heures) Sys.sleep(1800)
  }
  cat("Collecte terminee.\n")
}


lancer_collecte_auto(nb_heures = 240)