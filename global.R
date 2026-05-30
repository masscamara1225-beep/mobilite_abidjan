# ==============================================================================
# OBSERVATOIRE DE LA MOBILITÉ — GRAND ABIDJAN
# global.R — Chargé UNE SEULE FOIS au démarrage de l'app
#
# Équipe :
#   • CAMARA Massaram (P2) — 4 onglets : Accueil, Carte, Trafic, Exploration
#   • LOGBO Axelle    (P1) — 4 onglets : Réseau, ML, Données, Recommandations
#   • KOUADIO Ryu Emmanuel Marie   — Rapport Quarto
#
# Formation : M1 Data Science et IA — UFHB Abidjan
# Encadrant : Dr. Laurent Rouvière (Université Rennes 2)
# Client    : ONG Abidjan Mobilité Durable
# Deadline  : 27 mai 2025
# ==============================================================================

# ------------------------------------------------------------------------------
# 1. PACKAGES
# ------------------------------------------------------------------------------
# Core Shiny
library(shiny)
library(shinydashboard)
library(shinyWidgets)        # pickerInput, awesomeRadio, etc.
library(shinyjs)
library(shinycssloaders)     # withSpinner
library(waiter)              # écran chargement démarrage

library(geojsonsf)

# Manipulation données
library(dplyr)
library(tidyr)
library(readr)
library(stringr)
library(lubridate)
library(purrr)

# Visualisation
library(ggplot2)
library(plotly)
library(DT)
library(leaflet)
library(visNetwork)

# Spatial / Routing
library(sf)
library(osrm)   # itinéraires entre communes
library(class)    # pour kNN
library(rpart)    # pour arbre de décision

# Réseau
library(igraph)
library(randomForest)

# ------------------------------------------------------------------------------
# 2. PALETTE — Couleurs CI sobres
# ------------------------------------------------------------------------------
COULEURS <- list(
  orange  = "#F47920",   # Orange CI — accent principal
  vert    = "#009A44",   # Vert CI — succès
  gris    = "#0A0A0A",   # Texte principal (presque noir)
  muted   = "#71717A",   # Texte secondaire
  bleu    = "#2980B9",   # Info (IC 95%)
  rouge   = "#DC2626",   # Danger sobre
  jaune   = "#F39C12",   # Warning
  bg      = "#FAFAFA",   # Fond app
  white   = "#FFFFFF",
  border  = "#E4E4E7",   # Bordures fines
  sidebar = "#0A0A0A"    # Sidebar dark
)

# Couleurs par niveau de congestion
COUL_CONG <- c(
  "Fluide"        = COULEURS$vert,
  "Modéré"        = COULEURS$jaune,
  "Congestionné"  = COULEURS$orange,
  "Bloqué"        = COULEURS$rouge
)

# Centre carte Abidjan
ABIDJAN_LAT  <- 5.345
ABIDJAN_LON  <- -4.024
ABIDJAN_ZOOM <- 11

# ------------------------------------------------------------------------------
# 3. CHARGEMENT DES DONNÉES (avec garde-fous tant que P1 n'a pas livré)
# ------------------------------------------------------------------------------
flux_enrichi <- tryCatch(
  read_csv("data/processed/flux_enrichi.csv", show_col_types = FALSE) |>
    mutate(date = as.Date(date),
           timestamp = as.POSIXct(timestamp)) |>
    # Si commune_dep n'existe pas (ancien format), on le crée depuis commune
    (\(df) {
      if (!"commune_dep" %in% names(df)) {
        df |> separate(commune,
                       into = c("commune_dep", "commune_arr"),
                       sep = "/", fill = "right", remove = FALSE)
      } else {
        # Nouveau format : commune_dep existe déjà, on crée commune_arr depuis commune si besoin
        if (!"commune_arr" %in% names(df)) {
          df |> separate(commune,
                         into = c("tmp_dep", "commune_arr"),
                         sep = "/", fill = "right", remove = FALSE) |>
            select(-tmp_dep)
        } else {
          df
        }
      }
    })(),
  error = function(e) {
    message("⚠️  flux_enrichi.csv pas encore livré — stub utilisé")
    tibble(
      id_axe = character(), nom_axe = character(),
      commune = character(),
      commune_dep = character(), commune_arr = character(),
      destination = character(),
      lat_dep = double(), lon_dep = double(),
      vitesse_kmh = double(), vitesse_libre_ref = double(),
      indice_cong = double(), niveau_cong = character(),
      distance_m = integer(), duree_sec = integer(),
      date = as.Date(character()), heure = integer(),
      jour = character(), timestamp = as.POSIXct(character())
    )
  }
)


  communes_wiki <- tryCatch(
    read_csv("data/processed/stats_communes_2021.csv", show_col_types = FALSE) |>
      rename(commune = nom_commune,
             population = population_2021) |>
      mutate(commune = commune |>
               stringi::stri_trans_general("Latin-ASCII") |>
               stringr::str_remove("^Le ")),
    error = function(e) {
      message("⚠️  stats_communes_2021.csv pas encore livré — stub utilisé")
      tibble(commune = character(), statut = character(), population = integer())
    }
  )
  
  # Polygones des 13 communes (geometry sf)
  communes_geo <- tryCatch(
    readRDS("data/raw/osm/communes_abidjan_13.rds") |>
      mutate(
        commune = name |>
          stringi::stri_trans_general("Latin-ASCII")  # retire les accents
      ) |>
      select(commune, geometry),
    error = function(e) {
      message("⚠️  communes_abidjan_13.rds non trouvé — stub utilisé")
      sf::st_sf(commune = character(), geometry = sf::st_sfc())
    }
  )
  
  # ============================================================================
  # Indice de disparité par commune (cœur de la problématique)
  # Formule : indice_disparite(c) = indice_cong_moyen(c) × pop(c) / max(pop)
  # Lecture : impact humain réel de la congestion (pondéré par population)
  # ============================================================================
  indice_disparite <- tryCatch({
    
    # Indice de congestion moyen par commune (sur commune_dep + commune_arr)
    cong_par_commune <- bind_rows(
      flux_enrichi |> select(commune = commune_dep, indice_cong),
      flux_enrichi |> select(commune = commune_arr, indice_cong) |> filter(!is.na(commune))
    ) |>
      group_by(commune) |>
      summarise(indice_cong_moyen = mean(indice_cong, na.rm = TRUE),
                n_mesures         = n(),
                .groups = "drop")
    
    # Jointure avec la population et calcul de l'indice de disparité
    cong_par_commune |>
      left_join(communes_wiki |> select(commune, population), by = "commune") |>
      mutate(
        pop_max          = max(population, na.rm = TRUE),
        indice_disparite = indice_cong_moyen * population / pop_max
      ) |>
      arrange(desc(indice_disparite))
    
  }, error = function(e) {
    message("⚠️  Calcul indice_disparite impossible — données manquantes")
    tibble(commune = character(), indice_cong_moyen = double(),
           n_mesures = integer(), population = integer(),
           pop_max = integer(), indice_disparite = double())
  })
  
  
graphe_communes <- tryCatch(
  readRDS("data/processed/graphe_communes.rds"),
  error = function(e) {
    message("⚠️  graphe_communes.rds pas encore livré")
    NULL
  }
)

mod_rf <- tryCatch(
  readRDS("models/mod_rf_vitesse.rds"),
  error = function(e) {
    message("⚠️  mod_rf_vitesse.rds pas encore livré")
    NULL
  }
)

# ------------------------------------------------------------------------------
# 4. HELPERS STATISTIQUES — IC 95% (Chapitre 11)
# ------------------------------------------------------------------------------

#' Calcule l'IC 95% d'une moyenne.
#' Formule : mean ± qnorm(0.975) × sd / sqrt(n)
#' qnorm(0.975) = 1.96
#'
#' @param x vecteur numérique
#' @return liste avec moy, se, ic_lo, ic_hi, n
ic95 <- function(x) {
  x <- x[!is.na(x)]
  n <- length(x)
  if (n < 2) {
    return(list(moy = mean(x), se = NA, ic_lo = NA, ic_hi = NA, n = n))
  }
  moy <- mean(x)
  se  <- sd(x) / sqrt(n)
  list(
    moy   = moy,
    se    = se,
    ic_lo = moy - qnorm(0.975) * se,
    ic_hi = moy + qnorm(0.975) * se,
    n     = n
  )
}

#' Version dplyr — ajoute moy/se/ic_lo/ic_hi sur un summarise
#' Usage : df |> group_by(commune) |> summarise(ic_summary(vitesse_kmh))
ic_summary <- function(x) {
  x <- x[!is.na(x)]
  n <- length(x)
  moy <- mean(x)
  se  <- if (n >= 2) sd(x) / sqrt(n) else NA_real_
  tibble(
    moy   = moy,
    se    = se,
    ic_lo = moy - qnorm(0.975) * se,
    ic_hi = moy + qnorm(0.975) * se,
    n     = n
  )
}

# ------------------------------------------------------------------------------
# 5. COMPOSANTS UI MINIMALISTES
# ------------------------------------------------------------------------------

#' KPI card — minimaliste, pas d'icônes décoratives
kpi_card <- function(label, value, hint = NULL, accent = NULL) {
  tags$div(
    class = "kpi",
    style = if (!is.null(accent)) paste0("--a:", accent, ";") else NULL,
    tags$div(class = "kpi-label", label),
    tags$div(class = "kpi-value", value),
    if (!is.null(hint)) tags$div(class = "kpi-hint", hint)
  )
}

#' Sous-titre sobre (remplace les story_box dégoulinants)
section_subtitle <- function(text) {
  tags$p(class = "section-subtitle", text)
}

#' Note d'interprétation (sobre, gris clair, sans emoji)
note_box <- function(text) {
  tags$div(
    class = "note",
    tags$strong("Lecture · "),
    if (is.character(text)) HTML(text) else text
  )
}

#' Page header simple — titre + meta
page_header <- function(title, meta = NULL) {
  tags$div(
    class = "ph",
    tags$h2(class = "ph-title", title),
    if (!is.null(meta)) tags$p(class = "ph-meta", meta)
  )
}

#' Card minimaliste (remplace les box() shinydashboard pleines de couleurs)
card <- function(..., title = NULL, padded = TRUE) {
  tags$div(
    class = "card-min",
    if (!is.null(title)) tags$div(class = "card-min-header", title),
    tags$div(
      class = if (padded) "card-min-body" else "card-min-body-flush",
      ...
    )
  )
}

#' Affichage d'un IC 95% sous forme texte court
format_ic <- function(ic, unit = "", digits = 1) {
  if (is.na(ic$ic_lo)) return("—")
  sprintf("[%.*f ; %.*f] %s",
          digits, ic$ic_lo, digits, ic$ic_hi, unit)
}

# ------------------------------------------------------------------------------
# 6. INFOS DE DÉPLOIEMENT
# ------------------------------------------------------------------------------
APP_VERSION   <- "0.2.0-CDC-v7"
APP_DEPLOIEE  <- "https://[à-renseigner].shinyapps.io/mobilite_abidjan/"
RAPPORT_URL   <- "rapport.html"

EQUIPE <- list(
  list(nom = "CAMARA Massaram",  role = "P2 — Accueil, Carte, Trafic, Exploration"),
  list(nom = "LOGBO Axelle",     role = "P1 — Réseau, ML, Données, Recommandations"),
  list(nom = "KOUADIO Ryu Emmanuel Marie", role = "Rapport Quarto")
)

loader_carte <- Waiter$new(
  id = "carte_principale",
  html = tagList(
    tags$div(class = "boot-local",
             tags$p("Calcul de l'itinéraire"),
             tags$div(class = "bar-wrap",
                      tags$div(class = "bar-fill-load")
             )
    )
  ),
  color = "rgba(255, 255, 255, 0.85)"
)
message("✅ global.R chargé — version ", APP_VERSION)
