# ==============================================================================
# OBSERVATOIRE DE LA MOBILITÉ — GRAND ABIDJAN
# global.R — Chargé UNE SEULE FOIS au démarrage de l'app
# Personne 2 — Shiny + UI
# ==============================================================================

# ------------------------------------------------------------------------------
# 1. PACKAGES
# ------------------------------------------------------------------------------
# Core Shiny
library(shiny)
library(shinydashboard)
library(shinyjs)
library(waiter)

# Manipulation données
library(dplyr)
library(tidyr)
library(readr)
library(stringr)
library(lubridate)

# Visualisation
library(ggplot2)
library(plotly)
library(DT)
library(leaflet)
library(visNetwork)

# Spatial / Routing
library(sf)
# library(osrm)   # à activer quand on fait l'itinéraire

# Réseau
library(igraph)

# ------------------------------------------------------------------------------
# 2. CONSTANTES — Palette CI + paramètres app
# ------------------------------------------------------------------------------
COULEURS <- list(
  orange  = "#F47920",   # Orange CI
  vert    = "#009A44",   # Vert CI
  gris    = "#2C3E50",
  bleu    = "#2980B9",
  rouge   = "#E74C3C",
  jaune   = "#F39C12",
  violet  = "#8E44AD",
  bg      = "#F1F4F8",
  white   = "#FFFFFF",
  border  = "#E2E8F0",
  muted   = "#718096"
)

# Couleurs par niveau de congestion
COUL_CONG <- c(
  "Fluide"        = COULEURS$vert,
  "Modéré"        = COULEURS$jaune,
  "Congestionné"  = COULEURS$orange,
  "Bloqué"        = COULEURS$rouge
)

# Centre carte Abidjan
ABIDJAN_LAT <- 5.345
ABIDJAN_LON <- -4.024
ABIDJAN_ZOOM <- 11

# ------------------------------------------------------------------------------
# 3. CHARGEMENT DES DONNÉES (avec garde-fous tant que P1 n'a pas livré)
# ------------------------------------------------------------------------------
# NOTE : Tant que Personne 1 n'a pas livré les fichiers, on utilise des stubs.
#        Quand les vrais fichiers seront dans data/processed/, on bascule.

flux_enrichi <- tryCatch(
  read_csv("data/processed/flux_enrichi.csv", show_col_types = FALSE),
  error = function(e) {
    message("⚠️ flux_enrichi.csv pas encore livré — stub utilisé")
    tibble(
      axe_id = character(), commune_nom = character(),
      heure = integer(), jour = as.Date(character()),
      vitesse_kmh = double(), vitesse_libre = double(),
      indice_cong = double(), niveau_cong = character(),
      population = integer(), superficie = double(), zone = character()
    )
  }
)

communes_wiki <- tryCatch(
  read_csv("data/processed/communes_clean.csv", show_col_types = FALSE),
  error = function(e) {
    message("⚠️ communes_clean.csv pas encore livré — stub utilisé")
    tibble(commune = character(), population = integer(),
           superficie = double(), zone = character())
  }
)

graphe_communes <- tryCatch(
  readRDS("data/processed/graphe_communes.rds"),
  error = function(e) {
    message("⚠️ graphe_communes.rds pas encore livré")
    NULL
  }
)

mod_rf <- tryCatch(
  readRDS("models/mod_rf_vitesse.rds"),
  error = function(e) {
    message("⚠️ mod_rf_vitesse.rds pas encore livré")
    NULL
  }
)

# ------------------------------------------------------------------------------
# 4. COMPOSANTS UI RÉUTILISABLES
# ------------------------------------------------------------------------------

#' KPI Card — carte indicateur clé avec barre colorée en haut
kpi_card <- function(label, value, delta = NULL, color = COULEURS$orange,
                     icon = NULL, delta_dir = c("up", "dn", "neutral")) {
  delta_dir <- match.arg(delta_dir)
  tags$div(
    class = "kpi",
    style = paste0("--a:", color, ";"),
    if (!is.null(icon)) tags$div(class = "ki", icon),
    tags$div(class = "kl", toupper(label)),
    tags$div(class = "kv", value),
    if (!is.null(delta)) tags$div(class = paste("kd", delta_dir), delta)
  )
}

#' Story box — encadré narratif orange/vert (accroche d'onglet)
story_box <- function(text, insight = NULL) {
  tags$div(
    class = "story",
    tags$span(class = "story-ico", "📖"),
    tags$span(
      class = "story-txt",
      HTML(text),
      if (!is.null(insight)) tags$span(class = "story-insight", insight)
    )
  )
}

#' Interp box — encadré bleu d'interprétation sous un graphe
interp_box <- function(text) {
  tags$div(
    class = "interp",
    HTML(paste0("💡 <strong>À lire :</strong> ", text))
  )
}

#' Page header — titre + sous-titre + actions (boutons)
page_header <- function(title, subtitle = NULL, actions = NULL) {
  tags$div(
    class = "ph",
    tags$div(
      tags$div(class = "pt", title),
      if (!is.null(subtitle)) tags$div(class = "ps", subtitle)
    ),
    if (!is.null(actions)) tags$div(class = "pa", actions)
  )
}

#' Lien narratif vers l'onglet suivant (en bas d'onglet)
lien_suivant <- function(texte, onglet_cible) {
  tags$div(
    class = "story",
    style = "margin-top:14px;border-color:rgba(0,154,68,0.25);
             background:linear-gradient(135deg,rgba(0,154,68,0.06),rgba(244,121,32,0.04));",
    tags$span(class = "story-ico", "→"),
    tags$span(class = "story-txt", HTML(texte)),
    tags$span(
      style = "margin-left:8px;color:#F47920;font-weight:600;font-size:11px;",
      paste("→", onglet_cible)
    )
  )
}

# ------------------------------------------------------------------------------
# 5. INFOS DE DÉPLOIEMENT
# ------------------------------------------------------------------------------
APP_VERSION   <- "0.1.0-J1"
APP_DEPLOIEE  <- "https://[à-renseigner].shinyapps.io/mobilite_abidjan/"
RAPPORT_URL   <- "rapport.html"

message("✅ global.R chargé — version ", APP_VERSION)
