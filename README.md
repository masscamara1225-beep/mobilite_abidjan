# 🚗 Observatoire de la Mobilité — Grand Abidjan

> Application Shiny d'analyse des flux de transport urbain à Abidjan.
> *Comprendre les embouteillages d'Abidjan par les données.*

**Équipe** : Master MAS · INPHB Abidjan
**Encadrant** : Dr. Laurent Rouvière — Université Rennes 2
**Client** : ONG Abidjan Mobilité Durable
**Deadline** : 27 mai 2025

---

## 🚀 Lancer l'application en local

```r
# 1. Installer les packages (une seule fois)
install.packages(c(
  "shiny", "shinydashboard", "shinyjs", "waiter",
  "dplyr", "tidyr", "readr", "stringr", "lubridate",
  "ggplot2", "plotly", "DT", "leaflet", "visNetwork",
  "sf", "osrm", "igraph",
  "randomForest", "caret", "FNN", "rpart"
))

# 2. Ouvrir le projet dans RStudio puis :
shiny::runApp()
```

L'app se lance par défaut sur `http://127.0.0.1:XXXX`.

## 🌐 Application déployée

👉 [À renseigner — lien shinyapps.io]

## 📁 Structure du projet

```
mobilite_abidjan/
├── global.R              # Packages, données, composants UI
├── ui.R                  # Interface — 6 onglets
├── server.R              # Logique réactive
├── rapport.qmd           # Rapport Quarto 8 pages
├── scripts/              # Pipelines de collecte / nettoyage / ML
├── data/
│   ├── raw/              # Données brutes (non versionnées)
│   └── processed/        # Données nettoyées (versionnées)
├── models/               # Modèles ML sauvegardés (.rds)
├── www/
│   └── style.css         # Thème orange/vert CI
└── outputs/              # Graphiques pour le rapport Quarto
```

## 🗂️ Les 6 onglets

| # | Onglet | Question | Méthode R |
|---|--------|----------|-----------|
| 1 | 🏠 Accueil | État de la mobilité aujourd'hui ? | `valueBox` + `readr` |
| 2 | 🗺️ Carte | Où se forment les bouchons ? | `leaflet` + `sf` + `osrm` |
| 3 | 📊 Trafic | Quand la ville se bloque-t-elle ? | `plotly` + `dplyr` |
| 4 | 🔗 Réseau | Quelle commune paralyse tout ? | `igraph` + `visNetwork` |
| 5 | 🤖 ML | Peut-on anticiper ? | `randomForest` + `caret` |
| 6 | 📋 Données | Sont-elles fiables ? | `DT` + `readr` |

## 👥 Répartition du travail

- **Personne 1** — Data + Graphe + ML : `scripts/`, `data/processed/`, `models/`
- **Personne 2** — Shiny + UI + Carte + Déploiement : `ui.R`, `server.R`, `global.R`, `www/`

## 🔑 Variables d'environnement

Créer un fichier `.env` (non versionné) à la racine :

```
TOMTOM_KEY=votre_cle_ici
```

## 📊 Sources de données

- **GTFS Abidjan** — [hub.tumidata.org](https://hub.tumidata.org)
- **OpenStreetMap** — package `osmdata`
- **TomTom Traffic Flow API** — [developer.tomtom.com](https://developer.tomtom.com)
- **HDX OCHA Côte d'Ivoire** — [data.humdata.org](https://data.humdata.org)
- **Wikipedia** — scraping `rvest` des communes d'Abidjan
