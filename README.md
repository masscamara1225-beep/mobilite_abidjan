# 🚗 Observatoire de la Mobilité — Grand Abidjan

**Comprendre les embouteillages d'Abidjan par les données.** Application Shiny d'analyse des flux de transport urbain, croisant quatre sources de données pour identifier les axes à traiter en priorité.

**Application déployée → [cam-s.shinyapps.io/mobilite-abidjan](https://cam-s.shinyapps.io/mobilite-abidjan/)**

![R](https://img.shields.io/badge/R-4.5-276DC3?logo=r&logoColor=white)
![Shiny](https://img.shields.io/badge/Shiny-deployed-1E8CBE)
![Leaflet](https://img.shields.io/badge/Leaflet-maps-199900?logo=leaflet&logoColor=white)

---

## Le problème

Abidjan compte plus de **6,2 millions d'habitants**, et un actif abidjanais perd en moyenne **2h20 par jour** dans les transports. Selon l'AMUGA, cette congestion chronique ampute une part substantielle du PIB de la métropole.

Face à ce constat, la tentation est de traiter la ville comme un bloc homogène. Notre analyse montre que c'est précisément l'erreur à ne pas commettre.

> **Face à la congestion, toutes les communes sont-elles égales ? Quels axes traiter en priorité ?**

---

## Le résultat qui change la lecture

**Le paradoxe Adjamé / Yopougon.**

Adjamé affiche l'indice de congestion le plus élevé du district (0,60). C'est la commune qu'une politique publique classique traiterait en premier.

Mais Yopougon, commune dortoir de **1,5 million d'habitants**, subit un coût social bien plus lourd en volume horaire total perdu — pour un indice pourtant moins alarmant.

Autrement dit : **l'intensité de la congestion et son coût humain ne désignent pas la même commune.** Une politique de transport uniforme est donc structurellement inefficace, et prioriser sur le seul indice de congestion revient à mal allouer l'investissement public.

---

## Le défi technique : corriger des données faussées

L'API TomTom renvoie pour Abidjan des vitesses artificiellement lissées, autour de **33 km/h constants** — une valeur qui efface complètement le double pic de congestion vécu quotidiennement par les usagers.

Utilisées telles quelles, ces données auraient produit une analyse plausible mais fausse : une ville fluide, sans heure de pointe.

Les flux ont donc été **redressés** à l'aide des coefficients horaires de l'OFT/AMUGA et de pénalités communales, afin de restituer le profil réel de la journée abidjanaise. C'est cette étape de correction qui conditionne la validité de tout ce qui suit.

---

## Résultats

### Les disparités communales sont réelles, pas anecdotiques

Aux heures de pointe, Adjamé et Yopougon s'enfoncent **sous la barre critique des 15 km/h**, tandis que les secteurs littoraux comme Port-Bouët conservent des moyennes nettement supérieures.

Le test non-paramétrique de **Kruskal-Wallis** rejette l'hypothèse d'égalité des profils de vitesse entre communes (**p < 0,001**) : ces écarts territoriaux sont statistiquement établis, pas une impression de terrain.

### Le Plateau est un goulet d'étranglement structurel

La voirie a été modélisée sous forme de graphe — **62 arêtes reliant les 13 communes majeures** — pour mesurer la vulnérabilité du réseau.

| Indicateur | Valeur | Lecture |
|---|---:|---|
| Intermédiarité du Plateau | **20,9 %** | Plus d'un cinquième des itinéraires les plus courts de la métropole convergent obligatoirement vers cette commune |
| Modularité du réseau | **0,088** | Absence quasi totale de voies de contournement ou de sous-réseaux périphériques autonomes |

Ce résultat est le plus actionnable de l'étude : tant qu'aucun contournement n'existe, désengorger le Plateau localement ne suffira pas — le réseau n'offre aucune alternative de report.

### La congestion est prévisible

| Modèle | Performance |
|---|---|
| **Random Forest** | **R² = 0,85** |

Le modèle capture les interactions non linéaires du trafic avec précision. La hiérarchie des variables est instructive : l'**heure de la journée** et l'**identifiant de l'axe** dictent la chute de vitesse, tandis que le **jour de la semaine n'a aucun effet significatif** du lundi au jeudi.

Autrement dit, la paralysie est uniforme sur toute la semaine de travail — il n'existe pas de « bon jour » pour circuler.

### Un blocage synchrone à l'échelle de la ville

La heatmap spatio-temporelle révèle un effet d'entonnoir métropolitain : les plages de **8h00** et de **17h00–18h00** virent au rouge critique sur la quasi-totalité des lignes communales, simultanément.

Ce n'est pas un problème d'infrastructure isolé mais l'effet du **rythme de travail unique** imposé dans la capitale économique, qui sature l'ensemble de la voirie aux mêmes instants. Un étalement des horaires aurait mécaniquement plus d'impact que bien des aménagements routiers.

---

## L'application

Six onglets, chacun répondant à une question :

| # | Onglet | Question | Méthode |
|---|---|---|---|
| 1 | 🏠 Accueil | État de la mobilité aujourd'hui ? | valueBox + readr |
| 2 | 🗺️ Carte | Où se forment les bouchons ? | leaflet + sf + osrm |
| 3 | 📊 Trafic | Quand la ville se bloque-t-elle ? | plotly + dplyr |
| 4 | 🔗 Réseau | Quelle commune paralyse tout ? | igraph + visNetwork |
| 5 | 🤖 ML | Peut-on anticiper ? | randomForest + caret |
| 6 | 📋 Données | Sont-elles fiables ? | DT + readr |

Trois choix techniques méritent d'être signalés. **shinydashboard** structure l'application comme un tableau de bord destiné à des décideurs, pas comme une interface d'analyste. **osrm** calcule les temps de trajet sur la voirie réelle d'Abidjan plutôt qu'à vol d'oiseau, ce qui élimine un biais majeur dans une ville où le réseau contraint fortement les itinéraires. Et un **échantillonnage aléatoire** des traces GPS maintient la fluidité de l'application sans perte de représentativité statistique.

---

## Sources de données

| Source | Type | Rôle |
|---|---|---|
| [TomTom Traffic Flow API](https://developer.tomtom.com) | Flux de trafic | Vitesses et temps de parcours en temps réel |
| [GTFS Abidjan](https://hub.tumidata.org) | Réseau de transport | Offre SOTRA, gbakas et lignes théoriques |
| OpenStreetMap (package `osmdata`) | Infrastructures | Géométrie des axes et hiérarchie routière |
| INS / RGPH · [HDX OCHA](https://data.humdata.org) | Démographie | Densité de population par commune |

---

## Installation

```bash
git clone https://github.com/masscamara1225-beep/mobilite_abidjan.git
cd mobilite_abidjan
```

```r
install.packages(c(
  "shiny", "shinydashboard", "shinyjs", "waiter",
  "dplyr", "tidyr", "readr", "stringr", "lubridate",
  "ggplot2", "plotly", "DT", "leaflet", "visNetwork",
  "sf", "osrm", "igraph",
  "randomForest", "caret", "FNN", "rpart"
))

shiny::runApp()
```

**Clé API** — créer un fichier `.env` (non versionné) à la racine :

```
TOMTOM_KEY=votre_cle_ici
```

---

## Structure du dépôt

```
mobilite_abidjan/
├── global.R              # Packages, données, composants UI
├── ui.R                  # Interface — 6 onglets
├── server.R              # Logique réactive
├── rapport.qmd           # Rapport Quarto
├── scripts/              # Collecte, nettoyage, redressement, ML
├── data/
│   ├── raw/              # Données brutes (non versionnées)
│   └── processed/        # Données nettoyées
├── models/               # Modèles entraînés (.rds)
├── www/style.css         # Thème orange / vert
└── outputs/              # Graphiques du rapport
```

---

## Limites

Le redressement des données TomTom repose sur des coefficients externes (OFT/AMUGA) : il restitue un profil réaliste, mais reste une reconstruction, pas une mesure directe. L'analyse de réseau porte sur les 13 communes majeures et simplifie nécessairement la voirie réelle. Enfin, le modèle prédit la vitesse sur les axes observés et ne s'extrapole pas tel quel à des axes absents de l'échantillon.

**Prolongements** — intégrer des données de comptage terrain pour valider le redressement, et simuler l'effet d'un contournement du Plateau sur la modularité du réseau.

---

## Équipe

Projet réalisé à l'UFHB Abidjan — Master MAS, sous l'encadrement du **Dr. Laurent Rouvière** (Université Rennes 2).

**Kouadio Ryu Emmanuel Marie · CAMARA Massaram · LOGBO Axelle**

| Membre | Contributions |
|---|---|
| **CAMARA Massaram** | Architecture Shiny (`ui.R`, `server.R`, `global.R`), interface des 6 onglets, cartographie leaflet/osrm, thème visuel, déploiement sur shinyapps.io |
| **LOGBO Axelle** | Collecte multi-sources (TomTom, GTFS, OSM, INS), redressement des flux TomTom, analyse de réseau (igraph), modélisation Random Forest |
| **Kouadio Ryu Emmanuel Marie** | Rédaction du rapport Quarto — problématique urbaine, restitution des résultats, mise en forme |

**Portée** — mobilité urbaine à Abidjan et outil d'aide à la décision pour les décideurs publics.
