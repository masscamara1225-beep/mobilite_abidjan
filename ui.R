# ==============================================================================
# OBSERVATOIRE DE LA MOBILITÉ — GRAND ABIDJAN
# ui.R — Interface utilisateur — 8 onglets (CDC v7)
#
# Structure de la navigation :
#   1. Accueil           (P2)
#   2. Carte             (P2) — sidebarLayout
#   3. Trafic            (P2) — tabsetPanel 3 sous-onglets + IC 95%
#   4. Exploration EDA   (P2) — wellPanel + IC 95%
#   5. Réseau            (P1)
#   6. ML & Prédiction   (P1) — menuSubItem (Prédiction / Évaluation / Clustering)
#   7. Données           (P1)
#   8. Recommandations   (P1)
# ==============================================================================

ui <- dashboardPage(
  skin = "black",

  # ----------------------------------------------------------------------------
  # HEADER
  # ----------------------------------------------------------------------------
  dashboardHeader(
    title = tags$div(
      class = "brand",
      tags$span(class = "brand-dot"),
      tags$span(class = "brand-text",
                tags$span(class = "brand-main", "Mobilité Abidjan"),
                tags$span(class = "brand-sub", "Observatoire des données"))
    ),
    titleWidth = 280
  ),

  # ----------------------------------------------------------------------------
  # SIDEBAR — Navigation 8 onglets
  # ----------------------------------------------------------------------------
  dashboardSidebar(
    width = 280,
    sidebarMenu(
      id = "main_tabs",

      # Section P2 (Massaram)
      tags$div(class = "side-section", "Diagnostic"),
      menuItem("Accueil",       tabName = "accueil",     icon = icon("house")),
      menuItem("Carte",         tabName = "carte",       icon = icon("map")),
      menuItem("Trafic",        tabName = "trafic",      icon = icon("chart-line")),
      menuItem("Exploration",   tabName = "exploration", icon = icon("magnifying-glass-chart")),

      # Section P1 (Axelle)
      tags$div(class = "side-section", "Analyse avancée"),
      menuItem("Réseau",        tabName = "reseau", icon = icon("diagram-project")),
      menuItem("Prédiction ML", icon = icon("robot"), startExpanded = FALSE,
               menuSubItem("Prédire un trajet", tabName = "ml_pred"),
               menuSubItem("Performance modèles", tabName = "ml_eval"),
               menuSubItem("Profils de communes", tabName = "ml_clust")),
      menuItem("Données",       tabName = "donnees",         icon = icon("table")),
      menuItem("Recommandations", tabName = "recommandations", icon = icon("lightbulb"))
    ),

    # Footer sidebar — équipe + version
    tags$div(class = "side-footer",
      tags$div("CAMARA Massaram · LOGBO Axelle"),
      tags$div("M1 DS IA · UFHB · 2025"),
      tags$div(style = "margin-top:6px; opacity:0.6;", paste("v", APP_VERSION))
    )
  ),

  # ----------------------------------------------------------------------------
  # BODY
  # ----------------------------------------------------------------------------
  dashboardBody(
    useShinyjs(),
    use_waiter(),
    waiterShowOnLoad(
      html = tagList(
        tags$div(class = "boot",
           tags$div(class = "boot-dot"),
           tags$h3("Mobilité Abidjan"),
           tags$p("Chargement des données…")
        )
      ),
      color = "#0A0A0A"
    ),
    tags$head(
      tags$link(rel = "stylesheet", type = "text/css", href = "style.css"),
      tags$meta(charset = "UTF-8")
    ),

    tabItems(

      # ====================================================================
      # ONGLET 1 — ACCUEIL (P2)
      # ====================================================================
      tabItem(
        tabName = "accueil",
        page_header(
          title = "Vue d'ensemble",
          meta  = "Données du Grand Abidjan · Mai 2025 · GTFS · TomTom · OSM"
        ),
        section_subtitle(
          "Toutes les communes d'Abidjan ne sont pas égales face à la congestion. 
          Cette plateforme mesure les disparités à partir de 2 040 mesures collectées sur 20 axes critiques."
        ),

        # KPIs (4 cards minimalistes)
        fluidRow(
          column(3, kpi_card("Communes",   "13",
                             hint = "Grand Abidjan", accent = COULEURS$orange)),
          column(3, kpi_card("Arrêts",     "847",
                             hint = "SOTRA · Gbaka · Woro", accent = COULEURS$bleu)),
          column(3, kpi_card("Axes bloqués",
                             textOutput("kpi_axes_bloques", inline = TRUE),
                             hint = "en heure de pointe", accent = COULEURS$rouge)),
          column(3, kpi_card("Vitesse moy.", "33 km/h",
                             hint = "sur 6 jours observés", accent = COULEURS$vert))
        ),

        # Présentation + état actuel
        fluidRow(
          column(7,
            card(
              title = "À propos du projet",
              tags$p("Cet observatoire a été développé par ",
                     tags$strong("CAMARA Massaram"), " et ",
                     tags$strong("LOGBO Axelle"),
                     " (M1 Data Science et IA · UFHB) pour l'ONG ",
                     tags$strong("Abidjan Mobilité Durable"), "."),
              tags$p("Il croise trois sources de données — réseau GTFS des bus
                     et woro-woro, vitesses TomTom sur 20 axes, et géométrie
                     OpenStreetMap — pour rendre visible une crise jusqu'ici
                     invisibilisée par l'absence de données ouvertes."),
              tags$div(class = "btn-row",
                actionButton("go_carte",  "Voir la carte",     class = "btn-pri"),
                actionButton("go_rapport","Lire le rapport",   class = "btn-sec")
              )
            )
          ),
          column(5,
            card(
              title = "Niveau actuel par commune",
              uiOutput("etat_temps_reel")
            )
          )
        )
      ),

      # ====================================================================
      # ONGLET 2 — CARTE (P2) — sidebarLayout
      # ====================================================================
      tabItem(
        tabName = "carte",
        page_header(
          title = "Carte du réseau",
          meta  = "Couleur des routes = niveau de congestion · Cliquez sur un arrêt pour le détail"
        ),
        section_subtitle(
          "Les chiffres deviennent géographie. Voyez physiquement où se forment
           les bouchons et calculez votre itinéraire optimal."
        ),
        sidebarLayout(
          sidebarPanel(
            width = 3,
            tags$h4("Filtres", class = "panel-h"),
            checkboxGroupInput("filtre_transport", "Transport",
              choices  = c("Bus SOTRA", "Gbaka", "Woro-woro"),
              selected = c("Bus SOTRA", "Gbaka", "Woro-woro")),
            selectInput("filtre_commune", "Commune",
              choices = c("Toutes" = "all"), selected = "all"),
            tags$hr(),
            tags$h4("Itinéraire", class = "panel-h"),
            selectizeInput(
              "itin_depart", "Départ",
              choices  = NULL,
              options  = list(
                create      = TRUE,
                placeholder = "ex : Plateau",
                onInitialize = I('function() { this.setValue(""); }')
              )
            ),
            selectizeInput(
              "itin_arrivee", "Arrivée",
              choices  = NULL,
              options  = list(
                create      = TRUE,
                placeholder = "ex : Yopougon",
                onInitialize = I('function() { this.setValue(""); }')
              )
            ),
            actionButton("btn_itin", "Calculer", class = "btn-pri btn-block"),
            uiOutput("resultat_itin")
          ),
          mainPanel(
            width = 9,
            withSpinner(
              leafletOutput("carte_principale", height = 600),
              color = COULEURS$orange, type = 6
            ),
            note_box("Le Pont HKB et l'axe Adjamé–Plateau concentrent l'essentiel
                      de la congestion. Contourner ces deux points peut faire
                      gagner 23 min en heure de pointe.")
          )
        )
      ),

      # ====================================================================
      # ONGLET 3 — TRAFIC (P2) — tabsetPanel + IC 95%
      # ====================================================================
      tabItem(
        tabName = "trafic",
        page_header(
          title = "Patterns de congestion",
          meta  = "TomTom Traffic Flow · 20 axes · 6 jours · IC 95 % visualisés"
        ),
        section_subtitle(
          "Abidjan suit un rythme prévisible : deux pics quotidiens à 8 h et 17 h.
           La zone bleue autour des courbes représente l'intervalle de confiance à 95 %."
        ),
        tabsetPanel(
          id = "trafic_subtabs",
          type = "tabs",

          tabPanel(
            "Courbe journalière",
            br(),
            fluidRow(
              column(3,
                wellPanel(
                  pickerInput("trafic_communes", "Communes",
                    choices  = NULL, multiple = TRUE,
                    options  = pickerOptions(actionsBox = TRUE, size = 8)),
                  checkboxGroupInput("trafic_jours", "Jours",
                    choices  = c("Lun","Mar","Mer","Jeu","Ven","Sam","Dim"),
                    selected = c("Lun","Mar","Mer","Jeu","Ven"),
                    inline   = TRUE),
                  radioButtons("trafic_y", "Indicateur",
                    choices  = c("Vitesse (km/h)" = "vitesse_kmh",
                                 "Indice de congestion" = "indice_cong"),
                    selected = "vitesse_kmh")
                )
              ),
              column(9,
                withSpinner(
                  plotlyOutput("courbe_journaliere", height = 420),
                  color = COULEURS$bleu, type = 6
                ),
                note_box(textOutput("insight_courbe", inline = TRUE))
              )
            )
          ),

          tabPanel(
            "Carte de chaleur",
            br(),
            fluidRow(
              column(3,
                wellPanel(
                  selectInput("heat_jour", "Jour",
                    choices = c("Tous","Lun","Mar","Mer","Jeu","Ven","Sam","Dim"),
                    selected = "Tous"),
                  selectInput("heat_aggreg", "Agrégation",
                    choices = c("Moyenne" = "mean",
                                "Médiane" = "median",
                                "Maximum" = "max"),
                    selected = "mean")
                )
              ),
              column(9,
                withSpinner(
                  plotlyOutput("heatmap_hebdo", height = 420),
                  color = COULEURS$orange, type = 6
                )
              )
            )
          ),

          tabPanel(
            "Comparateur d'axes",
            br(),
            fluidRow(
              column(3,
                wellPanel(
                  checkboxGroupInput("comp_axes", "Axes à comparer",
                    choices = NULL),
                  sliderInput("comp_heure", "Heure de référence",
                    min = 0, max = 23, value = 8, step = 1)
                )
              ),
              column(9,
                withSpinner(
                  plotlyOutput("barplot_pires", height = 420),
                  type = 6
                )
              )
            )
          )
        )
      ),

      # ====================================================================
      # ONGLET 4 — EXPLORATION EDA (P2) — wellPanel + IC 95%
      # ====================================================================
      tabItem(
        tabName = "exploration",
        page_header(
          title = "Exploration statistique",
          meta  = "Analyse exploratoire · IC 95 % par commune · Données flux_enrichi.csv"
        ),
        section_subtitle(
          "Au-delà des moyennes, les distributions et intervalles de confiance
           révèlent la fiabilité réelle des temps de trajet."
        ),

        # Filtres groupés dans un wellPanel (cours chap.10)
        wellPanel(
          class = "well-min",
          fluidRow(
            column(3, selectInput("explo_commune", "Commune",
                                  choices = NULL, multiple = FALSE)),
            column(3, selectInput("explo_niveau", "Niveau de congestion",
                                  choices = c("Tous","Fluide","Modéré",
                                              "Congestionné","Bloqué"),
                                  selected = "Tous")),
            column(3, sliderInput("explo_bins", "Classes de l'histogramme",
                                  min = 10, max = 60, value = 25, step = 5)),
            column(3, checkboxInput("explo_outliers", "Afficher les outliers",
                                    value = TRUE))
          )
        ),

        # Boxplot + Distribution + IC
        fluidRow(
          column(6,
            card(
              title = "Distribution des vitesses par commune",
              withSpinner(plotlyOutput("expl_boxplot", height = 380), type = 6)
            )
          ),
          column(6,
            card(
              title = "Histogramme + IC 95 %",
              withSpinner(plotlyOutput("expl_histo", height = 380), type = 6),
              tags$p(class = "ic-line",
                "IC 95 % : ", tags$strong(textOutput("expl_ic_text", inline = TRUE)))
            )
          )
        ),

        fluidRow(
          column(12,
            card(
              title = "Statistiques descriptives par commune (avec IC 95 %)",
              withSpinner(DTOutput("expl_stats_table"), type = 6)
            )
          )
        ),

        note_box("Adjamé et Abobo affichent un IC large (forte variabilité),
                  signe que leurs temps de trajet sont peu prévisibles.
                  Yopougon et Port-Bouët ont un IC étroit : circulation stable.")
      ),

      # ====================================================================
      # ONGLET 5 — RÉSEAU (P1)
      # ====================================================================
      tabItem(
        tabName = "reseau",
        page_header(
          title = "Réseau inter-communes",
          meta  = "igraph · betweenness · closeness · Louvain"
        ),
        section_subtitle(
          "Quelle commune paralyse tout Abidjan si elle est saturée ?
           La théorie des graphes révèle les nœuds critiques."
        ),
        sidebarLayout(
          sidebarPanel(
            width = 3,
            tags$h4("Paramètres", class = "panel-h"),
            sliderInput("seuil_flux", "Seuil minimum de flux",
                        min = 0, max = 100, value = 10, post = " %"),
            selectInput("metrique_choix", "Trier par",
              choices = c("Intermédiarité" = "intermediar",
                          "Proximité"      = "proximite",
                          "Degré"          = "degre_total")),
            tags$hr(),
            verbatimTextOutput("modularite"),
            uiOutput("communautes_resume")
          ),
          mainPanel(
            width = 9,
            withSpinner(
              visNetworkOutput("graphe_communes_vis", height = 500),
              color = COULEURS$vert, type = 6
            ),
            tableOutput("tableau_metriques"),
            note_box("Adjamé concentre la majorité des flux inter-communes.
                      Si Adjamé est saturée, l'ensemble du Grand Abidjan l'est.")
          )
        )
      ),

      # ====================================================================
      # ONGLET 6 — ML : 3 sous-pages (menuSubItem)
      # ====================================================================
      tabItem(
        tabName = "ml_pred",
        page_header(
          title = "Prédire un temps de trajet",
          meta  = "Random Forest · IC 95 % sur la prédiction"
        ),
        fluidRow(
          column(4,
            card(
              title = "Paramètres du trajet",
              selectInput("ml_depart",  "Départ",  choices = NULL),
              selectInput("ml_arrivee", "Arrivée", choices = NULL),
              sliderInput("ml_heure",   "Heure de départ",
                          min = 0, max = 23, value = 8, step = 1),
              selectInput("ml_modele",  "Modèle",
                choices = c("Random Forest"        = "rf",
                            "Régression linéaire"  = "lm",
                            "Arbre de décision"    = "rpart",
                            "k-NN"                 = "knn")),
              actionButton("ml_predire", "Prédire", class = "btn-pri btn-block")
            )
          ),
          column(8,
            conditionalPanel(
              condition = "input.ml_predire > 0",
              uiOutput("resultat_prediction")
            ),
            conditionalPanel(
              condition = "input.ml_predire == 0",
              tags$div(class = "empty-state",
                tags$p("Renseignez un trajet à gauche puis cliquez sur Prédire.")
              )
            )
          )
        )
      ),

      tabItem(
        tabName = "ml_eval",
        page_header(
          title = "Performance des modèles",
          meta  = "RMSE · R² · Importance des variables"
        ),
        fluidRow(
          column(6, card(title = "Comparaison RMSE / R²",
                         tableOutput("comparaison_modeles"))),
          column(6, card(title = "Importance des variables (Random Forest)",
                         withSpinner(plotOutput("importance_vars", height = 360))))
        )
      ),

      tabItem(
        tabName = "ml_clust",
        page_header(
          title = "Profils des communes",
          meta  = "k-means · ACP · 3 clusters"
        ),
        fluidRow(
          column(3, wellPanel(
            sliderInput("kmeans_k", "Nombre de clusters",
                        min = 2, max = 6, value = 3)
          )),
          column(9, withSpinner(plotOutput("clustering_acp", height = 480)))
        )
      ),

      # ====================================================================
      # ONGLET 7 — DONNÉES (P1)
      # ====================================================================
      tabItem(
        tabName = "donnees",
        page_header(
          title = "Explorer les données brutes",
          meta  = "readr · dplyr · DT · Téléchargement CSV"
        ),
        section_subtitle(
          "Transparence et reproductibilité : voici les données qui ont
           produit toutes les analyses précédentes."
        ),

        wellPanel(class = "well-min",
          fluidRow(
            column(4, selectInput("dt_dataset", "Dataset",
              choices = c("flux_enrichi" = "flux",
                          "Communes (Wikipedia)"  = "communes",
                          "Arrêts GTFS"           = "stops"))),
            column(4, selectInput("dt_filtre_commune", "Commune",
              choices = c("Toutes" = "all"))),
            column(4, sliderInput("dt_filtre_heure", "Plage horaire",
              min = 0, max = 23, value = c(0, 23)))
          ),
          downloadButton("dt_export", "Télécharger CSV", class = "btn-pri")
        ),

        fluidRow(
          column(3, kpi_card("Lignes",
            textOutput("dt_n_lignes", inline = TRUE), accent = COULEURS$orange)),
          column(3, kpi_card("Vitesse moy.",
            textOutput("dt_vit_moy", inline = TRUE), accent = COULEURS$vert)),
          column(3, kpi_card("% bloqué",
            textOutput("dt_pct_bloque", inline = TRUE), accent = COULEURS$rouge)),
          column(3, kpi_card("Pire axe",
            textOutput("dt_axe_pire", inline = TRUE), accent = COULEURS$jaune))
        ),

        card(DTOutput("table_principale")),
        tags$p(class = "source-note", textOutput("source_note", inline = TRUE))
      ),

      # ====================================================================
      # ONGLET 8 — RECOMMANDATIONS (P1)
      # ====================================================================
      tabItem(
        tabName = "recommandations",
        page_header(
          title = "Recommandations actionnables",
          meta  = "Synthèse pour les décideurs · ONG Abidjan Mobilité Durable"
        ),
        section_subtitle(
          "Cinq mesures concrètes, classées par impact attendu et faisabilité,
           découlant directement des analyses précédentes."
        ),
        fluidRow(
          column(12, card(
            title = "Tableau des recommandations",
            withSpinner(tableOutput("table_reco"), type = 6)
          ))
        ),
        note_box("Les recommandations s'appuient sur les résultats des onglets
                  Réseau (commune critique), ML (variables explicatives) et
                  Exploration (variabilité des temps).")
      )

    ) # /tabItems
  )   # /dashboardBody
)
