# ==============================================================================
# OBSERVATOIRE DE LA MOBILITÉ — GRAND ABIDJAN
# ui.R — Interface utilisateur — 8 onglets (CDC v7)
# ==============================================================================

ui <- dashboardPage(
  skin = "black",
  
  dashboardHeader(
    title = tags$div(class = "brand",
                     tags$span(class = "brand-dot"),
                     tags$span(class = "brand-text",
                               tags$span(class = "brand-main", "Mobilité Abidjan"),
                               tags$span(class = "brand-sub", "Observatoire des données"))),
    titleWidth = 280),
  
  dashboardSidebar(
    width = 280,
    sidebarMenu(id = "main_tabs",
                tags$div(class = "side-section", "Diagnostic"),
                menuItem("Accueil",     tabName = "accueil",     icon = icon("house")),
                menuItem("Carte",       tabName = "carte",       icon = icon("map")),
                menuItem("Trafic",      tabName = "trafic",      icon = icon("chart-line")),
                menuItem("Exploration", tabName = "exploration", icon = icon("magnifying-glass-chart")),
                tags$div(class = "side-section", "Analyse avancée"),
                menuItem("Réseau",      tabName = "reseau", icon = icon("diagram-project")),
                menuItem("Prédiction ML", icon = icon("robot"), startExpanded = FALSE,
                         menuSubItem("Prédire un trajet",   tabName = "ml_pred"),
                         menuSubItem("Performance modèles", tabName = "ml_eval"),
                         menuSubItem("Profils de communes", tabName = "ml_clust")),
                menuItem("Données",         tabName = "donnees",         icon = icon("table")),
                menuItem("Recommandations", tabName = "recommandations", icon = icon("lightbulb"))
    ),
    tags$div(class = "side-footer",
             tags$div("CAMARA Massaram · LOGBO Axelle"),
             tags$div("M1 DS IA · UFHB · 2025"),
             tags$div(style = "margin-top:6px;opacity:0.6;", paste("v", APP_VERSION)))
  ),
  
  dashboardBody(
    useShinyjs(), use_waiter(),
    waiterShowOnLoad(html = tagList(tags$div(class = "boot",
                                             tags$div(class = "boot-dot"), tags$h3("Mobilité Abidjan"),
                                             tags$p("Chargement des données…"))), color = "#0A0A0A"),
    tags$head(
      tags$link(rel = "stylesheet", type = "text/css", href = "style.css"),
      tags$meta(charset = "UTF-8"),
      tags$meta(name = "robots", content = "noindex, nofollow")),
    
    tabItems(
      
      # ==================== ACCUEIL ====================
      tabItem(tabName = "accueil",
              page_header(title = "Vue d'ensemble",
                          meta = "Données du Grand Abidjan · Mai 2025 · GTFS · TomTom · OSM"),
              section_subtitle("Abidjan perd en moyenne 2 h 20 par jour dans les transports.
                          Cet observatoire révèle où, quand et pourquoi la ville se bloque."),
              fluidRow(
                column(3, kpi_card("Communes", "13", hint = "Grand Abidjan", accent = COULEURS$orange)),
                column(3, kpi_card("Arrêts", "847", hint = "SOTRA · Gbaka · Woro", accent = COULEURS$bleu)),
                column(3, kpi_card("Axes bloqués", textOutput("kpi_axes_bloques", inline = TRUE),
                                   hint = "en heure de pointe", accent = COULEURS$rouge)),
                column(3, kpi_card("Temps perdu", "2 h 20", hint = "vs 45 min en 2015", accent = COULEURS$vert))),
              fluidRow(
                column(7, card(title = "À propos du projet",
                               tags$p("Cet observatoire a été développé par ", tags$strong("CAMARA Massaram"), " et ",
                                      tags$strong("LOGBO Axelle"), " (M1 Data Science et IA · UFHB) sous la direction du ",
                                      tags$strong("Dr Laurent Rouvière"), "."),
                               tags$p("Face à la congestion, toutes les communes ne sont pas égales.
                   Cette étude identifie les disparités et propose une priorisation des axes à traiter."),
                               tags$div(class = "btn-row",
                                        actionButton("go_carte", "Voir la carte", class = "btn-pri"),
                                        actionButton("go_rapport", "Lire le rapport", class = "btn-sec")))),
                column(5, card(title = "Niveau actuel par commune", uiOutput("etat_temps_reel")))),
              note_box("4 sources croisées : TomTom Traffic (42 axes, 16 jours),
                  GTFS (847 arrêts), OpenStreetMap et Wikipedia (population 2021).")),
      
      # ==================== CARTE ====================
      tabItem(tabName = "carte",
              page_header(title = "Carte du réseau", meta = "Couleur des routes = niveau de congestion"),
              section_subtitle("Les chiffres deviennent géographie. Voyez où se forment les bouchons."),
              sidebarLayout(
                sidebarPanel(width = 3,
                             tags$h4("Filtres", class = "panel-h"),
                             checkboxGroupInput("filtre_transport", "Transport",
                                                choices = c("Bus SOTRA","Gbaka","Woro-woro"), selected = c("Bus SOTRA","Gbaka","Woro-woro")),
                             selectInput("filtre_commune", "Commune", choices = c("Toutes" = "all")),
                             tags$hr(), tags$h4("Itinéraire", class = "panel-h"),
                             textInput("itin_depart", "Départ", placeholder = "ex : Plateau"),
                             textInput("itin_arrivee", "Arrivée", placeholder = "ex : Yopougon"),
                             actionButton("btn_itin", "Calculer", class = "btn-pri btn-block"),
                             uiOutput("resultat_itin")),
                mainPanel(width = 9,
                          withSpinner(leafletOutput("carte_principale", height = 600), color = COULEURS$orange, type = 6),
                          note_box("Le Pont HKB et l'axe Adjamé–Plateau concentrent l'essentiel de la congestion.")))),
      
      # ==================== TRAFIC ====================
      tabItem(tabName = "trafic",
              page_header(title = "Patterns de congestion", meta = "TomTom · 42 axes · 16 jours · IC 95 %"),
              section_subtitle("Deux pics quotidiens à 8 h et 17 h. La bande colorée = IC 95 %."),
              tabsetPanel(id = "trafic_subtabs", type = "tabs",
                          tabPanel("Courbe journalière", br(), fluidRow(
                            column(3, wellPanel(
                              pickerInput("trafic_communes", "Communes", choices = NULL, multiple = TRUE,
                                          options = pickerOptions(actionsBox = TRUE, size = 8)),
                              checkboxGroupInput("trafic_jours", "Jours",
                                                 choices = c("Lun","Mar","Mer","Jeu","Ven","Sam","Dim"),
                                                 selected = c("Lun","Mar","Mer","Jeu","Ven"), inline = TRUE),
                              radioButtons("trafic_y", "Indicateur",
                                           choices = c("Vitesse (km/h)" = "vitesse_kmh", "Indice de congestion" = "indice_cong")))),
                            column(9, withSpinner(plotlyOutput("courbe_journaliere", height = 420), color = COULEURS$bleu, type = 6),
                                   note_box(textOutput("insight_courbe", inline = TRUE))))),
                          tabPanel("Carte de chaleur", br(), fluidRow(
                            column(3, wellPanel(
                              selectInput("heat_jour", "Jour", choices = c("Tous","Lun","Mar","Mer","Jeu","Ven","Sam","Dim")),
                              selectInput("heat_aggreg", "Agrégation", choices = c("Moyenne"="mean","Médiane"="median","Maximum"="max")))),
                            column(9, withSpinner(plotlyOutput("heatmap_hebdo", height = 420), color = COULEURS$orange, type = 6),
                                   note_box("Zones rouges = congestion forte. Treichville et Plateau sont les plus touchés à 8h et 17h.")))),
                          tabPanel("Comparateur d'axes", br(), fluidRow(
                            column(3, wellPanel(
                              checkboxGroupInput("comp_axes", "Axes à comparer", choices = NULL),
                              sliderInput("comp_heure", "Heure", min = 0, max = 23, value = 8, step = 1))),
                            column(9, withSpinner(plotlyOutput("barplot_pires", height = 420), type = 6),
                                   note_box("Les axes les plus courts ne sont pas les plus rapides.")))))),
      
      # ==================== EXPLORATION ====================
      tabItem(tabName = "exploration",
              page_header(title = "Exploration statistique", meta = "IC 95 % par commune · Distributions"),
              section_subtitle("Les intervalles de confiance révèlent la fiabilité des temps de trajet."),
              wellPanel(class = "well-min", fluidRow(
                column(3, selectInput("explo_commune", "Commune", choices = NULL)),
                column(3, selectInput("explo_niveau", "Niveau",
                                      choices = c("Tous","Fluide","Modéré","Congestionné","Bloqué"))),
                column(3, sliderInput("explo_bins", "Classes", min = 10, max = 60, value = 25, step = 5)),
                column(3, checkboxInput("explo_outliers", "Outliers", value = TRUE)))),
              fluidRow(
                column(6, card(title = "Distribution des vitesses",
                               withSpinner(plotlyOutput("expl_boxplot", height = 380), type = 6))),
                column(6, card(title = "Histogramme + IC 95 %",
                               withSpinner(plotlyOutput("expl_histo", height = 380), type = 6),
                               tags$p(class = "ic-line", "IC 95 % : ", tags$strong(textOutput("expl_ic_text", inline = TRUE)))))),
              fluidRow(column(12, card(title = "Statistiques descriptives",
                                       withSpinner(DTOutput("expl_stats_table"), type = 6)))),
              note_box("IC large = trajet imprévisible (Adjamé, Abobo). IC étroit = circulation stable (Marcory, Songon).")),
      
      # ==================== RÉSEAU ====================
      tabItem(tabName = "reseau",
              page_header(title = "Réseau inter-communes"),
              section_subtitle("Les onglets précédents montrent quand et où la congestion frappe.
                  Ici, on comprend pourquoi : la structure même du réseau routier."),
              fluidRow(
                column(3, card(title = "Paramètres",
                               sliderInput("seuil_flux", "Seuil minimum de flux", min = 0, max = 100, value = 0, post = " obs"),
                               selectInput("metrique_choix", "Colorier par",
                                           choices = c("Communauté Louvain" = "groupe", "Degré" = "degre")),
                               tags$hr(), tags$p(class = "muted-small", textOutput("modularite")),
                               uiOutput("communautes_resume"))),
                column(9, card(withSpinner(visNetworkOutput("graphe_communes_vis", height = 480),
                                           color = COULEURS$vert, type = 6)))),
              fluidRow(column(12, card(title = "Ce que révèle le réseau", uiOutput("interpretation_reseau")))),
              fluidRow(
                column(7, card(title = "Métriques par commune", DTOutput("tableau_metriques"))),
                column(5, card(title = "Comment lire le graphe ?",
                               tags$p("Plus un ", tags$strong("cercle est gros"),
                                      ", plus la commune est peuplée."),
                               tags$p("Plus il y a de ", tags$strong("flèches"),
                                      " vers un cercle, plus la commune attire de trajets."),
                               tags$p("Les ", tags$strong("couleurs"), " = bassins de mobilité.
         Communes de même couleur = même zone de déplacements."),
                               tags$p("Cliquez sur une commune pour voir uniquement ses connexions.")))),
              fluidRow(column(12, card(title = "Ce que révèle le tableau", uiOutput("interpretation_tableau"))))),
      
      # ==================== ML PRÉDICTION ====================
      tabItem(tabName = "ml_pred",
              page_header(title = "Prédire un temps de trajet", meta = "Random Forest "),
              section_subtitle("Le réseau est sur-centralisé, les heures de pointe identifiées.
                  Peut-on prédire la vitesse sur un trajet donné ? Testez par vous-même."),
              fluidRow(
                column(4, card(title = "Paramètres du trajet",
                               selectInput("ml_commune_dep", "Commune de départ", choices = NULL),
                               selectInput("ml_axe_dep", "Axe de départ", choices = NULL),
                               selectInput("ml_commune_arr", "Commune d'arrivée", choices = NULL),
                               selectInput("ml_axe_arr", "Axe d'arrivée", choices = NULL),
                               dateInput("ml_date", "Date", value = Sys.Date(), min = Sys.Date(),
                                         format = "dd/mm/yyyy", language = "fr"),
                               sliderInput("ml_heure", "Heure", min = 0, max = 23, value = 8, step = 1),
                               actionButton("ml_predire", "Prédire", class = "btn-pri btn-block"))),
                column(8,
                       conditionalPanel(condition = "input.ml_predire > 0", uiOutput("resultat_prediction")),
                       conditionalPanel(condition = "input.ml_predire == 0",
                                        tags$div(class = "empty-state", tags$p("Sélectionnez votre trajet et cliquez sur Prédire."))))),
              fluidRow(column(12, card(title = "Ce que révèle la prédiction", uiOutput("interpretation_prediction"))))),
      
      # ==================== ML PERFORMANCE ====================
      tabItem(tabName = "ml_eval",
              section_subtitle("On sait quand, où et pourquoi. Reste à savoir qui souffre le plus.
                  Le clustering regroupe les 13 communes par niveau de congestion subi."),
              
              fluidRow(
                column(6, card(title = "Comparaison RMSE / R²", tableOutput("comparaison_modeles"))),
                column(6, card(title = "Importance des variables (Random Forest)",
                               withSpinner(plotOutput("importance_vars", height = 360))))),
              fluidRow(column(12, card(title = "Ce que révèlent les modèles", uiOutput("interpretation_modeles"))))),
      
      # ==================== CLUSTERING ====================
      tabItem(tabName = "ml_clust",
              page_header(title = "Profils de congestion des communes", meta = "k-means · ACP · Indice TomTom"),
              section_subtitle("On sait quand, où et pourquoi. Reste à savoir qui souffre le plus.
                  Le clustering regroupe les 13 communes par niveau de congestion subi."),
              fluidRow(
                column(8, withSpinner(plotOutput("clustering_acp", height = 500))),
                column(4, card(title = "Communes par profil", uiOutput("clustering_interpretation")))),
              fluidRow(column(12, card(title = "Ce que révèlent les profils", uiOutput("clustering_synthese"))))),
      
      # ==================== DONNÉES ====================
      tabItem(tabName = "donnees",
              page_header(title = "Explorer les données",
                          meta = "flux_enrichi · Communes · Arrêts GTFS"),
              section_subtitle("Toutes les analyses précédentes reposent sur ces données.
                  Consultez-les, filtrez-les, téléchargez-les."),
              
              wellPanel(class = "well-min", fluidRow(
                column(4, selectInput("dt_dataset", "Dataset",
                                      choices = c("flux_enrichi" = "flux", "Communes (Wikipedia)" = "communes",
                                                  "Arrêts GTFS" = "stops"))),
                column(4, selectInput("dt_filtre_commune", "Commune", choices = c("Toutes" = "all"))),
                column(4, sliderInput("dt_filtre_heure", "Plage horaire", min = 0, max = 23, value = c(0,23)))
              )),
              
              tabsetPanel(id = "donnees_subtabs", type = "tabs",
                          tabPanel("Données brutes", br(),
                                   fluidRow(
                                     column(3, kpi_card("Lignes", textOutput("dt_n_lignes", inline = TRUE), accent = COULEURS$orange)),
                                     column(3, kpi_card("Vitesse moy.", textOutput("dt_vit_moy", inline = TRUE), accent = COULEURS$vert)),
                                     column(3, kpi_card("% bloqué", textOutput("dt_pct_bloque", inline = TRUE), accent = COULEURS$rouge)),
                                     column(3, kpi_card("Pire axe", textOutput("dt_axe_pire", inline = TRUE), accent = COULEURS$jaune))),
                                   card(DTOutput("table_principale")),
                                   downloadButton("dt_export", "Télécharger CSV", class = "btn-pri"),
                                   tags$p(class = "source-note", textOutput("source_note", inline = TRUE))
                          ),
                          tabPanel("Description & nettoyage", br(),
                                   fluidRow(
                                     column(12, card(title = "Description du dataset sélectionné",
                                                     uiOutput("description_dataset")))),
                                   fluidRow(
                                     column(12, card(title = "Processus de nettoyage",
                                                     uiOutput("nettoyage_dataset"))))
                          )
              )
      ),
      
      # ==================== RECOMMANDATIONS ====================
      tabItem(tabName = "recommandations",
              page_header(title = "Recommandations",
                          meta = "Synthèse pour les décideurs · Basée sur les résultats des analyses"),
              section_subtitle("Les constats sont posés, les preuves réunies. Place aux solutions."),
              
              fluidRow(
                column(12, card(title = "Synthèse des constats",
                                uiOutput("reco_constats")))
              ),
              
              fluidRow(
                column(12, card(title = "Recommandations prioritaires",
                                uiOutput("reco_details")))
              ),
              
              fluidRow(
                column(12, card(title = "Ce que peut faire chaque acteur",
                                tags$div(
                                  tags$p(tags$strong("Mairies des 13 communes"), " — Décaler les horaires d'ouverture
               des services publics et marchés pour étaler les flux sur la matinée."),
                                  tags$p(tags$strong("AGEROUTE / Ministère des Transports"), " — Prioriser les axes
               identifiés comme critiques (Adjamé–Plateau, Pont HKB) pour les voies réservées aux bus."),
                                  tags$p(tags$strong("SOTRA / Opérateurs de transport"), " — Renforcer les lignes directes
               entre communes résidentielles et centres d'emploi sans transiter par Plateau."),
                                  tags$p(tags$strong("Citoyens"), " — Consulter les heures de pointe avant de partir.
               Éviter 7h–9h et 16h–18h réduit le temps de trajet de moitié.")
                                )
                ))
              )
      )
    ) # /tabItems
  )   # /dashboardBody
)