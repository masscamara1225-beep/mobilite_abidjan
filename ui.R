# ==============================================================================
# OBSERVATOIRE DE LA MOBILITÉ — GRAND ABIDJAN
# ui.R — Interface utilisateur — 9 onglets (CDC v7)
# Sara (P2) : Accueil, Carte, Trafic, Exploration, À propos
# Axelle (P1) : Réseau, ML, Données, Recommandations
# ==============================================================================

ui <- dashboardPage(
  skin = "black",
  
  dashboardHeader(
    title = tags$div(class = "brand",
                     tags$span(class = "brand-dot"),
                     tags$span(class = "brand-text",
                               tags$span(class = "brand-main", "Mobilité Abidjan"),
                               tags$span(class = "brand-sub", "Analyse de la mobilité urbaine"))),
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
                menuItem("Recommandations", tabName = "recommandations", icon = icon("lightbulb")),
                menuItem("À propos",        tabName = "apropos",         icon = icon("circle-info"))
    ),
    tags$div(class = "side-footer",
             tags$div("CAMARA Massaram · LOGBO Axelle · KOUADIO Ryu Emmanuel Marie"),
             tags$div("M1 DS&IA · UFHB · 2025-2026"),
             tags$div(style = "margin-top:6px;opacity:0.6;", paste("v", APP_VERSION)))
  ),
  
  dashboardBody(
    useShinyjs(), use_waiter(),
    waiterShowOnLoad(html = tagList(tags$div(class = "boot",
                                             tags$div(class = "boot-dot"), tags$h3("Mobilité Abidjan"),
                                             tags$p("Chargement des données…"))), color = "#0A0A0A"),
    tags$head(
      tags$link(rel = "stylesheet", type = "text/css",
                href = paste0("style.css?v=", as.numeric(Sys.time()))),
      tags$meta(charset = "UTF-8"),
      tags$meta(name = "robots", content = "noindex, nofollow")),
    
    tabItems(
      
      # ==================================================================
      # ONGLET 1 — ACCUEIL (SARA)
      # ==================================================================
      tabItem(tabName = "accueil",
              page_header(title = "Toutes les communes ne sont pas égales face aux bouchons",
                          meta = "Grand Abidjan · 13 communes · Mai 2026 · GTFS · TomTom · OSM"),
              section_subtitle("Abidjan, capitale économique de la Côte d'Ivoire, compte 6,2 millions
           d'habitants. Sa croissance rapide a généré une congestion routière
           qui touche inégalement les communes. Cette étude mesure ces disparités
           à partir de 4 430 mesures collectées sur 20 axes critiques."),
              fluidRow(
                column(3, kpi_card("Communes", "13", hint = "Grand Abidjan", accent = COULEURS$orange)),
                column(3, kpi_card("Mesures", "4 430", hint = "16 jours d'observation", accent = COULEURS$bleu)),
                column(3, kpi_card("Axes bloqués", textOutput("kpi_axes_bloques", inline = TRUE),
                                   hint = "en heure de pointe", accent = COULEURS$rouge)),
                column(3, kpi_card("Vitesse min.", "11 km/h", hint = "à 8h et 17h", accent = COULEURS$vert))),
              fluidRow(column(12, card(title = "Notre constat",
                                       tags$p(class = "constat-intro", "La commune où ça bouchonne le plus n'est pas celle où le plus de personnes en souffrent."),
                                       fluidRow(
                                         column(6, tags$div(class = "constat-box constat-box-orange",
                                                            tags$h4("Adjamé"), tags$p(tags$b("0.60"), " d'indice de congestion"),
                                                            tags$p(class = "muted-small", "341 000 habitants"),
                                                            tags$p("→ Forte congestion, population modérée"))),
                                         column(6, tags$div(class = "constat-box constat-box-red",
                                                            tags$h4("Yopougon"), tags$p(tags$b("0.52"), " d'indice de congestion"),
                                                            tags$p(class = "muted-small", "1 571 065 habitants"),
                                                            tags$p("→ Congestion modérée, mais ", tags$b("1,5 million"), " de personnes impactées chaque jour")))),
                                       tags$p(class = "constat-conclusion", "Cette étude identifie ces disparités à l'aide d'outils
                 statistiques et propose une priorisation des axes à traiter.")))),
              fluidRow(column(12, card(title = "Objectifs",
                                       tags$div(class = "objectif-principal",
                                                tags$p(class = "obj-label", "Objectif principal"),
                                                tags$p(class = "obj-text", "Mesurer et visualiser les disparités de congestion entre
                        les communes du Grand Abidjan, puis proposer une priorisation des axes à traiter.")),
                                       tags$div(class = "objectifs-specifiques",
                                                tags$p(class = "obj-label", "Objectifs spécifiques"),
                                                tags$ul(class = "obj-list",
                                                        tags$li("Cartographier la congestion subie par chaque commune"),
                                                        tags$li("Quantifier statistiquement les écarts (IC 95 %, tests)"),
                                                        tags$li("Identifier les axes routiers prioritaires pour intervention"),
                                                        tags$li("Mettre les données et l'analyse à disposition de manière transparente")))))),
              fluidRow(column(12, tags$h3(class = "section-title", "Parcours guidé"),
                              tags$p(class = "section-subtitle", "Suivez le fil de notre analyse, onglet par onglet."))),
              fluidRow(
                column(4, actionLink("nav_carte", class = "parcours-card",
                                     tags$div(tags$h4("🗺️  Carte"), tags$p("Voir géographiquement où se concentrent les bouchons.")))),
                column(4, actionLink("nav_trafic", class = "parcours-card",
                                     tags$div(tags$h4("📈  Trafic"), tags$p("Comprendre les rythmes horaires : deux pics à 8h et 17h.")))),
                column(4, actionLink("nav_exploration", class = "parcours-card",
                                     tags$div(tags$h4("🔍  Comparer"), tags$p("Tester si l'écart entre deux communes est réel."))))),
              fluidRow(
                column(4, actionLink("nav_reseau", class = "parcours-card",
                                     tags$div(tags$h4("🔗  Réseau"), tags$p("Identifier les communes pivots qui paralysent Abidjan.")))),
                column(4, actionLink("nav_ml", class = "parcours-card",
                                     tags$div(tags$h4("🤖  Prédiction"), tags$p("Anticiper le temps de trajet selon l'heure et la commune.")))),
                column(4, actionLink("nav_reco", class = "parcours-card",
                                     tags$div(tags$h4("💡  Recommandations"), tags$p("Synthèse des actions à prioriser.")))))),
      
      # ==================================================================
      # ONGLET 2 — CARTE (SARA)
      # ==================================================================
      tabItem(tabName = "carte",
              page_header(title = "Carte des disparités", meta = "13 communes du Grand Abidjan"),
              section_subtitle("L'accueil a posé le constat. Ici, les disparités prennent forme sur la carte."),
              sidebarLayout(
                sidebarPanel(width = 3,
                             tags$h4("Filtres", class = "panel-h"),
                             checkboxGroupInput("filtre_transport", "Transport",
                                                choices = c("Bus SOTRA","Gbaka","Woro-woro"), selected = c("Bus SOTRA","Gbaka","Woro-woro")),
                             radioButtons("carte_indice", "Indicateur affiché",
                                          choices = c("Où ça Bouchonne le plus" = "congestion", "Où ça impact le plus de Personne" = "disparite"),
                                          selected = "congestion"),
                             tags$hr(), tags$h4("Itinéraire", class = "panel-h"),
                             selectizeInput("itin_depart", "Départ", choices = NULL,
                                            options = list(create = TRUE, placeholder = "ex : Plateau", onInitialize = I('function() { this.setValue(""); }'))),
                             selectizeInput("itin_arrivee", "Arrivée", choices = NULL,
                                            options = list(create = TRUE, placeholder = "ex : Yopougon", onInitialize = I('function() { this.setValue(""); }'))),
                             actionButton("btn_itin", "Calculer", class = "btn-pri btn-block"),
                             uiOutput("resultat_itin")),
                mainPanel(width = 9,
                          withSpinner(leafletOutput("carte_principale", height = 600), color = COULEURS$orange, type = 6),
                          card(title = "Ce que révèle la carte", uiOutput("carte_lecture"))))),
      
      # ==================================================================
      # ONGLET 3 — TRAFIC (SARA)
      # ==================================================================
      tabItem(tabName = "trafic",
              page_header(title = "Patterns de congestion", meta = "TomTom · 42 axes · 16 jours · IC 95 %"),
              section_subtitle("Abidjan suit un rythme prévisible. Les courbes ci-dessous quantifient ce que chaque Abidjanais vit au quotidien."),
              tabsetPanel(id = "trafic_subtabs", type = "tabs",
                          tabPanel("Courbe journalière", br(),
                                   fluidRow(
                                     column(3, wellPanel(
                                       pickerInput("trafic_communes", "Communes", choices = NULL, multiple = TRUE,
                                                   options = pickerOptions(actionsBox = TRUE, size = 8)),
                                       checkboxGroupInput("trafic_jours", "Jours",
                                                          choices = c("Lun","Mar","Mer","Jeu","Ven","Sam","Dim"),
                                                          selected = c("Lun","Mar","Mer","Jeu","Ven"), inline = TRUE),
                                       radioButtons("trafic_y", "Indicateur",
                                                    choices = c("Vitesse (km/h)" = "vitesse_kmh", "Indice de fluidité" = "indice_cong")))),
                                     column(9, withSpinner(plotlyOutput("courbe_journaliere", height = 420), color = COULEURS$bleu, type = 6))),
                                   fluidRow(column(12, card(title = "Ce que révèlent les courbes", uiOutput("interpretation_courbe"))))),
                          tabPanel("Carte de chaleur", br(),
                                   fluidRow(
                                     column(3, wellPanel(
                                       selectInput("heat_jour", "Jour", choices = c("Tous","Lun","Mar","Mer","Jeu","Ven","Sam","Dim")),
                                       selectInput("heat_aggreg", "Agrégation", choices = c("Moyenne"="mean","Médiane"="median","Maximum"="max")))),
                                     column(9, withSpinner(plotlyOutput("heatmap_hebdo", height = 420), color = COULEURS$orange, type = 6))),
                                   fluidRow(column(12, card(title = "Ce que révèle la carte de chaleur", uiOutput("interpretation_heatmap"))))),
                          tabPanel("Comparateur d'axes", br(),
                                   fluidRow(
                                     column(3, wellPanel(
                                       checkboxGroupInput("comp_axes", "Axes à comparer", choices = NULL),
                                       sliderInput("comp_heure", "Heure", min = 0, max = 23, value = 8, step = 1))),
                                     column(9, withSpinner(plotlyOutput("barplot_pires", height = 420), type = 6))),
                                   fluidRow(column(12, card(title = "Ce que révèle la comparaison", uiOutput("interpretation_comparateur"))))))),
      
      # ==================================================================
      # ONGLET 4 — EXPLORATION (SARA)
      # ==================================================================
      tabItem(tabName = "exploration",
              page_header(title = "Comparer les communes", meta = "Distributions · IC 95 % · Test de Wilcoxon"),
              section_subtitle("La carte et le trafic montrent les disparités. Ici, les tests statistiques confirment qu'elles sont réelles."),
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
              fluidRow(
                column(6, card(title = "Ce que révèlent les boxplots", uiOutput("interpretation_boxplot"))),
                column(6, card(title = "Ce que révèle l'histogramme", uiOutput("interpretation_histo")))),
              fluidRow(column(12, card(title = "Statistiques descriptives",
                                       withSpinner(DTOutput("expl_stats_table"), type = 6)))),
              fluidRow(column(12, card(title = "Test statistique · comparer deux communes",
                                       tags$p(class = "card-desc", "Le test de Wilcoxon indique si l'écart est significatif."),
                                       fluidRow(
                                         column(6, selectInput("test_commune_a", "Commune A", choices = NULL)),
                                         column(6, selectInput("test_commune_b", "Commune B", choices = NULL))),
                                       uiOutput("test_resultat")))),
              note_box("Le test de Wilcoxon compare la distribution complète, pas seulement les moyennes.
                  p-value < 0.05 = écart réel.")),
      
      # ==================================================================
      # ONGLET 5 — RÉSEAU (AXELLE)
      # ==================================================================
      tabItem(tabName = "reseau",
              page_header(title = "Réseau inter-communes", meta = "igraph · betweenness · closeness · Louvain"),
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
                               tags$p("Plus un ", tags$strong("cercle est gros"), ", plus la commune est peuplée."),
                               tags$p("Plus il y a de ", tags$strong("flèches"), " vers un cercle, plus la commune attire de trajets."),
                               tags$p("Les ", tags$strong("couleurs"), " = bassins de mobilité."),
                               tags$p("Cliquez sur une commune pour voir ses connexions.")))),
              fluidRow(column(12, card(title = "Ce que révèle le tableau", uiOutput("interpretation_tableau"))))),
      
      # ==================================================================
      # ONGLET 6a — ML PRÉDICTION (AXELLE)
      # ==================================================================
      tabItem(tabName = "ml_pred",
              page_header(title = "Prédire un temps de trajet", meta = "Random Forest · OSRM · IC 95 %"),
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
      
      # ==================================================================
      # ONGLET 6b — ML PERFORMANCE (AXELLE)
      # ==================================================================
      tabItem(tabName = "ml_eval",
              page_header(title = "Performance des modèles", meta = "Quatre modèles testés pour prédire la congestion"),
              fluidRow(
                column(6, card(title = "Comparaison RMSE / R²", tableOutput("comparaison_modeles"))),
                column(6, card(title = "Importance des variables (Random Forest)",
                               withSpinner(plotOutput("importance_vars", height = 360))))),
              fluidRow(column(12, card(title = "Ce que révèlent les modèles", uiOutput("interpretation_modeles"))))),
      
      # ==================================================================
      # ONGLET 6c — CLUSTERING (AXELLE)
      # ==================================================================
      tabItem(tabName = "ml_clust",
              page_header(title = "Profils de congestion des communes", meta = "k-means · ACP · Indice TomTom"),
              section_subtitle("On sait quand, où et pourquoi. Reste à savoir qui souffre le plus.
                          Le clustering regroupe les 13 communes par niveau de congestion subi."),
              fluidRow(
                column(8, withSpinner(plotOutput("clustering_acp", height = 500))),
                column(4, card(title = "Communes par profil", uiOutput("clustering_interpretation")))),
              fluidRow(column(12, card(title = "Ce que révèlent les profils", uiOutput("clustering_synthese"))))),
      
      # ==================================================================
      # ONGLET 7 — DONNÉES (AXELLE)
      # ==================================================================
      tabItem(tabName = "donnees",
              page_header(title = "Explorer les données", meta = "flux_enrichi · Communes · GTFS · CSV"),
              section_subtitle("Toutes les analyses précédentes reposent sur ces données.
                          Consultez-les, filtrez-les, téléchargez-les."),
              wellPanel(class = "well-min", fluidRow(
                column(4, selectInput("dt_dataset", "Dataset",
                                      choices = c("flux_enrichi"="flux","Communes (Wikipedia)"="communes","Arrêts GTFS"="stops"))),
                column(4, selectInput("dt_filtre_commune", "Commune", choices = c("Toutes" = "all"))),
                column(4, sliderInput("dt_filtre_heure", "Plage horaire", min = 0, max = 23, value = c(0,23)))),
                downloadButton("dt_export", "Télécharger CSV", class = "btn-pri")),
              tabsetPanel(id = "donnees_subtabs", type = "tabs",
                          tabPanel("Données brutes", br(),
                                   fluidRow(
                                     column(3, kpi_card("Lignes", textOutput("dt_n_lignes", inline = TRUE), accent = COULEURS$orange)),
                                     column(3, kpi_card("Vitesse moy.", textOutput("dt_vit_moy", inline = TRUE), accent = COULEURS$vert)),
                                     column(3, kpi_card("% bloqué", textOutput("dt_pct_bloque", inline = TRUE), accent = COULEURS$rouge)),
                                     column(3, kpi_card("Pire axe", textOutput("dt_axe_pire", inline = TRUE), accent = COULEURS$jaune))),
                                   card(DTOutput("table_principale")),
                                   tags$p(class = "source-note", textOutput("source_note", inline = TRUE))),
                          tabPanel("Description & nettoyage", br(),
                                   fluidRow(column(12, card(title = "Description du dataset sélectionné", uiOutput("description_dataset")))),
                                   fluidRow(column(12, card(title = "Processus de nettoyage", uiOutput("nettoyage_dataset"))))))),
      
      # ==================================================================
      # ONGLET 8 — RECOMMANDATIONS (AXELLE)
      # ==================================================================
      tabItem(tabName = "recommandations",
              page_header(title = "Recommandations", meta = "Synthèse pour les décideurs"),
              section_subtitle("Les constats sont posés, les preuves réunies. Place aux solutions."),
              fluidRow(column(12, card(title = "Synthèse des constats", uiOutput("reco_constats")))),
              fluidRow(column(12, card(title = "Recommandations prioritaires", uiOutput("reco_details")))),
              fluidRow(column(12, card(title = "Ce que peut faire chaque acteur",
                                       tags$div(
                                         tags$p(tags$strong("Mairies des 13 communes"), " — Décaler les horaires d'ouverture
                   des services publics et marchés pour étaler les flux."),
                                         tags$p(tags$strong("AGEROUTE / Ministère des Transports"), " — Prioriser les axes
                   critiques (Adjamé–Plateau, Pont HKB) pour les voies réservées aux bus."),
                                         tags$p(tags$strong("SOTRA / Opérateurs de transport"), " — Renforcer les lignes directes
                   entre communes résidentielles et centres d'emploi sans transiter par Plateau."),
                                         tags$p(tags$strong("Citoyens"), " — Consulter les heures de pointe avant de partir.
                   Éviter 7h–9h et 16h–18h réduit le temps de trajet de moitié."),
                                         tags$p(tags$strong("Chercheurs / Urbanistes"), " — Reproduire cette analyse avec des données
                   temps réel pour affiner les recommandations.")))))),
      
      # ==================================================================
      # ONGLET 9 — À PROPOS (SARA)
      # ==================================================================
      tabItem(tabName = "apropos",
              page_header(title = "À propos du projet", meta = "Équipe · Sources · Stack technique"),
              section_subtitle("L'équipe, le cadre académique, les sources de données et la stack technique."),
              fluidRow(column(12, card(title = "Équipe", fluidRow(
                column(4, tags$div(class = "team-card",
                                   tags$div(class = "team-avatar team-avatar-orange", "M"),
                                   tags$h4("CAMARA Massaram"),
                                   tags$p(class = "team-role", "Conception UI/UX, cartographie, exploration statistique"))),
                column(4, tags$div(class = "team-card",
                                   tags$div(class = "team-avatar team-avatar-vert", "A"),
                                   tags$h4("LOGBO Axelle"),
                                   tags$p(class = "team-role", "Collecte données, analyse réseau, ML et recommandations"))),
                column(4, tags$div(class = "team-card",
                                   tags$div(class = "team-avatar team-avatar-bleu", "E"),
                                   tags$h4("KOUADIO Ryu Emmanuel Marie"),
                                   tags$p(class = "team-role", "Rédaction du rapport Quarto")))),
                tags$div(class = "team-encadrant", tags$p("Encadré par ", tags$strong("Dr Laurent Rouvière")))))),
              fluidRow(column(12, card(title = "Le projet", tags$div(class = "projet-meta",
                                                                     tags$div(class = "projet-item", tags$span(class = "projet-label", "Cadre"), tags$span(class = "projet-value", "M1 Data Science & IA · UFHB")),
                                                                     tags$div(class = "projet-item", tags$span(class = "projet-label", "Année"), tags$span(class = "projet-value", "2025-2026")),
                                                                     tags$div(class = "projet-item", tags$span(class = "projet-label", "Durée"), tags$span(class = "projet-value", "1 mois")),
                                                                     tags$div(class = "projet-item", tags$span(class = "projet-label", "Livraison"), tags$span(class = "projet-value", "2 juin 2026"))),
                                       tags$p(class = "projet-objectif", tags$strong("Objectif : "),
                                              "Mettre en pratique collecte, traitement, modélisation et restitution de données dans un contexte réel.")))),
              fluidRow(column(12, card(title = "Sources de données", tags$div(class = "sources-grid",
                                                                              tags$div(class = "source-item", tags$h5("TomTom Traffic Flow API"),
                                                                                       tags$p("Vitesses sur 42 axes, 16 jours."), tags$a(href = "https://developer.tomtom.com/traffic-api/", target = "_blank", "developer.tomtom.com")),
                                                                              tags$div(class = "source-item", tags$h5("GTFS"), tags$p("Réseau transport public : SOTRA, gbakas, woro-woros."),
                                                                                       tags$a(href = "https://gtfs.org/", target = "_blank", "gtfs.org")),
                                                                              tags$div(class = "source-item", tags$h5("OpenStreetMap & OSRM"), tags$p("Géométrie des 13 communes et itinéraires."),
                                                                                       tags$a(href = "https://www.openstreetmap.org/", target = "_blank", "openstreetmap.org")),
                                                                              tags$div(class = "source-item", tags$h5("INS Côte d'Ivoire"), tags$p("Données démographiques (recensement 2021)."),
                                                                                       tags$a(href = "https://www.ins.ci/", target = "_blank", "ins.ci")))))),
              fluidRow(column(12, card(title = "Stack technique & packages",
                                       tags$div(class = "stack-grid",
                                                tags$div(class = "stack-item", tags$h5("Framework"),
                                                         tags$p("shiny, shinydashboard, shinyjs, shinyWidgets, waiter")),
                                                tags$div(class = "stack-item", tags$h5("Visualisation"),
                                                         tags$p("leaflet, plotly, ggplot2, visNetwork, DT, ggrepel")),
                                                tags$div(class = "stack-item", tags$h5("Analyse & ML"),
                                                         tags$p("dplyr, tidyr, readr, randomForest, rpart, class, FactoMineR, factoextra")),
                                                tags$div(class = "stack-item", tags$h5("Spatial & Réseau"),
                                                         tags$p("sf, osrm, igraph, geojsonsf, stringr, purrr"))),
                                       tags$hr(),
                                       tags$p(tags$strong("Installation rapide :"), class = "muted-small"),
                                       tags$pre(style = "font-size: 12px; background: #f5f5f5; padding: 10px; border-radius: 6px;",
                                                'install.packages(c(
  "shiny", "shinydashboard", "shinyjs", "shinyWidgets", "waiter",
  "leaflet", "plotly", "ggplot2", "visNetwork", "DT", "ggrepel",
  "dplyr", "tidyr", "readr", "randomForest", "rpart", "class",
  "FactoMineR", "factoextra", "sf", "osrm", "igraph",
  "geojsonsf", "stringr", "purrr"
))')
              ))))
      
    ) # /tabItems
  )   # /dashboardBody
)