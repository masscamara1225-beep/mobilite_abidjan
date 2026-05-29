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
                tags$span(class = "brand-sub", "Analyse de la mobilité urbaine"))
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
      tags$div("CAMARA Massaram · LOGBO Axelle· KOUADIO Ryu Emmanuel Marie"),
      tags$div("M1 DS&IA · UFHBMI · 2025-2026"),
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
      tags$link(rel = "stylesheet", type = "text/css",
                href = paste0("style.css?v=", as.numeric(Sys.time()))),
      tags$meta(charset = "UTF-8")
    ),

    tabItems(

      # ====================================================================
      # ONGLET 1 — ACCUEIL (P2)
      # ====================================================================
      
      tabItem(
        tabName = "accueil",
        page_header(
          title = "Toutes les communes ne sont pas égales face aux bouchons",
          meta  = "Grand Abidjan · 13 communes · Mai 2026 · GTFS · TomTom · OSM"
        ),
        section_subtitle(
          "Abidjan, capitale économique de la Côte d'Ivoire, compte 6,2 millions
           d'habitants. Sa croissance rapide a généré une congestion routière
           qui touche inégalement les communes. Cette étude mesure ces disparités
           à partir de 4 430 mesures collectées sur 20 axes critiques."
        ),
        
        # KPIs (4 cards minimalistes)
        fluidRow(
          column(3, kpi_card("Communes",   "13",
                             hint = "Grand Abidjan", accent = COULEURS$orange)),
          column(3, kpi_card("Mesures",    "4 430",
                             hint = "16 jours d'observation", accent = COULEURS$bleu)),
          column(3, kpi_card("Axes bloqués",
                             textOutput("kpi_axes_bloques", inline = TRUE),
                             hint = "en heure de pointe", accent = COULEURS$rouge)),
          column(3, kpi_card("Vitesse min.", "11 km/h",
                             hint = "à 8h et 17h", accent = COULEURS$vert))
        ),
        
        # Notre constat (problématique)
        fluidRow(
          column(12,
                 card(
                   title = "Notre constat",
                   tags$p(class = "constat-intro",
                          "La commune où ça bouchonne le plus n'est pas celle où le plus
                 de personnes en souffrent."),
                   fluidRow(
                     column(6,
                            tags$div(class = "constat-box constat-box-orange",
                                     tags$h4("Adjamé"),
                                     tags$p(tags$b("0.60"), " d'indice de congestion"),
                                     tags$p(class = "muted-small", "341 000 habitants"),
                                     tags$p("→ Forte congestion, population modérée")
                            )
                     ),
                     column(6,
                            tags$div(class = "constat-box constat-box-red",
                                     tags$h4("Yopougon"),
                                     tags$p(tags$b("0.52"), " d'indice de congestion"),
                                     tags$p(class = "muted-small", "1 571 065 habitants"),
                                     tags$p("→ Congestion modérée, mais ", tags$b("1,5 million"),
                                            " de personnes impactées chaque jour")
                            )
                     )
                   ),
                   tags$p(class = "constat-conclusion",
                          "Cette étude identifie ces disparités à l'aide d'outils
                 statistiques et propose une priorisation des axes à traiter.")
                 )
          )
        ),
        
        # Objectifs de l'étude
        fluidRow(
          column(12,
                 card(
                   title = "Objectifs",
                   tags$div(class = "objectif-principal",
                            tags$p(class = "obj-label", "Objectif principal"),
                            tags$p(class = "obj-text",
                                   "Mesurer et visualiser les disparités de congestion entre
                        les communes du Grand Abidjan, puis proposer une
                        priorisation des axes à traiter.")
                   ),
                   tags$div(class = "objectifs-specifiques",
                            tags$p(class = "obj-label", "Objectifs spécifiques"),
                            tags$ul(class = "obj-list",
                                    tags$li("Cartographier la congestion subie par chaque commune"),
                                    tags$li("Quantifier statistiquement les écarts (IC 95 %, tests)"),
                                    tags$li("Identifier les axes routiers prioritaires pour intervention"),
                                    tags$li("Mettre les données et l'analyse à disposition de manière transparente")
                            )
                   )
                 )
          )
        ),
        
        # Parcours guidé
        fluidRow(
          column(12,
                 tags$h3(class = "section-title", "Parcours guidé"),
                 tags$p(class = "section-subtitle",
                        "Suivez le fil de notre analyse, onglet par onglet.")
          )
        ),
        fluidRow(
          column(4,
                 actionLink("nav_carte", class = "parcours-card",
                            tags$div(
                              tags$h4("🗺️  Carte"),
                              tags$p("Voir géographiquement où se concentrent les bouchons
                        et l'impact humain.")
                            )
                 )
          ),
          column(4,
                 actionLink("nav_trafic", class = "parcours-card",
                            tags$div(
                              tags$h4("📈  Trafic"),
                              tags$p("Comprendre les rythmes horaires : deux pics quotidiens
                        à 8h et 17h.")
                            )
                 )
          ),
          column(4,
                 actionLink("nav_exploration", class = "parcours-card",
                            tags$div(
                              tags$h4("🔍  Comparer"),
                              tags$p("Tester statistiquement si l'écart entre deux communes
                        est réel.")
                            )
                 )
          )
        ),
        fluidRow(
          column(4,
                 actionLink("nav_reseau", class = "parcours-card",
                            tags$div(
                              tags$h4("🔗  Réseau"),
                              tags$p("Identifier les communes pivots qui paralysent tout
                        Abidjan si saturées.")
                            )
                 )
          ),
          column(4,
                 actionLink("nav_ml", class = "parcours-card",
                            tags$div(
                              tags$h4("🤖  Prédiction"),
                              tags$p("Anticiper le temps de trajet selon l'heure, le jour
                        et la commune.")
                            )
                 )
          ),
          column(4,
                 actionLink("nav_reco", class = "parcours-card",
                            tags$div(
                              tags$h4("💡  Recommandations"),
                              tags$p("Synthèse des actions à prioriser pour réduire les
                        disparités.")
                            )
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
          title = "Carte des  disparités ",
          meta  = "13 communes du Grand Abidjan"
        ),
        section_subtitle(
          "Toutes les communes ne subissent pas les bouchons de la même manière.
           Choisissez à droite ce que vous voulez voir : où ça bouchonne le plus,
           ou bien où les bouchons touchent le plus de monde."
        ),
        
        sidebarLayout(
          sidebarPanel(
            width = 3,
            tags$h4("Filtres", class = "panel-h"),
            checkboxGroupInput("filtre_transport", "Transport",
              choices  = c("Bus SOTRA", "Gbaka", "Woro-woro"),
              selected = c("Bus SOTRA", "Gbaka", "Woro-woro")),
            radioButtons("carte_indice", "Indicateur affiché",
                choices  = c("Où ça Bouchonne le plus" = "congestion",
                                      "Où ça impact le plus de Personne " = "disparite"),
                selected = "congestion"),
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
            uiOutput("carte_lecture")
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
          title = "Comparer les communes",
          meta  = "Distributions · IC 95 % · Test statistique de Wilcoxon"
        ),
        section_subtitle(
          "Au-delà des moyennes, les distributions et les tests statistiques
           révèlent si l'écart entre deux communes est réel ou peut être
           dû au hasard."
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
        # Test statistique comparatif entre 2 communes
        fluidRow(
          column(12,
                 card(
                   title = "Test statistique · comparer deux communes",
                   tags$p(class = "card-desc",
                          "Sélectionnez deux communes. Le test de Wilcoxon-Mann-Whitney indique
                 si l'écart de vitesses entre les deux est statistiquement significatif
                 ou s'il peut être dû au hasard."),
                   fluidRow(
                     column(6, selectInput("test_commune_a", "Commune A",
                                           choices = NULL, multiple = FALSE)),
                     column(6, selectInput("test_commune_b", "Commune B",
                                           choices = NULL, multiple = FALSE))
                   ),
                   uiOutput("test_resultat")
                 )
          )
        ),
        note_box("Le test de Wilcoxon compare la distribution complète des vitesses,
                  pas seulement leurs moyennes. Une p-value < 0.05 signifie que
                  l'écart observé entre les deux communes est trop fort pour être
                  dû au hasard.")
      ),

      # ====================================================================
      # ONGLET 5 — RÉSEAU (P1)
      # ====================================================================
      tabItem(
        tabName = "reseau",
        page_header(
          title = "Patterns de congestion",
          meta  = "TomTom Traffic Flow · 20 axes · 16 jours · IC 95 % visualisés"
        ),
        section_subtitle(
          "Abidjan suit un rythme prévisible : deux pics quotidiens à 8 h et 17 h
           où la vitesse moyenne chute à 11 km/h. La zone colorée autour des
           courbes représente l'intervalle de confiance à 95 %."
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
