# ==============================================================================
# OBSERVATOIRE DE LA MOBILITÉ — GRAND ABIDJAN
# ui.R — Interface utilisateur — 6 onglets
# Personne 2 — Shiny + UI
# ==============================================================================

ui <- dashboardPage(
  skin = "black",

  # ----------------------------------------------------------------------------
  # HEADER (top bar)
  # ----------------------------------------------------------------------------
  dashboardHeader(
    title = tags$div(
      style = "display:flex;align-items:center;gap:8px;",
      tags$div(
        style = "width:30px;height:30px;background:#F47920;border-radius:8px;
                 display:flex;align-items:center;justify-content:center;
                 color:white;font-size:15px;",
        "🚗"
      ),
      tags$div(
        style = "line-height:1.1;",
        tags$div(style = "font-size:13px;font-weight:600;", "Observatoire Mobilité"),
        tags$div(style = "font-size:9px;opacity:0.7;", "Grand Abidjan")
      )
    ),
    titleWidth = 280
  ),

  # ----------------------------------------------------------------------------
  # SIDEBAR (navigation 6 onglets)
  # ----------------------------------------------------------------------------
  dashboardSidebar(
    width = 280,
    sidebarMenu(
      id = "main_tabs",
      menuItem("Accueil",                tabName = "accueil", icon = icon("home")),
      menuItem("Carte & Itinéraires",    tabName = "carte",   icon = icon("map")),
      menuItem("Analyse du Trafic",      tabName = "trafic",  icon = icon("chart-line")),
      menuItem("Réseau des Communes",    tabName = "reseau",  icon = icon("project-diagram")),
      menuItem("Prédictions ML",         tabName = "ml",      icon = icon("robot")),
      menuItem("Explorer les données",   tabName = "donnees", icon = icon("table"))
    ),
    tags$div(
      style = "padding:14px;font-size:9px;color:rgba(255,255,255,0.4);
               position:absolute;bottom:0;",
      paste("v", APP_VERSION),
      tags$br(),
      "INPHB × Abidjan Mobilité Durable"
    )
  ),

  # ----------------------------------------------------------------------------
  # BODY (contenu des onglets)
  # ----------------------------------------------------------------------------
  dashboardBody(
    # Charger CSS personnalisé + activer shinyjs/waiter
    useShinyjs(),
    use_waiter(),
    tags$head(
      tags$link(rel = "stylesheet", type = "text/css", href = "style.css"),
      tags$meta(charset = "UTF-8")
    ),

    tabItems(

      # ===== ONGLET 1 — ACCUEIL ============================================
      tabItem(
        tabName = "accueil",
        story_box(
          text = "<strong>L'histoire que ce tableau de bord raconte :</strong>
                  Abidjan perd chaque jour des millions d'heures dans les
                  embouteillages. Ce tableau de bord analyse les données pour
                  répondre à une question simple — ",
          insight = "où, quand et pourquoi la ville se bloque-t-elle ?"
        ),
        page_header(
          title    = "Vue d'ensemble — Mobilité du Grand Abidjan",
          subtitle = "Données : GTFS DT4A + TomTom + OSM • Mise à jour : Mai 2025"
        ),
        # KPIs (4 cards) — TODO J2 : remplir avec valueBoxOutput
        fluidRow(
          column(3, kpi_card("Communes couvertes", "13",
                             delta = "↑ Grand Abidjan complet",
                             color = COULEURS$orange, icon = "🏙️", delta_dir = "up")),
          column(3, kpi_card("Arrêts de transport", "847",
                             delta = "↑ SOTRA + Gbaka + Woro",
                             color = COULEURS$bleu, icon = "🚏", delta_dir = "up")),
          column(3, kpi_card("Axes en congestion", "—",
                             delta = "à connecter au reactive",
                             color = COULEURS$rouge, icon = "🔴", delta_dir = "dn")),
          column(3, kpi_card("Temps perdu / jour", "2h 20",
                             delta = "↑ vs 45 min en 2015",
                             color = COULEURS$vert, icon = "⏱️", delta_dir = "dn"))
        ),
        # Bloc "à propos" + état temps réel — TODO J2
        fluidRow(
          column(8, box(width = 12, status = "warning",
                        title = "🌍 À propos — Notre mission",
                        tags$p("TODO J2 — Texte de présentation projet + équipe."),
                        actionButton("go_carte", "🗺️ Voir la carte →",
                                     class = "btn-primary"))),
          column(4, box(width = 12, status = "danger",
                        title = "⚡ État actuel — Qui est bloqué ?",
                        uiOutput("etat_temps_reel")))
        )
      ),

      # ===== ONGLET 2 — CARTE ==============================================
      tabItem(
        tabName = "carte",
        story_box(
          text = "<strong>Où se forment les bouchons ?</strong>
                  La carte localise précisément les zones critiques —"
        ),
        page_header(
          "Réseau de transport & congestion en temps réel",
          "Couleur des routes = niveau de congestion • Points = arrêts (cliquez pour les détails)"
        ),
        fluidRow(
          column(3,
            box(width = 12, title = "Filtres", status = "primary",
              checkboxGroupInput("filtre_transport", "Type de transport",
                choices  = c("Bus SOTRA", "Gbaka", "Woro-woro"),
                selected = c("Bus SOTRA", "Gbaka", "Woro-woro")),
              selectInput("filtre_commune", "Commune",
                choices  = c("Toutes" = "all"),
                selected = "all"),
              tags$hr(),
              tags$h5("🧭 Calcul d'itinéraire"),
              textInput("itin_depart",  "Départ",  placeholder = "ex : Plateau"),
              textInput("itin_arrivee", "Arrivée", placeholder = "ex : Yopougon"),
              actionButton("btn_itin", "Calculer", class = "btn-primary"),
              uiOutput("resultat_itin")
            )
          ),
          column(9, leafletOutput("carte_principale", height = 600))
        ),
        interp_box("TODO J3-J4 — Le Pont HKB et l'axe Adjamé-Plateau sont en
                    rouge — ce sont les deux goulots d'étranglement du réseau."),
        lien_suivant(
          "Vous voyez où se forment les bouchons. L'onglet suivant vous explique
           à quelle heure ils apparaissent.",
          "Analyse du Trafic"
        )
      ),

      # ===== ONGLET 3 — TRAFIC =============================================
      tabItem(
        tabName = "trafic",
        story_box(
          text = "<strong>Quand la ville se bloque-t-elle ?</strong>
                  Abidjan suit un rythme prévisible — deux pics quotidiens à 8h et 17h."
        ),
        page_header(
          "Patterns de congestion — Qui bloque, quand et combien ?",
          "Source : TomTom Traffic Flow API • 10 axes • 5 jours de mesures"
        ),
        fluidRow(
          column(6,
            box(width = 12, title = "Vitesse au cours de la journée",
                status = "warning",
                # pickerInput nécessite shinyWidgets → on commence avec selectInput multi
                selectInput("trafic_communes", "Communes (multi)",
                  choices  = NULL, multiple = TRUE),
                plotlyOutput("courbe_journaliere", height = 350))
          ),
          column(6,
            box(width = 12, title = "Heatmap commune × heure",
                status = "danger",
                checkboxGroupInput("trafic_jours", "Jours",
                  choices  = c("Lun", "Mar", "Mer", "Jeu", "Ven", "Sam", "Dim"),
                  selected = c("Lun", "Mar", "Mer", "Jeu", "Ven"),
                  inline   = TRUE),
                plotlyOutput("heatmap_hebdo", height = 350))
          )
        ),
        fluidRow(
          column(12,
            box(width = 12, title = "Top 5 axes les plus congestionnés",
                status = "primary",
                plotlyOutput("barplot_pires", height = 280),
                interp_box(textOutput("insight_trafic", inline = TRUE))
            )
          )
        ),
        lien_suivant(
          "Vous savez maintenant où et quand. L'onglet Réseau vous explique
           pourquoi.", "Réseau des Communes"
        )
      ),

      # ===== ONGLET 4 — RÉSEAU ============================================
      tabItem(
        tabName = "reseau",
        story_box(
          text = "<strong>Quelle commune paralyse tout Abidjan ?</strong>
                  Adjamé n'est pas congestionné par hasard — c'est le nœud central du réseau."
        ),
        page_header(
          "Réseau de flux inter-communes — Qui dépend de qui ?",
          "Taille du nœud = population • Épaisseur de l'arête = volume de flux • Couleur = bassin Louvain"
        ),
        fluidRow(
          column(8,
            box(width = 12, title = "Graphe interactif des communes",
                status = "warning",
                sliderInput("seuil_flux", "Seuil minimum de flux",
                  min = 0, max = 100, value = 10, post = " %"),
                visNetworkOutput("graphe_communes", height = 500))
          ),
          column(4,
            box(width = 12, title = "Métriques clés",
                status = "primary",
                selectInput("metrique_choix", "Trier par",
                  choices = c("Intermédiairité (betweenness)" = "intermediar",
                              "Proximité (closeness)"         = "proximite",
                              "Degré"                         = "degre_total")),
                tableOutput("tableau_metriques")
            ),
            box(width = 12, title = "🔍 Bassins Louvain",
                status = "success",
                verbatimTextOutput("modularite"),
                uiOutput("communautes_resume"))
          )
        ),
        interp_box("TODO J7-J8 — Adjamé (betweenness élevé) est le carrefour
                    par lequel transitent la majorité des flux."),
        lien_suivant("L'onglet Prédictions ML va plus loin — il anticipe.",
                     "Prédictions ML")
      ),

      # ===== ONGLET 5 — ML ================================================
      tabItem(
        tabName = "ml",
        story_box(
          text = "<strong>Peut-on anticiper la congestion ?</strong>
                  Les modèles ML prédisent votre temps de trajet à partir
                  de l'heure et de l'origine."
        ),
        page_header(
          "Prédire et anticiper — La data science au service du citoyen",
          "Protocole train/test 80/20 • set.seed(42) • 4 modèles comparés"
        ),
        fluidRow(
          column(4,
            box(width = 12, title = "🔮 Faire une prédiction",
                status = "warning",
                selectInput("ml_depart",  "Départ",  choices = NULL),
                selectInput("ml_arrivee", "Arrivée", choices = NULL),
                sliderInput("ml_heure", "Heure de départ",
                            min = 0, max = 23, value = 8, step = 1),
                selectInput("ml_modele", "Modèle",
                  choices = c("Random Forest" = "rf",
                              "Régression linéaire" = "lm",
                              "Arbre de décision"   = "rpart",
                              "k-NN" = "knn")),
                actionButton("ml_predire", "🚀 Prédire", class = "btn-primary"),
                uiOutput("resultat_prediction")
            )
          ),
          column(8,
            tabsetPanel(
              tabPanel("Importance variables",
                       plotOutput("importance_vars", height = 400)),
              tabPanel("Comparaison modèles",
                       tableOutput("comparaison_modeles")),
              tabPanel("Clustering communes",
                       plotOutput("clustering_acp", height = 400))
            )
          )
        ),
        interp_box(textOutput("insight_ml", inline = TRUE)),
        lien_suivant("Place à la transparence — voici les données brutes.",
                     "Explorer les données")
      ),

      # ===== ONGLET 6 — DONNÉES ===========================================
      tabItem(
        tabName = "donnees",
        story_box(
          text = "<strong>D'où viennent les chiffres ?</strong>
                  Cet onglet montre les données brutes, filtrables et téléchargeables."
        ),
        page_header(
          "Les données brutes — Source de transparence et reproductibilité",
          "readr::read_csv() → dplyr::left_join() → DT::datatable()"
        ),
        fluidRow(
          column(3, kpi_card("Lignes (filtrées)", textOutput("dt_n_lignes", inline = TRUE),
                             color = COULEURS$orange, icon = "📊", delta_dir = "neutral")),
          column(3, kpi_card("Vitesse moyenne", textOutput("dt_vit_moy", inline = TRUE),
                             color = COULEURS$vert, icon = "🚗", delta_dir = "neutral")),
          column(3, kpi_card("% bloqué", textOutput("dt_pct_bloque", inline = TRUE),
                             color = COULEURS$rouge, icon = "🔴", delta_dir = "neutral")),
          column(3, kpi_card("Axe le pire", textOutput("dt_axe_pire", inline = TRUE),
                             color = COULEURS$jaune, icon = "⚠️", delta_dir = "neutral"))
        ),
        fluidRow(
          column(12,
            box(width = 12, status = "primary",
                fluidRow(
                  column(4, selectInput("dt_dataset", "Dataset",
                    choices = c("flux_enrichi (principal)" = "flux",
                                "communes (Wikipedia)"     = "communes",
                                "GTFS arrêts"               = "stops"))),
                  column(4, selectInput("dt_filtre_commune", "Commune",
                    choices = c("Toutes" = "all"))),
                  column(4, sliderInput("dt_filtre_heure", "Plage horaire",
                    min = 0, max = 23, value = c(0, 23)))
                ),
                downloadButton("dt_export", "⬇ Télécharger CSV (filtré)",
                               class = "btn-success"),
                tags$hr(),
                DTOutput("table_principale"),
                tags$div(style = "font-size:10px;color:#718096;margin-top:10px;",
                         textOutput("source_note"))
            )
          )
        )
      )

    ) # /tabItems
  )   # /dashboardBody
)
