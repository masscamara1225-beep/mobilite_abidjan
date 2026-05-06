# ==============================================================================
# OBSERVATOIRE DE LA MOBILITÉ — GRAND ABIDJAN
# server.R — Logique réactive
# Personne 2 — Shiny + UI
# ==============================================================================

server <- function(input, output, session) {

  # ----------------------------------------------------------------------------
  # MISE À JOUR DES CHOIX DYNAMIQUES (au démarrage)
  # ----------------------------------------------------------------------------
  observe({
    if (nrow(flux_enrichi) > 0) {
      communes_dispo <- sort(unique(flux_enrichi$commune_nom))
      updateSelectInput(session, "filtre_commune",
                        choices = c("Toutes" = "all", communes_dispo))
      updateSelectInput(session, "trafic_communes",
                        choices = communes_dispo,
                        selected = head(communes_dispo, 3))
      updateSelectInput(session, "ml_depart",  choices = communes_dispo)
      updateSelectInput(session, "ml_arrivee", choices = communes_dispo,
                        selected = communes_dispo[2])
      updateSelectInput(session, "dt_filtre_commune",
                        choices = c("Toutes" = "all", communes_dispo))
    }
  })

  # ============================================================================
  # ONGLET 1 — ACCUEIL
  # ============================================================================

  # Bouton "Voir la carte" → navigation
  observeEvent(input$go_carte, {
    updateTabItems(session, "main_tabs", "carte")
  })

  # État temps réel — barres de congestion par commune (TODO J2)
  output$etat_temps_reel <- renderUI({
    if (nrow(flux_enrichi) == 0) {
      return(tags$div(style = "padding:14px;color:#94A3B8;font-style:italic;",
                      "⏳ En attente du dataset flux_enrichi.csv (Personne 1)."))
    }
    # TODO J2 : calculer le niveau actuel par commune et afficher des barres
    tags$div("TODO — barres de congestion par commune")
  })

  # ============================================================================
  # ONGLET 2 — CARTE
  # ============================================================================

  # Carte de base — Abidjan vue d'ensemble (J3 : ajouter OSM + arrêts)
  output$carte_principale <- renderLeaflet({
    leaflet() |>
      addProviderTiles(providers$CartoDB.Positron) |>
      setView(lng = ABIDJAN_LON, lat = ABIDJAN_LAT, zoom = ABIDJAN_ZOOM) |>
      addControl(html = "<strong>Carte d'Abidjan</strong><br>
                          <small>TODO J3 — réseau OSM + arrêts GTFS</small>",
                 position = "topright")
  })

  # Itinéraire (TODO J4 — osrm::osrmRoute)
  observeEvent(input$btn_itin, {
    output$resultat_itin <- renderUI({
      tags$div(style = "padding:8px;background:#F1F4F8;border-radius:6px;
                        margin-top:10px;font-size:11px;",
               "TODO J4 — calcul osrm::osrmRoute() entre ",
               tags$strong(input$itin_depart), " et ",
               tags$strong(input$itin_arrivee))
    })
  })

  # ============================================================================
  # ONGLET 3 — TRAFIC
  # ============================================================================

  output$courbe_journaliere <- renderPlotly({
    validate(need(nrow(flux_enrichi) > 0,
                  "⏳ Dataset pas encore disponible"))
    validate(need(length(input$trafic_communes) > 0,
                  "Sélectionnez au moins une commune"))
    # TODO J5
    plot_ly(type = "scatter", mode = "lines") |>
      layout(title = "TODO J5 — courbe vitesse/heure")
  })

  output$heatmap_hebdo <- renderPlotly({
    validate(need(nrow(flux_enrichi) > 0,
                  "⏳ Dataset pas encore disponible"))
    # TODO J6
    plot_ly() |> layout(title = "TODO J6 — heatmap commune × heure")
  })

  output$barplot_pires <- renderPlotly({
    validate(need(nrow(flux_enrichi) > 0,
                  "⏳ Dataset pas encore disponible"))
    plot_ly() |> layout(title = "TODO J6 — top 5 axes")
  })

  output$insight_trafic <- renderText({
    "TODO J6 — phrase auto-générée selon les filtres (ex : Adjamé atteint sa vitesse min à 08h00)."
  })

  # ============================================================================
  # ONGLET 4 — RÉSEAU
  # ============================================================================

  output$graphe_communes <- renderVisNetwork({
    if (is.null(graphe_communes)) {
      return(visNetwork(
        nodes = data.frame(id = 1, label = "⏳ graphe_communes.rds non livré"),
        edges = data.frame()
      ))
    }
    # TODO J7 — vis_data <- toVisNetworkData(graphe_communes) ...
    visNetwork(
      nodes = data.frame(id = 1, label = "TODO J7"),
      edges = data.frame()
    )
  })

  output$tableau_metriques <- renderTable({
    data.frame(Commune = "TODO J7",
               Valeur  = "—")
  })

  output$modularite <- renderText({
    "TODO J8 — modularité Louvain"
  })

  output$communautes_resume <- renderUI({
    tags$div("TODO J8 — badges par bassin")
  })

  # ============================================================================
  # ONGLET 5 — ML
  # ============================================================================

  observeEvent(input$ml_predire, {
    output$resultat_prediction <- renderUI({
      if (is.null(mod_rf)) {
        return(tags$div(style = "color:#E74C3C;",
                        "⏳ mod_rf_vitesse.rds pas encore livré"))
      }
      # TODO J9 — predict(mod_rf, newdata = ...)
      tags$div(class = "kpi", style = "--a:#F47920;margin-top:14px;",
               tags$div(class = "kl", "TEMPS ESTIMÉ"),
               tags$div(class = "kv", "TODO J9"),
               tags$div(class = "kd", paste("Modèle :", input$ml_modele)))
    })
  })

  output$importance_vars <- renderPlot({
    plot.new()
    title("TODO J9 — varImpPlot(mod_rf)")
  })

  output$comparaison_modeles <- renderTable({
    tryCatch(
      read_csv("outputs/comparaison_modeles.csv", show_col_types = FALSE),
      error = function(e) data.frame(
        Modele = c("LM", "RF", "kNN", "rpart"),
        RMSE   = "TODO J9",
        R2     = "TODO J9"
      )
    )
  })

  output$clustering_acp <- renderPlot({
    plot.new()
    title("TODO J10 — k-means + ACP communes")
  })

  output$insight_ml <- renderText({
    "TODO J9 — Pour ce trajet, partez avant 6h30..."
  })

  # ============================================================================
  # ONGLET 6 — DONNÉES
  # ============================================================================

  # Dataset filtré (réactif)
  donnees_filtrees <- reactive({
    df <- switch(input$dt_dataset,
                 "flux"     = flux_enrichi,
                 "communes" = communes_wiki,
                 "stops"    = tibble(message = "TODO — charger stops.txt"))

    if (input$dt_dataset == "flux" && nrow(df) > 0) {
      if (input$dt_filtre_commune != "all") {
        df <- df |> filter(commune_nom == input$dt_filtre_commune)
      }
      df <- df |> filter(heure >= input$dt_filtre_heure[1],
                         heure <= input$dt_filtre_heure[2])
    }
    df
  })

  output$table_principale <- renderDT({
    datatable(donnees_filtrees(),
              options = list(pageLength = 10, scrollX = TRUE),
              rownames = FALSE,
              filter = "top")
  })

  output$dt_n_lignes  <- renderText({ format(nrow(donnees_filtrees()), big.mark = " ") })
  output$dt_vit_moy   <- renderText({
    df <- donnees_filtrees()
    if (!"vitesse_kmh" %in% names(df) || nrow(df) == 0) return("—")
    paste0(round(mean(df$vitesse_kmh, na.rm = TRUE), 1), " km/h")
  })
  output$dt_pct_bloque <- renderText({
    df <- donnees_filtrees()
    if (!"niveau_cong" %in% names(df) || nrow(df) == 0) return("—")
    paste0(round(mean(df$niveau_cong == "Bloqué", na.rm = TRUE) * 100, 1), " %")
  })
  output$dt_axe_pire <- renderText({
    df <- donnees_filtrees()
    if (!"axe_id" %in% names(df) || nrow(df) == 0) return("—")
    df |>
      group_by(axe_id) |>
      summarise(v = mean(vitesse_kmh, na.rm = TRUE), .groups = "drop") |>
      arrange(v) |>
      slice(1) |>
      pull(axe_id)
  })

  # Export CSV
  output$dt_export <- downloadHandler(
    filename = function() {
      paste0("mobilite_abidjan_", input$dt_dataset, "_",
             Sys.Date(), ".csv")
    },
    content = function(file) {
      write_csv(donnees_filtrees(), file)
    }
  )

  output$source_note <- renderText({
    paste0("Source : TomTom Traffic Flow API + GTFS DT4A + Wikipedia • ",
           "Collecte : Mai 2025 • Licence : ODbL")
  })

}
