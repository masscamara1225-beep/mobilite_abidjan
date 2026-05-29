# ==============================================================================
# OBSERVATOIRE DE LA MOBILITÉ — GRAND ABIDJAN
# server.R — Logique réactive (CDC v7 — 8 onglets)
# ==============================================================================

server <- function(input, output, session) {
  waiter_hide()
  # ----------------------------------------------------------------------------
  # MISE À JOUR DES CHOIX DYNAMIQUES (au démarrage)
  # ----------------------------------------------------------------------------
  observe({
    if (nrow(flux_enrichi) > 0) {
      communes_dispo <- sort(unique(flux_enrichi$commune))
      axes_dispo <- flux_enrichi |>
        distinct(id_axe, nom_axe) |>
        arrange(id_axe) |>
        (\(d) setNames(d$id_axe, d$nom_axe))()

      updateSelectInput(session, "filtre_commune",
                        choices = c("Toutes" = "all", communes_dispo))
      updatePickerInput(session, "trafic_communes",
                        choices = communes_dispo,
                        selected = head(communes_dispo, 3))
      updateCheckboxGroupInput(session, "comp_axes",
                               choices = axes_dispo,
                               selected = head(axes_dispo, 5))
      updateSelectInput(session, "explo_commune",
                        choices = communes_dispo,
                        selected = communes_dispo[1])
      
      # Test statistique : 2 sélecteurs (par défaut, 2 communes différentes)
      updateSelectInput(session, "test_commune_a",
                        choices  = communes_dispo,
                        selected = communes_dispo[1])
      updateSelectInput(session, "test_commune_b",
                        choices  = communes_dispo,
                        selected = communes_dispo[length(communes_dispo)])
      
      # Itinéraire : on propose les 13 communes officielles (sans "duos")
      communes_officielles <- sort(communes_geo$commune)
      updateSelectizeInput(session, "itin_depart",
                           choices  = communes_officielles,
                           server   = FALSE)
      updateSelectizeInput(session, "itin_arrivee",
                           choices  = communes_officielles,
                           server   = FALSE)
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

  # Boutons navigation
  observeEvent(input$go_carte, {
    updateTabItems(session, "main_tabs", "carte")
  })
  observeEvent(input$go_rapport, {
    showNotification("Rapport Quarto à générer (rapport.qmd)",
                     type = "message")
  })

  # KPI dynamique : axes bloqués actuellement
  output$kpi_axes_bloques <- renderText({
    if (nrow(flux_enrichi) == 0) return("—")
    n_bloques <- flux_enrichi |>
      filter(niveau_cong == "Bloqué") |>
      pull(id_axe) |>
      n_distinct()
    as.character(n_bloques)
  })

  # État temps réel — barres minimalistes par commune
  output$etat_temps_reel <- renderUI({
    if (nrow(flux_enrichi) == 0) {
      return(tags$div(class = "empty-state",
                      tags$p("En attente du dataset (Personne 1).")))
    }
    df <- flux_enrichi |>
      group_by(commune) |>
      summarise(indice = mean(indice_cong, na.rm = TRUE), .groups = "drop") |>
      arrange(indice) |>
      head(8)

    tags$div(class = "bars",
      lapply(seq_len(nrow(df)), function(i) {
        com   <- df$commune[i]
        ind   <- df$indice[i]
        pct   <- min(100, max(5, round((1 - ind) * 100)))
        coul  <- if (ind < 0.3) COULEURS$rouge
                 else if (ind < 0.5) COULEURS$orange
                 else if (ind < 0.8) COULEURS$jaune
                 else COULEURS$vert
        tags$div(class = "bar-row",
          tags$span(class = "bar-label", com),
          tags$div(class = "bar-track",
                   tags$div(class = "bar-fill",
                            style = sprintf("width:%d%%;background:%s;", pct, coul))),
          tags$span(class = "bar-value", sprintf("%d %%", pct))
        )
      })
    )
  })

  # ============================================================================
  # ONGLET 2 — CARTE
  # ============================================================================

  output$carte_principale <- renderLeaflet({
    
    # 1. Choisir l'indicateur selon le radioButton
    if (input$carte_indice == "disparite") {
      df_carte <- indice_disparite |>
        select(commune, valeur = indice_disparite, n_mesures, population)
      titre_legende <- "Impact humain"
      domaine_pal   <- c(0, max(df_carte$valeur, na.rm = TRUE))
    } else {
      df_carte <- indice_disparite |>
        select(commune, valeur = indice_cong_moyen, n_mesures, population)
      titre_legende <- "Niveau bouchons"
      domaine_pal   <- c(0.3, 0.7)
    }
    
    # 2. Joindre aux polygones
    geo_cong <- communes_geo |>
      left_join(df_carte, by = "commune")
    
    # 3. Palette de couleurs
    pal <- colorNumeric(palette = c("#0F9D58", "#F4B400", "#DB4437"),
                        domain  = domaine_pal,
                        na.color = "#CCCCCC")
    geo_cong$couleur <- pal(geo_cong$valeur)
    
    # 4. Centroïdes pour les labels
    centroides_sf <- sf::st_centroid(geo_cong)
    coords <- sf::st_coordinates(centroides_sf)
    centroides <- data.frame(
      lng     = coords[, "X"],
      lat     = coords[, "Y"],
      commune = geo_cong$commune,
      valeur  = geo_cong$valeur
    )
    centroides$label_text <- ifelse(
      is.na(centroides$valeur),
      centroides$commune,
      paste0(centroides$commune, " · ", round(centroides$valeur, 2))
    )
    
    # 5. Carte de base
    m <- leaflet() |>
      addProviderTiles(providers$CartoDB.Positron)
    
    # 6. Ajouter chaque commune comme polygone séparé
    for (i in seq_len(nrow(geo_cong))) {
      one_geo <- geojsonsf::sf_geojson(geo_cong[i, ])
      m <- m |> addGeoJSON(
        one_geo,
        weight      = 2,
        color       = "#0F2E1F",
        fillColor   = geo_cong$couleur[i],
        fillOpacity = 0.65
      )
    }
    
    # 7. Labels + légende + vue
    m |>
      addLabelOnlyMarkers(
        data = centroides,
        lng = ~lng, lat = ~lat,
        label = ~label_text,
        labelOptions = labelOptions(
          noHide = TRUE,
          direction = "center",
          textOnly = TRUE,
          style = list(
            "color"       = "#0F2E1F",
            "font-size"   = "12px",
            "font-weight" = "600",
            "text-shadow" = "1px 1px 2px white, -1px -1px 2px white"
          )
        )
      ) |>
      addLegend(
        position = "bottomright",
        pal      = pal,
        values   = domaine_pal,
        title    = titre_legende,
        opacity  = 0.85
      ) |>
      setView(lng = -4.01, lat = 5.36, zoom = 11)
  })
  # Bande "Lecture" dynamique selon le mode de la carte
  output$carte_lecture <- renderUI({
    
    if (input$carte_indice == "disparite") {
      
      top <- indice_disparite |>
        arrange(desc(indice_disparite)) |>
        slice(1:2)
      
      note_box(HTML(paste0(
        "<b>Lecture · Impact humain</b><br/>",
        "On pondère ici les bouchons par la population de chaque commune. ",
        "<b>", top$commune[1], "</b> et <b>", top$commune[2], "</b> ressortent ",
        "comme les plus touchées : pas forcément celles qui bouchonnent le plus, ",
        "mais celles où le plus de personnes en subissent les conséquences."
      )))
      
    } else {
      
      top <- indice_disparite |>
        arrange(desc(indice_cong_moyen)) |>
        slice(1:2)
      
      note_box(HTML(paste0(
        "<b>Lecture · Niveau de bouchons</b><br/>",
        "Plus la couleur tire vers le rouge, plus la commune subit ",
        "d'embouteillages en moyenne. <b>", top$commune[1], "</b> (",
        round(top$indice_cong_moyen[1], 2), ") et <b>", top$commune[2], "</b> (",
        round(top$indice_cong_moyen[2], 2), ") sont techniquement les plus bouchées."
      )))
    }
  })
  
  # Itinéraire (TODO J4 — osrm::osrmRoute)
  observeEvent(input$btn_itin, {
    req(nzchar(input$itin_depart), nzchar(input$itin_arrivee))
    
    loader_carte$show()
    Sys.sleep(1.2)   # simule le temps d'appel osrm — à enlever quand connecté
    loader_carte$hide()
    
    output$resultat_itin <- renderUI({
      tags$div(class = "itin-result",
               tags$p(tags$strong(input$itin_depart), " → ",
                      tags$strong(input$itin_arrivee)),
               tags$p(class = "muted-small", "Calcul osrm — à connecter J4")
      )
    })
  })

  # ============================================================================
  # ONGLET 3 — TRAFIC + IC 95%
  # ============================================================================

  # Données filtrées pour la courbe journalière
  data_courbe <- reactive({
    req(nrow(flux_enrichi) > 0)
    req(length(input$trafic_communes) > 0)

    flux_enrichi |>
      filter(commune %in% input$trafic_communes) |>
      group_by(commune, heure) |>
      summarise(ic_summary(.data[[input$trafic_y]]), .groups = "drop")
  })

  output$courbe_journaliere <- renderPlotly({
    validate(need(nrow(flux_enrichi) > 0,
                  "Dataset pas encore disponible (Personne 1)"))
    validate(need(length(input$trafic_communes) > 0,
                  "Sélectionnez au moins une commune"))

    df <- data_courbe()
    label_y <- if (input$trafic_y == "vitesse_kmh") "Vitesse (km/h)"
               else "Indice de congestion"

    p <- plot_ly(df, x = ~heure, color = ~commune,
                 colors = "Set2") |>
      add_ribbons(ymin = ~ic_lo, ymax = ~ic_hi,
                  line = list(width = 0),
                  opacity = 0.2, showlegend = FALSE,
                  name = "IC 95%") |>
      add_lines(y = ~moy, line = list(width = 2.5)) |>
      layout(
        xaxis = list(title = "Heure", dtick = 2),
        yaxis = list(title = label_y),
        hovermode = "x unified",
        plot_bgcolor = "#FFFFFF", paper_bgcolor = "#FFFFFF",
        margin = list(t = 30, l = 50, r = 30, b = 40)
      )
    p
  })

  output$insight_courbe <- renderText({
    if (nrow(flux_enrichi) == 0) return("Données non disponibles")
    df <- data_courbe()
    if (nrow(df) == 0) return("Aucune donnée pour la sélection")
    pire <- df |> arrange(moy) |> slice(1)
    sprintf("À %dh, %s atteint sa valeur minimale (%.1f). IC 95 %% : [%.1f ; %.1f].",
            pire$heure, pire$commune, pire$moy, pire$ic_lo, pire$ic_hi)
  })

  output$heatmap_hebdo <- renderPlotly({
    validate(need(nrow(flux_enrichi) > 0,
                  "Dataset pas encore disponible"))
    fn <- match.fun(input$heat_aggreg)
    df <- flux_enrichi |>
      group_by(commune, heure) |>
      summarise(val = fn(vitesse_kmh, na.rm = TRUE), .groups = "drop")

    plot_ly(df, x = ~heure, y = ~commune, z = ~val,
            type = "heatmap", colors = "RdYlGn",
            hovertemplate = "Commune: %{y}<br>Heure: %{x}h<br>Vitesse: %{z:.1f}<extra></extra>") |>
      layout(
        xaxis = list(title = "Heure"),
        yaxis = list(title = ""),
        margin = list(t = 30, l = 100, r = 30, b = 40)
      )
  })

  output$barplot_pires <- renderPlotly({
    validate(need(nrow(flux_enrichi) > 0,
                  "Dataset pas encore disponible"))
    validate(need(length(input$comp_axes) > 0,
                  "Sélectionnez au moins un axe"))

    df <- flux_enrichi |>
      filter(id_axe %in% input$comp_axes,
             heure == input$comp_heure) |>
      group_by(id_axe, nom_axe) |>
      summarise(v = mean(vitesse_kmh, na.rm = TRUE), .groups = "drop") |>
      arrange(v)

    plot_ly(df, x = ~v, y = ~reorder(nom_axe, v),
            type = "bar", orientation = "h",
            marker = list(color = COULEURS$orange)) |>
      layout(
        xaxis = list(title = "Vitesse (km/h)"),
        yaxis = list(title = ""),
        margin = list(t = 30, l = 120, r = 30, b = 40)
      )
  })

  # ============================================================================
  # ONGLET 4 — EXPLORATION EDA + IC 95%
  # ============================================================================

  data_explo <- reactive({
    req(nrow(flux_enrichi) > 0)
    df <- flux_enrichi
    if (input$explo_niveau != "Tous") {
      df <- df |> filter(niveau_cong == input$explo_niveau)
    }
    df
  })

  output$expl_boxplot <- renderPlotly({
    validate(need(nrow(flux_enrichi) > 0,
                  "Dataset pas encore disponible"))
    df <- data_explo()
    p <- ggplot(df, aes(x = reorder(commune, vitesse_kmh, FUN = median),
                        y = vitesse_kmh, fill = commune)) +
      geom_boxplot(outlier.shape = if (input$explo_outliers) 16 else NA,
                   outlier.alpha = 0.3) +
      coord_flip() +
      labs(x = NULL, y = "Vitesse (km/h)") +
      theme_minimal(base_size = 11) +
      theme(legend.position = "none",
            panel.grid.minor = element_blank())
    ggplotly(p) |>
      layout(margin = list(t = 20, l = 100, r = 20, b = 40))
  })

  output$expl_histo <- renderPlotly({
    validate(need(nrow(flux_enrichi) > 0,
                  "Dataset pas encore disponible"))
    req(input$explo_commune)

    df <- flux_enrichi |> filter(commune == input$explo_commune)
    ic <- ic95(df$vitesse_kmh)

    p <- ggplot(df, aes(x = vitesse_kmh)) +
      geom_histogram(bins = input$explo_bins,
                     fill = COULEURS$orange, alpha = 0.8,
                     color = "white") +
      geom_vline(xintercept = ic$moy,
                 color = COULEURS$gris, linewidth = 0.8) +
      geom_vline(xintercept = c(ic$ic_lo, ic$ic_hi),
                 color = COULEURS$bleu, linetype = "dashed",
                 linewidth = 0.6) +
      labs(x = "Vitesse (km/h)", y = "Effectif",
           title = paste("Commune :", input$explo_commune)) +
      theme_minimal(base_size = 11) +
      theme(panel.grid.minor = element_blank())
    ggplotly(p)
  })

  output$expl_ic_text <- renderText({
    if (nrow(flux_enrichi) == 0 || !nzchar(input$explo_commune)) return("—")
    ic <- ic95(flux_enrichi |>
                 filter(commune == input$explo_commune) |>
                 pull(vitesse_kmh))
    format_ic(ic, "km/h")
  })

  output$expl_stats_table <- renderDT({
    validate(need(nrow(flux_enrichi) > 0,
                  "Dataset pas encore disponible"))

    stats <- flux_enrichi |>
      group_by(commune) |>
      summarise(ic_summary(vitesse_kmh), .groups = "drop") |>
      mutate(across(c(moy, se, ic_lo, ic_hi), ~ round(.x, 2))) |>
      transmute(Commune = commune,
                `Moyenne (km/h)` = moy,
                `IC 95 % bas`    = ic_lo,
                `IC 95 % haut`   = ic_hi,
                `n observations` = n)

    datatable(stats,
              options = list(pageLength = 13, dom = 't', searching = FALSE),
              rownames = FALSE)
  })
  
  # --- Test statistique de Wilcoxon entre 2 communes ---
  output$test_resultat <- renderUI({
    
    req(input$test_commune_a, input$test_commune_b)
    
    a <- input$test_commune_a
    b <- input$test_commune_b
    
    if (a == b) {
      return(tags$div(class = "test-result test-neutral",
                      tags$p("Sélectionnez deux communes ", tags$b("différentes"),
                             " pour effectuer la comparaison.")
      ))
    }
    
    # Récupérer les vitesses des 2 communes
    v_a <- flux_enrichi |>
      filter(commune == a) |>
      pull(vitesse_kmh)
    
    v_b <- flux_enrichi |>
      filter(commune == b) |>
      pull(vitesse_kmh)
    
    if (length(v_a) < 3 || length(v_b) < 3) {
      return(tags$div(class = "test-result test-neutral",
                      tags$p("Pas assez de données pour comparer ces deux communes.")
      ))
    }
    
    # Test de Wilcoxon-Mann-Whitney
    res <- tryCatch(
      wilcox.test(v_a, v_b, exact = FALSE),
      error = function(e) NULL
    )
    
    if (is.null(res)) {
      return(tags$div(class = "test-result test-neutral",
                      tags$p("Test impossible à calculer sur ces données.")
      ))
    }
    
    p <- res$p.value
    m_a <- round(mean(v_a, na.rm = TRUE), 1)
    m_b <- round(mean(v_b, na.rm = TRUE), 1)
    ecart <- abs(m_a - m_b)
    plus_rapide <- if (m_a > m_b) a else b
    plus_lent   <- if (m_a > m_b) b else a
    
    # Interprétation en langage clair
    if (p < 0.001) {
      verdict_class <- "test-strong"
      verdict_text  <- paste0(
        "L'écart entre ", tags$b(a), " et ", tags$b(b),
        " est très significatif (p < 0.001). ",
        tags$b(plus_rapide), " roule en moyenne ", ecart,
        " km/h plus vite que ", plus_lent, "."
      )
    } else if (p < 0.05) {
      verdict_class <- "test-significant"
      verdict_text  <- paste0(
        "L'écart entre ", tags$b(a), " et ", tags$b(b),
        " est statistiquement significatif (p = ", signif(p, 3), "). ",
        tags$b(plus_rapide), " roule en moyenne ", ecart,
        " km/h plus vite que ", plus_lent, "."
      )
    } else {
      verdict_class <- "test-ns"
      verdict_text  <- paste0(
        "L'écart entre ", tags$b(a), " et ", tags$b(b),
        " n'est pas statistiquement significatif (p = ", signif(p, 3), "). ",
        "La différence observée (", ecart, " km/h) ",
        "peut être due au hasard."
      )
    }
    
    tags$div(class = paste("test-result", verdict_class),
             tags$p(HTML(verdict_text)),
             tags$p(class = "test-meta",
                    "Moyennes : ", tags$b(a), " = ", m_a, " km/h · ",
                    tags$b(b), " = ", m_b, " km/h · ",
                    "Test : Wilcoxon-Mann-Whitney · ",
                    "n(A) = ", length(v_a), ", n(B) = ", length(v_b))
    )
  })

  # ============================================================================
  # ONGLET 5 — RÉSEAU (P1)
  # ============================================================================

  output$graphe_communes_vis <- renderVisNetwork({
    if (is.null(graphe_communes)) {
      return(visNetwork(
        nodes = data.frame(id = 1, label = "graphe_communes.rds non livré"),
        edges = data.frame(),
        background = "#FAFAFA"
      ))
    }
    # TODO P1 J7 — toVisNetworkData(graphe_communes)
    visNetwork(
      nodes = data.frame(id = 1, label = "TODO P1"),
      edges = data.frame()
    )
  })

  output$tableau_metriques <- renderTable({
    data.frame(Commune = "TODO P1", Valeur = "—")
  })

  output$modularite <- renderText({ "Modularité Louvain : à venir (P1)" })
  output$communautes_resume <- renderUI({
    tags$p(class = "muted-small", "Bassins Louvain — à venir (P1)")
  })

  # ============================================================================
  # ONGLET 6 — ML (3 sous-pages, P1)
  # ============================================================================

  observeEvent(input$ml_predire, {
    shinyjs::disable("ml_predire")
    
    withProgress(message = "Prédiction en cours", value = 0, {
      incProgress(0.3, detail = "Préparation des données…")
      Sys.sleep(0.3)
      incProgress(0.4, detail = "Application du modèle…")
      Sys.sleep(0.3)
      incProgress(0.3, detail = "Calcul de l'IC 95 %…")
      Sys.sleep(0.2)
    })
    
    shinyjs::enable("ml_predire")
    showNotification("Prédiction calculée", type = "message", duration = 2)
    
    output$resultat_prediction <- renderUI({
      if (is.null(mod_rf)) {
        return(card(title = "Prédiction",
                    tags$p("Modèle pas encore livré (Personne 1).")))
      }
      # TODO P1 J9 — predict(mod_rf, newdata = ...)
      card(title = "Prédiction",
        tags$p("De ", tags$strong(input$ml_depart),
               " à ", tags$strong(input$ml_arrivee),
               " · départ ", input$ml_heure, "h"),
        tags$div(class = "kpi-value",
          textOutput("ml_pred_value", inline = TRUE)),
        tags$p(class = "muted-small",
               "IC 95 % : ", textOutput("ml_pred_ic", inline = TRUE)))
    })
    output$ml_pred_value <- renderText("— min")
    output$ml_pred_ic    <- renderText("± à venir P1")
  })

  output$importance_vars <- renderPlot({
    plot.new()
    title("Importance des variables — TODO P1 J9 (varImpPlot)")
  })

  output$comparaison_modeles <- renderTable({
    tryCatch(
      read_csv("outputs/comparaison_modeles.csv", show_col_types = FALSE),
      error = function(e) data.frame(
        Modele = c("Random Forest","Régression linéaire",
                   "Arbre","k-NN"),
        RMSE   = "TODO P1",
        R2     = "TODO P1"
      )
    )
  })

  output$clustering_acp <- renderPlot({
    plot.new()
    title("k-means + ACP — TODO P1 J11")
  })

  # ============================================================================
  # ONGLET 7 — DONNÉES (P1)
  # ============================================================================

  donnees_filtrees <- reactive({
    df <- switch(input$dt_dataset,
                 "flux"     = flux_enrichi,
                 "communes" = communes_wiki,
                 "stops"    = tibble(message = "TODO — charger stops.txt"))

    if (input$dt_dataset == "flux" && nrow(df) > 0) {
      if (input$dt_filtre_commune != "all") {
        df <- df |> filter(commune == input$dt_filtre_commune)
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
              filter = "top",
              class = "compact stripe")
  })

  output$dt_n_lignes <- renderText({
    format(nrow(donnees_filtrees()), big.mark = " ")
  })
  output$dt_vit_moy <- renderText({
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
    if (!"id_axe" %in% names(df) || nrow(df) == 0) return("—")
    df |>
      group_by(id_axe) |>
      summarise(v = mean(vitesse_kmh, na.rm = TRUE), .groups = "drop") |>
      arrange(v) |>
      slice(1) |>
      pull(id_axe)
  })

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
    "Sources : TomTom Traffic Flow API · GTFS DT4A · Wikipedia · OSM · Mai 2025"
  })

  # ============================================================================
  # ONGLET 8 — RECOMMANDATIONS (P1)
  # ============================================================================

  output$table_reco <- renderTable({
    data.frame(
      Priorité    = c("1", "2", "3", "4", "5"),
      Recommandation = c(
        "Voies bus dédiées Adjamé–Plateau",
        "Décalage horaires entrée 7h–9h",
        "Bus express Yopougon–Plateau",
        "Réorganiser ronds-points Adjamé",
        "Application temps réel pour usagers"
      ),
      Impact       = c("Très élevé", "Élevé", "Moyen", "Moyen", "Faible"),
      Faisabilité  = c("Moyenne", "Élevée", "Moyenne", "Faible", "Élevée"),
      Source       = c("Réseau + Trafic", "Trafic", "Trafic + ML",
                       "Réseau", "Données + ML")
    )
  })

}
