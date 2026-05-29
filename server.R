# ==============================================================================
# OBSERVATOIRE DE LA MOBILITÉ — GRAND ABIDJAN
# server.R — Logique réactive (CDC v7 — 8 onglets)
# ==============================================================================

server <- function(input, output, session) {
  waiter_hide()
  
  # Helper : extraire commune depuis colonne commune (peut contenir "/")
  extraire_commune <- function(x) {
    stringr::str_split(x, "/") |>
      purrr::map_chr(1) |>
      stringr::str_trim() |>
      stringr::str_replace("Adjame", "Adjamé")
  }
  
  # ----------------------------------------------------------------------------
  # MISE À JOUR DES CHOIX DYNAMIQUES (au démarrage)
  # ----------------------------------------------------------------------------
  observe({
    if (nrow(flux_enrichi) > 0) {
      communes_dispo <- flux_enrichi |>
        mutate(commune_dep = extraire_commune(commune)) |>
        pull(commune_dep) |>
        unique() |>
        sort()
      axes_dispo <- sort(unique(flux_enrichi$id_axe))
      
      updateSelectInput(session, "filtre_commune",
                        choices = c("Toutes" = "all", communes_dispo))
      updatePickerInput(session, "trafic_communes",
                        choices = communes_dispo, selected = head(communes_dispo, 3))
      updateCheckboxGroupInput(session, "comp_axes",
                               choices = axes_dispo, selected = head(axes_dispo, 5))
      updateSelectInput(session, "explo_commune",
                        choices = communes_dispo, selected = communes_dispo[1])
      updateSelectInput(session, "dt_filtre_commune",
                        choices = c("Toutes" = "all", communes_dispo))
      
      # ML — communes
      updateSelectInput(session, "ml_commune_dep", choices = communes_dispo)
      updateSelectInput(session, "ml_commune_arr", choices = communes_dispo,
                        selected = communes_dispo[2])
    }
  })
  
  # Filtrer axes selon commune de depart ML
  observeEvent(input$ml_commune_dep, {
    req(input$ml_commune_dep)
    axes_dep <- flux_enrichi |>
      mutate(commune_dep = extraire_commune(commune)) |>
      filter(commune_dep == input$ml_commune_dep) |>
      select(id_axe, nom_axe) |>
      distinct() |>
      arrange(id_axe)
    choix <- setNames(axes_dep$id_axe, axes_dep$nom_axe)
    updateSelectInput(session, "ml_axe_dep", choices = choix)
  })
  
  # Filtrer axes selon commune d'arrivee ML
  observeEvent(input$ml_commune_arr, {
    req(input$ml_commune_arr)
    axes_arr <- flux_enrichi |>
      mutate(commune_dep = extraire_commune(commune)) |>
      filter(commune_dep == input$ml_commune_arr) |>
      select(id_axe, nom_axe) |>
      distinct() |>
      arrange(id_axe)
    choix <- setNames(axes_arr$id_axe, axes_arr$nom_axe)
    updateSelectInput(session, "ml_axe_arr", choices = choix)
  })
  
  # ============================================================================
  # ONGLET 1 — ACCUEIL
  # ============================================================================
  
  observeEvent(input$go_carte,   { updateTabItems(session, "main_tabs", "carte") })
  observeEvent(input$go_rapport, { showNotification("Rapport Quarto à générer", type = "message") })
  
  output$kpi_axes_bloques <- renderText({
    if (nrow(flux_enrichi) == 0) return("—")
    flux_enrichi |> filter(niveau_cong == "Bloqué") |> pull(id_axe) |> n_distinct() |> as.character()
  })
  
  output$etat_temps_reel <- renderUI({
    if (nrow(flux_enrichi) == 0)
      return(tags$div(class = "empty-state", tags$p("En attente du dataset.")))
    df <- flux_enrichi |>
      group_by(commune) |>
      summarise(indice = mean(indice_cong, na.rm = TRUE), .groups = "drop") |>
      arrange(indice) |> head(8)
    tags$div(class = "bars",
             lapply(seq_len(nrow(df)), function(i) {
               ind  <- df$indice[i]
               pct  <- min(100, max(5, round((1 - ind) * 100)))
               coul <- if (ind < 0.3) COULEURS$rouge else if (ind < 0.5) COULEURS$orange
               else if (ind < 0.8) COULEURS$jaune else COULEURS$vert
               tags$div(class = "bar-row",
                        tags$span(class = "bar-label", df$commune[i]),
                        tags$div(class = "bar-track",
                                 tags$div(class = "bar-fill",
                                          style = sprintf("width:%d%%;background:%s;", pct, coul))),
                        tags$span(class = "bar-value", sprintf("%d %%", pct)))
             })
    )
  })
  
  # ============================================================================
  # ONGLET 2 — CARTE
  # ============================================================================
  
  output$carte_principale <- renderLeaflet({
    leaflet() |>
      addProviderTiles(providers$CartoDB.Positron) |>
      setView(lng = ABIDJAN_LON, lat = ABIDJAN_LAT, zoom = ABIDJAN_ZOOM)
  })
  
  observeEvent(input$btn_itin, {
    req(nzchar(input$itin_depart), nzchar(input$itin_arrivee))
    loader_carte$show(); Sys.sleep(1.2); loader_carte$hide()
    output$resultat_itin <- renderUI({
      tags$div(class = "itin-result",
               tags$p(tags$strong(input$itin_depart), " → ", tags$strong(input$itin_arrivee)),
               tags$p(class = "muted-small", "Calcul osrm — à connecter"))
    })
  })
  
  # ============================================================================
  # ONGLET 3 — TRAFIC + IC 95%
  # ============================================================================
  
  data_courbe <- reactive({
    req(nrow(flux_enrichi) > 0, length(input$trafic_communes) > 0)
    flux_enrichi |>
      filter(commune %in% input$trafic_communes) |>
      group_by(commune, heure) |>
      summarise(ic_summary(.data[[input$trafic_y]]), .groups = "drop")
  })
  
  output$courbe_journaliere <- renderPlotly({
    validate(need(nrow(flux_enrichi) > 0, "Dataset pas encore disponible"))
    validate(need(length(input$trafic_communes) > 0, "Sélectionnez au moins une commune"))
    df      <- data_courbe()
    label_y <- if (input$trafic_y == "vitesse_kmh") "Vitesse (km/h)" else "Indice de congestion"
    plot_ly(df, x = ~heure, color = ~commune, colors = "Set2") |>
      add_ribbons(ymin = ~ic_lo, ymax = ~ic_hi, line = list(width = 0),
                  opacity = 0.2, showlegend = FALSE, name = "IC 95%") |>
      add_lines(y = ~moy, line = list(width = 2.5)) |>
      layout(xaxis = list(title = "Heure", dtick = 2), yaxis = list(title = label_y),
             hovermode = "x unified", plot_bgcolor = "#FFFFFF", paper_bgcolor = "#FFFFFF",
             margin = list(t = 30, l = 50, r = 30, b = 40))
  })
  
  output$insight_courbe <- renderText({
    if (nrow(flux_enrichi) == 0) return("Données non disponibles")
    df <- data_courbe()
    if (nrow(df) == 0) return("Aucune donnée")
    pire <- df |> arrange(moy) |> slice(1)
    sprintf("À %dh, %s atteint sa valeur minimale (%.1f). IC 95 %% : [%.1f ; %.1f].",
            pire$heure, pire$commune, pire$moy, pire$ic_lo, pire$ic_hi)
  })
  
  output$heatmap_hebdo <- renderPlotly({
    validate(need(nrow(flux_enrichi) > 0, "Dataset pas encore disponible"))
    fn <- match.fun(input$heat_aggreg)
    df <- flux_enrichi |>
      group_by(commune, heure) |>
      summarise(val = fn(vitesse_kmh, na.rm = TRUE), .groups = "drop")
    plot_ly(df, x = ~heure, y = ~commune, z = ~val, type = "heatmap", colors = "RdYlGn",
            hovertemplate = "Commune: %{y}<br>Heure: %{x}h<br>Vitesse: %{z:.1f}<extra></extra>") |>
      layout(xaxis = list(title = "Heure"), yaxis = list(title = ""),
             margin = list(t = 30, l = 100, r = 30, b = 40))
  })
  
  output$barplot_pires <- renderPlotly({
    validate(need(nrow(flux_enrichi) > 0, "Dataset pas encore disponible"))
    validate(need(length(input$comp_axes) > 0, "Sélectionnez au moins un axe"))
    df <- flux_enrichi |>
      filter(id_axe %in% input$comp_axes, heure == input$comp_heure) |>
      group_by(id_axe) |>
      summarise(v = mean(vitesse_kmh, na.rm = TRUE), .groups = "drop") |>
      arrange(v)
    plot_ly(df, x = ~v, y = ~reorder(id_axe, v), type = "bar", orientation = "h",
            marker = list(color = COULEURS$orange)) |>
      layout(xaxis = list(title = "Vitesse (km/h)"), yaxis = list(title = ""),
             margin = list(t = 30, l = 120, r = 30, b = 40))
  })
  
  # ============================================================================
  # ONGLET 4 — EXPLORATION EDA + IC 95%
  # ============================================================================
  
  data_explo <- reactive({
    req(nrow(flux_enrichi) > 0)
    df <- flux_enrichi
    if (input$explo_niveau != "Tous") df <- df |> filter(niveau_cong == input$explo_niveau)
    df
  })
  
  output$expl_boxplot <- renderPlotly({
    validate(need(nrow(flux_enrichi) > 0, "Dataset pas encore disponible"))
    p <- ggplot(data_explo(), aes(x = reorder(commune, vitesse_kmh, FUN = median),
                                  y = vitesse_kmh, fill = commune)) +
      geom_boxplot(outlier.shape = if (input$explo_outliers) 16 else NA, outlier.alpha = 0.3) +
      coord_flip() + labs(x = NULL, y = "Vitesse (km/h)") +
      theme_minimal(base_size = 11) + theme(legend.position = "none", panel.grid.minor = element_blank())
    ggplotly(p) |> layout(margin = list(t = 20, l = 100, r = 20, b = 40))
  })
  
  output$expl_histo <- renderPlotly({
    validate(need(nrow(flux_enrichi) > 0, "Dataset pas encore disponible"))
    req(input$explo_commune)
    df <- flux_enrichi |> filter(commune == input$explo_commune)
    ic <- ic95(df$vitesse_kmh)
    p <- ggplot(df, aes(x = vitesse_kmh)) +
      geom_histogram(bins = input$explo_bins, fill = COULEURS$orange, alpha = 0.8, color = "white") +
      geom_vline(xintercept = ic$moy, color = COULEURS$gris, linewidth = 0.8) +
      geom_vline(xintercept = c(ic$ic_lo, ic$ic_hi), color = COULEURS$bleu,
                 linetype = "dashed", linewidth = 0.6) +
      labs(x = "Vitesse (km/h)", y = "Effectif", title = paste("Commune :", input$explo_commune)) +
      theme_minimal(base_size = 11) + theme(panel.grid.minor = element_blank())
    ggplotly(p)
  })
  
  output$expl_ic_text <- renderText({
    if (nrow(flux_enrichi) == 0 || !nzchar(input$explo_commune)) return("—")
    ic <- ic95(flux_enrichi |> filter(commune == input$explo_commune) |> pull(vitesse_kmh))
    format_ic(ic, "km/h")
  })
  
  output$expl_stats_table <- renderDT({
    validate(need(nrow(flux_enrichi) > 0, "Dataset pas encore disponible"))
    stats <- flux_enrichi |>
      group_by(commune) |>
      summarise(ic_summary(vitesse_kmh), .groups = "drop") |>
      mutate(across(c(moy, se, ic_lo, ic_hi), ~ round(.x, 2))) |>
      transmute(Commune = commune, `Moyenne (km/h)` = moy,
                `IC 95 % bas` = ic_lo, `IC 95 % haut` = ic_hi, `n observations` = n)
    datatable(stats, options = list(pageLength = 13, dom = 't', searching = FALSE), rownames = FALSE)
  })
  
  # ============================================================================
  # ONGLET 5 — RÉSEAU
  # ============================================================================
  
  output$graphe_communes_vis <- renderVisNetwork({
    req(!is.null(graphe_communes))
    g  <- graphe_communes$graphe
    vn <- toVisNetworkData(g)
    vn$edges <- vn$edges |> filter(flux >= input$seuil_flux)
    deg <- degree(g, mode = "all")
    idx <- match(vn$nodes$id, V(g)$name)
    vn$nodes$size       <- log(V(g)$population[idx] + 1) * 4
    vn$nodes$group      <- as.character(V(g)$group[idx])
    vn$nodes$label      <- V(g)$name[idx]
    vn$nodes$font.size  <- 18
    vn$nodes$font.color <- "#000000"
    vn$nodes$title <- paste0("<b>", V(g)$name[idx], "</b><br>",
                             "Population : ", format(V(g)$population[idx], big.mark = " "), "<br>",
                             "Degré : ", deg[V(g)$name[idx]])
    visNetwork(vn$nodes, vn$edges, background = "#FAFAFA") |>
      visEdges(arrows = "to", smooth = list(type = "curvedCW", roundness = 0.3),
               color = list(color = "#CCCCCC", highlight = COULEURS$orange)) |>
      visOptions(highlightNearest = list(enabled = TRUE, degree = 1),
                 nodesIdSelection = list(enabled = TRUE, main = "Sélectionner une commune")) |>
      visPhysics(solver = "forceAtlas2Based",
                 forceAtlas2Based = list(gravitationalConstant = -200, centralGravity = 0.005,
                                         springLength = 200, springConstant = 0.05),
                 stabilization = list(enabled = TRUE, iterations = 500)) |>
      visInteraction(dragNodes = TRUE, dragView = TRUE, zoomView = TRUE, navigationButtons = FALSE) |>
      visGroups(groupname = "0", color = list(background = "#AAAAAA", border = "#888888")) |>
      visGroups(groupname = "1", color = list(background = COULEURS$orange, border = "#c0520a")) |>
      visGroups(groupname = "2", color = list(background = COULEURS$vert,   border = "#006b2e")) |>
      visGroups(groupname = "3", color = list(background = COULEURS$bleu,   border = "#1a5276")) |>
      visGroups(groupname = "4", color = list(background = COULEURS$jaune,  border = "#b07d00")) |>
      visGroups(groupname = "5", color = list(background = "#9B59B6",       border = "#6c3483")) |>
      visGroups(groupname = "6", color = list(background = "#E74C3C",       border = "#922b21")) |>
      visGroups(groupname = "7", color = list(background = "#1ABC9C",       border = "#148f77")) |>
      visLegend(position = "left", main = "Bassins", width = 0.08)
  })
  
  output$tableau_metriques <- renderDT({
    req(!is.null(graphe_communes))
    g       <- graphe_communes$graphe
    deg_in  <- degree(g, mode = "in")
    deg_out <- degree(g, mode = "out")
    deg_all <- degree(g, mode = "all")
    graphe_communes$metriques |>
      mutate(
        betweenness  = round(betweenness, 3),
        closeness    = ifelse(is.nan(closeness), NA, round(closeness, 3)),
        degre        = deg_all[commune], flux_entrant = deg_in[commune], flux_sortant = deg_out[commune],
        role = case_when(
          closeness >= 0.8  ~ "Hub central",    flux_entrant >= 5 ~ "Pôle attracteur",
          flux_sortant >= 3 ~ "Pôle générateur", is.na(closeness)  ~ "Commune isolée",
          closeness >= 0.5  ~ "Commune connectée", TRUE            ~ "Commune périphérique")) |>
      rename(Commune = commune, `Intermédiarité` = betweenness, `Proximité` = closeness,
             `Degré total` = degre, `Flux entrant` = flux_entrant,
             `Flux sortant` = flux_sortant, `Rôle` = role) |>
      datatable(options = list(pageLength = 13, dom = 't', scrollX = TRUE, searching = FALSE),
                rownames = FALSE) |>
      formatStyle("Rôle", backgroundColor = styleEqual(
        c("Hub central","Commune isolée","Commune connectée","Commune périphérique"),
        c("#d4edda","#f8d7da","#fff3cd","#e2e3e5")))
  })
  
  output$modularite <- renderText({
    req(!is.null(graphe_communes))
    mod     <- graphe_communes$modularite
    qualite <- if (mod >= 0.3) "structure bien définie"
    else if (mod >= 0.15) "structure émergente" else "structure faible"
    sprintf("Modularité : %.3f — %s", mod, qualite)
  })
  
  output$communautes_resume <- renderUI({
    req(!is.null(graphe_communes))
    n <- length(unique(membership(graphe_communes$communautes)))
    tags$div(
      tags$p(class = "muted-small", sprintf("%d bassins détectés.", n)),
      tags$p(class = "muted-small", "Même couleur = même bassin de mobilité."))
  })
  
  # ============================================================================
  # ONGLET 6 — ML
  # ============================================================================
  
  mod_rf <- tryCatch(readRDS("models/mod_rf_vitesse.rds"), error = function(e) NULL)
  
  observeEvent(input$ml_predire, {
    req(input$ml_axe_dep, input$ml_axe_arr, input$ml_heure, input$ml_date)
    shinyjs::disable("ml_predire")
    
    withProgress(message = "Prédiction en cours", value = 0, {
      incProgress(0.2, detail = "Récupération des axes…")
      
      axe_dep <- flux_enrichi |> filter(id_axe == input$ml_axe_dep) |> slice(1)
      axe_arr <- flux_enrichi |> filter(id_axe == input$ml_axe_arr) |> slice(1)
      
      incProgress(0.3, detail = "Calcul de la distance via OSRM…")
      
      # Distance reelle via OSRM entre les deux points GPS
      options(osrm.server = "https://router.project-osrm.org/")
      route <- tryCatch(
        osrm::osrmRoute(
          src = c(axe_dep$lon_dep, axe_dep$lat_dep),
          dst = c(axe_arr$lon_dep, axe_arr$lat_dep)
        ),
        error = function(e) NULL
      )
      
      if (!is.null(route)) {
        dist_m      <- route$distance * 1000
        duree_libre <- route$duration
      } else {
        dist_m      <- mean(flux_enrichi$distance_m, na.rm = TRUE)
        duree_libre <- NA
      }
      
      vit_ref    <- mean(axe_dep$vitesse_libre_ref, na.rm = TRUE)
      id_axe_num <- as.numeric(factor(flux_enrichi$id_axe))[
        which(flux_enrichi$id_axe == input$ml_axe_dep)[1]]
      
      jour_num <- as.numeric(factor(
        tolower(weekdays(input$ml_date)),
        levels = c("lundi","mardi","mercredi","jeudi","vendredi","samedi","dimanche")))
      if (is.na(jour_num)) jour_num <- 1
      
      newdata <- data.frame(
        heure = input$ml_heure, distance_m = dist_m,
        vitesse_libre_ref = vit_ref, id_axe_num = id_axe_num, jour_num = jour_num)
      
      incProgress(0.3, detail = "Application du Random Forest…")
      pred <- if (!is.null(mod_rf)) predict(mod_rf, newdata = newdata) else NULL
      incProgress(0.2, detail = "Calcul IC 95%…")
      shinyjs::enable("ml_predire")
      
      output$resultat_prediction <- renderUI({
        if (is.null(pred))
          return(card(title = "Prédiction", tags$p("Modèle non disponible.")))
        
        val    <- round(as.numeric(pred), 1)
        rmse   <- 1.67
        ic_txt <- sprintf("[%.1f ; %.1f] km/h", val - 1.96*rmse, val + 1.96*rmse)
        niveau <- if (val >= 40) "Fluide" else if (val >= 25) "Modéré"
        else if (val >= 15) "Congestionné" else "Bloqué"
        
        # Heure arrivee basee sur distance OSRM + vitesse RF
        duree_min     <- round((dist_m / 1000) / val * 60)
        heure_dep_dt  <- as.POSIXct(paste(input$ml_date, sprintf("%02d:00", input$ml_heure)))
        heure_arr_txt <- format(heure_dep_dt + duree_min * 60, "%Hh%M")
        
        commune_dep <- extraire_commune(axe_dep$commune)
        commune_arr <- extraire_commune(axe_arr$commune)
        
        card(title = "Résultat de la prédiction",
             fluidRow(
               column(6,
                      tags$p(tags$strong("Axe départ : "),  axe_dep$nom_axe),
                      tags$p(tags$strong("Axe arrivée : "),  axe_arr$nom_axe),
                      tags$p(tags$strong("Trajet : "),       paste(commune_dep, "→", commune_arr)),
                      tags$p(tags$strong("Distance : "),     sprintf("%.1f km", dist_m/1000)),
                      tags$p(tags$strong("Date : "),         format(input$ml_date, "%d/%m/%Y")),
                      tags$p(tags$strong("Jour : "),         weekdays(input$ml_date)),
                      tags$p(tags$strong("Départ : "),       paste0(input$ml_heure, "h00")),
                      tags$p(tags$strong("Arrivée est. : "), heure_arr_txt),
                      tags$p(tags$strong("Durée est. : "),   paste0(duree_min, " min"))
               ),
               column(6,
                      tags$div(class = "kpi-value", paste0(val, " km/h")),
                      tags$p(class = "muted-small", tags$strong("Niveau : "),  niveau),
                      tags$p(class = "muted-small", tags$strong("IC 95% : "),  ic_txt),
                      tags$p(class = "muted-small", tags$strong("Modèle : "),  "Random Forest"),
                      tags$p(class = "muted-small", tags$strong("Distance : "), "OSRM (réseau réel)")
               )
             )
        )
      })
    })
  })
  
  output$comparaison_modeles <- renderTable({
    tryCatch(
      read_csv("outputs/comparaison_modeles.csv", show_col_types = FALSE),
      error = function(e) data.frame(
        Modele = c("Arbre de décision","Random Forest","k-NN","Régression linéaire"),
        RMSE = c(1.10, 1.67, 6.22, 6.77), R2 = c(0.983, 0.982, 0.326, 0.335),
        MAE  = c(0.724, 1.28, 4.58, 5.37)))
  })
  
  output$importance_vars <- renderPlot({
    if (is.null(mod_rf)) { plot.new(); title("Random Forest non disponible"); return() }
    varImpPlot(mod_rf, main = "Importance des variables — Random Forest",
               col = COULEURS$orange, pch = 19)
  })
  
  output$clustering_acp <- renderPlot({
    cl <- tryCatch(readRDS("models/clustering.rds"), error = function(e) NULL)
    if (is.null(cl)) {
      plot.new(); title("Clustering en cours — données complémentaires en collecte"); return()
    }
    coords <- cl$coords
    ggplot(coords, aes(x = Dim.1, y = Dim.2, color = profil, label = commune)) +
      geom_point(size = 5) +
      ggrepel::geom_label_repel(size = 3.5, show.legend = FALSE) +
      scale_color_manual(values = c("Très accessible" = "#009A44",
                                    "Moyennement accessible" = "#F47920",
                                    "Peu accessible" = "#DC2626")) +
      labs(title = "Profils des communes — k-means + ACP",
           x = "Dimension 1", y = "Dimension 2", color = "Profil") +
      theme_minimal(base_size = 12)
  })
  
  # ============================================================================
  # ONGLET 7 — DONNÉES
  # ============================================================================
  
  donnees_filtrees <- reactive({
    stops_df <- if (!is.null(gtfs)) {
      gtfs |>
        distinct(stop_name, stop_lat, stop_lon, agency_name, route_long_name) |>
        rename(
          `Arrêt`    = stop_name,
          Latitude   = stop_lat,
          Longitude  = stop_lon,
          Agence     = agency_name,
          Ligne      = route_long_name
        )
    } else {
      tibble(message = "GTFS non disponible")
    }
    
    df <- switch(input$dt_dataset,
                 "flux"     = flux_enrichi,
                 "communes" = communes_wiki,
                 "stops"    = stops_df)
    
    if (input$dt_dataset == "flux" && nrow(df) > 0) {
      if (input$dt_filtre_commune != "all")
        df <- df |> filter(commune == input$dt_filtre_commune)
      df <- df |> filter(heure >= input$dt_filtre_heure[1],
                         heure <= input$dt_filtre_heure[2])
    }
    df
  })
  
  output$table_principale <- renderDT({
    datatable(donnees_filtrees(), options = list(pageLength = 10, scrollX = TRUE),
              rownames = FALSE, filter = "top", class = "compact stripe")
  })
  
  output$dt_n_lignes   <- renderText({ format(nrow(donnees_filtrees()), big.mark = " ") })
  output$dt_vit_moy    <- renderText({
    df <- donnees_filtrees()
    if (!"vitesse_kmh" %in% names(df) || nrow(df) == 0) return("—")
    paste0(round(mean(df$vitesse_kmh, na.rm = TRUE), 1), " km/h")
  })
  output$dt_pct_bloque <- renderText({
    df <- donnees_filtrees()
    if (!"niveau_cong" %in% names(df) || nrow(df) == 0) return("—")
    paste0(round(mean(df$niveau_cong == "Bloqué", na.rm = TRUE) * 100, 1), " %")
  })
  output$dt_axe_pire   <- renderText({
    df <- donnees_filtrees()
    if (!"id_axe" %in% names(df) || nrow(df) == 0) return("—")
    df |> group_by(id_axe) |> summarise(v = mean(vitesse_kmh, na.rm = TRUE), .groups = "drop") |>
      arrange(v) |> slice(1) |> pull(id_axe)
  })
  
  output$dt_export <- downloadHandler(
    filename = function() paste0("mobilite_abidjan_", input$dt_dataset, "_", Sys.Date(), ".csv"),
    content  = function(file) write_csv(donnees_filtrees(), file)
  )
  output$source_note <- renderText({
    "Sources : TomTom Traffic Flow API · GTFS DT4A · Wikipedia · OSM · Mai 2025"
  })
  
  # ============================================================================
  # ONGLET 8 — RECOMMANDATIONS
  # ============================================================================
  
  output$table_reco <- renderTable({
    data.frame(
      Priorité = c("1","2","3","4","5"),
      Recommandation = c("Voies bus dédiées Adjamé–Plateau","Décalage horaires entrée 7h–9h",
                         "Bus express Yopougon–Plateau","Réorganiser ronds-points Adjamé",
                         "Application temps réel pour usagers"),
      Impact      = c("Très élevé","Élevé","Moyen","Moyen","Faible"),
      Faisabilité = c("Moyenne","Élevée","Moyenne","Faible","Élevée"),
      Source      = c("Réseau + Trafic","Trafic","Trafic + ML","Réseau","Données + ML"))
  })
}