# ==============================================================================
# OBSERVATOIRE DE LA MOBILITÉ — GRAND ABIDJAN
# server.R — Sara (P2) + Axelle (P1) fusionné
# ==============================================================================

server <- function(input, output, session) {
  waiter_hide()
  
  extraire_commune <- function(x) {
    stringr::str_split(x, "/") |> purrr::map_chr(1) |>
      stringr::str_trim() |> stringr::str_replace("Adjame", "Adjamé")
  }
  
  # ---------- CHOIX DYNAMIQUES (Sara + Axelle) ----------
  observe({
    if (nrow(flux_enrichi) > 0) {
      communes_dispo <- sort(unique(flux_enrichi$commune))
      axes_dispo <- flux_enrichi |> distinct(id_axe, nom_axe) |> arrange(id_axe) |>
        (\(d) setNames(d$id_axe, d$nom_axe))()
      
      # Sara
      updateSelectInput(session, "filtre_commune", choices = c("Toutes" = "all", communes_dispo))
      updatePickerInput(session, "trafic_communes", choices = communes_dispo, selected = head(communes_dispo, 3))
      updateCheckboxGroupInput(session, "comp_axes", choices = axes_dispo, selected = head(axes_dispo, 5))
      updateSelectInput(session, "explo_commune", choices = communes_dispo, selected = communes_dispo[1])
      updateSelectInput(session, "test_commune_a", choices = communes_dispo, selected = communes_dispo[1])
      updateSelectInput(session, "test_commune_b", choices = communes_dispo, selected = communes_dispo[length(communes_dispo)])
      updateSelectInput(session, "dt_filtre_commune", choices = c("Toutes" = "all", communes_dispo))
      
      # Sara — itinéraire
      if (exists("communes_geo") && !is.null(communes_geo)) {
        communes_officielles <- sort(communes_geo$commune)
        updateSelectizeInput(session, "itin_depart", choices = communes_officielles, server = FALSE)
        updateSelectizeInput(session, "itin_arrivee", choices = communes_officielles, server = FALSE)
      }
      
      # Axelle — ML (communes extraites sans les "/")
      communes_ml <- flux_enrichi |>
        mutate(cd = extraire_commune(commune)) |>
        pull(cd) |> unique() |> sort()
      updateSelectInput(session, "ml_commune_dep", choices = communes_ml)
      updateSelectInput(session, "ml_commune_arr", choices = communes_ml, selected = communes_ml[2])
    }
  })
  
  # Axelle — filtrer axes ML
  observeEvent(input$ml_commune_dep, {
    req(input$ml_commune_dep)
    axes_dep <- flux_enrichi |> mutate(cd = extraire_commune(commune)) |>
      filter(cd == input$ml_commune_dep) |> select(id_axe, nom_axe) |> distinct() |> arrange(id_axe)
    updateSelectInput(session, "ml_axe_dep", choices = setNames(axes_dep$id_axe, axes_dep$nom_axe))
  })
  observeEvent(input$ml_commune_arr, {
    req(input$ml_commune_arr)
    axes_arr <- flux_enrichi |> mutate(cd = extraire_commune(commune)) |>
      filter(cd == input$ml_commune_arr) |> select(id_axe, nom_axe) |> distinct() |> arrange(id_axe)
    updateSelectInput(session, "ml_axe_arr", choices = setNames(axes_arr$id_axe, axes_arr$nom_axe))
  })
  
  observeEvent(input$graphe_communes_vis_selected, {
    sel <- input$graphe_communes_vis_selected
    if (is.null(sel) || sel == "") return()
    g <- graphe_communes$graphe; m <- graphe_communes$metriques |> filter(commune == sel)
    if (nrow(m) == 0) return()
    showNotification(sprintf("%s — Entrant : %d | Sortant : %d | Interm. : %.3f",
                             sel, degree(g, v = sel, mode = "in"), degree(g, v = sel, mode = "out"), m$betweenness),
                     type = "message", duration = 5)
  })
  
  # ==========================================================================
  # ONGLET 1 — ACCUEIL (SARA)
  # ==========================================================================
  observeEvent(input$go_carte, { updateTabItems(session, "main_tabs", "carte") })
  observeEvent(input$go_rapport, { showNotification("Rapport Quarto à générer", type = "message") })
  observeEvent(input$nav_carte, { updateTabItems(session, "main_tabs", "carte") })
  observeEvent(input$nav_trafic, { updateTabItems(session, "main_tabs", "trafic") })
  observeEvent(input$nav_exploration, { updateTabItems(session, "main_tabs", "exploration") })
  observeEvent(input$nav_reseau, { updateTabItems(session, "main_tabs", "reseau") })
  observeEvent(input$nav_ml, { updateTabItems(session, "main_tabs", "ml_pred") })
  observeEvent(input$nav_reco, { updateTabItems(session, "main_tabs", "recommandations") })
  
  output$kpi_axes_bloques <- renderText({
    if (nrow(flux_enrichi) == 0) return("—")
    flux_enrichi |> filter(niveau_cong == "Bloque") |> pull(id_axe) |> n_distinct() |> as.character()
  })
  
  output$etat_temps_reel <- renderUI({
    if (nrow(flux_enrichi) == 0) return(tags$div(class = "empty-state", tags$p("En attente.")))
    df <- flux_enrichi |> group_by(commune) |>
      summarise(indice = mean(indice_cong, na.rm = TRUE), .groups = "drop") |> arrange(indice) |> head(8)
    tags$div(class = "bars", lapply(seq_len(nrow(df)), function(i) {
      ind <- df$indice[i]; pct <- min(100, max(5, round((1 - ind) * 100)))
      coul <- if (ind < 0.3) COULEURS$rouge else if (ind < 0.5) COULEURS$orange
      else if (ind < 0.8) COULEURS$jaune else COULEURS$vert
      tags$div(class = "bar-row", tags$span(class = "bar-label", df$commune[i]),
               tags$div(class = "bar-track", tags$div(class = "bar-fill",
                                                      style = sprintf("width:%d%%;background:%s;", pct, coul))),
               tags$span(class = "bar-value", sprintf("%d %%", pct)))
    }))
  })
  
  # ==========================================================================
  # ONGLET 2 — CARTE (SARA) — code complet de Sara
  # ==========================================================================
  output$carte_principale <- renderLeaflet({
    if (input$carte_indice == "disparite") {
      df_carte <- indice_disparite |> select(commune, valeur = indice_disparite, n_mesures, population)
      titre_legende <- "Impact humain"; domaine_pal <- c(0, max(df_carte$valeur, na.rm = TRUE))
    } else {
      df_carte <- indice_disparite |> select(commune, valeur = indice_cong_moyen, n_mesures, population)
      titre_legende <- "Niveau bouchons"; domaine_pal <- c(0.3, 0.7)
    }
    geo_cong <- communes_geo |> left_join(df_carte, by = "commune")
    pal <- colorNumeric(palette = c("#0F9D58","#F4B400","#DB4437"), domain = domaine_pal, na.color = "#CCCCCC")
    geo_cong$couleur <- pal(geo_cong$valeur)
    centroides_sf <- sf::st_centroid(geo_cong); coords <- sf::st_coordinates(centroides_sf)
    centroides <- data.frame(lng = coords[,"X"], lat = coords[,"Y"], commune = geo_cong$commune, valeur = geo_cong$valeur)
    centroides$label_text <- ifelse(is.na(centroides$valeur), centroides$commune,
                                    paste0(centroides$commune, " · ", round(centroides$valeur, 2)))
    m <- leaflet() |> addProviderTiles(providers$CartoDB.Positron)
    for (i in seq_len(nrow(geo_cong))) {
      one_geo <- geojsonsf::sf_geojson(geo_cong[i, ])
      m <- m |> addGeoJSON(one_geo, weight = 2, color = "#0F2E1F", fillColor = geo_cong$couleur[i], fillOpacity = 0.65)
    }
    m |> addLabelOnlyMarkers(data = centroides, lng = ~lng, lat = ~lat, label = ~label_text,
                             labelOptions = labelOptions(noHide = TRUE, direction = "center", textOnly = TRUE,
                                                         style = list("color" = "#0F2E1F", "font-size" = "12px", "font-weight" = "600",
                                                                      "text-shadow" = "1px 1px 2px white, -1px -1px 2px white"))) |>
      addLegend(position = "bottomright", pal = pal, values = domaine_pal, title = titre_legende, opacity = 0.85) |>
      setView(lng = -4.01, lat = 5.36, zoom = 11)
  })
  
  output$carte_lecture <- renderUI({
    if (input$carte_indice == "disparite") {
      top <- indice_disparite |> arrange(desc(indice_disparite)) |> slice(1:2)
      tags$div(
        tags$p(tags$strong(top$commune[1]), " et ", tags$strong(top$commune[2]),
               " sont les communes où la congestion touche le plus de monde."),
        tags$p("L'indice pondère par la population : une commune peu congestionnée
               mais très peuplée peut avoir un impact plus fort qu'une commune très
               embouteillée mais peu habitée."))
    } else {
      top <- indice_disparite |> arrange(desc(indice_cong_moyen)) |> slice(1:2)
      bas <- indice_disparite |> arrange(indice_cong_moyen) |> slice(1)
      tags$div(
        tags$p(tags$strong(top$commune[1]), " et ", tags$strong(top$commune[2]),
               " sont les communes où la circulation est la plus difficile."),
        tags$p(tags$strong(bas$commune), " reste fluide — mais elle bénéficie de voies larges
               et d'un trafic dilué hors heures de bureau."))
    }
  })
  
  observeEvent(input$btn_itin, {
    req(nzchar(input$itin_depart), nzchar(input$itin_arrivee))
    loader_carte$show()
    coords_dep <- communes_geo |> filter(commune == input$itin_depart)
    coords_arr <- communes_geo |> filter(commune == input$itin_arrivee)
    if (nrow(coords_dep) == 0 || nrow(coords_arr) == 0) {
      loader_carte$hide()
      output$resultat_itin <- renderUI({ tags$div(class = "itin-result", tags$p(class = "muted-small", "Commune introuvable.")) })
      return()
    }
    centro_dep <- sf::st_centroid(coords_dep) |> sf::st_coordinates()
    centro_arr <- sf::st_centroid(coords_arr) |> sf::st_coordinates()
    route <- tryCatch(osrm::osrmRoute(src = c(centro_dep[1,"X"], centro_dep[1,"Y"]),
                                      dst = c(centro_arr[1,"X"], centro_arr[1,"Y"]), overview = "full",
                                      osrm.server = "https://routing.openstreetmap.de/routed-car/", osrm.profile = "car"), error = function(e) NULL)
    loader_carte$hide()
    if (is.null(route)) {
      output$resultat_itin <- renderUI({ tags$div(class = "itin-result", tags$p("OSRM indisponible.")) })
      return()
    }
    leafletProxy("carte_principale") |> clearGroup("itineraire") |>
      addPolylines(data = route, color = "#0F2E1F", weight = 5, opacity = 0.9, group = "itineraire") |>
      addCircleMarkers(lng = centro_dep[1,"X"], lat = centro_dep[1,"Y"], radius = 8,
                       color = "#E8721C", fillColor = "#E8721C", fillOpacity = 1, weight = 2, group = "itineraire") |>
      addCircleMarkers(lng = centro_arr[1,"X"], lat = centro_arr[1,"Y"], radius = 8,
                       color = "#0F9D58", fillColor = "#0F9D58", fillOpacity = 1, weight = 2, group = "itineraire")
    output$resultat_itin <- renderUI({
      tags$div(class = "itin-result",
               tags$p(tags$strong(input$itin_depart), " → ", tags$strong(input$itin_arrivee)),
               tags$p(class = "itin-stats", tags$span(tags$b(round(route$distance, 1)), " km"), " · ",
                      tags$span(tags$b(round(route$duration)), " min en circulation fluide")),
               tags$p(class = "muted-small", "OSRM via OpenStreetMap"))
    })
  })
  
  # ==========================================================================
  # ONGLET 3 — TRAFIC (SARA)
  # ==========================================================================
  data_courbe <- reactive({
    req(nrow(flux_enrichi) > 0, length(input$trafic_communes) > 0)
    flux_enrichi |> filter(commune %in% input$trafic_communes) |>
      group_by(commune, heure) |> summarise(ic_summary(.data[[input$trafic_y]]), .groups = "drop")
  })
  output$courbe_journaliere <- renderPlotly({
    validate(need(nrow(flux_enrichi) > 0, "Dataset pas encore disponible"))
    validate(need(length(input$trafic_communes) > 0, "Sélectionnez au moins une commune"))
    df <- data_courbe()
    label_y <- if (input$trafic_y == "vitesse_kmh") "Vitesse (km/h)" else "Indice de fluidité"
    plot_ly(df, x = ~heure, color = ~commune, colors = "Set2") |>
      add_ribbons(ymin = ~ic_lo, ymax = ~ic_hi, line = list(width = 0), opacity = 0.2, showlegend = FALSE) |>
      add_lines(y = ~moy, line = list(width = 2.5)) |>
      layout(xaxis = list(title = "Heure", dtick = 2), yaxis = list(title = label_y),
             hovermode = "x unified", plot_bgcolor = "#FFFFFF", paper_bgcolor = "#FFFFFF",
             margin = list(t = 30, l = 50, r = 30, b = 40))
  })
  output$interpretation_courbe <- renderUI({
    if (nrow(flux_enrichi) == 0) return(NULL)
    df <- data_courbe(); if (nrow(df) == 0) return(NULL)
    pire <- df |> arrange(moy) |> slice(1); meilleur <- df |> arrange(desc(moy)) |> slice(1)
    ecart_pct <- round((1 - pire$moy / meilleur$moy) * 100)
    unite <- if (input$trafic_y == "vitesse_kmh") "km/h" else ""
    tags$div(
      tags$p(sprintf("À %dh, %s chute à %.1f %s — soit %d%% en dessous de la valeur nocturne (%.1f %s à %dh).",
                     pire$heure, pire$commune, pire$moy, unite, abs(ecart_pct), meilleur$moy, unite, meilleur$heure)),
      tags$p("Les pics à 8h et 17h correspondent aux flux domicile→travail.
             Cette symétrie matin/soir est la signature d'une congestion structurelle."))
  })
  output$heatmap_hebdo <- renderPlotly({
    validate(need(nrow(flux_enrichi) > 0, "Dataset pas encore disponible"))
    fn <- match.fun(input$heat_aggreg)
    df <- flux_enrichi |> group_by(commune, heure) |> summarise(val = fn(vitesse_kmh, na.rm = TRUE), .groups = "drop")
    plot_ly(df, x = ~heure, y = ~commune, z = ~val, type = "heatmap", colors = "RdYlGn",
            hovertemplate = "Commune: %{y}<br>Heure: %{x}h<br>Vitesse: %{z:.1f}<extra></extra>") |>
      layout(xaxis = list(title = "Heure"), yaxis = list(title = ""), margin = list(t = 30, l = 100, r = 30, b = 40))
  })
  output$interpretation_heatmap <- renderUI({
    if (nrow(flux_enrichi) == 0) return(NULL)
    fn <- match.fun(input$heat_aggreg)
    df <- flux_enrichi |> group_by(commune, heure) |> summarise(val = fn(vitesse_kmh, na.rm = TRUE), .groups = "drop")
    pire <- df |> arrange(val) |> slice(1)
    pire_c <- df |> group_by(commune) |> summarise(v = mean(val, na.rm = TRUE), .groups = "drop") |> arrange(v) |> slice(1)
    tags$div(
      tags$p(sprintf("Le pire moment est %s à %dh (%.1f km/h). %s est la commune la plus impactée en moyenne.",
                     pire$commune, pire$heure, pire$val, pire_c$commune)),
      tags$p("Les colonnes 8h et 17h sont uniformément rouges — c'est un phénomène à l'échelle de toute la métropole."))
  })
  output$barplot_pires <- renderPlotly({
    validate(need(nrow(flux_enrichi) > 0, "Dataset pas encore disponible"))
    validate(need(length(input$comp_axes) > 0, "Sélectionnez au moins un axe"))
    df <- flux_enrichi |> filter(id_axe %in% input$comp_axes, heure == input$comp_heure) |>
      group_by(id_axe, nom_axe) |> summarise(v = mean(vitesse_kmh, na.rm = TRUE), .groups = "drop") |> arrange(v)
    plot_ly(df, x = ~v, y = ~reorder(nom_axe, v), type = "bar", orientation = "h",
            marker = list(color = COULEURS$orange)) |>
      layout(xaxis = list(title = "Vitesse (km/h)"), yaxis = list(title = ""), margin = list(t = 30, l = 120, r = 30, b = 40))
  })
  output$interpretation_comparateur <- renderUI({
    if (nrow(flux_enrichi) == 0 || length(input$comp_axes) == 0) return(NULL)
    df <- flux_enrichi |> filter(id_axe %in% input$comp_axes, heure == input$comp_heure) |>
      group_by(id_axe, nom_axe) |> summarise(v = mean(vitesse_kmh, na.rm = TRUE), .groups = "drop") |> arrange(v)
    if (nrow(df) == 0) return(NULL)
    pire <- df |> slice(1); meilleur <- df |> slice(n())
    tags$div(
      tags$p(tags$strong(pire$nom_axe), sprintf(" est le plus lent (%.1f km/h) contre %.1f km/h pour ",
                                                pire$v, meilleur$v), tags$strong(meilleur$nom_axe), "."),
      tags$p("Tous les axes ne se valent pas à la même heure. Certains ont une géométrie
             défavorable (pont, voie unique) qui crée une congestion permanente."))
  })
  
  # ==========================================================================
  # ONGLET 4 — EXPLORATION (SARA)
  # ==========================================================================
  data_explo <- reactive({
    req(nrow(flux_enrichi) > 0); df <- flux_enrichi
    if (input$explo_niveau != "Tous") df <- df |> filter(niveau_cong == input$explo_niveau)
    df
  })
  output$expl_boxplot <- renderPlotly({
    validate(need(nrow(flux_enrichi) > 0, "Dataset pas encore disponible"))
    p <- ggplot(data_explo(), aes(x = reorder(commune, vitesse_kmh, FUN = median), y = vitesse_kmh, fill = commune)) +
      geom_boxplot(outlier.shape = if (input$explo_outliers) 16 else NA, outlier.alpha = 0.3) +
      coord_flip() + labs(x = NULL, y = "Vitesse (km/h)") +
      theme_minimal(base_size = 11) + theme(legend.position = "none", panel.grid.minor = element_blank())
    ggplotly(p) |> layout(margin = list(t = 20, l = 100, r = 20, b = 40))
  })
  output$interpretation_boxplot <- renderUI({
    if (nrow(flux_enrichi) == 0) return(NULL)
    stats <- data_explo() |> group_by(commune) |>
      summarise(med = median(vitesse_kmh, na.rm = TRUE), iqr = IQR(vitesse_kmh, na.rm = TRUE), .groups = "drop") |> arrange(med)
    pire <- stats |> slice(1); disp <- stats |> arrange(desc(iqr)) |> slice(1)
    tags$div(
      tags$p(tags$strong(pire$commune), sprintf(" a la médiane la plus basse (%.1f km/h) — congestion chronique.", pire$med)),
      tags$p(tags$strong(disp$commune), sprintf(" a la plus grande variabilité (IQR = %.1f km/h) — tantôt fluide, tantôt bloquée.
             Une commune stable mais lente = problème d'infrastructure. Une commune variable = besoin d'info temps réel.", disp$iqr)))
  })
  output$expl_histo <- renderPlotly({
    validate(need(nrow(flux_enrichi) > 0, "Dataset pas encore disponible")); req(input$explo_commune)
    df <- flux_enrichi |> filter(commune == input$explo_commune); ic <- ic95(df$vitesse_kmh)
    p <- ggplot(df, aes(x = vitesse_kmh)) +
      geom_histogram(bins = input$explo_bins, fill = COULEURS$orange, alpha = 0.8, color = "white") +
      geom_vline(xintercept = ic$moy, color = COULEURS$gris, linewidth = 0.8) +
      geom_vline(xintercept = c(ic$ic_lo, ic$ic_hi), color = COULEURS$bleu, linetype = "dashed", linewidth = 0.6) +
      labs(x = "Vitesse (km/h)", y = "Effectif", title = paste("Commune :", input$explo_commune)) +
      theme_minimal(base_size = 11) + theme(panel.grid.minor = element_blank())
    ggplotly(p)
  })
  output$interpretation_histo <- renderUI({
    if (nrow(flux_enrichi) == 0) return(NULL); req(input$explo_commune)
    df <- flux_enrichi |> filter(commune == input$explo_commune); if (nrow(df) < 5) return(NULL)
    ic <- ic95(df$vitesse_kmh); cv <- round(sd(df$vitesse_kmh, na.rm = TRUE) / ic$moy * 100)
    tags$div(
      tags$p(sprintf("Vitesse moyenne = %.1f km/h, IC 95%% : [%.1f ; %.1f]. Variabilité (CV) = %d%%.",
                     ic$moy, ic$ic_lo, ic$ic_hi, cv)),
      tags$p(if (cv > 40) "Forte variabilité : cette commune alterne entre moments fluides et bloqués."
             else "Variabilité modérée : la congestion y est régulière et prévisible."))
  })
  output$expl_ic_text <- renderText({
    if (nrow(flux_enrichi) == 0 || !nzchar(input$explo_commune)) return("—")
    ic <- ic95(flux_enrichi |> filter(commune == input$explo_commune) |> pull(vitesse_kmh))
    format_ic(ic, "km/h")
  })
  output$expl_stats_table <- renderDT({
    validate(need(nrow(flux_enrichi) > 0, "Dataset pas encore disponible"))
    stats <- flux_enrichi |> group_by(commune) |> summarise(ic_summary(vitesse_kmh), .groups = "drop") |>
      mutate(across(c(moy, se, ic_lo, ic_hi), ~round(.x, 2))) |>
      transmute(Commune = commune, `Moyenne` = moy, `IC bas` = ic_lo, `IC haut` = ic_hi, n = n)
    datatable(stats, options = list(pageLength = 13, dom = "t", searching = FALSE), rownames = FALSE)
  })
  output$test_resultat <- renderUI({
    req(input$test_commune_a, input$test_commune_b)
    a <- input$test_commune_a; b <- input$test_commune_b
    if (a == b) return(tags$div(class = "test-result test-neutral", tags$p("Sélectionnez deux communes différentes.")))
    v_a <- flux_enrichi |> filter(commune == a) |> pull(vitesse_kmh)
    v_b <- flux_enrichi |> filter(commune == b) |> pull(vitesse_kmh)
    if (length(v_a) < 3 || length(v_b) < 3) return(tags$div(class = "test-result test-neutral", tags$p("Pas assez de données.")))
    res <- tryCatch(wilcox.test(v_a, v_b, exact = FALSE), error = function(e) NULL)
    if (is.null(res)) return(tags$div(class = "test-result test-neutral", tags$p("Test impossible.")))
    p <- res$p.value; m_a <- round(mean(v_a, na.rm = TRUE), 1); m_b <- round(mean(v_b, na.rm = TRUE), 1)
    ecart <- abs(m_a - m_b); plus_rapide <- if (m_a > m_b) a else b
    verdict_class <- if (p < 0.001) "test-strong" else if (p < 0.05) "test-significant" else "test-ns"
    verdict_text <- if (p < 0.05) {
      sprintf("L'écart entre <b>%s</b> et <b>%s</b> est significatif (p = %s). <b>%s</b> roule %.1f km/h plus vite.",
              a, b, signif(p, 3), plus_rapide, ecart)
    } else {
      sprintf("L'écart entre <b>%s</b> et <b>%s</b> n'est pas significatif (p = %s). Différence due au hasard.", a, b, signif(p, 3))
    }
    tags$div(class = paste("test-result", verdict_class), tags$p(HTML(verdict_text)),
             tags$p(class = "test-meta", sprintf("Wilcoxon · %s = %.1f km/h · %s = %.1f km/h · p = %s", a, m_a, b, m_b, signif(p, 3))))
  })
  
  # ==========================================================================
  # ONGLET 5 — RÉSEAU (AXELLE)
  # ==========================================================================
  output$graphe_communes_vis <- renderVisNetwork({
    req(!is.null(graphe_communes))
    g <- graphe_communes$graphe; vn <- toVisNetworkData(g)
    vn$edges <- vn$edges |> filter(flux >= input$seuil_flux)
    deg <- degree(g, mode = "all"); idx <- match(vn$nodes$id, V(g)$name)
    vn$nodes$size <- log(V(g)$population[idx] + 1) * 4
    if (input$metrique_choix == "groupe") {
      vn$nodes$group <- as.character(V(g)$group[idx])
    } else {
      deg_vals <- deg[V(g)$name[idx]]
      deg_norm <- (deg_vals - min(deg_vals)) / max(1, max(deg_vals) - min(deg_vals))
      vn$nodes$color <- sapply(deg_norm, function(d) {
        if (d >= 0.7) COULEURS$rouge else if (d >= 0.4) COULEURS$orange else COULEURS$vert })
      vn$nodes$group <- NULL
    }
    vn$nodes$label <- V(g)$name[idx]; vn$nodes$font.size <- 18
    vn$nodes$borderWidth <- 2; vn$nodes$shadow <- TRUE; vn$nodes$font.color <- "#000000"
    vn$nodes$title <- paste0("<b>", V(g)$name[idx], "</b><br>Pop: ",
                             format(V(g)$population[idx], big.mark = " "), "<br>Degré: ", deg[V(g)$name[idx]])
    visNetwork(vn$nodes, vn$edges, background = "#FAFAFA") |>
      visEdges(arrows = list(to = list(enabled = TRUE, scaleFactor = 0.5)),
               smooth = list(type = "curvedCW", roundness = 0.2),
               color = list(color = "rgba(200,200,200,0.6)", highlight = COULEURS$orange), width = 1.5) |>
      visOptions(highlightNearest = list(enabled = TRUE, degree = 1),
                 nodesIdSelection = list(enabled = TRUE, main = "Sélectionner une commune")) |>
      visPhysics(solver = "forceAtlas2Based",
                 forceAtlas2Based = list(gravitationalConstant = -300, centralGravity = 0.01,
                                         springLength = 150, springConstant = 0.08, damping = 0.4),
                 stabilization = list(enabled = TRUE, iterations = 1000)) |>
      visInteraction(dragNodes = TRUE, dragView = TRUE, zoomView = TRUE) |>
      visGroups(groupname = "0", color = list(background = "#AAAAAA", border = "#888888")) |>
      visGroups(groupname = "1", color = list(background = COULEURS$orange, border = "#c0520a")) |>
      visGroups(groupname = "2", color = list(background = COULEURS$vert, border = "#006b2e")) |>
      visGroups(groupname = "3", color = list(background = COULEURS$bleu, border = "#1a5276")) |>
      visGroups(groupname = "4", color = list(background = COULEURS$jaune, border = "#b07d00")) |>
      visLegend(position = "left", main = "Bassins", width = 0.08)
  })
  output$tableau_metriques <- renderDT({
    req(!is.null(graphe_communes)); g <- graphe_communes$graphe
    graphe_communes$metriques |>
      mutate(betweenness = round(betweenness, 3), closeness = ifelse(is.nan(closeness), NA, round(closeness, 3)),
             degre = degree(g, mode = "all")[commune], flux_entrant = degree(g, mode = "in")[commune],
             flux_sortant = degree(g, mode = "out")[commune],
             role = case_when(closeness >= 0.8 ~ "Hub central", flux_entrant >= 5 ~ "Pôle attracteur",
                              flux_sortant >= 3 ~ "Pôle générateur", is.na(closeness) ~ "Isolée",
                              closeness >= 0.5 ~ "Connectée", TRUE ~ "Périphérique")) |>
      rename(Commune = commune, `Interm.` = betweenness, `Prox.` = closeness,
             Degré = degre, Entrant = flux_entrant, Sortant = flux_sortant, Rôle = role) |>
      datatable(options = list(pageLength = 5, dom = "tp", scrollX = TRUE, searching = FALSE), rownames = FALSE) |>
      formatStyle("Rôle", backgroundColor = styleEqual(
        c("Hub central","Isolée","Connectée","Périphérique"), c("#d4edda","#f8d7da","#fff3cd","#e2e3e5")))
  })
  output$modularite <- renderText({
    req(!is.null(graphe_communes)); mod <- graphe_communes$modularite
    q <- if (mod >= 0.3) "structure bien définie" else if (mod >= 0.15) "structure émergente" else "structure faible"
    sprintf("Modularité : %.3f — %s", mod, q)
  })
  output$communautes_resume <- renderUI({
    req(!is.null(graphe_communes))
    n <- length(unique(membership(graphe_communes$communautes)))
    tags$div(tags$p(class = "muted-small", sprintf("%d bassins détectés.", n)),
             tags$p(class = "muted-small", "Même couleur = même bassin."))
  })
  output$interpretation_reseau <- renderUI({
    req(!is.null(graphe_communes))
    m <- graphe_communes$metriques |> arrange(desc(betweenness)); top <- m |> slice(1:3)
    mod <- graphe_communes$modularite; g <- graphe_communes$graphe
    top_e <- m |> arrange(desc(flux_entrant)) |> slice(1); top_s <- m |> arrange(desc(flux_sortant)) |> slice(1)
    tags$div(
      tags$p(tags$strong(top$commune[1]), sprintf(" contrôle le réseau : %.1f%% des trajets passent par cette commune.
        Si elle est bloquée, tout le réseau l'est aussi.", top$betweenness[1] * 100)),
      tags$p(sprintf("Chaque matin, %s reçoit %d flux entrants pendant que %s en envoie %d.
        Ce déséquilibre crée un goulot d'étranglement.", top_e$commune, top_e$flux_entrant, top_s$commune, top_s$flux_sortant)),
      tags$p(sprintf("Sur %d communes, %d liaisons. Modularité %.3f = pas de sous-réseaux indépendants.",
                     vcount(g), ecount(g), mod)))
  })
  output$interpretation_tableau <- renderUI({
    req(!is.null(graphe_communes)); m <- graphe_communes$metriques
    hubs <- m |> filter(closeness >= 0.8) |> pull(commune)
    gen <- m |> filter(flux_sortant >= 5) |> pull(commune)
    iso <- m |> filter(degre <= 2) |> pull(commune)
    tags$div(
      if (length(hubs) > 0) tags$p(tags$strong("Hubs centraux : "), paste(hubs, collapse = ", "),
                                   " — accessibles partout, mais saturent vite."),
      if (length(gen) > 0) tags$p(tags$strong("Grands émetteurs : "), paste(gen, collapse = ", "),
                                  " — génèrent les déplacements du matin."),
      if (length(iso) > 0) tags$p(tags$strong("Enclavées : "), paste(iso, collapse = ", "),
                                  " — peu de connexions, trajets rallongés."))
  })
  
  # ==========================================================================
  # ONGLET 6 — ML (AXELLE)
  # ==========================================================================
  # mod_rf déjà chargé dans global.R
  
  observeEvent(input$ml_predire, {
    req(input$ml_axe_dep, input$ml_axe_arr, input$ml_heure, input$ml_date)
    shinyjs::disable("ml_predire")
    withProgress(message = "Prédiction en cours", value = 0, {
      incProgress(0.2, detail = "Axes…")
      axe_dep <- flux_enrichi |> filter(id_axe == input$ml_axe_dep) |> slice(1)
      axe_arr <- flux_enrichi |> filter(id_axe == input$ml_axe_arr) |> slice(1)
      incProgress(0.3, detail = "OSRM…")
      options(osrm.server = "https://router.project-osrm.org/")
      route <- tryCatch(osrm::osrmRoute(src = c(axe_dep$lon_dep, axe_dep$lat_dep),
                                        dst = c(axe_arr$lon_dep, axe_arr$lat_dep)), error = function(e) NULL)
      dist_m <- if (!is.null(route)) route$distance * 1000 else mean(flux_enrichi$distance_m, na.rm = TRUE)
      vit_ref <- mean(axe_dep$vitesse_libre_ref, na.rm = TRUE)
      id_axe_num <- as.numeric(factor(flux_enrichi$id_axe))[which(flux_enrichi$id_axe == input$ml_axe_dep)[1]]
      jour_num <- as.numeric(factor(tolower(weekdays(input$ml_date)),
                                    levels = c("lundi","mardi","mercredi","jeudi","vendredi","samedi","dimanche")))
      if (is.na(jour_num)) jour_num <- 1
      newdata <- data.frame(heure = input$ml_heure, distance_m = dist_m,
                            vitesse_libre_ref = vit_ref, id_axe_num = id_axe_num, jour_num = jour_num)
      incProgress(0.3, detail = "Random Forest…")
      pred <- if (!is.null(mod_rf)) predict(mod_rf, newdata = newdata) else NULL
      shinyjs::enable("ml_predire")
      output$resultat_prediction <- renderUI({
        if (is.null(pred)) return(card(title = "Prédiction", tags$p("Modèle non disponible.")))
        val <- round(as.numeric(pred), 1); rmse <- 3.91
        ic_txt <- sprintf("[%.1f ; %.1f] km/h", val - 1.96*rmse, val + 1.96*rmse)
        niveau <- if (val >= 40) "Fluide" else if (val >= 25) "Modéré" else if (val >= 15) "Congestionné" else "Bloqué"
        duree_min <- round((dist_m / 1000) / val * 60)
        heure_dep_dt <- as.POSIXct(paste(input$ml_date, sprintf("%02d:00", input$ml_heure)))
        heure_arr_txt <- format(heure_dep_dt + duree_min * 60, "%Hh%M")
        commune_dep <- extraire_commune(axe_dep$commune); commune_arr <- extraire_commune(axe_arr$commune)
        card(title = "Résultat de la prédiction", fluidRow(
          column(6, tags$p(tags$strong("Axe départ : "), axe_dep$nom_axe),
                 tags$p(tags$strong("Axe arrivée : "), axe_arr$nom_axe),
                 tags$p(tags$strong("Trajet : "), paste(commune_dep, "→", commune_arr)),
                 tags$p(tags$strong("Distance : "), sprintf("%.1f km", dist_m/1000)),
                 tags$p(tags$strong("Date : "), format(input$ml_date, "%d/%m/%Y")),
                 tags$p(tags$strong("Jour : "), weekdays(input$ml_date)),
                 tags$p(tags$strong("Départ : "), paste0(input$ml_heure, "h00")),
                 tags$p(tags$strong("Arrivée est. : "), heure_arr_txt),
                 tags$p(tags$strong("Durée est. : "), paste0(duree_min, " min"))),
          column(6, tags$div(class = "kpi-value", paste0(val, " km/h")),
                 tags$p(class = "muted-small", tags$strong("Niveau : "), niveau),
                 tags$p(class = "muted-small", tags$strong("IC 95% : "), ic_txt),
                 tags$p(class = "muted-small", tags$strong("Modèle : "), "Random Forest"),
                 tags$p(class = "muted-small", tags$strong("Distance : "), "OSRM"))))
      })
    })
  })
  output$interpretation_prediction <- renderUI({
    if (is.null(input$ml_predire) || input$ml_predire == 0)
      return(tags$p("Choisissez un trajet, une date et une heure puis cliquez sur Prédire."))
    req(input$ml_axe_dep, input$ml_axe_arr)
    axe_dep <- flux_enrichi |> filter(id_axe == input$ml_axe_dep) |> slice(1)
    vit_libre <- mean(axe_dep$vitesse_libre_ref, na.rm = TRUE); heure <- input$ml_heure
    commune_d <- extraire_commune(axe_dep$commune)
    commune_a <- extraire_commune((flux_enrichi |> filter(id_axe == input$ml_axe_arr) |> slice(1))$commune)
    moment <- if (heure %in% c(7,8,9)) "en pleine heure de pointe du matin"
    else if (heure %in% c(16,17,18)) "en pleine heure de pointe du soir"
    else if (heure %in% c(0:5, 22,23)) "en heure creuse (nuit)" else "en dehors des heures de pointe"
    conseil <- if (heure %in% c(7,8,9,16,17,18))
      "En partant 2h plus tôt ou plus tard, vous pourriez diviser votre temps de trajet par deux."
    else sprintf("Vous voyagez %s — c'est le bon moment.", moment)
    tags$div(tags$p(sprintf("Ce trajet %s → %s est prédit %s. Vitesse sans trafic : %.0f km/h.",
                            commune_d, commune_a, moment, vit_libre)), tags$p(conseil))
  })
  output$interpretation_modeles <- renderUI({
    comp <- tryCatch(read_csv("outputs/comparaison_modeles.csv", show_col_types = FALSE), error = function(e) NULL)
    if (is.null(comp)) return(tags$p("Données non disponibles."))
    best <- comp |> arrange(RMSE) |> slice(1); worst <- comp |> arrange(desc(RMSE)) |> slice(1)
    tags$div(
      tags$p(tags$strong(best$Modele), sprintf(" = meilleur (erreur moyenne %.1f km/h, explique %.0f%% des variations).",
                                               best$MAE, best$R2 * 100)),
      tags$p(tags$strong("L'heure"), " est la variable la plus déterminante. Le jour a peu d'effet : congestion quotidienne."),
      tags$p(tags$strong(worst$Modele), sprintf(" échoue (R² = %.2f) : la congestion n'est pas linéaire.", worst$R2)))
  })
  output$comparaison_modeles <- renderTable({
    tryCatch(read_csv("outputs/comparaison_modeles.csv", show_col_types = FALSE),
             error = function(e) data.frame(Modele = c("Random Forest","Arbre","k-NN","LM"),
                                            RMSE = c(3.91,4.69,5.78,7.54), R2 = c(0.85,0.73,0.59,0.29), MAE = c(3.08,3.70,4.64,6.11)))
  })
  output$importance_vars <- renderPlot({
    if (is.null(mod_rf)) { plot.new(); title("RF non disponible"); return() }
    randomForest::varImpPlot(mod_rf, main = "Importance des variables", col = COULEURS$orange, pch = 19)
  })
  output$clustering_acp <- renderPlot({
    cl <- tryCatch(readRDS("models/clustering.rds"), error = function(e) NULL)
    if (is.null(cl)) { plot.new(); title("Clustering non disponible"); return() }
    ggplot(cl$coords, aes(x = Dim.1, y = Dim.2, color = profil, label = commune)) +
      geom_point(size = 6) + ggrepel::geom_label_repel(size = 4, show.legend = FALSE, fontface = "bold") +
      scale_color_manual(values = c("Très congestionné" = "#DC2626",
                                    "Moyennement congestionné" = "#F47920", "Peu congestionné" = "#009A44")) +
      labs(title = "Profils — k-means + ACP", x = "Axe 1 (congestion)", y = "Axe 2 (variabilité)", color = "Profil") +
      theme_minimal(base_size = 13)
  })
  output$clustering_interpretation <- renderUI({
    cl <- tryCatch(readRDS("models/clustering.rds"), error = function(e) NULL)
    if (is.null(cl)) return(tags$p("Non disponible"))
    profils_list <- split(cl$coords$commune, cl$coords$profil)
    tags$div(lapply(names(profils_list), function(p) {
      couleur <- switch(p, "Très congestionné" = COULEURS$rouge,
                        "Moyennement congestionné" = COULEURS$orange, "Peu congestionné" = COULEURS$vert, COULEURS$gris)
      tags$div(style = "margin-bottom:10px;",
               tags$p(style = sprintf("color:%s;font-weight:600;margin-bottom:2px;", couleur), paste0("● ", p)),
               tags$p(style = "font-size:13px;color:#555;margin:0;", paste(profils_list[[p]], collapse = ", ")))
    }))
  })
  output$clustering_synthese <- renderUI({
    cl <- tryCatch(readRDS("models/clustering.rds"), error = function(e) NULL)
    if (is.null(cl)) return(NULL)
    df <- cl$df; acp <- cl$acp; var1 <- round(acp$eig[1,2], 1); var2 <- round(acp$eig[2,2], 1)
    contrib <- as.data.frame(acp$var$contrib)
    top1 <- rownames(contrib)[which.max(contrib[,1])]; top2 <- rownames(contrib)[which.max(contrib[,2])]
    trad <- c(indice_moy = "l'indice de congestion", vitesse_pointe = "la vitesse en heure de pointe",
              variabilite = "la variabilité des vitesses")
    pire <- df |> arrange(indice_moy) |> slice(1); mieux <- df |> arrange(desc(indice_moy)) |> slice(1)
    n_tres <- as.numeric(sum(cl$coords$profil == "Très congestionné")); n_total <- as.numeric(nrow(cl$coords))
    tags$div(
      tags$p(tags$strong(sprintf("Axe 1 (%s%%)", var1)), sprintf(" sépare selon %s. Gauche = plus congestionné.", trad[top1])),
      tags$p(tags$strong(sprintf("Axe 2 (%s%%)", var2)), sprintf(" différencie selon %s. Haut = imprévisible.", trad[top2])),
      tags$p("À ", tags$strong(pire$commune_dep), sprintf(" on roule à %.0f km/h en pointe. À ", pire$vitesse_pointe),
             tags$strong(mieux$commune_dep), sprintf(" = %.0f km/h mais moins bien desservie.", mieux$vitesse_pointe)),
      tags$p(tags$strong(sprintf("%.0f communes sur %.0f", n_tres, n_total)),
             " sont « très congestionnées ». La congestion touche la grande majorité du réseau."))
  })
  
  # ==========================================================================
  # ONGLET 7 — DONNÉES (AXELLE)
  # ==========================================================================
  donnees_filtrees <- reactive({
    stops_df <- if (!is.null(gtfs)) {
      gtfs |> distinct(stop_name, stop_lat, stop_lon, agency_name, route_long_name) |>
        rename(`Arrêt` = stop_name, Latitude = stop_lat, Longitude = stop_lon, Agence = agency_name, Ligne = route_long_name)
    } else { tibble(message = "GTFS non disponible") }
    df <- switch(input$dt_dataset, "flux" = flux_enrichi, "communes" = communes_wiki, "stops" = stops_df)
    if (input$dt_dataset == "flux" && nrow(df) > 0) {
      if (input$dt_filtre_commune != "all") df <- df |> filter(commune == input$dt_filtre_commune)
      df <- df |> filter(heure >= input$dt_filtre_heure[1], heure <= input$dt_filtre_heure[2])
    }
    df
  })
  output$table_principale <- renderDT({
    datatable(donnees_filtrees(), options = list(pageLength = 10, scrollX = TRUE),
              rownames = FALSE, filter = "top", class = "compact stripe")
  })
  output$dt_n_lignes <- renderText({ format(nrow(donnees_filtrees()), big.mark = " ") })
  output$dt_vit_moy <- renderText({
    df <- donnees_filtrees()
    if (!"vitesse_kmh" %in% names(df) || nrow(df) == 0) return("—")
    paste0(round(mean(df$vitesse_kmh, na.rm = TRUE), 1), " km/h")
  })
  output$dt_pct_bloque <- renderText({
    df <- donnees_filtrees()
    if (!"niveau_cong" %in% names(df) || nrow(df) == 0) return("—")
    paste0(round(mean(df$niveau_cong == "Bloque", na.rm = TRUE) * 100, 1), " %")
  })
  output$dt_axe_pire <- renderText({
    df <- donnees_filtrees()
    if (!"id_axe" %in% names(df) || nrow(df) == 0) return("—")
    df |> group_by(id_axe) |> summarise(v = mean(vitesse_kmh, na.rm = TRUE), .groups = "drop") |>
      arrange(v) |> slice(1) |> pull(id_axe)
  })
  output$dt_export <- downloadHandler(
    filename = function() paste0("mobilite_", input$dt_dataset, "_", Sys.Date(), ".csv"),
    content = function(file) write_csv(donnees_filtrees(), file))
  output$source_note <- renderText({ "Sources : TomTom · GTFS DT4A · Wikipedia · OSM · Mai 2025" })
  output$description_dataset <- renderUI({
    if (input$dt_dataset == "flux") {
      tags$div(tags$p(tags$strong("flux_enrichi"), " — 4 430 obs × 16 colonnes. Chaque ligne = une mesure de vitesse."),
               tags$table(class = "table table-sm", style = "font-size:13px;",
                          tags$thead(tags$tr(tags$th("Colonne"), tags$th("Description"))),
                          tags$tbody(
                            tags$tr(tags$td(tags$code("id_axe")), tags$td("Identifiant unique (AX_01 à AX_R12)")),
                            tags$tr(tags$td(tags$code("vitesse_kmh")), tags$td("Vitesse enrichie par facteurs horaires")),
                            tags$tr(tags$td(tags$code("indice_cong")), tags$td("Ratio vitesse/vitesse_libre (bas = congestionné)")),
                            tags$tr(tags$td(tags$code("niveau_cong")), tags$td("Fluide / Modéré / Congestionné / Bloqué")),
                            tags$tr(tags$td(tags$code("heure")), tags$td("Heure de la mesure (0–23)")),
                            tags$tr(tags$td(tags$code("distance_m")), tags$td("Longueur du tronçon (mètres)")),
                            tags$tr(tags$td(tags$code("type")), tags$td("principal / complement / retour")))),
               tags$p(class = "muted-small", "Source : TomTom Traffic Flow API · 42 axes · 16 jours"))
    } else if (input$dt_dataset == "communes") {
      tags$div(tags$p(tags$strong("Communes"), " — 13 lignes. Population INS 2021."),
               tags$p(class = "muted-small", "Source : Wikipedia / INS Côte d'Ivoire"))
    } else {
      tags$div(tags$p(tags$strong("Arrêts GTFS"), " — 847 arrêts, SOTRA/Gbaka/Woro-woro."),
               tags$p(class = "muted-small", "Source : Data Transport (DT4A) · GTFS 2023"))
    }
  })
  output$nettoyage_dataset <- renderUI({
    if (input$dt_dataset == "flux") {
      tags$div(
        tags$p(tags$strong("Problème : "), "TomTom retourne des vitesses statiques pour Abidjan."),
        tags$p(tags$strong("Solution : "), "Enrichissement horaire (×0.38 à 8h/17h, ×1.0 la nuit) + pénalités par commune."),
        tags$p(tags$strong("Résultat : "), "11.5 km/h à 8h vs 30.7 km/h à 23h — cohérent avec le terrain."),
        tags$p(tags$strong("Lien : "), "Ces données permettent de quantifier les disparités entre communes."))
    } else if (input$dt_dataset == "communes") {
      tags$div(tags$p("Population INS 2021 pour pondérer : Yopougon (1.5M) ≠ Plateau (7 000)."),
               tags$p(tags$strong("Lien : "), "Congestion × population = nombre de personnes affectées."))
    } else {
      tags$div(tags$p("847 arrêts GTFS = arêtes du graphe réseau inter-communes."),
               tags$p(tags$strong("Lien : "), "Révèle quelles communes sont bien desservies ou enclavées."))
    }
  })
  
  # ==========================================================================
  # ONGLET 8 — RECOMMANDATIONS (AXELLE)
  # ==========================================================================
  output$reco_constats <- renderUI({
    req(!is.null(graphe_communes))
    cl <- tryCatch(readRDS("models/clustering.rds"), error = function(e) NULL)
    pire_c <- ""; pire_v <- ""
    if (!is.null(cl)) { p <- cl$df |> arrange(indice_moy) |> slice(1); pire_c <- p$commune_dep; pire_v <- round(p$vitesse_pointe, 0) }
    tags$div(
      tags$p(tags$strong("1. Tout passe par Plateau."), " Une seule commune concentre la majorité des trajets. Pas de route alternative."),
      tags$p(tags$strong("2. Adjamé est un goulet d'étranglement."), " 2.8M d'habitants au nord n'ont qu'un seul passage vers le centre."),
      tags$p(tags$strong("3. Aux heures de pointe, on roule 3× moins vite."),
             sprintf(" 11.5 km/h à 8h vs 31 km/h la nuit. À %s : %s km/h.", pire_c, pire_v)),
      tags$p(tags$strong("4. Les communes ne sont pas égales."), " Plateau/Cocody = congestionnées toute la journée. Koumassi = quasi-coupée."),
      tags$p(tags$strong("5. Le problème est structurel."), " Mêmes axes, mêmes heures, chaque jour."))
  })
  output$reco_details <- renderUI({
    tags$table(class = "table", style = "font-size:14px;",
               tags$thead(tags$tr(tags$th("Priorité"), tags$th("Action"), tags$th("Pourquoi"), tags$th("Impact attendu"))),
               tags$tbody(
                 tags$tr(tags$td(tags$strong("1")), tags$td(tags$strong("Voies bus réservées Adjamé–Plateau")),
                         tags$td("Porte d'entrée vers Plateau pour 2.8M d'habitants du nord"), tags$td("Temps de traversée /2 en pointe")),
                 tags$tr(tags$td(tags$strong("2")), tags$td(tags$strong("Aménager l'échangeur d'Adjamé + 2ème accès Plateau")),
                         tags$td("6 flux convergent vers un seul point"), tags$td("Diviser le flux nord, -30% bouchons")),
                 tags$tr(tags$td(tags$strong("3")), tags$td(tags$strong("Bus express Yopougon–Cocody sans Plateau")),
                         tags$td("1.5M hab. envoient 9 flux via Plateau"), tags$td("Désengorger Plateau, trajet 1h → 35 min")),
                 tags$tr(tags$td(tags$strong("4")), tags$td(tags$strong("Connecter Koumassi au réseau")),
                         tags$td("Seule commune quasi-isolée sur 13"), tags$td("Accès pour 500 000 habitants")),
                 tags$tr(tags$td(tags$strong("5")), tags$td(tags$strong("Application mobile temps réel")),
                         tags$td("Axes imprévisibles (Adjamé, Abobo)"), tags$td("Éviter les axes bloqués"))))
  })
}