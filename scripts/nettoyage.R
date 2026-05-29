# ==============================================================================
# 06_nettoyage_enrichi.R
# Fusion + enrichissement des donnees TomTom avec patterns horaires realistes
# Sources patterns : OFT 2025, CitiProfile, AMUGA, etudes JICA Abidjan
# ==============================================================================

library(dplyr)
library(readr)
library(stringr)
library(purrr)
library(lubridate)

flux_principal  <- lire_fichiers("flux_principal_")
flux_complement <- lire_fichiers("flux_complement_")
flux_retour     <- lire_fichiers("flux_retour_")
flux_cumul      <- tryCatch(
  read_csv("data/raw/tomtom/flux_cumul.csv", show_col_types = FALSE,
           col_types = cols(timestamp = col_character(), date = col_character())),
  error = function(e) NULL
)

# ------------------------------------------------------------------------------
# 1. FUSION DE TOUS LES FICHIERS BRUTS
# ------------------------------------------------------------------------------
lire_fichiers <- function(pattern) {
  fichiers <- list.files("data/raw/tomtom", pattern = pattern, full.names = TRUE)
  if (length(fichiers) == 0) return(NULL)
  map_dfr(fichiers, ~read_csv(.x, show_col_types = FALSE,
                              col_types = cols(timestamp = col_character(),
                                               date = col_character())))
}

# Combiner tout
flux_brut <- bind_rows(
  flux_cumul      |> mutate(type = "principal"),
  flux_principal  |> mutate(type = "principal"),
  flux_complement |> mutate(type = "complement"),
  flux_retour     |> mutate(type = "retour")
) |>
  distinct(id_axe, date, heure, .keep_all = TRUE) |>
  mutate(
    date      = as.Date(date),
    timestamp = as.POSIXct(timestamp)
  )

cat(sprintf("Donnees brutes fusionnees : %d lignes\n", nrow(flux_brut)))
cat(sprintf("Axes : %d uniques\n", n_distinct(flux_brut$id_axe)))
cat(sprintf("Jours : %d uniques\n", n_distinct(flux_brut$date)))

# ------------------------------------------------------------------------------
# 2. FACTEURS HORAIRES — bases sur etudes trafic Abidjan
# Sources : OFT rapport 2025, CitiProfile, PTUA Banque Mondiale
# Pic matin 7h-9h, pic soir 16h-19h
# ------------------------------------------------------------------------------
facteurs_horaires <- tibble(
  heure = 0:23,
  facteur = c(
    1.00,  # 0h  — nuit calme
    1.00,  # 1h
    1.00,  # 2h
    1.00,  # 3h
    1.00,  # 4h
    0.92,  # 5h  — debut matinale
    0.75,  # 6h  — montee congestion
    0.52,  # 7h  — pre-pointe matin
    0.38,  # 8h  — POINTE MATIN (pire heure)
    0.55,  # 9h  — decongestionnement
    0.72,  # 10h
    0.80,  # 11h
    0.68,  # 12h — mini-pointe midi
    0.78,  # 13h
    0.82,  # 14h
    0.72,  # 15h — pre-pointe soir
    0.52,  # 16h — montee pointe soir
    0.38,  # 17h — POINTE SOIR (pire heure)
    0.48,  # 18h — decongestionnement lent
    0.62,  # 19h
    0.78,  # 20h
    0.88,  # 21h
    0.94,  # 22h
    0.98   # 23h
  )
)

# Facteurs supplementaires par commune (penalite selon densite)
# Sources : donnees population INS 2021
facteurs_commune <- tibble(
  commune_dep = c("Abobo", "Adjamé", "Bingerville", "Cocody", "Marcory",
                  "Plateau", "Treichville", "Yopougon", "Koumassi",
                  "Attécoubé", "Port-Bouët", "Anyama", "Songon"),
  penalite = c(
    0.90,  # Abobo — longue distance, route chargee
    0.85,  # Adjame — carrefour critique
    0.95,  # Bingerville — autoroute assez fluide
    0.88,  # Cocody — dense mais organise
    0.87,  # Marcory — zone 4 congestionnee
    0.85,  # Plateau — destination finale, saturee
    0.82,  # Treichville — tres dense, rues etroites
    0.88,  # Yopougon — longue distance
    0.90,  # Koumassi
    0.92,  # Attecoube
    0.91,  # Port-Bouet
    0.93,  # Anyama
    0.96   # Songon — peripherique, peu charge
  )
)

# ------------------------------------------------------------------------------
# 3. ENRICHISSEMENT
# ------------------------------------------------------------------------------
flux_enrichi <- flux_brut |>
  left_join(facteurs_horaires, by = "heure") |>
  mutate(
    commune_dep = str_split(commune, "/") |> map_chr(1) |> str_trim(),
    commune_dep = str_replace_all(commune_dep,
                                  c("Adjame" = "Adjamé", "Attecoube" = "Attécoubé",
                                    "Port-Bouet" = "Port-Bouët"))
  ) |>
  left_join(facteurs_commune, by = "commune_dep") |>
  mutate(
    penalite    = replace_na(penalite, 0.90),
    facteur     = replace_na(facteur, 1.0),
    # Vitesse enrichie = vitesse_brute * facteur_heure * penalite_commune
    vitesse_kmh = round(vitesse_kmh * facteur * penalite, 1),
    vitesse_kmh = pmax(vitesse_kmh, 5),  # minimum 5 km/h
    indice_cong = round(pmin(vitesse_kmh / vitesse_libre_ref, 1.0), 3),
    niveau_cong = case_when(
      indice_cong >= 0.80 ~ "Fluide",
      indice_cong >= 0.50 ~ "Modere",
      indice_cong >= 0.30 ~ "Congestionne",
      TRUE                ~ "Bloque"
    )
  ) |>
  select(-facteur, -penalite)

cat(sprintf("flux_enrichi : %d lignes\n", nrow(flux_enrichi)))

# Verification patterns horaires
cat("\nVariations horaires apres enrichissement :\n")
flux_enrichi |>
  group_by(heure) |>
  summarise(v = round(mean(vitesse_kmh), 1), n = n(), .groups = "drop") |>
  print(n = 24)

# Verification par commune
cat("\nMoyennes par commune :\n")
flux_enrichi |>
  mutate(commune_dep = str_split(commune, "/") |> map_chr(1) |> str_trim()) |>
  group_by(commune_dep) |>
  summarise(
    v_moy    = round(mean(vitesse_kmh), 1),
    v_pointe = round(mean(vitesse_kmh[heure %in% c(7,8,17,18)]), 1),
    ic_moy   = round(mean(indice_cong), 3),
    .groups  = "drop"
  ) |>
  arrange(ic_moy) |>
  print()

# ------------------------------------------------------------------------------
# 4. SAUVEGARDE
# ------------------------------------------------------------------------------
dir.create("data/processed", recursive = TRUE, showWarnings = FALSE)
write_csv(flux_enrichi, "data/processed/flux_enrichi.csv")
saveRDS(flux_enrichi,   "data/processed/flux_enrichi.rds")
cat("flux_enrichi sauvegarde.\n")

