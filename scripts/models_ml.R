library(dplyr)
library(readr)
library(stringr)
library(purrr)
library(caret)
library(randomForest)
library(rpart)
library(FNN)
library(tibble)
library(ggplot2)
library(FactoMineR)
library(ggrepel)

# ==============================================================================
# 09_modeles_ml.R — ML + Clustering
# Lit depuis data/processed/flux_enrichi.csv (DEJA enrichi par 06)
# NE RE-APPLIQUE PAS les facteurs horaires
# NE SAUVEGARDE PAS flux_enrichi.csv
# ==============================================================================

# 1. CHARGEMENT (deja enrichi)
flux <- read_csv("data/processed/flux_enrichi.csv", show_col_types = FALSE,
                 col_types = cols(timestamp = col_character(), date = col_character())) |>
  mutate(
    commune_dep = str_split(commune, "/") |> map_chr(1) |> str_trim(),
    commune_dep = str_replace_all(commune_dep,
                                  c("Adjame" = "Adjamé", "Attecoube" = "Attécoubé", "Port-Bouet" = "Port-Bouët")),
    jour_num   = as.numeric(factor(jour,
                                   levels = c("lundi","mardi","mercredi","jeudi",
                                              "vendredi","samedi","dimanche"))),
    id_axe_num = as.numeric(factor(id_axe))
  ) |>
  filter(!is.na(vitesse_kmh))

cat(sprintf("flux_enrichi charge : %d lignes | %d axes\n", nrow(flux), n_distinct(flux$id_axe)))

# Verification — NE DOIT PAS etre plat
flux |>
  group_by(heure) |>
  summarise(v = round(mean(vitesse_kmh), 1), .groups = "drop") |>
  arrange(heure) |>
  print(n = 24)

# ==============================================================================
# 2. DATASET ML
# ==============================================================================
df_ml <- flux |>
  select(vitesse_kmh, heure, distance_m,
         vitesse_libre_ref, id_axe_num, jour_num) |>
  na.omit()

cat(sprintf("Dataset ML : %d lignes x %d colonnes\n", nrow(df_ml), ncol(df_ml)))

# ==============================================================================
# 3. TRAIN / TEST SPLIT — aleatoire (meilleur apprentissage horaire)
# ==============================================================================
set.seed(42)
idx_train <- sample(nrow(df_ml), size = round(nrow(df_ml) * 0.8))
train     <- df_ml[idx_train, ]
test      <- df_ml[-idx_train, ]

cat(sprintf("Train : %d | Test : %d\n", nrow(train), nrow(test)))

# ==============================================================================
# 4. EVALUATION
# ==============================================================================
evaluer <- function(y_reel, y_pred) {
  tibble(
    RMSE = round(sqrt(mean((y_reel - y_pred)^2)), 3),
    R2   = round(cor(y_reel, y_pred)^2, 3),
    MAE  = round(mean(abs(y_reel - y_pred)), 3)
  )
}

# ==============================================================================
# 5-8. MODELES
# ==============================================================================
mod_lm     <- lm(vitesse_kmh ~ ., data = train)
pred_lm    <- predict(mod_lm, newdata = test)
eval_lm    <- evaluer(test$vitesse_kmh, pred_lm)
cat(sprintf("LM    : RMSE=%.3f | R2=%.3f\n", eval_lm$RMSE, eval_lm$R2))

mod_rpart  <- rpart(vitesse_kmh ~ ., data = train)
pred_rpart <- predict(mod_rpart, newdata = test)
eval_rpart <- evaluer(test$vitesse_kmh, pred_rpart)
cat(sprintf("Rpart : RMSE=%.3f | R2=%.3f\n", eval_rpart$RMSE, eval_rpart$R2))

mod_rf     <- randomForest(vitesse_kmh ~ ., data = train, ntree = 100, importance = TRUE)
pred_rf    <- predict(mod_rf, newdata = test)
eval_rf    <- evaluer(test$vitesse_kmh, pred_rf)
cat(sprintf("RF    : RMSE=%.3f | R2=%.3f\n", eval_rf$RMSE, eval_rf$R2))

X_train   <- train |> select(-vitesse_kmh) |> as.matrix()
X_test    <- test  |> select(-vitesse_kmh) |> as.matrix()
y_train   <- train$vitesse_kmh
k_optimal <- round(sqrt(nrow(train)))
pred_knn  <- knn.reg(train = X_train, test = X_test, y = y_train, k = k_optimal)$pred
eval_knn  <- evaluer(test$vitesse_kmh, pred_knn)
cat(sprintf("KNN k=%d : RMSE=%.3f | R2=%.3f\n", k_optimal, eval_knn$RMSE, eval_knn$R2))

# ==============================================================================
# 9. COMPARAISON
# ==============================================================================
comparaison <- tibble(
  Modele = c("Régression linéaire", "Arbre de décision", "Random Forest", "k-NN"),
  RMSE   = c(eval_lm$RMSE, eval_rpart$RMSE, eval_rf$RMSE, eval_knn$RMSE),
  R2     = c(eval_lm$R2,   eval_rpart$R2,   eval_rf$R2,   eval_knn$R2),
  MAE    = c(eval_lm$MAE,  eval_rpart$MAE,  eval_rf$MAE,  eval_knn$MAE)
) |> arrange(RMSE)

print(comparaison)

# ==============================================================================
# 10. SAUVEGARDE MODELES
# ==============================================================================
dir.create("models",  showWarnings = FALSE)
dir.create("outputs", showWarnings = FALSE)

saveRDS(mod_lm,    "models/mod_lm.rds")
saveRDS(mod_rpart, "models/mod_rpart.rds")
saveRDS(mod_rf,    "models/mod_rf_vitesse.rds")
saveRDS(list(X_train = X_train, y_train = y_train), "models/mod_knn_data.rds")
write_csv(comparaison, "outputs/comparaison_modeles.csv")
cat("Modeles sauvegardes.\n")

# ==============================================================================
# 11. CLUSTERING — Profils de CONGESTION des communes
# Base sur donnees TomTom enrichies (problematique = disparites congestion)
# ==============================================================================
df_cluster <- flux |>
  group_by(commune_dep) |>
  summarise(
    vitesse_moy    = mean(vitesse_kmh, na.rm = TRUE),
    vitesse_pointe = mean(vitesse_kmh[heure %in% c(7,8,17,18)], na.rm = TRUE),
    indice_moy     = mean(indice_cong, na.rm = TRUE),
    variabilite    = sd(vitesse_kmh, na.rm = TRUE),
    .groups        = "drop"
  )

cat("\nProfils par commune :\n")
print(df_cluster |> arrange(indice_moy))

mat_cl <- df_cluster |>
  select(indice_moy, vitesse_pointe, variabilite) |>
  scale()
rownames(mat_cl) <- df_cluster$commune_dep

set.seed(42)
km  <- kmeans(mat_cl, centers = 3, nstart = 25)
acp <- PCA(mat_cl, graph = FALSE)

coords <- as.data.frame(acp$ind$coord[, 1:2])
coords$commune <- df_cluster$commune_dep
coords$cluster <- as.factor(km$cluster)

# Labels bases sur indice de congestion moyen
profils <- df_cluster |>
  mutate(cluster = as.factor(km$cluster)) |>
  group_by(cluster) |>
  summarise(ic = mean(indice_moy), .groups = "drop") |>
  arrange(ic) |>
  mutate(label = c("Très congestionné", "Moyennement congestionné", "Peu congestionné"))

coords$profil <- profils$label[match(coords$cluster, profils$cluster)]

p_cluster <- ggplot(coords, aes(x = Dim.1, y = Dim.2, color = profil, label = commune)) +
  geom_point(size = 6) +
  geom_label_repel(size = 4, show.legend = FALSE, fontface = "bold") +
  scale_color_manual(values = c(
    "Très congestionné"        = "#DC2626",
    "Moyennement congestionné" = "#F47920",
    "Peu congestionné"         = "#009A44")) +
  labs(title = "Profils de congestion des communes — k-means + ACP",
       subtitle = "Basé sur vitesses TomTom enrichies · heures de pointe vs heures creuses",
       x = "Axe 1 (congestion)", y = "Axe 2 (variabilité)", color = "Profil") +
  theme_minimal(base_size = 13)

print(p_cluster)

saveRDS(list(km = km, acp = acp, df = df_cluster, coords = coords),
        "models/clustering.rds")
ggsave("outputs/clustering_acp.png", p_cluster, width = 10, height = 7, dpi = 300)
cat("Clustering congestion sauvegarde.\n")