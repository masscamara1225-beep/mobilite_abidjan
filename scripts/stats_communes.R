library(tidyverse)

stats_abidjan <- tibble(
  nom_commune <- c("Abobo", "Adjamé", "Anyama", "Attécoubé", "Bingerville", "Cocody", "Koumassi", "Marcory", "Le Plateau", "Port-Bouët", "Songon", "Treichville", "Yopougon"),

statut = c(
  "Borough", "Borough", "Sub-Prefecture", "Borough", "Sub-Prefecture",
  "Borough", "Borough", "Borough", "Borough", "Borough",
  "Sub-Prefecture", "Borough", "Borough"
),
population_2021 = c(
  1340083, 340892, 389592, 313135, 204656, 
  692583, 412282, 214061, 7186, 618795, 
  89778, 106552, 1571065
)
)

write_csv(stats_abidjan,"./data/processed/stats_communes_2021.csv")

