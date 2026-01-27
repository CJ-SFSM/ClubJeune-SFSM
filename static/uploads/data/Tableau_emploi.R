#!/usr/bin/env Rscript

# -------------------------------------------------------------------
# Script R : generate_jobs_table.R
#
# Objectif : lire le fichier CSV "data/jobs.csv", construire un tableau
# interactif avec reactable, puis sauvegarder ce tableau au format HTML
# (jobs_table.html).
#
# Prérequis :
#   - Le fichier "data/jobs.csv" doit exister dans le répertoire de travail.
#   - Les paquets R installés : readr, dplyr, reactable, htmlwidgets
#
# Usage (depuis un terminal R ou RStudio) :
#   1. Placer ce script à la racine de votre projet (ou adapter les chemins).
#   2. Exécuter :
#        Rscript generate_jobs_table.R
#   3. Le fichier "jobs_table.html" sera créé dans le même répertoire.
# -------------------------------------------------------------------

# Charger les bibliothèques nécessaires
library(readr)       # pour read_csv()
library(dplyr)       # pour mutate() et select()
library(reactable)   # pour générer le tableau interactif
library(htmlwidgets) # pour saveWidget()

# 1. Lecture du fichier CSV
jobs_filepath <- paste0(getwd(),"/static/uploads/data/jobs.csv")
enc_guess <- readr::guess_encoding(jobs_filepath, n_max = 100000)$encoding[1]
if (is.na(enc_guess)) enc_guess <- "UTF-8"   # repli sûr

jobs_df <- readr::read_csv(
  jobs_filepath,
  locale = readr::locale(encoding = enc_guess),
  col_types = cols(
    type        = col_character(),
    intitule    = col_character(),
    laboratoire = col_character(),
    lieu        = col_character(),
    deadline    = col_character(),
    link        = col_character()
  )
)

# 2. Génération d'une colonne HTML pour le lien cliquable
jobs_df <- jobs_df %>%
  mutate(
    lien = paste0(
      "<a href=\"", link, "\" target=\"_blank\" rel=\"noopener\">Lien</a>"
    )
  )%>%
  select(type, intitule, laboratoire, lieu, deadline, lien)

# 3. Construction du tableau reactable
tbl <- reactable(
  jobs_df,
  columns = list(
    type = colDef(
      name        = "Type",
      filterable  = T,
      filterInput = "select",
      # filterOptions = unique(jobs_df$type),
      align       = "center",
      cell        = function(value) {
        # Coloration selon la valeur du type
        color <- switch(value,
                        "CDD"   = "#ffc107",
                        "Thèse" = "#28a745",
                        "Stage" = "#17a2b8",
                        "Poste" = "#FF7FEB"
        )
        htmltools::tags$span(
          style = paste0(
            "display:inline-block; padding:4px 8px; ",
            "background-color:", color, "; color: #fff; border-radius:4px;"
          ),
          value
        )
      }
    ),
    intitule = colDef(
      name       = "Offre",
      filterable = TRUE,
      align      = "left"
    ),
    laboratoire = colDef(
      name       = "Laboratoire",
      filterable = F,
      align      = "left"
    ),
    lieu = colDef(
      name       = "Lieu",
      filterable = F,
      align      = "left"
    ),
    deadline = colDef(
      name       = "Date limite de réponse",
      filterable = F,
      align      = "center"
    ),
    lien = colDef(
      name = "Lien vers l'offre",
      html = T,
      align = "center"
    )
  ),
  defaultColDef = colDef(
    sortable    = TRUE,
    minWidth    = 100,
    headerStyle = list(background = "#f1f1f1")
  ),
  theme = reactableTheme(
    borderColor = "#dfe2e5",
    striped     = NULL,
    highlight   = NULL,
    cellPadding = "8px 12px"
  ),
  searchable         = TRUE,
  defaultPageSize    = 10,
  showPageSizeOptions= TRUE,
  pageSizeOptions    = c(5, 10, 20, 50)
)

# 4. Sauvegarde du tableau au format HTML
output_file <- "jobs_table.html"
saveWidget(
  widget     = tbl,
  file       = output_file,
  selfcontained = TRUE,
  background = "transparent")

message("Le fichier HTML interactif a été généré : ", output_file)
