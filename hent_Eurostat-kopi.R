# Henter husholdningernes forbrugsudgift (kvartal) for 9 lande fra Eurostat og
# gemmer uændret i data/Rådata. Kør fra projektets rod (åbn OLA2.Rproj).
#
# Vi henter NIVEAUER (kædede værdier, 2020-priser), ikke Eurostats færdige
# procentændring (CLV_PCH_SM), fordi den er afrundet til én decimal. Den årlige
# realvækst beregnes i Opgave5.R som (x_t / x_(t-4) - 1) * 100.
#
# Tabel:   namq_10_fcs  (Household final consumption expenditure - quarterly data)
# na_item: P31_S14      (Final consumption expenditure of household)
# unit:    CLV20_MEUR   (Chain linked volumes 2020, million euro)
# s_adj:   SCA          (sæson- og kalenderjusteret)

library(eurostat)
library(readr)

dir.create("data/Rådata", recursive = TRUE, showWarnings = FALSE)

# Landene vælges ét sted: Script/00_vaelg_lande.R (variablen "lande")
source("Script/00_vaelg_lande.R")

d <- get_eurostat(
  "namq_10_fcs",
  filters = list(geo = lande, unit = "CLV20_MEUR", na_item = "P31_S14", s_adj = "SCA"),
  cache = FALSE
)

write_csv(d, "data/Rådata/Eurostat_namq_10_fcs.csv")
cat("Eurostat_namq_10_fcs.csv:", nrow(d), "rækker\n")
