# Henter tabeller fra Danmarks Statistik og gemmer dem uændret i data/Rådata.
# Kør fra projektets rod (åbn OLA2.Rproj). Kør scriptet igen for at opdatere.

library(dkstat)
library(readr)

dir.create("data/Rådata", recursive = TRUE, showWarnings = FALSE)

hent_dst <- function(tabel, filtre, fil) {
  cat("Henter", tabel, "...\n")
  d <- dst_get_data(table = tabel, query = filtre, lang = "da")
  write_csv(d, file.path("data/Rådata", fil))
  cat("  ->", fil, ":", nrow(d), "rækker\n")
  invisible(d)
}

# Forbrugerforventninger (måned)
hent_dst("FORV1", list(INDIKATOR = "*", Tid = "*"), "DST_FORV1.csv")

# Husholdningernes forbrug efter formål, kvartal (alle priser og sæsoner)
hent_dst("NKHC021", list(FORMAAAL = "*", PRISENHED = "*", SÆSON = "*", Tid = "*"),
         "DST_NKHC021.csv")

# Husholdningernes forbrug efter formål, år
hent_dst("NAHC021", list(FORMAAAL = "*", PRISENHED = "*", Tid = "*"),
         "DST_NAHC021.csv")

# Befolkning i byområder (1. januar). Ændr året for at opdatere.
by_aar <- "2026"
hent_dst("BY1", list(BYER = "*", Tid = by_aar), "DST_BY1.csv")
