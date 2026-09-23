library(ggplot2)
library(tidyverse)
library(scales)
library(dkstat)

#ff data================================================================

#Forbrugerforventninger
#dst_meta(table = "FORV1", lang = "da")

FORV1_meta_filters <- list(
  INDIKATOR = "*",
  Tid = "*"
)

ff <- dst_get_data(table = "FORV1", query = FORV1_meta_filters, lang = "da")

ff$TID <- as.Date(format(ff$TID, "%Y-%m-%d"))

year <- as.numeric(format(ff$TID, "%Y"))
month <- as.numeric(format(ff$TID, "%m"))
quarter_start_month <- c(1,1,1, 4,4,4, 7,7,7, 10,10,10)[month]
ff$TID <- as.Date(sprintf("%d-%02d-01", year, quarter_start_month))

ff2 <- aggregate(
  value ~ INDIKATOR + TID,
  data = ff,
  FUN = mean,
  na.rm = TRUE,
  na.action = na.pass
)

ff2 <- ff2[order(ff2$INDIKATOR, ff2$TID), ]
row.names(ff2) <- NULL

ffd <- pivot_wider(ff2, names_from = INDIKATOR, values_from = value)

ff96 <- ffd[ffd$TID >= as.Date("1996-01-01"),]

#pfgk data====================================================================

#dst_meta(table = "NKHC021", lang = "da")

NKHC021_meta_filters <- list(
  FORMAAAL = "*",
  PRISENHED = "2020-priser, kædede værdier",
  SÆSON = "Sæsonkorrigeret",
  Tid = "*"
)

pfgk <- dst_get_data(table = "NKHC021", query = NKHC021_meta_filters, lang = "da")
names(pfgk)[5] = "Mio. kr."
pfgk$FORMAAAL <- substring(pfgk$FORMAAAL, 5)
pfgk <- pfgk[c(1, 4, 5)]


#pfga data====================================================================

dst_meta(table = "NAHC021", lang = "da")

NAHC021_meta_filters <- list(
  FORMAAAL = "*",
  PRISENHED = "2020-priser, kædede værdier",
  Tid = "*"
)

pfga <- dst_get_data(table = "NAHC021", query = NAHC021_meta_filters, lang = "da")
names(pfga)[4] = "Mio. kr."
pfga$FORMAAAL <- substring(pfga$FORMAAAL, 5)
pfga <- pfga[c(1, 3, 4)]


#4.1==========================================================================

# Definer begivenhedsperioder
kriser <- data.frame(
  start = as.Date(c("2001-03-01", "2007-06-01", "2020-02-01", "2021-10-01", "2024-09-01")),
  end   = as.Date(c("2003-03-01", "2009-06-01", "2020-07-01", "2023-06-01", "2025-06-01")),
  label = c("IT-boblen", "Finanskrisen", "Covid-19", "Energi- og\ninflationskrise", "Ny amerikansk\npræsident"),
  y_pos = c(22, 22, 22, 16, 22)  # skiftevis højde for at undgå overlap,
)
godkriser <- data.frame(
  start = as.Date(c("1997-01-01", "2005-06-01", "2014-02-01")),
  end   = as.Date(c("2000-01-01", "2007-06-01", "2016-07-01")),
  label = c("Faldende ledighed\nog god vækst", "Boligpriser der\nsteg voldsomt", "Lav rente og\nstigende boligpriser"),
  y_pos = c(-22, -22, -22)  # skiftevis højde for at undgå overlap,
)


ggplot(ff96, aes(x = TID, y = `F1 Forbrugertillidsindikatoren`)) +
  scale_x_date(
    date_breaks = "2 years",
    labels = function(x) paste0(format(x, "%Y"), " Q", (as.numeric(format(x, "%m")) - 1) %/% 3 + 1)) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  geom_rect(data = kriser,
            aes(xmin = start, xmax = end, ymin = -Inf, ymax = Inf),
            inherit.aes = F, fill = "grey75", alpha = 0.4) +
  geom_rect(data = godkriser,
            aes(xmin = start, xmax = end, ymin = -Inf, ymax = Inf),
            inherit.aes = F, fill = "lightgreen", alpha = 0.4) +
  geom_text(data = kriser,
            aes(x = start + (end - start)/2, y = y_pos, label = label),
            inherit.aes = F, size = 3, lineheight = 1) +
  geom_text(data = godkriser,
            aes(x = start + (end - start)/2, y = y_pos, label = label),
            inherit.aes = F, size = 3, lineheight = 1) +
  labs(
    title = "DST's forbrugertillidsindikator, 1996-i dag",
    subtitle = "Kvartalsvise værdier",
    x = "År",
    y = "DST's forbrugertillidsindikator"
  ) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_line(linewidth = 1) 

#4.2 yap med mennesker=======================================================
mean(ff96$`F9 Anskaffelse af større forbrugsgoder, fordelagtigt for øjeblikket`)
ggplot(ff96, aes(x = TID, y = `F9 Anskaffelse af større forbrugsgoder, fordelagtigt for øjeblikket`)) +
  scale_x_date(
    date_breaks = "2 years",
    labels = function(x) paste0(format(x, "%Y"), " Q", (as.numeric(format(x, "%m")) - 1) %/% 3 + 1)) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  geom_line() +
  geom_hline(yintercept = mean(ff96$`F9 Anskaffelse af større forbrugsgoder, fordelagtigt for øjeblikket`)) +
  labs(
    title = "DST's forbrugertillidsindikator, 1996-i dag",
    subtitle = "Kvartalsvise værdier",
  ) 

#4.3=========================================================================
#===============================hvad skal jeg samligne når vi skal sammenligne år??????????????????????

plotdf <- pfga[pfga$TID == as.Date("2025-01-01") & pfga$FORMAAAL != "I alt",]
rownames(plotdf) <- NULL




ggplot(plotdf, aes(x = reorder(FORMAAAL, `Mio. kr.`), y = `Mio. kr.`, fill = reorder(FORMAAAL, `Mio. kr.`))) +
  geom_col() +
  coord_flip() +
  scale_y_continuous(labels = label_number(big.mark = ".", decimal.mark = ",")) +
  labs(
    title = "Husholdningernes forbrug fordelt på formål, 2025",
    subtitle = "Mio. kr., kædede 2020-priser",
    x = NULL, y = "Mio. kr."
  ) +
  theme_minimal() +
  guides(fill = "none")


#fra 2023q2 til 2026q2


plotdf <- data.frame(
  FORMAAAL = pfgk[pfgk$TID == as.Date("2023-04-01") & pfgk$FORMAAAL != "I alt",]$FORMAAAL,
  veardi23q2 = pfgk[pfgk$TID == as.Date("2023-04-01") & pfgk$FORMAAAL != "I alt",]$`Mio. kr.`,
  veardi26q2 = pfgk[pfgk$TID == as.Date("2026-04-01") & pfgk$FORMAAAL != "I alt",]$`Mio. kr.`

)
plotdf$Diff_procent <- ((plotdf$veardi26q2 / plotdf$veardi23q2) - 1) * 100

ggplot(plotdf, aes(x = reorder(FORMAAAL, Diff_procent), y = Diff_procent, fill = reorder(FORMAAAL, Diff_procent))) +
  geom_col() +
  coord_flip() +
  labs(
    title = "Husholdningernes forbrug fordelt på formål: ændring 2023Q2–2026Q2",
    subtitle = "Procentvis ændring i forbrug, kædede 2020-priser",
    x = NULL,
    y = "Ændring i forbrug (%)"
  ) +
  theme_minimal() +
  guides(fill = "none")


#4.4=========================================================================

#lav DI indikator============================================================.

ff96$DI_Indikator <- (ff96$`F2 Familiens økonomiske situation i dag, sammenlignet med for et år siden` + 
                             ff96$`F4 Danmarks økonomiske situation i dag, sammenlignet med for et år siden` +
                             ff96$`F9 Anskaffelse af større forbrugsgoder, fordelagtigt for øjeblikket` +
                             ff96$`F10 Anskaffelse af større forbrugsgoder, inden for de næste 12 mdr.`
) / 4
ff96 <- relocate(ff96, DI_Indikator, .after = 1)

#prøv med for loop===========================================================.

ff96[,i]

pfgk <- pfgk[pfgk$TID >= as.Date("2000-01-01"),]
pfgk_wide <- pivot_wider(pfgk, names_from = FORMAAAL, values_from = `Mio. kr.`)

lmdata <- inner_join(pfgk_wide, ff96[,1:3])
lmdata <- relocate(lmdata, DI_Indikator, .after = 1)
lmdata <- relocate(lmdata, `F1 Forbrugertillidsindikatoren`, .after = 1)

Resultater <- list()

for (i in 5:ncol(lmdata)) {
  navn <- names(lmdata)[i]
  
  DST_lm <- summary(lm(lmdata$`F1 Forbrugertillidsindikatoren` ~ lmdata[[i]]))
  DI_lm <- summary(lm(lmdata$DI_Indikator ~ lmdata[[i]]))
  
  Resultater[[navn]] <- list(
    DST_lm = DST_lm,
    DI_lm  = DI_lm
  )
}


