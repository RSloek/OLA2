library(dkstat)
library(dplyr)
library(readr)
library(stringr)
library(ggplot2)


# 1.1
search <- dst_search(string = "befolkning", field = "text")
nrow(search)

cat("Vi samler variabelnavnene til én tekststreng pr. tabel, så vi kan søge")
search$variable_text <- sapply(search$variables, paste, collapse = ", ")

search2 <- search[grepl("by", search$variable_text, ignore.case = TRUE),
                  c("id", "text", "variable_text")]
print(search2)

meta <- dst_meta(table = "BY1", lang = "da")
print(meta$variables) 
head(meta$values$BYER)         
tail(meta$values$Tid)          


cat("1.2 Kategorivariabel")

byer_raw <- read.csv("DST_BY1.csv", stringsAsFactors = FALSE, encoding = "UTF-8")
head(byer_raw)
nrow(byer_raw)


head(byer_raw$BYER, 10)

cat("Hvilke rækker er IKKE rigtige byer? Vi kigger efter totaler og særlige koder.")
byer_raw[grepl("Hovedstadsområdet$", byer_raw$BYER), ]      
byer_raw[grepl("Kommune$", byer_raw$BYER), ][1:3, ]         
head(byer_raw[substr(byer_raw$BYER, 4, 8) %in% c("99999", "99997"), ])


byer_raw$kode    <- substr(byer_raw$BYER, 1, 8)
byer_raw$kommune <- substr(byer_raw$kode, 1, 3)
byer_raw$bynr    <- substr(byer_raw$kode, 4, 8)
byer_raw$navn    <- sub("^[0-9]{8} [0-9]{3}-[0-9]{5} ", "", byer_raw$BYER)
byer_raw$navn    <- sub(" \\(del af.*\\)$", "", byer_raw$navn)


byer <- byer_raw[byer_raw$kode != "00001100" &
                 byer_raw$bynr != "99999" &
                 byer_raw$bynr != "99997" &
                 byer_raw$bynr != paste0("00", byer_raw$kommune), ]
cat("Antal byområder:", nrow(byer), "\n")

cat("Boligsiden skriver bynavne som i URL'er (Sønderborg -> soenderborg).Funktionen laver DST's navne om til samme format, så vi kan merge på dem.")
til_slug <- function(x) {
  x <- tolower(x)
  x <- gsub("æ", "ae", x)
  x <- gsub("ø", "oe", x)
  x <- gsub("å", "aa", x)
  x <- gsub("[^a-z0-9]+", "-", x)
  gsub("^-+|-+$", "", x)
}

# Hovedstadsområdet (kode 00001100) er fjernet ovenfor, fordi det er mange sammenvoksede
# byer og kommuner. DST opgør dem som kommunedele ("Herlev (del af Hovedstadsområdet)"
# osv.), og dem beholder vi som selvstændige byer med eget indbyggertal.
byer$by_slug <- til_slug(byer$navn)
byer <- aggregate(value ~ by_slug, data = byer, FUN = sum)
names(byer)[2] <- "indbyggere"

dst_bystoerrelser <- data.frame(
  dst_klasse = c("Landdistrikter under 200", "Byer mellem 200 og 249", "Byer mellem 250 og 499",
                 "Byer mellem 500 og 999", "Byer mellem 1.000 og 1.999", "Byer mellem 2.000 og 4.999",
                 "Byer mellem 5.000 og 9.999", "Byer mellem 10.000 og 19.999",
                 "Byer mellem 20.000 og 49.999", "Byer mellem 50.000 og 99.999",
                 "Byer mellem 100.000 og 999.999"),
  min = c(0, 200, 250, 500, 1000, 2000, 5000, 10000, 20000, 50000, 100000),
  max = c(199, 249, 499, 999, 1999, 4999, 9999, 19999, 49999, 99999, 999999)
)

byer$bycat <- cut(byer$indbyggere,
                  breaks = c(-Inf, 1000, 5000, 20000, 100000, Inf),
                  labels = c("landsby", "lille by", "almindelig by", "større by", "storby"),
                  right = FALSE)
table(byer$bycat)

cat("Tabel")
kategori_tabel <- data.frame(
  Kategori     = c("landsby", "lille by", "almindelig by", "større by", "storby"),
  Indbyggere   = c("under 1.000", "1.000 - 4.999", "5.000 - 19.999", "20.000 - 99.999", "100.000 eller flere"),
  DST_klasser  = c("landdistrikt, 200-249, 250-499, 500-999",
                   "1.000-1.999, 2.000-4.999",
                   "5.000-9.999, 10.000-19.999",
                   "20.000-49.999, 50.000-99.999",
                   "100.000-999.999"),
  Antal_byer   = as.numeric(table(byer$bycat))
)
kategori_tabel


cat("1.3 Merge boligdata (OLA 1) med bycat")

boliger <- read.csv("Boliger_samlet.csv", stringsAsFactors = FALSE, encoding = "UTF-8")
boliger$by_slug <- til_slug(boliger$by)

mean(boliger$by_slug %in% byer$by_slug, na.rm = TRUE)
head(sort(table(boliger$by_slug[!boliger$by_slug %in% byer$by_slug]), decreasing = TRUE), 15)

# Mange af de umatchede er bydele i Hovedstadsområdet (Valby, Kastrup, Hellerup, Søborg).
# DST opgør dem under kommunedelen (fx Hellerup under Gentofte, Søborg under Gladsaxe),
# så vi kobler dem via postnummer til kommunedelen. Det er en tilnærmelse, da
# postnumre ikke følger kommunegrænserne helt præcist.
kommunedel <- c("2100" = "koebenhavn", "2150" = "koebenhavn", "2200" = "koebenhavn",
                "2300" = "koebenhavn", "2400" = "koebenhavn", "2450" = "koebenhavn",
                "2500" = "koebenhavn", "2700" = "koebenhavn", "2720" = "koebenhavn",
                "2600" = "glostrup",   "2605" = "broendby",   "2660" = "broendby",
                "2610" = "roedovre",   "2620" = "albertslund", "2625" = "vallensbaek",
                "2665" = "vallensbaek", "2635" = "ishoej",    "2650" = "hvidovre",
                "2670" = "greve-strand", "2730" = "herlev",   "2740" = "ballerup",
                "2750" = "ballerup",   "2760" = "ballerup",   "2770" = "taarnby",
                "2800" = "lyngby-taarbaek", "2830" = "lyngby-taarbaek",
                "2840" = "rudersdal",  "2850" = "rudersdal",  "2950" = "rudersdal",
                "2860" = "gladsaxe",   "2880" = "gladsaxe",
                "2820" = "gentofte",   "2870" = "gentofte",   "2900" = "gentofte",
                "2920" = "gentofte",   "2930" = "gentofte")
p <- boliger$postnr
postnr_by <- ifelse(!is.na(p) & p >= 1000 & p <= 1799, "koebenhavn",
             ifelse(!is.na(p) & p >= 1800 & p <= 2000, "frederiksberg",
                    unname(kommunedel[as.character(p)])))

# Derfor matcher vi i fire trin (første trin der rammer, vinder):
#  1) håndlavede aliasser hvor Boligsidens navn afviger fra DST's
#  2) postnummer i Hovedstadsområdet -> kommunedel (København, Frederiksberg, Gentofte ...)
#  3) direkte match på bynavn
#  4) fjern retningssuffiks ("aarhus-c" -> "aarhus") og prøv igen
alias <- c("nykoebing-sj" = "nykoebing-s",
           "risskov"      = "aarhus",
           "hoejbjerg"    = "aarhus")
uden_suffiks <- sub("-[a-z]{1,2}$", "", boliger$by_slug)

boliger$match_slug <- ifelse(boliger$by_slug %in% names(alias), alias[boliger$by_slug],
                      ifelse(!is.na(postnr_by), postnr_by,
                      ifelse(boliger$by_slug %in% byer$by_slug, boliger$by_slug,
                      ifelse(uden_suffiks %in% byer$by_slug, uden_suffiks, NA))))


boliger_med_bycat <- merge(boliger, byer[, c("by_slug", "indbyggere", "bycat")],
                           by.x = "match_slug", by.y = "by_slug", all.x = TRUE)



cat("Boliger med bycat:", sum(!is.na(boliger_med_bycat$bycat)), "af", nrow(boliger_med_bycat), "\n")
head(sort(table(boliger_med_bycat$by_slug[is.na(boliger_med_bycat$bycat)]), decreasing = TRUE), 15)


cat("1.4 Plot\n")
# Vi bruger medianen frem for gennemsnittet, da enkelte dyre boliger trækker
# gennemsnittet op (især i landsbyer). Først tjekker vi forskellen:
plot_raa <- subset(boliger_med_bycat, !is.na(bycat) & !is.na(kvmpris))
tapply(plot_raa$kvmpris, plot_raa$bycat, mean)
tapply(plot_raa$kvmpris, plot_raa$bycat, median)

plot_data <- aggregate(kvmpris ~ bycat, data = plot_raa, FUN = median)
names(plot_data)[2] <- "median_kvmpris"
plot_data$antal <- as.numeric(table(plot_raa$bycat))
plot_data

# Tal til overskrift og labels
kr <- function(x) paste0(format(round(x), big.mark = ".", decimal.mark = ",", trim = TRUE), " kr.")
faktor <- plot_data$median_kvmpris[plot_data$bycat == "storby"] /
          plot_data$median_kvmpris[plot_data$bycat == "landsby"]
faktor   # ca. 3,7

# Plot med konkluderende overskrift og pris på hver søjle
ggplot(plot_data, aes(x = bycat, y = median_kvmpris, fill = bycat)) +
  geom_col(width = 0.7) +
  geom_text(aes(label = kr(median_kvmpris)), vjust = -0.6, size = 4, fontface = "bold") +
  scale_fill_brewer(palette = "Blues") +
  scale_y_continuous(labels = function(x) format(x, big.mark = ".", decimal.mark = ","),
                     expand = expansion(mult = c(0, 0.12))) +
  labs(
    title = paste0("Boliger i storbyer koster ca. ", format(round(faktor, 1), decimal.mark = ","),
                   " gange så meget pr. m² som i landsbyer"),
    subtitle = "Median kvadratmeterpris efter bytype. Landsby og lille by ligger på samme niveau.",
    x = NULL, y = "Median pris pr. m² (kr.)",
    caption = paste0("Kilde: Boligsiden og Danmarks Statistik (BY1). ",
                     format(nrow(plot_raa), big.mark = ".", decimal.mark = ","), " boliger.")
  ) +
  theme_minimal(base_size = 13) +
  theme(legend.position = "none",
        plot.title = element_text(face = "bold"))

merge_visning <- boliger_med_bycat[!is.na(boliger_med_bycat$by) & !is.na(boliger_med_bycat$bycat) &
                                   grepl("^[a-zæøå]", boliger_med_bycat$by),   # springer bynavne der starter med tal over
                                   c("by", "pris", "kvmpris", "bycat")]
merge_visning <- merge_visning[order(merge_visning$by), ]
merge_visning <- head(merge_visning, 10)
merge_visning$pris    <- kr(merge_visning$pris)
merge_visning$kvmpris <- format(round(merge_visning$kvmpris), big.mark = ".", decimal.mark = ",", trim = TRUE)
merge_visning$bycat   <- as.character(merge_visning$bycat)
rownames(merge_visning) <- NULL
merge_visning

library(gridExtra)
library(grid)
merge_tema <- ttheme_minimal(
  core    = list(fg_params = list(hjust = 0, x = 0.05, fontsize = 10)),
  colhead = list(fg_params = list(hjust = 0, x = 0.05, fontsize = 10, fontface = "bold"),
                 bg_params = list(fill = "#D9E2F3"))
)
grid.newpage()
grid.table(merge_visning, rows = NULL, theme = merge_tema)


pris  <- aggregate(kvmpris ~ match_slug + bycat, data = plot_raa, FUN = median)
antal <- aggregate(kvmpris ~ match_slug + bycat, data = plot_raa, FUN = length)
by_pris <- merge(pris, antal, by = c("match_slug", "bycat"))
names(by_pris) <- c("match_slug", "bycat", "median_kvmpris", "antal")
by_pris <- by_pris[by_pris$antal >= 10, ]

navne <- byer_raw[!duplicated(til_slug(byer_raw$navn)), ]
navne <- data.frame(match_slug = til_slug(navne$navn), By = navne$navn)
by_pris <- merge(by_pris, navne, by = "match_slug", all.x = TRUE)

by_pris <- by_pris[order(by_pris$bycat, -by_pris$median_kvmpris), ]
top3 <- do.call(rbind, lapply(split(by_pris, by_pris$bycat), head, 3))
top3$Rang <- ave(top3$median_kvmpris, top3$bycat, FUN = function(x) rank(-x))
top3_tabel <- data.frame(Bytype = top3$bycat, Rang = top3$Rang, By = top3$By,
                         `Median pris pr. m²` = kr(top3$median_kvmpris),
                         `Antal boliger` = format(top3$antal, big.mark = ".", decimal.mark = ",", trim = TRUE),
                         check.names = FALSE)
top3_tabel

grid.newpage()
grid.table(top3_tabel, rows = NULL, theme = tabel_tema)

