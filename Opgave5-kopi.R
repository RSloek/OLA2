library(eurostat)
library(ggplot2)

# Vælg lande her (ved landekode): se Script/00_vaelg_lande.R, variablen "lande".
source("Script/00_vaelg_lande.R")

stopifnot("Åbn OLA2.Rproj (working directory skal være projektets rod)" = dir.exists("data/Rådata"))
dir.create("data/Klar", showWarnings = FALSE)
dir.create("Plots", showWarnings = FALSE)


# 5.1 Kvartalsvis årlig realvækst i husholdningernes forbrugsudgift

eurosearch <- search_eurostat("Household final consumption expenditure", type = "dataset")
print(eurosearch[, c("code", "title")])


get_eurostat_dic("na_item")[get_eurostat_dic("na_item")$code_name == "P31_S14", ]
get_eurostat_dic("unit")[get_eurostat_dic("unit")$code_name %in% c("CLV20_MEUR", "CLV_PCH_SM"), ]
get_eurostat_dic("s_adj")


eu <- read.csv("data/Rådata/Eurostat_namq_10_fcs.csv", stringsAsFactors = FALSE)
eu$time <- as.Date(eu$time)
eu <- eu[eu$geo %in% lande, ]
stopifnot("Ingen data for de valgte lande: kør data/API/hent_Eurostat.R igen" = nrow(eu) > 0)
head(eu)
table(eu$geo)


landekoder <- read.csv2("data/Rådata/Landekoder.csv", stringsAsFactors = FALSE, encoding = "UTF-8")
landekoder$TITEL[landekoder$KODE == "NL"] <- "Holland"
iso_kode <- function(x) ifelse(x == "EL", "GR", ifelse(x == "UK", "GB", x))


vaekst_pct <- function(v) {
  n <- length(v)
  c(rep(NA, 4), (v[5:n] - v[1:(n - 4)]) / v[1:(n - 4)] * 100)
}

vaekst <- data.frame()
for (l in unique(eu$geo)) {
  x <- eu[eu$geo == l, ]
  x <- x[order(x$time), ]
  x$vaekst <- vaekst_pct(x$values)  
  vaekst <- rbind(vaekst, x[, c("time", "geo", "vaekst")])
}

# Kode til seneste kvartal
vaekst <- subset(vaekst, time >= as.Date("2000-01-01"))
seneste <- min(tapply(vaekst$time[!is.na(vaekst$vaekst)], vaekst$geo[!is.na(vaekst$vaekst)], max))
seneste <- as.Date(seneste, origin = "1970-01-01")
vaekst <- subset(vaekst, time <= seneste)
seneste   


vaekst_bred <- reshape(vaekst[, c("time", "geo", "vaekst")], idvar = "time", timevar = "geo", direction = "wide")
names(vaekst_bred) <- sub("vaekst.", "", names(vaekst_bred), fixed = TRUE)
rownames(vaekst_bred) <- NULL
head(vaekst_bred)
dim(vaekst_bred)   




# Landekoder og landenavne (navnet bruges kun til visning)
lande_kode <- names(vaekst_bred)[-1]
landenavn  <- setNames(landekoder$TITEL[match(iso_kode(lande_kode), landekoder$KODE)], lande_kode)
maerkat   <- function(k) paste0(k, " - ", landenavn[k])   # fx "DK - Danmark"



farvepalet  <- c("#E69F00", "#56B4E9", "#009E73", "#D62728", "#0072B2", "#9467BD",
                 "#CC79A7", "#8C564B", "#8C8C00", "#17BECF", "#E377C2", "#7F7F7F")
landefarver <- setNames(rep_len(farvepalet, length(lande_kode)), lande_kode)   

tegn_land <- function(k) {
  d <- vaekst[vaekst$geo == k & !is.na(vaekst$vaekst), ]
  d <- d[order(d$time), ]
  plot(d$time, d$vaekst, type = "o", pch = 19, cex = 0.9, lwd = 2, col = landefarver[k],
       xaxt = "n", yaxt = "n", xlab = "", ylab = "Realvækst i % (samme kvartal året før)",
       main = paste0(landenavn[k], " (", k, ")"), bty = "l", cex.main = 1.4)
  
  trin <- pretty(d$vaekst, n = 20)                  
  abline(h = trin, col = "grey88", lty = 1)                                              
  abline(v = seq(as.Date("2000-01-01"), max(d$time), by = "1 year"), col = "grey93", lty = 1)   
  abline(h = 0, col = "grey40")                                            
  lines(d$time, d$vaekst, type = "o", pch = 19, cex = 0.9, lwd = 2, col = landefarver[k])   
  axis(2, at = trin, labels = format(trin, decimal.mark = ","), las = 1, cex.axis = 0.85)   
  aar <- seq(as.Date("2000-01-01"), max(d$time), by = "2 years")
  axis.Date(1, at = aar, format = "%Y", las = 2)                           
  axis.Date(1, at = seq(as.Date("2000-01-01"), max(d$time), by = "1 year"), labels = FALSE, tcl = -0.3)   
}


gem_billede <- function(fil, bredde, hoejde, res, tegn) {
  tryCatch(tegn(), error = function(e) {
    message("Plots-panelet er for lille til at vise figuren. Den gemmes alligevel som billede i mappen Plots.")
    try(graphics.off(), silent = TRUE)       
  })                                                         
  png(fil, width = bredde, height = hoejde, res = res)      
  tegn()
  dev.off()
}

# Ét plot pr. land
for (k in lande_kode) {
  land_figur <- function() {
    par(mfrow = c(1, 1), mar = c(5, 5, 3, 1))
    tegn_land(k)
    mtext("Kilde: Eurostat, tabel namq_10_fcs Plot: Genereret i RStudios", side = 1, line = 4, adj = 1, cex = 0.8)
  }
  gem_billede(paste0("Plots/opg5_1_", k, ".png"), 2400, 1500, 220, land_figur)
}

# Alle landene i ét billede
raekker <- ceiling(length(lande_kode) / 3)
sammenligning <- function() {
  par(mfrow = c(raekker, 3), mar = c(4.5, 4.5, 3, 1), oma = c(0, 0, 3, 0))
  for (k in lande_kode) tegn_land(k)
  mtext(paste0("Årlig realvækst i husholdningernes forbrug i ", length(lande_kode), " lande, 2000Q1 til dags dato"),
        outer = TRUE, font = 2, cex = 1.3)
}
gem_billede("Plots/opg5_1_sammenligning.png", 3300, 900 * raekker, 250, sammenligning)
try(par(mfrow = c(1, 1), oma = c(0, 0, 0, 0)), silent = TRUE)   

kvartal_navn <- function(d) paste0(format(d, "%Y"), " Q", (as.numeric(format(d, "%m")) - 1) %/% 3 + 1)
cat("Svar 5.1: Kvartalsvis årlig realvækst er beregnet for", length(lande_kode), "lande (", paste(lande_kode, collapse = ", "), ") fra",
    kvartal_navn(min(vaekst_bred$time)), "til", kvartal_navn(max(vaekst_bred$time)), "(", nrow(vaekst_bred), "kvartaler).\n")


# 5.2 Højeste gennemsnitlige kvartalsvise årlige realvækst

gns <- colMeans(vaekst_bred[, lande_kode])
gns_sorteret <- sort(gns, decreasing = TRUE)
round(gns_sorteret, 2)
names(gns_sorteret)[1]   
landenavn[names(gns_sorteret)[1]]   

gns_data <- data.frame(kode = names(gns_sorteret), gns = as.numeric(gns_sorteret))
gns_data$land <- factor(maerkat(gns_data$kode),
                        levels = rev(maerkat(gns_data$kode)))

ggplot(gns_data, aes(x = land, y = gns, fill = kode == names(gns_sorteret)[1])) +
  geom_col(width = 0.7) +
  coord_flip() +
  scale_fill_manual(values = c("TRUE" = "#08519C", "FALSE" = "#9ECAE1")) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.12))) +
  labs(title = paste0(landenavn[names(gns_sorteret)[1]], " har haft den højeste gennemsnitlige forbrugsvækst siden 2000"),
       subtitle = "Gennemsnitlig årlig realvækst i husholdningernes forbrug pr. kvartal, 2000Q1 til dags dato",
       x = NULL, y = "Gennemsnitlig årlig realvækst (%)",
       caption = "Kilde: Eurostat, tabel namq_10_fcs. Plot: Genereret i RStudios") +
  theme_minimal(base_size = 13) +
  theme(legend.position = "none", plot.title = element_text(face = "bold"))
ggsave("Plots/opg5_gennemsnit.png", width = 9, height = 5)

cat("Svar 5.2:", landenavn[names(gns_sorteret)[1]], "har den højeste gennemsnitlige kvartalsvise årlige realvækst (", format(round(gns_sorteret[1], 2), nsmall = 2, decimal.mark = ","),
    "%). Den laveste har", landenavn[names(gns_sorteret)[length(gns_sorteret)]], "(", format(round(gns_sorteret[length(gns_sorteret)], 2), nsmall = 2, decimal.mark = ","), "%).\n")


# 5.3 Coronakrisen som outlier
graense    <- 2
foer       <- vaekst_bred[vaekst_bred$time < as.Date("2020-01-01"), lande_kode]
foer_gns   <- sapply(foer, mean)
foer_sd    <- sapply(foer, sd)
vindue     <- vaekst_bred$time >= as.Date("2020-01-01") & vaekst_bred$time <= as.Date("2022-04-01")

corona_land <- data.frame(kode = lande_kode, land = unname(landenavn[lande_kode]), start = as.Date(NA), slut = as.Date(NA), antal = NA)
uden_corona <- vaekst_bred   
for (i in seq_along(lande_kode)) {
  l <- lande_kode[i]
  z <- abs(vaekst_bred[[l]] - foer_gns[l]) / foer_sd[l]
  paavirket <- vindue & z > graense
  if (!any(paavirket)) { corona_land$antal[i] <- 0; next }   
  start <- min(vaekst_bred$time[paavirket])
  slut  <- max(vaekst_bred$time[paavirket])
  fjern <- vaekst_bred$time >= start & vaekst_bred$time <= slut
  corona_land$start[i] <- start
  corona_land$slut[i]  <- slut
  corona_land$antal[i] <- sum(fjern)
  uden_corona[fjern, l] <- NA
}

cat("Corona periode for hver enkelt land")
corona_land 


gns_uden <- colMeans(uden_corona[, lande_kode], na.rm = TRUE)
effekt <- data.frame(kode = lande_kode, land = unname(landenavn[lande_kode]),
                     med_corona = as.numeric(gns[lande_kode]),
                     uden_corona = as.numeric(gns_uden[lande_kode]))
effekt$forskel <- effekt$uden_corona - effekt$med_corona  
effekt <- effekt[order(-effekt$forskel), ]
effekt[, -(1:2)] <- round(effekt[, -(1:2)], 2)
effekt
effekt$land[1]  

# Højeste gennemsnit efter Corona er fjernet
uden_sorteret <- sort(gns_uden, decreasing = TRUE)  
round(uden_sorteret, 2)

# ggplot2-version (5.3): med og uden Corona, sorteret efter Coronaens effekt
laang3 <- data.frame(kode = rep(effekt$kode, 2),
                     land = rep(effekt$land, 2),
                     gns  = c(effekt$med_corona, effekt$uden_corona),
                     type = rep(c("Med Corona", "Uden Corona"), each = nrow(effekt)))
laang3$kode <- factor(laang3$kode, levels = effekt$kode)
laang3$type <- factor(laang3$type, levels = c("Med Corona", "Uden Corona"))

ggplot(laang3, aes(x = kode, y = gns, fill = type)) +
  geom_col(position = position_dodge(width = 0.8), width = 0.75) +
  scale_fill_manual(values = c("Med Corona" = "#9ECAE1", "Uden Corona" = "#08519C")) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.12))) +
  labs(title = paste0("Uden Corona stiger gennemsnittet mest i ", effekt$land[1], " (", effekt$kode[1], ")"),
       subtitle = "Gennemsnitlig årlig realvækst pr. kvartal med og uden hvert lands Coronaperiode, 2000Q1 til dags dato",
       x = "Landekode", y = "Årlig realvækst (%)", fill = NULL,
       caption = "Kilde: Eurostat, tabel namq_10_fcs. Plot: Genereret i RStudios.") +
  theme_minimal(base_size = 12) +
  theme(legend.position = "top", plot.title = element_text(face = "bold"))
ggsave("Plots/opg5_med_uden_corona.png", width = 10, height = 5.5)

cat("Svar 5.3: Coronakrisen har haft størst effekt i", effekt$land[1], "- gennemsnittet stiger med",
    format(effekt$forskel[1], nsmall = 2, decimal.mark = ","), "procentpoint, når Coronakvartalerne fjernes (fra",
    format(effekt$med_corona[1], nsmall = 2, decimal.mark = ","), "til", format(effekt$uden_corona[1], nsmall = 2, decimal.mark = ","), "%).\n")


# 5.4 Hvilket land har den laveste gennemsnitlige kvartalsvise realvækst fra 1. kvartal 2020 og til dags dato?

corona_periode <- vaekst_bred[vaekst_bred$time >= as.Date("2020-01-01"), ]   
gns_corona <- colMeans(corona_periode[, lande_kode])
corona_sorteret <- sort(gns_corona)                   
round(corona_sorteret, 2)
names(corona_sorteret)[1]                             
landenavn[names(corona_sorteret)[1]]                  

gns_data4 <- data.frame(kode = names(corona_sorteret), gns = as.numeric(corona_sorteret))
gns_data4$land <- factor(maerkat(gns_data4$kode),
                         levels = rev(maerkat(gns_data4$kode)))  

ggplot(gns_data4, aes(x = land, y = gns, fill = kode == names(corona_sorteret)[1])) +
  geom_col(width = 0.7) +
  coord_flip() +
  scale_fill_manual(values = c("TRUE" = "#08519C", "FALSE" = "#9ECAE1")) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.18))) +
  labs(title = paste0(landenavn[names(corona_sorteret)[1]], " har haft den laveste gennemsnitlige vækst fra 2020 til i dag"),
       subtitle = "Gennemsnitlig årlig realvækst pr. kvartal, 1. kvartal 2020 til dags dato",
       x = NULL, y = "Gennemsnitlig årlig realvækst (%)",
       caption = "Kilde: Eurostat, tabel namq_10_fcs Plot: Genereret i RStudios.") +
  theme_minimal(base_size = 13) +
  theme(legend.position = "none", plot.title = element_text(face = "bold"))
ggsave("Plots/opg5_4_laveste_vaekst.png", width = 9, height = 5)


a20 <- vaekst_bred[format(vaekst_bred$time, "%Y") == "2020", ]
a21 <- vaekst_bred[format(vaekst_bred$time, "%Y") == "2021", ]
genopretning <- data.frame(
  kode = lande_kode, land = unname(landenavn[lande_kode]),
  gns_2020 = sapply(lande_kode, function(k) mean(a20[[k]])),
  laveste_2020 = sapply(lande_kode, function(k) min(a20[[k]])),
  laveste_kvt = sapply(lande_kode, function(k) kvartal_navn(a20$time[which.min(a20[[k]])])),
  gns_2021 = sapply(lande_kode, function(k) mean(a21[[k]])),
  hoejeste_2021 = sapply(lande_kode, function(k) max(a21[[k]])),
  hoejeste_kvt = sapply(lande_kode, function(k) kvartal_navn(a21$time[which.max(a21[[k]])])))
genopretning[, c("gns_2020", "laveste_2020", "gns_2021", "hoejeste_2021")] <- round(genopretning[, c("gns_2020", "laveste_2020", "gns_2021", "hoejeste_2021")], 1)
rownames(genopretning) <- NULL
genopretning

cat("Svar 5.4:", landenavn[names(corona_sorteret)[1]], "har den laveste gennemsnitlige kvartalsvise årlige realvækst fra 1. kvartal 2020 til", kvartal_navn(max(vaekst_bred$time)),
    "(", format(round(corona_sorteret[1], 2), nsmall = 2, decimal.mark = ","), "%).\n")
