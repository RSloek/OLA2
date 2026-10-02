# ============================================
# PAKKER
# ============================================
library(readxl)
library(dkstat)
library(car)
library(dplyr)
library(gt)
library(lubridate)
library(ggplot2)
# ============================================
# Opgave 2.1: Opdatering af DI's forbrugertillidsindikator
# ============================================

# ---- 1) DI-FTI: hent og aggreger til kvartal --------------------------------
FORV1meta <- dst_meta(table = "FORV1", col_names = FALSE)
FORV1meta$values$INDIKATOR$text

FORV1meta_filters <- list(
  INDIKATOR = c("Familiens økonomiske situation i dag, sammenlignet med for et år siden",
                "Danmarks økonomiske situation i dag, sammenlignet med for et år siden",
                "Anskaffelse af større forbrugsgoder, fordelagtigt for øjeblikket",
                "Anskaffelse af større forbrugsgoder, inden for de næste 12 mdr."),
  Tid = "*"
)

FORV1data <- dst_get_data(table = "FORV1", query = FORV1meta_filters, lang = "da")

di_fti_kvartal <- FORV1data %>%
  group_by(TID) %>%
  summarise(di_fti_maaned = sum(value, na.rm = FALSE) / 4) %>%
  mutate(kvartal = paste0(year(TID), "Q", quarter(TID))) %>%
  group_by(kvartal) %>%
  summarise(DI_FTI = mean(di_fti_maaned, na.rm = TRUE))

#Husk her at der kun er data tilgængelig til og med aug 2026. 2026Q3, er altså kun et mean() af to måneder

# ---- 2) Husholdningernes forbrug: hent og aggreger til kvartal -------------
husfb_filters <- list(
  TRANSAKT  = "P.31 Husholdningernes forbrugsudgifter",
  PRISENHED = "2020-priser, kædede værdier, (mia. kr.)",
  SÆSON     = "Sæsonkorrigeret",
  Tid       = "*"
)
# TJEK: kør dst_meta("NKN1")$values og bekræft at disse tre teksters ordlyd
# matcher 1:1 med DST's egne værdi-tekster.
raa_husfb <- dst_get_data("NKN1", query = husfb_filters)

aar_h     <- as.numeric(format(raa_husfb$TID, "%Y"))
maaned_h  <- as.numeric(format(raa_husfb$TID, "%m"))
kvartal_h <- paste0(aar_h, "Q", ceiling(maaned_h / 3))

husfb_kvartal <- data.frame(kvartal = kvartal_h, forbrug = raa_husfb$value, stringsAsFactors = FALSE)

# årlig realvækst (år-til-år, kvartalsvis, i pct.)
husfb_kvartal <- husfb_kvartal %>%
  mutate(Aarlig_realvækst = (forbrug - lag(forbrug, 4)) / lag(forbrug, 4) * 100)

# ---- 3) Merge DI-FTI med realvækst i husholdningernes forbrug --------------------------------------------
merged_di_fti_realvækst <- inner_join(husfb_kvartal, di_fti_kvartal, by = "kvartal")
nrow(merged_di_fti_realvækst)
head(merged_di_fti_realvækst)

# ---- 4) DST's egen FTI: hent og aggreger til kvartal ------------------------
FORV1meta_DST <- list(
  INDIKATOR = "Forbrugertillidsindikatoren",
  Tid = "*"
)
DST_forbrugertillidsindikator_data <- dst_get_data(table = "FORV1", query = FORV1meta_DST, lang = "da")

# Kun 1 indikator hentes her (DST's allerede-beregnede indeks) -> ingen /4-division,
# bare aggreger måned -> kvartal med et almindeligt gennemsnit.
DST_fti_kvartal <- DST_forbrugertillidsindikator_data %>%
  mutate(kvartal = paste0(year(TID), "Q", quarter(TID))) %>%
  group_by(kvartal) %>%
  summarise(DST_FTI = mean(value, na.rm = TRUE))

# ---- 5) Merge DST FTI med forbrug -------------------------------------------
merged_DST_fti_realvækst <- inner_join(husfb_kvartal, DST_fti_kvartal, by = "kvartal")
nrow(merged_DST_fti_realvækst)
head(merged_DST_fti_realvækst)

# ==============================================================================
# 6) FILTRÉR BEGGE datasæt til perioden 2000-Q1 til 2026-Q2
#    (kommer FØRST HERFRA, fordi begge merged-datasæt nu findes)
# ==============================================================================

range(di_fti_kvartal$kvartal)
tail(di_fti_kvartal)
filtrer_periode <- function(df) {
  df %>%
    mutate(
      aar_num = as.numeric(substr(kvartal, 1, 4)),
      kvt_num = as.numeric(substr(kvartal, 6, 6))
    ) %>%
    filter(
      (aar_num > 2000) | (aar_num == 2000 & kvt_num >= 1),   # fra og med 2000-Q1
      (aar_num < 2026) | (aar_num == 2026 & kvt_num <= 2)    # til og med 2026-Q2
    ) %>%
    select(-aar_num, -kvt_num)
}

merged_di_fti_realvækst  <- filtrer_periode(merged_di_fti_realvækst)
merged_DST_fti_realvækst <- filtrer_periode(merged_DST_fti_realvækst)

# Tjek at det virkede
range(merged_di_fti_realvækst$kvartal)
range(merged_DST_fti_realvækst$kvartal)

# ==============================================================================
# 7) Korrelation og forklaringsgrad -- for BEGGE indikatorer
# ==============================================================================
korrelation_di_fti <- cor(merged_di_fti_realvækst$DI_FTI,
                          merged_di_fti_realvækst$Aarlig_realvækst,
                          use = "complete.obs")

korrelation_dst_fti <- cor(merged_DST_fti_realvækst$DST_FTI,
                           merged_DST_fti_realvækst$Aarlig_realvækst,
                           use = "complete.obs")

model_di_fti  <- lm(Aarlig_realvækst ~ DI_FTI, data = merged_di_fti_realvækst)
model_dst_fti <- lm(Aarlig_realvækst ~ DST_FTI, data = merged_DST_fti_realvækst)

# Samlet sammenligning (samme opstilling som DI's egen Boks 1-tabel)
sammenligning <- data.frame(
  Indikator      = c("DI-FTI", "FTI (DST)"),
  Korrelation    = c(korrelation_di_fti, korrelation_dst_fti),
  Forklaringsgrad = c(summary(model_di_fti)$r.squared, summary(model_dst_fti)$r.squared)
)
sammenligning
#Konklusion, vi kan se at DI fortsat har en stærkere sammenhæng med den årlige realvækst i forbrug. 
#da DI har i korr 0,6201 og en forr 0,3845 vs DST korr 0.5213 og Forr 0.2718
#det er højere en DST på korr 0,5213 og forr 0,2718
#korr og forr er faldet både for DI og DST. Der skal dog tages højde for at de tidligere tal, er regnet ud fra personlig forbrug, og vi her har regnet med husholdningernes forbrug -> argumenter for det valg i rapporten.


# ============================================================
# Opgave 2.2 FORUDSIGELSE AF Q3 2026
# ============================================================

# 1. Lav regressionsmodellen
# Y = årlig realvækst
# X = DI's forbrugertillidsindikator

# DI-FTI ------------------------------------------------------------

# Hent DI-FTI-værdien for 2026Q3
DI_2026Q3 <- di_fti_kvartal %>%
  filter(kvartal == "2026Q3") %>%
  select(DI_FTI)

# Lav prediction af årlig realvækst
prediction_DI <- predict(
  model_di_fti,
  newdata = DI_2026Q3,
  interval = "prediction"
)

prediction_DI


# DST-FTI -----------------------------------------------------------

# Hent DST-FTI-værdien for 2026Q3
DST_2026Q3 <- DST_fti_kvartal %>%
  filter(kvartal == "2026Q3") %>%
  select(DST_FTI)

View(DST_2026Q3)

# Lav prediction af årlig realvækst
prediction_DST <- predict(
  model_dst_fti,
  newdata = DST_2026Q3,
  interval = "prediction"
)

prediction_DST

# Fit: Modellens konkrete forudsigelse af den årlige realvækst.
# Lwr: Den nedre grænse for 95 % prediction-intervallet.
# Upr: Den øvre grænse for 95 % prediction-intervallet.

# Konklusion:
# DI-modellen forudsiger en årlig realvækst i husholdningernes forbrug på -0,28  %
# i 2026Q3, med et prediction-interval fra -4,69 % til 3,99 %.
#
# DST-modellen forudsiger en årlig realvækst på -0,80 %,
# med et prediction-interval fra -5,55 % til 3,94 %.
#
# Begge modeller forudsiger altså en svagt negativ realvækst i 2026Q3.
# Prediction-intervallerne er dog brede og indeholder både negativ og positiv vækst,
# hvilket viser, at der er stor usikkerhed forbundet med forudsigelserne.
#
# FTI-værdierne for 2026Q3 er kun baseret på juli og august,
# da data for september endnu ikke er tilgængelige.

# ============================================================
# Opgave 2.3  Salg resten af året
# ============================================================

# Begge modeller forudsiger en svagt negativ årlig realvækst i 2026Q3,
# på henholdsvis -0,35 % for DI-FTI og -0,81 % for DST-FTI.
# Det kan umiddelbart give anledning til bekymring for salget.

# Forbruget har dog haft positiv årlig realvækst siden 2024Q2,
# hvilket den simple regressionsmodel ikke direkte tager højde for.
# Modellen baserer predictionen på sammenhængen mellem FTI og realvækst
# i hele perioden og bruger kun FTI som forklarende variabel.

# Samtidig er prediction-intervallerne brede og indeholder både negativ
# og positiv vækst. Vi vil derfor ikke konkludere sikkert, at forbruget
# vil falde, men predictionen kan ses som et tegn på en risiko for lavere
# forbrugsvækst.

#-0,35 % virker lidt overraskende efter en længere periode med positiv vækst, men: 
#predictionen fortæller, hvad FTI-modellen alene siger, 
#ikke nødvendigvis hvad den seneste udvikling i forbruget peger på. 
#til det ville vi skulle lave en multipel regression hvor tidligere forbrugsvækst også bruges som forklarende variabel.


# ============================================================
# Opgave 3.1 - Beregn modellernes estimerede værdier
# ============================================================

# DI-FTI

# Hent de estimerede koefficienter fra DI-modellen
beta0_di <- coef(model_di_fti)[1]   # Intercept
beta1_di <- coef(model_di_fti)[2]   # Koefficient for DI_FTI

# Beregn hvilken realvækst modellen ville have forventet
# ud fra FTI-værdien i hvert kvartal
merged_di_fti_realvækst <- merged_di_fti_realvækst %>%
  mutate(
    fitted_manuelt = beta0_di + beta1_di * DI_FTI
  )


# DST-FTI

# Hent de estimerede koefficienter fra DST-modellen
beta0_dst <- coef(model_dst_fti)[1]   # Intercept
beta1_dst <- coef(model_dst_fti)[2]   # Koefficient for DST_FTI

# Beregn hvilken realvækst modellen ville have forventet
# ud fra FTI-værdien i hvert kvartal
merged_DST_fti_realvækst <- merged_DST_fti_realvækst %>%
  mutate(
    fitted_manuelt = beta0_dst + beta1_dst * DST_FTI
  )

#kan også løses med fitted()
#estimerede_værdier_di <- fitted(model_di_fti)
#estimerede_værdier_dst <- fitted(model_dst_fti)

# ============================================================
# Opgave 3.2 - Beregn residualer
# ============================================================

#Manuel kode:
# DI-FTI
merged_di_fti_realvækst <- merged_di_fti_realvækst %>%
  mutate(
    residual = Aarlig_realvækst - fitted_manuelt
  )

# DST-FTI
merged_DST_fti_realvækst <- merged_DST_fti_realvækst %>%
  mutate(
    residual = Aarlig_realvækst - fitted_manuelt
  )

#kan også løses med residuls()
#residualer_di <- residuals(model_di_fti)
#residualer_dst <- residuals(model_dst_fti)

#Individuelle plots for DI og DST

# Plot residualer for DI-FTI
ggplot(merged_di_fti_realvækst,
       aes(x = fitted_manuelt, y = residual)) +
  geom_point() +
  geom_hline(yintercept = 0, linetype = "dashed") +
  labs(
    title = "Residualer vs. estimerede værdier - DI-FTI",
    x = "Estimeret årlig realvækst",
    y = "Residual"
  ) +
  theme_minimal()


# Plot residualer for DST-FTI
ggplot(merged_DST_fti_realvækst,
       aes(x = fitted_manuelt, y = residual)) +
  geom_point() +
  geom_hline(yintercept = 0, linetype = "dashed") +
  labs(
    title = "Residualer vs. estimerede værdier - DST-FTI",
    x = "Estimeret årlig realvækst",
    y = "Residual"
  ) +
  theme_minimal()

# ============================================================
# Plot residualer over tid - DI-FTI og DST-FTI
# ============================================================

ggplot() +
  
  # Residualer for DI-FTI
  geom_point(
    data = merged_di_fti_realvækst,
    aes(x = kvartal, y = residual, group = 1, color = "DI-FTI"),
    position = "dodge"
  ) +
  
  # Residualer for DST-FTI
  geom_point(
    data = merged_DST_fti_realvækst,
    aes(x = kvartal, y = residual, group = 1, color = "DST-FTI"),
    position = "dodge"
  ) +
  
  # Vandret linje ved residual = 0
  geom_hline(
    yintercept = 0,
    linetype = "dashed"
  ) +
  
  labs(
    title = "Modellens præcision varierer over tid",
    subtitle = "I nogle perioder ligger estimaterne tæt på den faktiske realvækst, mens den \ni andre perioder afviger markant mere.",
    x = "Kvartal",
    y = "Residualer",
    color = "Model",
    caption = "Kilde: Egen tilvirkning efter data fra DST"
  ) +
  
  theme_minimal() +
  
  # Vis kun nogle af kvartalerne på x-aksen
  scale_x_discrete(
    breaks = merged_di_fti_realvækst$kvartal[
      seq(1, nrow(merged_di_fti_realvækst), by = 4)
    ]
  ) +
  
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1)
  )


# Noter til at forstå plottet
# Den stiplede linje ved 0 er referencen:
# Punkt over 0 → den faktiske realvækst var højere end modellen estimerede → modellen underestimerede.
# Punkt under 0 → den faktiske realvækst var lavere end modellen estimerede → modellen overestimerede.
# Jo længere et punkt er fra 0, desto større fejl lavede modellen for den observation.

# Konklusion
# Residualerne ligger både over og under 0 i begge modeller.
# Størrelsen på residualerne varierer dog over tid, og der er perioder,
# hvor modellerne rammer væsentligt længere fra den faktiske realvækst.
# Modellernes præcision er dermed ikke konstant over tid.

# ============================================================
# Opgave 3.3 - Beregn RSS og TSS
# ============================================================

# RSS for DI-FTI
RSS_di <- sum(merged_di_fti_realvækst$residual^2)

# RSS for DST-FTI
RSS_dst <- sum(merged_DST_fti_realvækst$residual^2)

# TSS - den samlede variation i den årlige realvækst
TSS <- sum(
  (merged_di_fti_realvækst$Aarlig_realvækst -
     mean(merged_di_fti_realvækst$Aarlig_realvækst))^2
)

RSS_di
RSS_dst
TSS

# TSS på 793,35 er den samlede variation i den årlige realvækst.
# RSS_di på 488,27 er den del af variationen, som DI-modellen ikke kan forklare.
# RSS_dst på 577,72 er den del af variationen, som DST-modellen ikke kan forklare.
# Forskellen mellem TSS og RSS er dermed den variation, som modellen kan forklare.

# ============================================================
# Opgave 3.4 - Beregn R²
# ============================================================


R2_di <- 1 - (RSS_di / TSS)
R2_dst <- 1 - (RSS_dst / TSS)

R2_di
R2_dst

# DI-modellen har en forklaringsgrad (R²) på 0,385, hvilket betyder, at modellen forklarer ca. 38,5 % af variationen i den årlige realvækst.

# DST-modellen har en forklaringsgrad (R²) på 0,272 og forklarer dermed ca. 27,2 % af variationen i den årlige realvækst.

# DI-FTI forklarer altså en større del af variationen i realvæksten end DST-FTI i vores modeller.