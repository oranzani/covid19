###########################################################
###
###   Cumulative curves Spain / Lombardy
###   Otavio Ranzani, 18/03/2020
###
###########################################################

#### loading packages ####
library(tidylog)
library(dplyr)
library(RCurl)
library(ggplot2)
library(ggrepel)
library(ggforce)
library(lubridate)

#### downloading data from Italy, official from DPC ####
urlfile_province <- "https://raw.githubusercontent.com/pcm-dpc/COVID-19/master/dati-province/dpc-covid19-ita-province.csv"

province <- readr::read_csv(url(urlfile_province))

####downloading data from Spain, extract by @victorvicpal from PDFs of Ministry of Health reports  ####
urlfile_comunidad <- "https://raw.githubusercontent.com/victorvicpal/COVID19_es/master/data/csv_data/data/dataCOVID19_es.csv"

comunidad <- readr::read_csv(url(urlfile_comunidad))

#### Italy pre-plot  ####
## Building x-axis after 40 cases or more
# Italy
province_long_40 <- province %>% 
  filter(totale_casi>=40 & denominazione_regione == "Lombardia") %>% 
  group_by(denominazione_provincia) %>% 
  mutate(day = 1:n())

# selecting most affected to plot
province_long_800 <- province_long_40 %>% 
  filter(max(totale_casi) >= 800)

# preparing for label

province_long_800_label <- province_long_800 %>% 
  filter(totale_casi==max(totale_casi)) %>%
  distinct(denominazione_regione, .keep_all = TRUE)

province_long_800_label_2 <- province_long_800 %>% 
  filter(data == lubridate::as_datetime("2020-03-08 18:00:00") & # Lockdown Bergamo
           denominazione_provincia %in% c("Bergamo", "Lodi")) %>% 
  mutate(desc = "Start measures of lockdown",
         day = case_when(denominazione_provincia == "Lodi" ~  0L, # Lockdown Lodi 23/02, considered 0
                         TRUE ~ day),
         totale_casi = case_when(denominazione_provincia == "Lodi" ~  100,
                                 TRUE ~ totale_casi))

alpha_ifi <- c(rep(0.5,59), # highlight Lodi
               rep(1,22),
               rep(0.5,36))


#### Spain pre-plot ####

comunidad_long_40 <- comunidad %>% 
  filter(casos>=40) %>% 
  group_by(CCAA) %>% 
  mutate(day = 1:n())

# selecting most affected to plot
comunidad_long_300 <- comunidad_long_40 %>% 
  filter(max(casos) >= 300)


# preparing for label and adjusting for ggrepel
comunidad_long_300_label <- comunidad_long_300 %>% 
  filter(casos==max(casos)) %>%
  distinct(CCAA, .keep_all = TRUE) %>% 
  mutate(day = case_when(
    CCAA == "CastillaLaMacha" ~  6L, #castilla-lamancha
    CCAA == "Andalucía" ~  9L, #andalucia
    TRUE ~ day))

labelx   <- c("Castilla-La Mancha")
labely   <- c("Andalucía", "Castilla y León")

alpha_ife <- c(rep(0.5,45), # highlight La Rioja y País Vasco
               rep(1,12),
               rep(0.5,20),
               rep(1, 12))

# lockdown Madrid, LaRioja, PaísVasco
comunidad_long_300_label_3 <- comunidad_long_300 %>% filter(fecha == "2020-03-10") 

comunidad_long_300_label_3$desc <- "Start measures of lockdown"


#### Ploting Italy  ####

quartz( , 9.5, 7) # open for Mac

province_long_800 %>% 
  ggplot(aes(x = day, y = totale_casi, col = denominazione_provincia)) + 
  geom_line(size = 1, alpha = alpha_ifi) +
  theme(legend.position = "none") +
  ggtitle("COVID-19 in most affect Provinces of Lombardy, Italy (as of 17/03/2020)") +
  labs(x = "Days from 40 cases or more", 
       y = "Cumulative number of confirmed cases (log scale)",
       caption = "Data from: Dipartimento di Protezione Civile, Italy") +
  annotate("text", x = 20, y = 40, label = "@otavio_ranzani", 
           colour = "blue", size = 4)  +
  scale_y_log10() +
  scale_x_continuous(breaks = 0:22) + 
  theme(axis.text.y = element_text(size = 14),
        axis.title.y = element_text(size = 16),
        axis.title.x = element_text(size = 14),
        axis.text.x = element_text(size =12)) + 
  geom_text_repel(data = province_long_800_label, 
                  aes(label = denominazione_provincia),
                  direction = "both",
                  force = 5) +
  geom_mark_ellipse(data = province_long_800_label_2,
                    aes(fill = denominazione_provincia, 
                        description = desc),
                    label.fontsize = 12)

#### Plotting Spain ####
quartz( , 12, 8) # open for Mac

comunidad_long_300 %>% 
  ggplot(aes(x = day, y = casos, col = CCAA)) + 
  geom_line(size = 1, 
            alpha = alpha_ife) +
  theme(legend.position = "none") +
  ggtitle("COVID-19 in most affect Regions of Spain (Boletín 17/03/2020)") +
  labs(x = "Days from 40 cases or more", 
       y = "Cumulative number of confirmed cases (log scale)",
       caption = "Data from: Spanish Ministry of Health, extract by @victorvicpal") +
  annotate("text", x = 14.2, y = 40, label = "@otavio_ranzani",
           colour = "blue", size = 5)  +
  scale_y_log10() +
  scale_x_continuous(breaks = 1:15) + 
  theme(axis.text.y = element_text(size = 14),
        axis.title.y = element_text(size = 16),
        axis.title.x = element_text(size = 14),
        axis.text.x = element_text(size =12)) + 
  geom_text_repel(data = comunidad_long_300_label, 
                  aes(label = CCAA),
                  direction = "both",
                  force = 5,
                  nudge_x = 
                    ifelse(comunidad_long_300_label$CCAA %in% labelx, -2, 0.5),
                  nudge_y = 
                    ifelse(comunidad_long_300_label$CCAA %in% labely, 0.1, 0)) +
  geom_mark_ellipse(data = comunidad_long_300_label_3,
                    aes(fill = CCAA, filter = CCAA %in% c("Madrid", "LaRioja", "PaísVasco"),
                        description = desc),
                    label.fontsize = 12)