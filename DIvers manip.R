library(plyr)
library(tidyverse)
library(lubridate)
library(naniar)
library(scales)
library(ggsci)
library(GGally)
library(ggthemes)
library(lemon)


these <- as_tibble(read_csv("jeux_de_donnees/PhD_v2.csv"))
these$`Date de premiere inscription en doctorat` <- dmy(these$`Date de premiere inscription en doctorat`)
these$`Date de soutenance` <- dmy(these$`Date de soutenance`)
these$`Publication dans theses.fr` <- dmy(these$`Publication dans theses.fr`)
these$`Mise a jour dans theses.fr` <- dmy(these$`Mise a jour dans theses.fr`)
these$Statut <- as.factor(these$Statut)
these$`Langue de la these` <- as.factor(these$`Langue de la these`)
these$`Accessible en ligne` <- as.factor(these$`Accessible en ligne`)
these$`Identifiant directeur` <- na_if(these$`Identifiant directeur`,"na")

head(these)
glimpse(these)
summary(these)


these %>% 
  summarise(across(everything(), n_distinct)) %>% 
  glimpse()




### Données manquantes JdD entier
these_na <- these

#### fun stuff
these_na <- these_na %>% 
  select(c(2, 6, 8, 11, 12, 13, 14))

vis_miss(these_na, warn_large_data = FALSE)
gg_miss_upset(these_na)

### Real stuff
vis_miss(these, warn_large_data = FALSE) +
  scale_y_continuous(labels = comma) +
  theme(axis.text.x =  element_text(angle = 90))

gg_miss_upset(these)

#############################################################
these_NA <- these %>% select(Statut, `Date de premiere inscription en doctorat`, `Date de soutenance`, 
                                   Year, `Langue de la these`) 

these_NA_encours <- these_NA %>% filter(Statut == "enCours")
these_NA_soutenue <- these_NA %>% filter(Statut == "soutenue")

vis_miss(these_NA_encours, warn_large_data = FALSE) +
  scale_y_continuous(labels = comma) +
  theme(axis.text.x =  element_text(angle = 90))
gg_miss_upset(these_NA_encours)

vis_miss(these_NA_soutenue, warn_large_data = FALSE) +
  scale_y_continuous(labels = comma) +
  theme(axis.text.x =  element_text(angle = 90))
gg_miss_upset(these_NA_soutenue)





### Problemes soutenance these.
these_soutenance <- these %>% select(Year, `Date de soutenance`) 
these_soutenance <- these_soutenance %>% 
  mutate(month =  as.factor(month(`Date de soutenance`, label = TRUE)),
         day = as.factor(day(`Date de soutenance`)))

these_soutenance %>%
  filter(Year > 1983 & Year < 2019) %>% 
  count(month) %>% 
  ggplot(aes(month, n)) +
  geom_col(fill = "steelblue", color = "black") +
  scale_y_continuous(labels = comma,
                     breaks = seq(0, 300000, 50000)) +
  labs(x = "\nMois",
       y = "Total thèses soutenues\n") +
  theme_stata() +
  theme(axis.text.y = element_text(angle = 0))

#############################################################
these_soutenance %>% 
  filter(Year > 2004 & Year < 2019) %>% 
  count(Year, month) %>%
  ggplot(aes(month, n)) +
  geom_col(fill = "steelblue", color = "black") +
  facet_wrap( ~ Year) +
  scale_x_discrete(guide = guide_axis(angle = 90)) +
  labs(x = "\nMois",
       y = "Total\n") +
  theme_stata() +
  theme(axis.text.y = element_text(angle = 0))

#############################################################
these_soutenance_count_year <- these_soutenance %>% 
  filter(Year > 2004 & Year < 2019) %>% 
  count(Year) %>% 
  rename(total_year = n)

these_soutenance_year_month <-  these_soutenance  %>% 
  filter(Year > 2004 & Year < 2019) %>% 
  count(Year, month) %>% 
  rename(total_month = n)

these_soutenance_count_year_no_first <- these_soutenance %>% 
  filter(day != "1" & Year > 2004 & Year < 2019) %>% 
  count(Year) %>% 
  rename(total_year = n)

these_soutenance_year_month_no_first <-  these_soutenance  %>% 
  filter(day != "1" & Year > 2004 & Year < 2019) %>% 
  count(Year, month) %>% 
  rename(total_month = n)

these_soutenance_full <- full_join(these_soutenance_count_year, 
                                            these_soutenance_year_month, 
                                            by = "Year") %>% 
  mutate(freq = total_month / total_year) %>% 
  drop_na()

these_soutenance_full_no_first <- full_join(these_soutenance_count_year_no_first, 
                                   these_soutenance_year_month_no_first, 
                                   by = "Year") %>% 
  mutate(freq = total_month / total_year) %>% 
  drop_na()

these_soutenance_full %>% 
  ddply(~month, summarise, mean = mean(freq, na.rm = TRUE), sd = sd(freq, na.rm = TRUE)) %>% 
  ggplot(aes(month, mean)) +
  geom_col(fill = "steelblue", color = "black") +
  geom_errorbar(aes(ymin = mean - sd, ymax = mean + sd)) +
  scale_y_continuous(labels = percent_format()) +
  labs(x = "\nMois",
       y = "Pourcentage de soutenance\n") +
  theme_stata() +
  theme(axis.text.y = element_text(angle = 0))

these_soutenance_full_no_first %>% 
  ddply(~month, summarise, mean = mean(freq, na.rm = TRUE), sd = sd(freq, na.rm = TRUE)) %>% 
  ggplot(aes(month, mean)) +
  geom_col(fill = "steelblue", color = "black") +
  geom_errorbar(aes(ymin = mean - sd, ymax = mean + sd)) +
  scale_y_continuous(labels = percent_format()) +
  labs(x = "\nMois",
       y = "Pourcentage de soutenance\n") +
  theme_stata() +
  theme(axis.text.y = element_text(angle = 0))

#############################################################
these_soutenance_count_year <- these_soutenance %>% 
  count(Year) %>% 
  rename(total_year = n)

these_soutenance_count_janv <- these_soutenance %>% 
  filter(month == "janv" & day == "1") %>% 
  group_by(Year) %>% 
  count(Year) %>% 
  rename(total_janv = n)

these_soutenance_year_janv <- full_join(these_soutenance_count_year, 
                                           these_soutenance_count_janv, by = "Year") %>% 
  mutate(freq = total_janv / total_year)

these_soutenance_year_janv %>% 
  ggplot(aes(Year, freq)) +
  geom_line(color = "steelblue", size = 1) +
  scale_x_continuous(breaks = seq(1970, 2020, 5)) +
  scale_y_continuous(labels = percent_format()) +
  labs(x = "\nAnnée",
       y = "Pourcentage des défenses de thèses\n") +
  theme_stata() +
  theme(axis.text.y = element_text(angle = 0))


### Probleme homonyme
#### Probleme homonyme Cécile Martin
these_cecile_martin <- these %>% 
  filter(str_detect(Auteur, "Cecile Martin")) %>% 
  slice(-c(4,6, 8, 9, 12))

glimpse(these_cecile_martin %>% 
          summarise(across(everything(), n_distinct)))

#############################################################
these_cecile_martin %>% 
  vis_miss() +
  scale_y_continuous(labels = comma) +
  theme(axis.text.x =  element_text(angle = 90))

#############################################################
these_cecile_martin %>% 
  ggplot(aes(x = Year)) +
  geom_histogram(binwidth = 1, fill = "steelblue", color = "black") +
  scale_x_continuous(breaks = seq(1985, 2020, 3)) +
  scale_y_continuous(breaks = c(0, 1)) +
  labs(x = "\nAnnée",
       y = "Total thèse\n") +
  theme_stata() +
  theme(axis.text.x =  element_text(angle = 45,
                                    vjust = 0.5),
        axis.text.y = element_text(angle = 0))

#############################################################
these_cecile_martin %>% 
  filter(`Identifiant auteur` == "81323557") %>% 
  ggplot(aes(x = Year)) +
  geom_bar(binwidth = 1, fill = "steelblue", color = "black") +
  scale_x_continuous(breaks = seq(1990, 2002, 1)) +
  scale_y_continuous(breaks = c(0, 1)) +
  labs(x = "\nAnnée",
       y = "Total thèse\n") +
  theme_stata() +
  theme(axis.text.y = element_text(angle = 0))




##Outliers
these_director <- these[!grepl(",", these$`Directeur de these (nom prenom)`), ]
these_director <- these_director[!grepl("@", these_director$`Directeur de these (nom prenom)`), ]

these_director <- these_director %>%
  filter(Year > 1983 & Year < 2019) %>% 
  select(`Directeur de these (nom prenom)`, `Identifiant directeur`) %>% 
  group_by(`Directeur de these (nom prenom)`) %>% 
  mutate(total_these_diriger = n())

vis_miss(these_director, warn_large_data = FALSE) +
  scale_y_continuous(labels = comma) +
  theme(axis.text.x =  element_text(angle = 90))

dim(these_director)
n_distinct(these_director$`Directeur de these (nom prenom)`)
n_distinct(these_director$`Identifiant directeur`)


# Mean and Standard deviation (SD)
## Tmin, Tmax = mean(+-)(k*sd), k = 3
Tmin_msd <-  mean(these_director$total_these_diriger) - ( 3 * sd(these_director$total_these_diriger))
Tmax_msd <-  mean(these_director$total_these_diriger) + ( 3 * sd(these_director$total_these_diriger))
msd <- which(these_director$total_these_diriger < Tmin_msd | 
               these_director$total_these_diriger > Tmax_msd)
msd_outliers <- these_director[msd,]
min(msd_outliers$total_these_diriger)
max(msd_outliers$total_these_diriger)

# Median and Median Absolute Deviation (MAD)
## MAD = b * median(|xi - median(x)|), b = 1.4826 for normal distribution
## Tmin, Tmax = median(+-)(k*MAD), k = 3
med <- median(these_director$total_these_diriger)
abs_med <- abs(these_director$total_these_diriger - med)
mad <- 1.4826 * median(abs_med)
Tmin_mad <- med - ( 3 * mad)
Tmax_mad <- med + ( 3 * mad)
mad <- which(these_director$total_these_diriger < Tmin_mad | 
               these_director$total_these_diriger > Tmax_mad)
mad_outliers <- these_director[mad,]
min(mad_outliers$total_these_diriger)
max(mad_outliers$total_these_diriger)

# Interquartile Range (IQR)
## Tmin = Q1 - (c * IQR) c = 1.5(mild)
## Tmax = Q3 + (c * IQR) c = 1.5(mild)
summary(these_director)
IQR(these_director$total_these_diriger)
Tmin_iqr_mild <- 4 - (1.5 * 14)
Tmax_iqr_mild <- 18 + (1.5 * 14)
iqr_mild <- which(these_director$total_these_diriger < Tmin_iqr_mild | 
                    these_director$total_these_diriger > Tmax_iqr_mild)
iqr_mild_outliers <- these_director[iqr_mild,]
min(iqr_mild_outliers$total_these_diriger)
max(iqr_mild_outliers$total_these_diriger)

## Tmin = Q1 - (c * IQR) c = 3(extreme)
## Tmax = Q3 + (c * IQR) c = 3(extreme)
Tmin_iqr_ext <- 4 - (3 * 14)
Tmax_iqr_ext <- 18 + (3 * 14)
iqr_ext <- which(these_director$total_these_diriger < Tmin_iqr_ext | 
                   these_director$total_these_diriger > Tmax_iqr_ext)
iqr_ext_outliers <- these_director[iqr_ext,]
min(iqr_ext_outliers$total_these_diriger)
max(iqr_ext_outliers$total_these_diriger)

############################ Outliers viz #################################
these_director %>% 
  ggplot(aes(total_these_diriger)) +
  geom_histogram(binwidth = 3, fill = "steelblue", color = "Black") +
  geom_vline(aes(xintercept = median(total_these_diriger)), color = "yellow3", size = 1) +
  scale_y_continuous(labels = comma) +
  labs(x = "\nNombre de thèses",
       y = "Total directeurs\n") +
  scale_x_continuous(breaks = seq(0, 800, 50)) +
  theme_stata() +
  theme(axis.text.y = element_text(angle = 45))

these_director %>% 
  ggplot(aes(total_these_diriger)) +
  geom_histogram(binwidth = 1, fill = "steelblue", color = "Black") +
  geom_vline(aes(xintercept = median(total_these_diriger)), color = "yellow3", size = 1) +
  scale_x_continuous(limits = c(0, 220), breaks = seq(0, 220, 10)) +
  scale_y_continuous(labels = comma) +
  labs(x = "\nNombre de thèses",
       y = "Total directeurs\n") +
  theme_stata() +
  theme(axis.text.y = element_text(angle = 45))

these_director %>% 
  ggplot(aes(total_these_diriger)) +
  geom_histogram(binwidth = 1, fill = "steelblue", color = "Black") +
  geom_vline(aes(xintercept = median(total_these_diriger)), color = "yellow3", size = 1) +
  geom_vline(aes(xintercept = min(iqr_mild_outliers$total_these_diriger))) +
  scale_x_continuous(limits = c(0, 220), breaks = seq(0, 220, 10)) +
  scale_y_continuous(labels = comma) +
  labs(x = "\nNombre de thèses",
       y = "Total directeurs\n") +
  theme_stata() +
  theme(axis.text.y = element_text(angle = 45))

these_director %>% 
  ggplot(aes(total_these_diriger)) +
  geom_boxplot(color = "steelblue", outlier.color = "red", outlier.alpha = 0.5) +
  scale_x_continuous(breaks = seq(0, 750, 50)) +
  labs(x = "\nNombre de thèses") +
  theme_stata() +
  theme(axis.text.y = element_text(angle = 0))

these_director %>% 
  filter(total_these_diriger >= 40) %>% 
  ggplot(aes(total_these_diriger)) +
  geom_histogram(binwidth = 5, fill = "steelblue", color = "Black") +
  scale_x_continuous(breaks = seq(40, 720, 30)) +
  scale_y_continuous(labels = comma) +
  labs(x = "\nNombre de thèses",
       y = "Total directeurs\n") +
  theme_stata() +
  theme(axis.text.y = element_text(angle = 45))

############################ Outliers small #################################
these_director_outliers <- these_director %>% filter(total_these_diriger >= 40 &
                                                       total_these_diriger <= 140) 
dim(these_director_outliers)
n_distinct(these_director_outliers$`Directeur de these (nom prenom)`)
n_distinct(these_director_outliers$`Identifiant directeur`)
glimpse(these_director_outliers)

vis_miss(these_director_outliers)

ggplot(these_director_outliers, aes(total_these_diriger)) +
  geom_histogram(binwidth = 2, fill = "steelblue", color = "Black") +
  geom_vline(aes(xintercept = median(total_these_diriger)), color = "yellow3", size = 1) +
  labs(x = "\nNombre de thèses",
       y = "Total directeurs\n") +
  scale_x_continuous(breaks = seq(40, 140, 10)) +
  scale_y_continuous(breaks = seq(0, 2600, 400)) +
  theme_stata() +
  theme(axis.text.y = element_text(angle = 45))

############################ Outliers middle #################################
these_director_outliers_middle <- these_director %>% filter(total_these_diriger >= 140 &
                                                              total_these_diriger <= 240) 
dim(these_director_outliers_middle)
n_distinct(these_director_outliers_middle$`Directeur de these (nom prenom)`)
n_distinct(these_director_outliers_middle$`Identifiant directeur`)
glimpse(these_director_outliers_middle)

vis_miss(these_director_outliers_middle)

ggplot(these_director_outliers_middle, aes(total_these_diriger)) +
  geom_histogram(binwidth = 1, fill = "steelblue", color = "Black") +
  geom_vline(aes(xintercept = median(total_these_diriger)), color = "yellow3", size = 1) +
  labs(x = "\nNombre de thèses",
       y = "Total directeurs\n") +
  scale_x_continuous(breaks = seq(170, 210, 10)) +
  scale_y_continuous(breaks = seq(0, 200, 20)) +
  theme_stata() +
  theme(axis.text.y = element_text(angle = 45))

########################### Outliers big ##################################
these_director_outliers_big <- these_director %>% filter(total_these_diriger > 250)  
dim(these_director_outliers_big)
n_distinct(these_director_outliers_big$`Directeur de these (nom prenom)`)
unique(these_director_outliers_big$`Directeur de these (nom prenom)`)
n_distinct(these_director_outliers_big$`Identifiant directeur`)
glimpse(these_director_outliers_big)

vis_miss(these_director_outliers_big)



######################### Langue thingy #####################################
these_langue <- these
these_langue <- rename(these_langue, Langue = `Langue de la these`)
these_langue <- these_langue %>% 
  mutate(Langue = as.factor(case_when(
    is.na(Langue) ~ "NA",
    Langue ==  "fr" ~ "Français",
    Langue == "en" ~ "Anglais",
    Langue == "enfr" | Langue == "fren" ~ "Bilingue",
    TRUE ~ "Autres")))
levels(these_langue$Langue)
summary(these_langue)

vis_miss(these_langue, warn_large_data = FALSE)+
  scale_y_continuous(labels = comma) +
  theme(axis.text.x =  element_text(angle = 90))

these_langue %>% 
  summarise(across(everything(), n_distinct)) %>% 
  glimpse()

##############################################################
these_langue_count_year <- these_langue %>% 
  select(Year) %>% 
  count(Year) %>% 
  rename(total_year = n)

these_langue_count_langue <- these_langue %>%
  count(Year, Langue) %>% 
  rename(langue_count = n)

these_langue_fulljoin <- full_join(these_langue_count_year,
               these_langue_count_langue,
               by = "Year") %>% 
  mutate(freq = langue_count / total_year)

these_langue_fulljoin %>% 
ggplot(aes(Year, freq,  color = Langue)) +
  geom_line(size = 1) + 
  scale_x_continuous(breaks = seq(1970, 2020, 5)) +
  scale_y_continuous(labels = percent_format()) +
  labs(x = "\nAnnées",
       y = "Fréquences\n") +
  theme_stata() +
  theme(axis.text.y = element_text(angle = 0))

these_langue_fulljoin %>% 
  filter(Year >= 2004 & Year <= 2019) %>% 
  ggplot(aes(Year, freq, color = Langue)) +
  geom_step(size = 1) + 
  scale_x_continuous(breaks = seq(2004, 2019, 1)) +
  scale_y_continuous(labels = percent_format()) +
  labs(x = "\nAnnées",
       y = "Fréquences\n") +
  theme_stata() +
  theme(axis.text.x =  element_text(angle = 45,
                                    vjust = 0.5),
        axis.text.y = element_text(angle = 0,
                                   hjust = 0.5))

## Travail en bonus
######################### heatmap ##########################
these_missing_heatmap <- these %>% 
  select(Year, `Langue de la these`, `Identifiant etablissement`, `Identifiant directeur`,
         `Identifiant auteur`, `Date de soutenance`, `Date de premiere inscription en doctorat`, 
         Statut) %>% 
  group_by(Statut) %>% 
  miss_var_summary()
these_missing_heatmap$pct_miss <- round(these_missing_heatmap$pct_miss, 1)

these_missing_heatmap %>% 
  ggplot(aes(Statut, variable, fill = pct_miss)) +
  geom_tile(color = "black") +
  geom_text(aes(label = pct_miss), color = "black", size = 4) +
  labs(y = "Variables\n") +
  scale_fill_gradient2(low = "#075AFF",
                       mid = "#FFFFCC",
                       high = "#FF0000") +
  guides(fill = guide_colourbar(barwidth = 0.5,
                                barheight = 15)) +
  coord_fixed()

gg_miss_fct(x = these, fct = Statut)

######################### Discipline/Genre/Langue ##########################
#### these_v2
these_gender <- as_tibble(read_csv("jeux_de_donnees/PhD_v2_gender.csv"))
these_gender <- subset(these_gender, select = -c(...1))
these_gender$`Date de premiere inscription en doctorat` <- dmy(these_gender$`Date de premiere inscription en doctorat`)
these_gender$`Date de soutenance` <- dmy(these_gender$`Date de soutenance`)
these_gender$`Publication dans theses.fr` <- dmy(these_gender$`Publication dans theses.fr`)
these_gender$`Mise a jour dans theses.fr` <- dmy(these_gender$`Mise a jour dans theses.fr`)
these_gender$Statut <- as.factor(these_gender$Statut)
these_gender$`Langue de la these` <- as.factor(these_gender$`Langue de la these`)
these_gender$`Accessible en ligne` <- as.factor(these_gender$`Accessible en ligne`)
these_gender$`Identifiant directeur` <- na_if(these_gender$`Identifiant directeur`,"na")
these_gender$gender <- as.factor(these_gender$gender)
these_gender$Gender <- these_gender$gender


these_gender %>% 
  subset(select = -c(gender)) %>% 
  summarise(across(everything(), n_distinct)) %>% 
  glimpse()

these_gender_top_5_discipline <- these_gender %>% 
  select(Discipline, Gender) %>% 
  count(Discipline, sort = TRUE) %>% 
  slice(1:5) %>% 
  subset(select = -c(n)) %>% 
  pull()

these_gender_top_5_discipline <- these_gender %>% 
  filter(Discipline %in% these_gender_top_5_discipline)


these_gender_top_5_discipline %>% 
  ggplot(aes(Gender, after_stat(count/sum(count)), fill = Discipline)) + 
  geom_bar( position = "dodge", color = "black") + 
  facet_wrap( ~ Discipline, scales = "free") +
  scale_y_continuous(labels = percent_format()) +
  labs(y = "Proportions") +
  theme_stata() +
  theme(legend.position = "none",
        axis.text.x = element_text(angle = 45, hjust = 1, size = 9),
        strip.text.x = element_text(size = 10),
        axis.text.y = element_text(angle = 0))

these_gender_top_5_discipline %>% 
  ggplot(aes(Gender, after_stat(count/sum(count)), fill = Discipline, by = Gender)) + 
  geom_bar( position = "fill", color = "black") + 
  geom_text(stat = "prop", position = position_fill(.5), 
            colour = "white", fontface = "bold", size = 3.5) +
  scale_y_continuous(labels = percent_format()) +
  labs(y = "Proportions") +
  theme_stata() +
  theme(legend.position = "bottom",
        legend.key.size = unit(0.3, 'cm'), 
        legend.key.height = unit(0.3, 'cm'), 
        legend.key.width = unit(0.3, 'cm'), 
        legend.title = element_text(size=14), 
        legend.text = element_text(size=10),
        axis.text.y = element_text(angle = 0))

these_gender_top_5_discipline %>% 
  ggplot(aes(Gender, after_stat(count/sum(count)), fill = Discipline)) + 
  geom_bar(position = "dodge", color = "black") + 
  scale_y_continuous(labels = percent_format()) +
  labs(y = "Proportions") +
  theme_stata() +
  theme(legend.position = "bottom",
        legend.key.size = unit(0.3, 'cm'), 
        legend.key.height = unit(0.3, 'cm'), 
        legend.key.width = unit(0.3, 'cm'), 
        legend.title = element_text(size = 14), 
        legend.text = element_text(size = 10),
        axis.text.y = element_text(angle = 0))


#### these_v3
these_v3 <- as_tibble(read_csv("jeux_de_donnees/PhD_v3.csv"))
these_v3 <- subset(these_v3, select = -c(...1))
these_v3$`Date de premiere inscription en doctorat` <- dmy(these_v3$`Date de premiere inscription en doctorat`)
these_v3$`Date de soutenance` <- dmy(these_v3$`Date de soutenance`)
these_v3$`Publication dans theses.fr` <- dmy(these_v3$`Publication dans theses.fr`)
these_v3$`Mise a jour dans theses.fr` <- dmy(these_v3$`Mise a jour dans theses.fr`)
these_v3$Statut <- as.factor(these_v3$Statut)
these_v3$`Langue de la these` <- as.factor(these_v3$`Langue de la these`)
these_v3$`Accessible en ligne` <- as.factor(these_v3$`Accessible en ligne`)
these_v3$`Identifiant directeur` <- na_if(these_v3$`Identifiant directeur`,"na")
these_v3$Genre <- as.factor(these_v3$Genre)
these_v3 <- rename(these_v3, Discipline_prediction = `Discipline_prÃ©di`)
these_v3$Discipline_prediction <- as.factor(these_v3$Discipline_prediction)
levels(these_v3$Discipline_prediction)[levels(these_v3$Discipline_prediction) == "MathÃ©matiques"] <- "Mathematiques"
levels(these_v3$Discipline_prediction)[levels(these_v3$Discipline_prediction) == "Science de l'ingÃ©nieur" ] <- "Science de l'ingenieur" 


these_v3 %>% 
  summarise(across(everything(), n_distinct)) %>% 
  glimpse()

these_v3 %>% 
  ggplot(aes(Genre, after_stat(count/sum(count)), fill = Discipline_prediction)) + 
  geom_bar( position = "dodge", color = "black") + 
  facet_wrap( ~ Discipline_prediction, scales = "free") +
  scale_y_continuous(labels = percent_format()) +
  labs(y = "Proportions") +
  theme_stata() +
  theme(legend.position = "none",
        axis.text.x = element_text(angle = 45, hjust = 1),
        strip.text.x = element_text(size = 10),
        axis.text.y = element_text(angle = 0))

these_v3 %>% 
  ggplot(aes(Genre, after_stat(count/sum(count)), fill = Discipline_prediction, by = Genre)) + 
  geom_bar( position = "fill", color = "black") + 
  geom_text(stat = "prop", position = position_fill(.5), 
            colour = "white", fontface = "bold", size = 3.5) +
  scale_y_continuous(labels = percent_format()) +
  theme_stata() +
  theme(legend.position = "bottom",
        axis.text.y = element_text(angle = 0))

these_v3 %>% 
  ggplot(aes(Genre, after_stat(count/sum(count)), fill = Discipline_prediction)) + 
  geom_bar(position = "dodge", color = "black") + 
  scale_y_continuous(labels = percent_format()) +
  theme_stata() +
  theme(legend.position = "bottom",
        axis.text.y = element_text(angle = 0))

######################### Discipline/Genre ##########################
these_v3_count_year <- these_v3 %>% 
  filter(Year >= 1985 & Year <= 2018) %>% 
  count(Year) %>% 
  rename(total_year = n)

these_v3_year_genre_discipline <-  these_v3  %>% 
  filter(Year >= 1985 & Year <= 2018) %>% 
  count(Year, Genre, Discipline_prediction) %>% 
  rename(total = n)

these_genre_discipline_full <- full_join(these_v3_count_year, 
                                         these_v3_year_genre_discipline, 
                                         by = "Year") %>% 
  mutate(freq = total / total_year) %>% 
  drop_na()

these_genre_discipline_full %>% 
  ggplot(aes(Year, freq, fill = Discipline_prediction)) +
  geom_area(color = "black") +
  facet_wrap( ~ Genre, scales = "free_y") +
  scale_x_continuous(breaks = seq(1985, 2020, 5)) +
  scale_y_continuous(labels = percent_format()) +
  labs(x = "\nYear",
       y = "Proportions\n")  +
  theme_stata() +
  theme(axis.text.x =  element_text(angle = 45,
                                    vjust = 0.5),
        axis.text.y = element_text(angle = 0,
                                   hjust = 0.5),
        legend.key.size = unit(0.2, "cm"),
        legend.text = element_text(size = '8'),
        legend.title = element_blank())


######################### Discipline/Langue ##########################
these_v3_langue <- these_v3
these_v3_langue <- rename(these_v3_langue, Langue = `Langue de la these`)
these_v3_langue <- these_v3_langue %>% 
  mutate(Langue = as.factor(case_when(
    is.na(Langue) ~ "NA",
    Langue ==  "fr" ~ "Français",
    Langue == "en" ~ "Anglais",
    Langue == "enfr" | Langue == "fren" ~ "Bilingue",
    TRUE ~ "Autres")))

these_soutenance_year_langue_discipline <-  these_v3_langue  %>% 
  filter(Year >= 1985 & Year <= 2018) %>% 
  count(Year, Langue, Discipline_prediction) %>% 
  rename(total = n)

these_langue_discipline_full <- full_join(these_v3_count_year, 
                                         these_soutenance_year_langue_discipline, 
                                         by = "Year") %>% 
  mutate(freq = total / total_year)

these_glangue_discipline_full %>% 
  ggplot(aes(Year, freq, fill = Discipline_prediction)) +
  geom_area(color = "black", alpha = 0.7) +
  facet_wrap( ~ Langue , scales = "free_y") +
  scale_x_continuous(breaks = seq(1985, 2020, 5)) +
  scale_y_continuous(labels = percent_format()) +
  labs(y = "Proportions\n") +
  theme_stata() +
  theme(axis.text.x =  element_text(angle = 45,
                                    vjust = 0.5),
        axis.text.y = element_text(angle = 0,
                                   hjust = 0.5),
        legend.key.size = unit(0.2, "cm"),
        legend.text = element_text(size = '10'),
        legend.title = element_blank())

############################### SQL #######################
business <- read_csv("jeux_de_donnees/business.csv")
View(business)
glimpse(business)
