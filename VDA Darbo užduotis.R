################################################################################
####  VDA Darbo užduotis
################################################################################

library(flipAPI)
library(dplyr)
library(magrittr)
library(tidyverse)
library(dplyr)
library(ggplot2)
library(ggrepel)
library(ggtext)
library(ggthemes)
library(directlabels)

################################################################################
####  Duomenų tvarkymas
################################################################################

### Duomenų importavimas
covid.atvejai <- read.csv('Agreguoti_COVID19_atvejai_ir_mirtys_808338661064373677.csv')
gyv.skaicius <- read.csv('gyv_skaicius_2022.csv')

### Nereikalingų duomenų ištrinimas
covid.atvejai <- subset(covid.atvejai, select = -c(infection_1, infection_2,
                                                   infection_3, infection_4,
                                                   deaths_all, deaths_cov1,
                                                   deaths_cov2, deaths_cov3, object_id))

### Pasirenku didžiausio sergamumo laikotarpį
covid.atvejai$date <- as.Date(covid.atvejai$date, '%m/%d/%Y')
covid.atvejai <- subset(covid.atvejai, date >= as.Date('2022-01-24') & date <= as.Date('2022-02-22'))

### Amiaus grupių vienodas perkodavimas
gyv.skaicius$age_gr <- recode(gyv.skaicius$age_gr,
                 "0–4" = "0-9",
                 "5–9" = "0-9",
                 "10–14" = "10-19",
                 "15–19" = "10-19",
                 "20–24" = "20-29",
                 "25–29" = "20-29",
                 "30–34" = "30-39",
                 "35–39" = "30-39",
                 "40–44" = "40-49",
                 "45–49" = "40-49",
                 "50–54" = "50-59",
                 "55–59" = "50-59",
                 "60–64" = "60-69",
                 "65–69" = "60-69",
                 "70–74" = "70-79",
                 "75–79" = "70-79",
                 "80–84" = "80 ir vyresni",
                 "85 ir vyresni" = "80 ir vyresni",
)
unique(gyv.skaicius$age_gr)
table(gyv.skaicius$age_gr)

### Keičiu stulpelių eiliškumą
covid.atvejai <- covid.atvejai %>% 
  relocate(sex, .after="municipality_name") %>% 
  relocate(age_gr, .after="municipality_name") %>% 
  relocate(date, .after="sex")

covid.atvejai$age_gr <- recode(covid.atvejai$age_gr,
                              "80-89" = "80 ir vyresni",
                              "90-99" = "80 ir vyresni",
                              "100-109" = "80 ir vyresni",
                              "110-119" = "80 ir vyresni")

### Gyvontojų kiekio sustambinimas nuo 5 iki 10 metų tarpo
gyv.skaicius.aggr <- aggregate(gyv.skaicius$pop,
                               by = list(gyv.skaicius$municipality_name,
                                         gyv.skaicius$age_gr,
                                         gyv.skaicius$sex),
                               FUN = sum)

### Stulpelių pavadinimų suvienodinimas
names(gyv.skaicius.aggr)[names(gyv.skaicius.aggr) == 'Group.1'] <- 'municipality_name'
names(gyv.skaicius.aggr)[names(gyv.skaicius.aggr) == 'Group.2'] <- 'age_gr'
names(gyv.skaicius.aggr)[names(gyv.skaicius.aggr) == 'Group.3'] <- 'sex'
names(gyv.skaicius.aggr)[names(gyv.skaicius.aggr) == 'x'] <- 'pop'

### Duomenų masyvų sujungimas

intersect(names(covid.atvejai),
          names(gyv.skaicius))

summary(covid.atvejai)
summary(gyv.skaicius)

covid.atvejai.gyv.skaicius <- merge(covid.atvejai, gyv.skaicius.aggr, all.x = TRUE,
                                    by=c("municipality_name", "age_gr", "sex"))

### "Nenustatyta" reikšmių ištrinimas
covid.atvejai.gyv.skaicius <- covid.atvejai.gyv.skaicius[!grepl('Nenustatyta',
                                                                covid.atvejai.gyv.skaicius$municipality_name),]
covid.atvejai.gyv.skaicius <- covid.atvejai.gyv.skaicius[!grepl('Nenustatyta',
                                                                covid.atvejai.gyv.skaicius$age_gr),]
covid.atvejai.gyv.skaicius <- covid.atvejai.gyv.skaicius[!grepl('Nenustatyta',
                                                                covid.atvejai.gyv.skaicius$sex),]

################################################################################
####  Antrinio sergamumo rodiklio skaičiavimas
################################################################################

### Stulpelių pervadinimas
names(covid.atvejai.gyv.skaicius)[names(covid.atvejai.gyv.skaicius) == 'incidence'] <- 'serg.incidence'
names(covid.atvejai.gyv.skaicius)[names(covid.atvejai.gyv.skaicius) == 'pop'] <- 'serg.pop'

# Duomenų sustambinimas iki savivaldybių/dienų lygmens, susumuojant COVID susirgimų ir populiacijos duomenys

covid.serg <- covid.atvejai.gyv.skaicius %>% 
  group_by(municipality_name, date) %>%
  summarise(across(starts_with('serg'), sum))

### Antrinio sergamumo rodiklo skaičiavimas, normalizuoto prie populiacijos

covid.serg$serg <- (covid.serg$serg.incidence/covid.serg$serg.pop)*100000

# Ištrinu nebereikalingus duomenys
covid.serg <- subset(covid.serg, select = -c(serg.incidence, serg.pop))

################################################################################
####  Savivaldybių/dienų rangavimas
################################################################################

dienos <- split(covid.serg, list(covid.serg$date)) #Išskiriu atskiras dienas skaičiavimui

# Savivaldybių rangavimas kiekvieną dieną
dienos$`2022-01-24`$serg.rank <- rank(dienos$`2022-01-24`$serg)
dienos$`2022-01-25`$serg.rank <- rank(dienos$`2022-01-25`$serg)
dienos$`2022-01-26`$serg.rank <- rank(dienos$`2022-01-26`$serg)
dienos$`2022-01-27`$serg.rank <- rank(dienos$`2022-01-27`$serg)
dienos$`2022-01-28`$serg.rank <- rank(dienos$`2022-01-28`$serg)
dienos$`2022-01-29`$serg.rank <- rank(dienos$`2022-01-29`$serg)
dienos$`2022-01-30`$serg.rank <- rank(dienos$`2022-01-30`$serg)
dienos$`2022-01-31`$serg.rank <- rank(dienos$`2022-01-31`$serg)
dienos$`2022-02-01`$serg.rank <- rank(dienos$`2022-02-01`$serg)
dienos$`2022-02-02`$serg.rank <- rank(dienos$`2022-02-02`$serg)
dienos$`2022-02-03`$serg.rank <- rank(dienos$`2022-02-03`$serg)
dienos$`2022-02-04`$serg.rank <- rank(dienos$`2022-02-04`$serg)
dienos$`2022-02-05`$serg.rank <- rank(dienos$`2022-02-05`$serg)
dienos$`2022-02-06`$serg.rank <- rank(dienos$`2022-02-06`$serg)
dienos$`2022-02-07`$serg.rank <- rank(dienos$`2022-02-07`$serg)
dienos$`2022-02-08`$serg.rank <- rank(dienos$`2022-02-08`$serg)
dienos$`2022-02-09`$serg.rank <- rank(dienos$`2022-02-09`$serg)
dienos$`2022-02-10`$serg.rank <- rank(dienos$`2022-02-10`$serg)
dienos$`2022-02-11`$serg.rank <- rank(dienos$`2022-02-11`$serg)
dienos$`2022-02-12`$serg.rank <- rank(dienos$`2022-02-12`$serg)
dienos$`2022-02-13`$serg.rank <- rank(dienos$`2022-02-13`$serg)
dienos$`2022-02-14`$serg.rank <- rank(dienos$`2022-02-14`$serg)
dienos$`2022-02-15`$serg.rank <- rank(dienos$`2022-02-15`$serg)
dienos$`2022-02-16`$serg.rank <- rank(dienos$`2022-02-16`$serg)
dienos$`2022-02-17`$serg.rank <- rank(dienos$`2022-02-17`$serg)
dienos$`2022-02-18`$serg.rank <- rank(dienos$`2022-02-18`$serg)
dienos$`2022-02-19`$serg.rank <- rank(dienos$`2022-02-19`$serg)
dienos$`2022-02-20`$serg.rank <- rank(dienos$`2022-02-20`$serg)
dienos$`2022-02-21`$serg.rank <- rank(dienos$`2022-02-21`$serg)
dienos$`2022-02-22`$serg.rank <- rank(dienos$`2022-02-22`$serg)

covid.serg.rang <- do.call("rbind", dienos) # Sujungiu ranguotous duomenys kartu
covid.serg.rang <- subset(covid.serg.rang, select = -serg) # Ištrinu nereikalingą stulpėlį

# Vidutinio savivaldybės rango skaičiavimas
rangas <- aggregate(covid.serg.rang$serg.rank,
                    by = list(covid.serg.rang$municipality_name),
                    FUN = mean)

################################################################################
####  Vizualizavimas
################################################################################

# Sukuriu atskirus kintamuosius, kad paspalvinti pirmas ir paskutines 5 savivaldybes
covid.serg.rang <- mutate(covid.serg.rang,
                          color = case_when(
                            municipality_name=="Kupiškio r. sav." ~ 1,
                            municipality_name=="Trakų r. sav." ~ 2,
                            municipality_name=="Neringos sav." ~ 3,
                            municipality_name=="Visagino sav." ~ 4,
                            municipality_name=="Palangos m. sav." ~ 5,
                            municipality_name=="Širvintų r. sav.." ~ 6,
                            municipality_name=="Kelmės r. sav." ~ 7,
                            municipality_name=="Vilkaviškio r. sav." ~ 8,
                            municipality_name=="Tauragės r. sav." ~ 9,
                            municipality_name=="Anykščių r. sav." ~ 10,
                            TRUE ~ NA
                       ))
table(covid.serg.rang$color)

covid.serg.rang <- mutate(covid.serg.rang,
                          color.2 = case_when(
                            municipality_name=="Vilniaus m. sav." ~ 1,
                            municipality_name=="Birštono sav." ~ 2,
                            municipality_name=="Šiaulių m. sav." ~ 3,
                            municipality_name=="Klaipėdos m. sav." ~ 4,
                            municipality_name=="Palangos m. sav." ~ 5,
                            municipality_name=="Pasvalio r. sav." ~ 6,
                            municipality_name=="Lazdijų r. sav." ~ 7,
                            municipality_name=="Panevėžio r. sav." ~ 8,
                            municipality_name=="Kauno r. sav." ~ 9,
                            municipality_name=="Alytaus r. sav." ~ 10,
                            TRUE ~ NA
                          ))
table(covid.serg.rang$color.2)

covid.serg.rang <- mutate(covid.serg.rang,
                          width = case_when(
                            color >= 1 ~ 2,
                            TRUE ~ 1
                          ))
table(covid.serg.rang$width)

covid.serg.rang <- mutate(covid.serg.rang,
                          width.2 = case_when(
                            color.2 >= 1 ~ 2,
                            TRUE ~ 1
                          ))
table(covid.serg.rang$width.2)

# Paskutinės dienos rango vizualizavimas
ggplot(covid.serg.rang, aes(x = date, y = serg.rank)) +
  geom_point(aes(group = municipality_name, color = factor(color)), size = 1.75, alpha = 0.8) +
  geom_line(aes(group = municipality_name, color = factor(color), size = factor(width)), alpha = 0.8) +
  geom_text(data=covid.serg.rang %>% filter(date=="2022-02-22"),
            aes(label = municipality_name, color = factor(color)),
            nudge_x = 0.5, hjust = 0, size = 3) +
  scale_x_continuous(breaks = covid.serg.rang$date,
                     expand = expansion(mult = c(0.005, 0.11))) +
  scale_size_discrete(range = c(0.4, 3)) +
  scale_colour_manual(values=c(
    "#f94144",
    "#f3722c",
    "#f8961e",
    "#f9844a",
    "#f9c74f",
    "#90be6d",
    "#43aa8b",
    "#4d908e",
    "#577590",
    "#277da1")) +
  annotate("segment", x = as.numeric(as.Date("2022-02-08")), xend = as.numeric(as.Date("2022-02-08")),
           y = -0.5, yend = 61.5, color = "#f94144", linetype = "dashed", size=1) +
  annotate("text", x = as.numeric(as.Date("2022-02-08")), y = 64,
           label = "Didžiausias\nsergamumas šalyje", col = "#f94144",
           size = 4, fontface = 'bold') +
  labs(x = "", y = "Savivaldybės rangas pagal antrinio sergamumo lygį") +
  theme_grey() +
  theme(legend.position = "none",
        legend.title=element_blank(),
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

# Vidutinio rango vizualizavimas
ggplot(covid.serg.rang, aes(x = date, y = serg.rank)) +
  geom_point(aes(group = municipality_name, color = factor(color.2)), size = 1.75, alpha = 0.8) +
  geom_line(aes(group = municipality_name, color = factor(color.2), size = factor(width.2)), alpha = 0.9) +
  geom_text(data=covid.serg.rang %>% filter(date=="2022-02-22"),
            aes(label = municipality_name, color = factor(color.2)),
            nudge_x = 0.5, hjust = 0, size = 3) +
  scale_x_continuous(breaks = covid.serg.rang$date,
                     expand = expansion(mult = c(0.005, 0.11))) +
  scale_size_discrete(range = c(0.4, 3)) +
  scale_colour_manual(values=c(
    "#f94144",
    "#f3722c",
    "#f8961e",
    "#f9844a",
    "#f9c74f",
    "#90be6d",
    "#43aa8b",
    "#4d908e",
    "#577590",
    "#277da1")) +
  annotate("segment", x = as.numeric(as.Date("2022-02-08")), xend = as.numeric(as.Date("2022-02-08")),
           y = -0.5, yend = 61.5, color = "#f94144", linetype = "dashed", size=1) +
  annotate("text", x = as.numeric(as.Date("2022-02-08")), y = 64,
           label = "Didžiausias\nsergamumas šalyje", col = "#f94144",
           size = 4, fontface = 'bold') +
  labs(x = "", y = "Savivaldybės rangas pagal antrinio sergamumo lygį") +
  theme_grey() +
  theme(legend.position = "none",
        legend.title=element_blank(),
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))



  






