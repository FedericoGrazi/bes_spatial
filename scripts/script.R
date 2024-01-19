library(maps)
library(dplyr)
library(ggplot2)
library(tidyverse)
library(readxl)
library(utils)
library(sf)
library(eurostat)
library(sfdep)
library(sp)
library(spdep)
library(stats)
library(NbClust)
library(cowplot)
library(caret)
library(skmeans)
library(broom)
library(GGally)
library(ggplot2)
library(xtable)
library(spatialprobit)
library(ggsci)
library(tmap)
library(factoextra)
library(flexclust)
library(ClustGeo)
library(xtable)
library(mapping)


# map_data: 97 (Aggiornata al 1989)
# dati_xxx: 111 (Aggiorantai al 2017)
# spost/pop: 107 (Aggiornati al 2017)

setwd("D:/Uni/3.2 - LAB STAT/Bes_dei_territori_edizione_092021/")
# load("dataset.RData")
# load("dataset_full_of_schifo.RData")
# load("dataset_full_of_schifo_aggiornato.RData")
load("ultimo_env.RData")

geom_ita <- loadCoordIT(unit = "provincia", year = "2017")

ggplot(data = geom_ita) +
  geom_sf()+
  theme_void()

# DATASET ####

# mapit <- map('italy', fill = F, col = "black")
mapit <- map_data('italy')

regioni_da_usare <- mapit %>% distinct(region) %>% select(region) %>% arrange


## ISTAT ####
istat <- read_excel("Indicatori_per_provincia_sesso_ed.2021.xlsx")
polarity <- read_excel("Indicatori_per_provincia_sesso_ed.2021.xlsx",
                  sheet = "Foglio1") %>% 
  rename(nome = `...4`,
         polarita = Polarità )

## REGIONI UTILI ####
regioni <- c("Lombardia", "Lazio", "Campania", "Sicilia", "Veneto", "Emilia-Romagna",
            "Piemonte", "Puglia", "Toscana", "Calabria", "Sardegna", "Liguria", "Marche",
          "Abruzzo", "Friuli-Venezia Giulia", "Trentino-Alto Adige/Südtirol", "Umbria", "Basilicata",
          "Molise","Valle d'Aosta/Vallée d'Aoste",
          "Nord","Mezzogiorno","Italia","Centro")


geom_reg <- geom_ita %>% as_tibble %>% select(provincia) %>% arrange(provincia)
istat_reg <- istat %>% select(TERRITORIO) %>% distinct() %>% filter(!TERRITORIO %in% regioni) %>% arrange(TERRITORIO) 

poly_ita <- geom_ita %>% 
  as_tibble %>% 
  select(provincia, geometry) %>% 
  rename(region = provincia) %>% 
  mutate(region = case_when(
    region == "Valle d'Aosta/Vallée d'Aoste"~"Aosta",
    region == "BarlettaAndriaTrani"~ "Barletta-Andria-Trani",
    region == "ForlìCesena"~ "Forlì-Cesena",
    region == "MassaCarrara"~ "Massa-Carrara",
    region == "VerbanoCusioOssola"~ "Verbano-Cusio-Ossola",
    TRUE~ region
  ))

## Nice format ####

dati_gather <- istat %>% 
  rename(region = TERRITORIO) %>% 
  filter(!region %in% regioni) %>% 
  filter(SESSO == "Totale") %>% 
  select(DOMINIO,CODICE,region,V_2017) %>% 
  group_by(region)


## Domini Utili ####


dati_gather %>% 
  group_by(DOMINIO) %>% 
  distinct(CODICE) %>% 
  summarize(n=n()) %>% 
  arrange(-n)


dom_yes <- dati_gather %>% 
  group_by(DOMINIO) %>% 
  distinct(CODICE) %>% 
  summarize(n=n()) %>% 
  arrange(-n) %>% 
  filter(n>5 | DOMINIO == "Sicurezza") %>% 
  select(DOMINIO)  %>% 
  as.matrix


dati_gather_filt <- dati_gather %>%
  filter(DOMINIO %in% dom_yes) %>% 
  inner_join(polarity) %>% 
  select(DOMINIO, nome, V_2017) %>% 
  ungroup

## Spread Format and Polygon join ####

dati_spread <- dati_gather_filt %>% 
  select(-DOMINIO) %>% 
  spread(
    nome,
    V_2017
    ) 

# inner_join with map object

dati_geo <- dati_spread %>% 
  inner_join(poly_ita) %>% 
  relocate(geometry, .after = region) %>% 
  filter(!st_is_empty(geometry))
  

### Esempio 1 - Alcuni indicatori generali ####

eg1.1 <- dati_geo %>% 
  st_as_sf() %>% 
  ggplot() +
  geom_sf(aes(fill = redd.pc))+
  scale_fill_gradient2(midpoint = mean(dati_spread$redd.pc,na.rm = T),low = "red", high = "blue",mid = "gold",
                      name = "valori in €" )+
  theme_void()

eg1.2 <- dati_geo %>% 
  st_as_sf() %>% 
  ggplot() +
  geom_sf(aes(fill = No.partecipazione.giov))+
  scale_fill_gradient2(midpoint = mean(dati_spread$No.partecipazione.giov,na.rm = T),low = "red", high = "blue",mid = "gold",
                       name = "% di assenza")+
  theme_void()


eg1.3 <- dati_geo %>% 
  st_as_sf() %>% 
  mutate(
    nb = st_contiguity(geometry),
    wt = st_weights(nb),
    var_lag = st_lag(redd.pc, nb, wt,na_ok = T)
  ) %>% 
  ggplot() +
  geom_sf(aes(fill = var_lag))+
  scale_fill_gradient2(midpoint =mean(dati_geo$redd.pc,na.rm = T),
                       low = "red", high = "blue",mid = "gold",
                       name = "valore in €",
                       limits = c(12000,25000))+
  theme_void()

eg1.4 <- dati_geo %>% 
  st_as_sf() %>% 
  mutate(
    nb = st_contiguity(geometry),
    wt = st_weights(nb),
    var_lag = st_lag(No.partecipazione.giov, nb, wt,na_ok = T)
  ) %>% 
  ggplot() +
  geom_sf(aes(fill = var_lag))+
  scale_fill_gradient2(midpoint =mean(dati_geo$No.partecipazione.giov,na.rm = T),
                                      name = "% di assenza",
                       low = "red", high = "blue",mid = "gold",
                       limits = c(15,65))+
  theme_void()

plot_grid(
  eg1.1,eg1.3,
  labels = c('Reddito Pro Capite', ' Valori Laggati'),
  ncol = 2
  )

plot_grid(
  eg1.2,eg1.4,
  labels = c("Mancata Partecipazione Giovanile al Mondo del Lavoro", '                               Valori Laggati'),
  ncol = 2
  )




# PCAS ####

pcaCharts <- function(x) {
  x.var <- x$sdev ^ 2
  x.pvar <- x.var/sum(x.var)
  print("proportions of variance:")
  print(x.pvar)
  
  par(mfrow=c(1,2))
  plot(cumsum(x.pvar),xlab="Principal component", ylab="Cumulative Proportion of variance explained", 
       ylim=c(0,1), type='b')+grid()
  screeplot(x,type="l", main = "")+grid()
  par(mfrow=c(1,1))
}



## SALUTE ######################
salute_pca <- dati_gather_filt %>% 
  filter(DOMINIO == "Salute") %>%
  spread(
    nome,
    V_2017
  ) %>% 
  select(-DOMINIO,-region) %>% 
  drop_na %>% 
  princomp(cor = T, scores = T)

summary(salute_pca)
pcaCharts(salute_pca)
biplot(salute_pca, col = c("grey75","darkred"), xlab = "Comp.1 (44%)", ylab = "Comp.2 (19%)")
salute_pca$loadings

salute <- salute_pca$scores[,1:3]%>% 
  as_tibble %>% 
  mutate(dati_gather_filt %>% 
           filter(DOMINIO == "Salute") %>% 
           spread(
             nome,
             V_2017
           ) %>% 
           drop_na %>% 
           select(region)) %>% 
  mutate(Comp.1 = -Comp.1) %>% 
  rename(
    vita.longeva = Comp.1,
    mortalità.avanzata = Comp.2,
    mortalità.improvvisa = Comp.3
  )


## AMBIENTE (eliminare/cambiare?) ######################
ambiente_pca <- dati_gather_filt %>% 
  filter(DOMINIO == "Ambiente") %>%  
  spread(
    nome,
    V_2017
  ) %>% 
  select(-DOMINIO,-region) %>% 
  drop_na %>% 
  princomp(cor = T,scores = T)

summary(ambiente_pca)
pcaCharts(ambiente_pca)
biplot(ambiente_pca, col = c("grey75","darkred"), xlab = "Comp.1 (32%)", ylab = "Comp.2 (19%)")
ambiente_pca$loadings

ambiente <- ambiente_pca$scores[,1:3] %>%
  as_tibble %>% 
  mutate(dati_gather_filt %>% 
           filter(DOMINIO == "Ambiente") %>% 
           spread(
             nome,
             V_2017
           ) %>% 
           drop_na %>% 
           select(region))  %>% 
  select(-Comp.2) %>% 
  rename(
    città.moderna = Comp.1,
    città.verde = Comp.3
  )

  

## ISTRUZIONE ######################
istr_pca <- dati_gather_filt %>% 
  filter(DOMINIO == "Istruzione e formazione") %>% 
  spread(
    nome,
    V_2017
  ) %>% 
  select(-DOMINIO,-region) %>% 
  drop_na %>% 
  princomp(cor = T, scores = T)

summary(istr_pca)
pcaCharts(istr_pca)
biplot(istr_pca, col = c("grey75","darkred"), xlab = "Comp.1 (62.2%)", ylab = "Comp.2 (11.3%)")
istr_pca$loadings

istr <- istr_pca$scores[,1] %>%
  as_tibble %>% 
  mutate(value = -value) %>% 
  mutate(dati_gather_filt %>% 
           filter(DOMINIO == "Istruzione e formazione") %>% 
           spread(
             nome,
             V_2017
           ) %>% 
           drop_na %>% 
           select(region)) %>% 
  rename(
    istruiti = value
  )
istr_mtrx <- as.matrix(istr_pca$loadings[,1])
colnames(istr_mtrx) <- colnames(istr)[1]

## QUALITA' ######################
qual_pca <- dati_gather_filt %>% 
  filter(DOMINIO == "Qualità dei servizi") %>% 
  spread(
    nome,
    V_2017
  ) %>% 
  select(-DOMINIO,-region) %>% 
  drop_na %>% 
  princomp(cor = T, scores = T)

summary(qual_pca)
pcaCharts(qual_pca)
biplot(qual_pca, col = c("grey75","darkred"), xlab = "Comp.1 (35.8%)", ylab = "Comp.2 (22.9%)")
qual_pca$loadings

qual <- qual_pca$scores[,1:2] %>% 
  as_tibble %>% 
  mutate(dati_gather_filt %>% 
           filter(DOMINIO == "Qualità dei servizi") %>% 
           spread(
             nome,
             V_2017
           ) %>% 
           drop_na %>% 
           select(region)) %>% 
  rename(
    disponibilità.sanitaria = Comp.1,
    differenziazione.sanitaria = Comp.2  )

qual_mtrx <- as.matrix(qual_pca$loadings[,1:2])
colnames(qual_mtrx) <- colnames(qual)[1:2]


## ECONOMIA ######################
eco_pca <- dati_gather_filt %>% 
  filter(DOMINIO == "Benessere economico") %>% 
  filter(nome != "retr.media") %>% 
  spread(
    nome,
    V_2017
  ) %>% 
  drop_na %>% 
  select(-DOMINIO,-region) %>% 
  princomp(cor = T, scores = T)

summary(eco_pca)
pcaCharts(eco_pca)
biplot(eco_pca, col = c("grey75","darkred"), xlab = "Comp.1 (80%)", ylab = "Comp.2 (9%)")
eco_pca$loadings

eco <- eco_pca$scores[,1]%>% 
  as_tibble %>% 
  mutate(dati_gather_filt %>% 
           filter(DOMINIO == "Benessere economico") %>% 
           filter(nome != "retr.media") %>% 
           spread(
             nome,
             V_2017
           ) %>% 
           drop_na %>% 
           select(region)) %>% 
  rename(
    ricchezza = value
  )

eco_mtrx <- as.matrix(eco_pca$loadings[,1])
colnames(eco_mtrx) <- colnames(eco)[1]


## LAVORO ######################
lav_pca <- dati_gather_filt %>% 
  filter(DOMINIO == "Lavoro e conciliazione dei tempi di vita") %>% 
  spread(
    nome,
    V_2017
  ) %>% 
  drop_na %>% 
  select(-DOMINIO,-region) %>% 
  princomp(cor = T, scores = T)

summary(lav_pca)
pcaCharts(lav_pca)
biplot(lav_pca, col = c("grey75","darkred"), xlab = "Comp.1 (76.8%)", ylab = "Comp.2 (15.4%)")
lav_pca$loadings

lav <- lav_pca$scores[,1] %>% 
  as_tibble %>% 
  mutate(dati_gather_filt %>% 
           filter(DOMINIO == "Lavoro e conciliazione dei tempi di vita") %>% 
           spread(
             nome,
             V_2017
           ) %>% 
           drop_na %>% 
           select(region)) %>% 
  rename(
    qualità.del.lavoro = value
  )

lav_mtrx <- as.matrix(lav_pca$loadings[,1])
colnames(lav_mtrx) <- colnames(lav)[1]



## POLITICA ######################
pol_pca <- dati_gather_filt %>% 
  filter(DOMINIO == "Politica e istituzioni") %>% 
  filter(!nome %in% c("riscossione.amm","riscossione.com","carceri.affollati")) %>% 
  spread(
    nome,
    V_2017
  ) %>% 
  drop_na %>% 
  select(-DOMINIO,-region) %>% 
  princomp(cor = T, scores = T)

summary(pol_pca)
pcaCharts(pol_pca)
biplot(pol_pca, col = c("grey75","darkred"), xlab = "Comp.1 (28.3%)", ylab = "Comp.2 (20.7%)")
pol_pca$loadings

pol <- pol_pca$scores[,1:2]%>% 
  as_tibble %>% 
  mutate(dati_gather_filt %>% 
           filter(DOMINIO == "Politica e istituzioni") %>% 
           filter(!nome %in% c("riscossione.amm","riscossione.com","carceri.affollati")) %>% 
           spread(
             nome,
             V_2017
           ) %>% 
           drop_na %>% 
           select(region)) %>% 
  rename(
    elez.o.giov = Comp.1,
    amm.donne.giov = Comp.2
    )

pol_mtrx <- as.matrix(pol_pca$loadings[,1:2])
colnames(pol_mtrx) <- colnames(pol)[1:2]

## SICUREZZA (aggiungere) ######################
sic_pca <- dati_gather_filt %>% 
  filter(DOMINIO == "Sicurezza") %>% 
  spread(
    nome,
    V_2017
  ) %>% 
  drop_na %>% 
  select(-DOMINIO,-region) %>% 
  princomp(cor = T, scores = T)

summary(sic_pca)
pcaCharts(sic_pca)+ text("Sicurezza")
biplot(sic_pca, col = c("grey75","darkred"), xlab = "Comp.1 (28.3%)", ylab = "Comp.2 (20.7%)")
sic_pca$loadings

sic <- sic_pca$scores[,1:2]%>% 
  as_tibble %>% 
  mutate(dati_gather_filt %>% 
           filter(DOMINIO == "Sicurezza") %>% 
           spread(
             nome,
             V_2017
           ) %>% 
           drop_na %>% 
           select(region)) %>% 
  rename(
      delitti = Comp.1,
      morti.passive = Comp.2
  )

sic_mtrx <- as.matrix(sic_pca$loadings[,1:2])
colnames(sic_mtrx) <- colnames(sic)[1:2]

### Descriptive PCA####

transf <- function(x) {
  t(x) %>% as_tibble %>% mutate(indicatori = colnames(x)) %>% relocate(indicatori, .before = rownames(x)[1]) %>% 
                        gather(key,value,-indicatori) %>% filter(abs(value)>0.2) %>% group_by(indicatori) %>% arrange(indicatori)
  
}
tabella_descr <- function(x){

a <- transf(x)[which(transf(x)$value>0),] %>% 
  summarize(pos = paste0(key, collapse = ", ") )
b <- transf(x)[which(transf(x)$value<0),] %>% 
  summarize(neg = paste0(key, collapse = ", ") )

full_join(a,b)
}


tabella_descr(lav_mtrx) %>% 
  full_join(tabbella_des)
full_join(tabella_descr(pol_mtrx)) %>% 
full_join(tabella_descr(eco_mtrx)) %>% 
full_join(tabella_descr(istr_mtrx)) %>% 
full_join(tabella_descr(qual_mtrx)) %>% 
full_join(tabella_descr(sic_mtrx)) %>% 
  mutate(DOMINIO = c("Lavoro",
                     "Politica",
                     "Politica",
                     "Benessere Economico",
                     "Istruzione e Formazione",
                     "Qualità dei servizi",
                     "Qualità dei servizi",
                     "Sicurezza",
                     "Sicurezza"
                     )) %>% 
  relocate(DOMINIO, .before = indicatori) %>% 
  xtable(
    caption = "Indicatori PCA per Dominio",
    label = "tab:PCA"
  )
  

table <- c(
  "sicurezza",.60,.6,
  "disponibilità sanitaria",.35,.35,
  "differenziazione sanitaria",.22,.57,
  "ricchezza",.79,.79,
  "qualità del lavoro",.76,.76,
  "qualità del lavoro",.76,.76,
  "elezioni o giovani ",.48,.48,
  "amm donne giovani",.35,.83,
  "delitti",.44,.44,
  "morti passive",.29,.73
)

matrix(table, ncol = 3, byrow = T) %>% 
  as_tibble %>% 
  mutate("Var Spiegata" = paste0(as.numeric(V2)*100,"%"),
         "Var Cumlata" = paste0(as.numeric(V3)*100,"%")) %>% 
  select(-V2,-V3) %>% 
  xtable(caption = "Varianze per ogni PC",
         label = "tab:var")

## Dataset PCA ####


# load("Spatial_pcas.RData")

mpi <- function(x) ((x-min(x, na.rm = T))/(max(x,na.rm = T)-min(x,na.rm = T))*60+70)

dati_pca <- poly_ita %>%
  select(region,geometry) %>% 
  distinct %>% 
  full_join(pol, by = "region") %>% 
  full_join(salute, by = "region") %>% 
  full_join(sic, by = "region")  %>% 
  full_join(qual, by = "region") %>%
  full_join(eco, by = "region") %>%
  full_join(istr, by = "region") %>%
  full_join(lav, by = "region") %>% 
  filter(region != "Bolzano-Bozen") %>% 
  as_tibble


dati_pca <- dati_pca %>% 
  mutate_if(is.numeric, mpi)


dati_pca %>% ncol
dati_pca %>% colnames

dati_pca %>% 
  select(-region,-geometry) %>% 
  ggpairs()

dati_pca %>% select(region,morti.passive) %>% 
  arrange(-morti.passive)

dati_pca %>% 
  ungroup %>% 
  select(-region,-geometry) %>% 
  cor(use = "na.or.complete") %>% 
  heatmap( col = colorRampPalette(c("darkblue", "white", "indianred"))(13), symm = T)


### Esempio 2 - Indicatori PCA####
summary(dati_pca)

eg2.1 <- dati_pca %>% 
  st_as_sf() %>% 
  ggplot() +
  geom_sf(aes(fill = vita.longeva))+
  scale_fill_gradient2(midpoint = mean(dati_pca$vita.longeva,na.rm = T),low = "red", high = "blue",mid = "gold",
                       name = "valore")+
  ggtitle(label = "Indicatore Vita Longeva","(calcolato tramite PCA)")+
  theme_void()

eg2.2 <- dati_pca %>% 
  st_as_sf() %>% 
  ggplot() +
  geom_sf(aes(fill = istruiti))+
  scale_fill_gradient2(midpoint = mean(dati_pca$istruiti,na.rm = T),low = "red", high = "blue",mid = "gold",
                       name = "valore")+
  ggtitle(label = "Indicatore Istruiti","(calcolato tramite PCA)")+
  theme_void()

plot_grid(eg2.1,eg2.2)

# SPATIAL LAG ####
## Polygon object  ####
dati_pca

# Quanti NAs?
dati_pca %>% 
  gather(PCAs, value,elez.o.giov:qualità.del.lavoro) %>% 
  group_by(PCAs) %>% 
  filter(is.na(value)) %>% 
  count %>% 
  arrange(-n) 


dati_polygon <- dati_pca %>% filter(!st_is_empty(geometry)) %>% st_as_sf()

### Esempio 3 - Spatial Lag ####

### Normali
eg3.1.1 <- dati_geo %>% 
  st_as_sf() %>% 
  mutate(
    nb = st_contiguity(geometry),
    wt = st_weights(nb),
    var_lag = st_lag(Ammini.donne, nb, wt,na_ok = T)
  ) %>% 
  ggplot() +
  geom_sf(aes(fill = var_lag), col = "black")+
  scale_fill_gradient2(midpoint =mean(dati_geo$Ammini.donne,na.rm = T),
                       low = "red", high = "blue",mid = "gold",
                       name = "valore")+
  ggtitle(label = "Amministratori Donne Lagged", "(valori in % al 2017)")+
  theme_void()
  
eg3.1.2 <- dati_geo %>% 
  st_as_sf() %>% 
  mutate(
    nb = st_contiguity(geometry),
    wt = st_weights(nb),
    var_lag = st_lag(Mort.tum, nb, wt,na_ok = T)
  ) %>% 
  ggplot() +
  geom_sf(aes(fill = var_lag), col = "black")+
  scale_fill_gradient2(midpoint =mean(dati_geo$Mort.tum,na.rm = T),
                       low = "red", high = "blue",mid = "gold",
                       name = "valore")+
  ggtitle(label = "Amministratori Giovani Lagged", "(valori in % al 2017)")+
  theme_void()

plot_grid(eg3.1.1,eg3.1.2)

### PCAs
eg3.2.1 <- dati_polygon %>% 
  mutate(
    nb = st_contiguity(geometry),
    wt = st_weights(nb),
    var_lag = st_lag(vita.longeva, nb, wt,na_ok = T)
  ) %>% 
    ggplot() +
    geom_sf(aes(fill = var_lag), col = "black")+
    scale_fill_gradient2(midpoint =mean(dati_polygon$vita.longeva,na.rm = T),low = "red", high = "blue",mid = "gold",
                         name = "valore")+
    ggtitle(label = "Indicatore Vita Longeva Lagged")+
    theme_void()

eg3.2.2 <- dati_polygon %>% 
  mutate(
    nb = st_contiguity(geometry),
    wt = st_weights(nb),
    var_lag = st_lag(ricchezza, nb, wt,na_ok = T)
  ) %>% 
    ggplot() +
    geom_sf(aes(fill = var_lag), col = "black")+
    scale_fill_gradient2(midpoint =mean(dati_polygon$ricchezza,na.rm = T),low = "red", high = "blue",mid = "gold",
                         name = "valore")+
    ggtitle(label = "Indicatore Ricchezza Lagged")+
    theme_void()

plot_grid(eg3.2.1,eg3.2.2)





# MIGRAZIONE ####


emigr <- read.csv(file = "DCIS_MIGRAZIONI_17032023170737437.csv") %>% 
  filter(ISO == "Total" | Sesso == "totale") %>% 
  filter(Territorio.di.origine != Territorio.di.di.destinazione) %>% 
  select(
    Territorio.di.origine,
    Territorio.di.di.destinazione,
    Value
  ) %>% 
  group_by(Territorio.di.origine) %>% 
  summarize(emigr = sum(Value)) %>% 
  rename(region = Territorio.di.origine) %>% 
  filter(!region %in% c("Sud", "Nord-ovest","Nord-est", "Centro", "Mezzogiorno","Isole")) %>%
  filter(!region %in% regioni)

immigr <- read.csv(file = "DCIS_MIGRAZIONI_17032023170737437.csv") %>% 
  filter(ISO == "Total" | Sesso == "totale") %>%
  filter(Territorio.di.origine != Territorio.di.di.destinazione) %>% 
  select(
    Territorio.di.origine,
    Territorio.di.di.destinazione,
    Value
  ) %>% 
  group_by(Territorio.di.di.destinazione) %>% 
  summarize(immigr = sum(Value)) %>% 
  rename(region = Territorio.di.di.destinazione) %>% 
  filter(!region %in% c("Sud", "Nord-ovest","Nord-est", "Centro", "Mezzogiorno", "Isole")) %>%
  filter(!region %in% regioni)



spost <- merge(immigr,emigr) %>%
  as_tibble %>%
  mutate(ratio = immigr-emigr)%>% 
  mutate(region = case_when(
    region == "Valle d'Aosta / Vallée d'Aoste" ~"Aosta",
    region == "Provincia Autonoma Bolzano / Bozen" ~"Bolzano/Bozen",
    TRUE ~region
  )) 



## Descriptive ####
spost %>% 
  select(-region,-ratio) %>% 
  ggpairs

summary(spost)

spost %>% 
  top_n(5,emigr) %>% 
  arrange(-emigr) %>% 
  select(region, emigr)

spost %>% 
  top_n(5,immigr) %>% 
  arrange(-immigr) %>% 
  select(region, immigr)

## Popolazione ####

popolazione <- read_excel("popolazione residente.xlsx") %>% 
  rename(
    region = Territorio,
    residenti = ...2
  ) 

spost %>% 
  full_join(popolazione) %>% 
  group_by(region) %>% 
  transmute(
    im_perc = immigr/residenti*100,
    em_perc = emigr/residenti
  ) %>% 
  ungroup %>% 
  full_join(dati_polygon) %>% 
  top_n(5,im_perc) %>% 
  arrange(-im_perc) %>% 
  select(region,im_perc) %>% 
  xtable(caption = "Immigrazioni sui Residenti totali",label = "tab:imgr")

spost %>% 
  full_join(popolazione) %>% 
  group_by(region) %>% 
  transmute(
    im_perc = immigr/residenti,
    em_perc = emigr/residenti
  ) %>% 
  ungroup %>% 
  full_join(dati_polygon) %>% 
  top_n(5,em_perc) %>% 
  arrange(-em_perc) %>% 
  select(region,em_perc)
### Esempio 4 - Distribuzione degli Spostamenti ####


spost %>% 
  full_join(popolazione) %>% 
  group_by(region) %>% 
  transmute(
    im_perc = immigr/residenti*100,
    em_perc = emigr/residenti*100
  ) %>% 
  ungroup %>% 
  full_join(dati_polygon) %>% 
  st_as_sf() %>% 
  ggplot() +
  geom_sf(aes(fill = em_perc))+
  scale_fill_gradient2(midpoint = 4.439345,low = "red", high = "blue", mid = "gold")+
  ggtitle(label =  "Emigrazioni dalla Provincia", subtitle = " (valori in % al 2017)")+
  theme_void()

spost %>% 
  full_join(popolazione) %>% 
  group_by(region) %>% 
  transmute(
    im_perc = immigr/residenti,
    em_perc = emigr/residenti
  ) %>% 
  ungroup %>% 
  full_join(dati_polygon) %>% 
  st_as_sf() %>% 
  ggplot() +
  geom_sf(aes(fill = im_perc))+
  scale_fill_gradient2(midpoint = 0.04379821,low = "red", high = "blue", mid = "gold")+
  ggtitle(label =  "Immigrazione nella Provincia", subtitle = " (valori in % al 2017)")+
  theme_void()


# Regression ####

bin <- spost %>% 
  mutate(bin = ifelse(ratio >0, 1, 0)) %>%
  select(region,bin) %>% 
  filter(region %in% as.matrix(dati_pca$region))

load("y maps.RData")
plot_grid(bin_map, ratio_map)

## KNN ####


dati <- dati_pca %>%
  left_join(spost) %>% 
  select(-città.moderna,-città.verde)

dati_sub <- dati %>% 
  select(
    region,
    ricchezza,
    elez.o.giov,
    vita.longeva,
    delitti,
    istruiti,
    disponibilità.sanitaria,
    mortalità.avanzata,
    ratio) %>% 
  drop_na


### coords for knn ####

region_for_knn <- dati_sub %>% 
  select(region) %>% 
  as.matrix

centroid <- dati_polygon %>% 
  as_tibble %>% 
  filter(region %in% region_for_knn) %>% 
  st_as_sf() %>% 
  as_Spatial() %>% 
  rgeos::gCentroid(byid=TRUE)

coords <- centroid@coords


## AIC Selection ####

full.model <- lm(ratio ~. -emigr -immigr -region, data = dati_tot)
step.model <- MASS::stepAIC(full.model, direction = "both", 
                      trace = FALSE)
summary(step.model)

## CV Stepwise Selection ####

train.control <- trainControl(method = "cv", number = 10)
step.model <- train(ratio ~.-emigr -immigr-region, data = dati %>% select(-geometry) %>% drop_na,
                    method = "leapSeq", 
                    tuneGrid = data.frame(nvmax = 3:10),
                    trControl = train.control
)
step.model$results[,c(1,2,4)] %>% xtable(caption = "Statistiche per modelli da 3 a 10 Covariate",label = "tab:cv")
step.model$bestTune
summary(step.model$finalModel)

### Knn neibourgh list ####


pval <- matrix(0,nrow = 30, ncol = 1)
for(i in 2:30){
  knn_nb_i <- knn2nb(knearneigh(coords,k=i))
  ww1_i    <- nb2listw(knn_nb_i,style="W")        
  
  lagm <- lagsarlm(ratio ~ ricchezza+elez.o.giov+vita.longeva+
             delitti+ istruiti+0,
           data=dati_sub,ww1_i,method="eigen")
  pval[i] <- lagm$rho/lagm$rho.se
}
par(mfrow = c(1,1))
plot(pval, type = "l", ylab = "stastica t")+abline(v = which.max(pval), col= "red", lty = "dashed")



knn <- knn2nb(knearneigh(coords,k=15))
plot(knn,coords)
ww1<- nb2listw(knn,style="W")    

Wknn <- as(ww1,"CsparseMatrix")
image(Wknn)

## OLS MODEL ####
ols <- lm(ratio ~ ricchezza+elez.o.giov+vita.longeva+
            delitti+ istruiti+0,
          data=dati_sub)

summary(ols)
tols <- ols %>% tidy() %>% 
  mutate(sign = case_when(
             p.value <0.001 ~ "***",
             p.value<0.01 & p.value >0.001 ~ "**",
             p.value <0.05 & p.value >0.01 ~ "*",
             p.value <0.1 & p.value >0.05 ~ ".",
             TRUE~""
           )) %>% 
  mutate(coef_ols = paste0(round(estimate,0), sign)) %>%
  select(term, coef_ols)

# H0: rho = 0
lm.morantest(ols, listw = ww1 , alternative="two.sided")


## SAR MODEL ####


lag <- lagsarlm(ratio ~ ricchezza+elez.o.giov+vita.longeva+
                delitti+ istruiti +0,
              data=dati_sub,ww1,method="eigen")
summary(lag)
lag$rho
impacts(lag, listw = ww1)

tlag <- lag %>% tidy() %>% 
  mutate(sign = case_when(
    p.value <0.001 ~ "***",
    p.value<0.01 & p.value >0.001 ~ "**",
    p.value <0.05 & p.value >0.01 ~ "*",
    p.value <0.1 & p.value >0.05 ~ ".",
    TRUE~""
  )) %>% 
  mutate(coef_sar = paste0(round(estimate,3), sign)) %>%
  select(term, coef_sar)

full_join(tols,tlag) %>% 
  xtable(caption = "Coefficienti per i modello OLS e SAR", label = "tab:coef")


r2 <- sum((lag$fitted.values-mean(lag$y))^2)/sum((lag$y-mean(lag$y))^2)


1-((1-r2)*(105-1)/(105-7-1))

sqrt(sum((lag$fitted.values-lag$y)^2)/105)
sqrt(sum((ols$fitted.values-dati_sub$ratio)^2)/105)

sum(abs(lag$fitted.values-lag$y))/105
sum(abs(ols$fitted.values-dati_sub$ratio))/105

### Impacts on Original Variables ####
eco_eff <- matrix(0,nrow = 5,ncol = 2)
eco_eff[,1] <- eco_pca$loadings[,1] * impacts(lag, listw = ww1)$direct[1]
eco_eff[,2] <- eco_pca$loadings[,1] * impacts(lag, listw = ww1)$indirect[1]
rownames(eco_eff) <- rownames(eco_pca$loadings)
colnames(eco_eff) <- c("direct","indirect")

istr_eff <- matrix(0,nrow =9,ncol = 2)
istr_eff[,1] <- istr_pca$loadings[,1] * impacts(lag, listw = ww1)$direct[5]
istr_eff[,2] <- istr_pca$loadings[,1] * impacts(lag, listw = ww1)$indirect[5]
rownames(istr_eff) <- rownames(istr_pca$loadings)
colnames(istr_eff) <- c("direct","indirect")

sal_eff <- matrix(0,nrow =6,ncol = 2)
sal_eff[,1] <- salute_pca$loadings[,1] * impacts(lag, listw = ww1)$direct[3]
sal_eff[,2] <- salute_pca$loadings[,1] * impacts(lag, listw = ww1)$indirect[3]
rownames(sal_eff) <- rownames(salute_pca$loadings)
colnames(sal_eff) <- c("direct","indirect")

sic_eff <- matrix(0,nrow =4,ncol = 2)
sic_eff[,1] <- sic_pca$loadings[,1] * impacts(lag, listw = ww1)$direct[4]
sic_eff[,2] <- sic_pca$loadings[,1] * impacts(lag, listw = ww1)$indirect[4]
rownames(sic_eff) <- rownames(sic_pca$loadings)
colnames(sic_eff) <- c("direct","indirect")

pol_eff <- matrix(0,nrow =3,ncol = 2)
pol_eff[,1] <- pol_pca$loadings[,1] * impacts(lag, listw = ww1)$direct[2]
pol_eff[,2] <- pol_pca$loadings[,1] * impacts(lag, listw = ww1)$indirect[2]
rownames(pol_eff) <- rownames(pol_pca$loadings)
colnames(pol_eff) <- c("direct","indirect")


eco_eff
istr_eff
sic_eff
pol_eff
sal_eff




### Esempio 5 -Impact for each region ####
W <- nb2mat(knn)
y_lag <- W %*% dati_sub$ratio

SME <- matrix(0,nrow = 105, ncol = 5)
for(i in 2:6){
  beta <- coef(lag)[i]
  SME[,i-1] <- beta * y_lag
}
colnames(SME) <- names(coef(lag))[2:6]



sme1 <- SME %>% 
  as_tibble %>% 
  mutate(region = dati_sub$region) %>% 
  full_join(dati_polygon %>% select(region,geometry) )%>% 
  st_as_sf() %>% 
  ggplot()+
  geom_sf(aes(fill = ricchezza))+
  scale_fill_viridis_c()+
  theme_void()

sme2 <- SME %>% 
  as_tibble %>% 
  mutate(region = dati_sub$region) %>% 
  full_join(dati_polygon %>% select(region,geometry) )%>% 
  st_as_sf() %>% 
  ggplot()+
  geom_sf(aes(fill = elez.o.giov))+
  scale_fill_viridis_c()+
  theme_void()

sme3 <- SME %>% 
  as_tibble %>% 
  mutate(region = dati_sub$region) %>% 
  full_join(dati_polygon %>% select(region,geometry) )%>% 
  st_as_sf() %>% 
  ggplot()+
  geom_sf(aes(fill = vita.longeva))+
  scale_fill_viridis_c()+
  theme_void()

sme4 <- SME %>% 
  as_tibble %>% 
  mutate(region = dati_sub$region) %>% 
  full_join(dati_polygon %>% select(region,geometry) )%>% 
  st_as_sf() %>% 
  ggplot()+
  geom_sf(aes(fill = delitti))+
  scale_fill_viridis_c()+
  theme_void()

sme5 <- SME %>% 
  as_tibble %>% 
  mutate(region = dati_sub$region) %>% 
  full_join(dati_polygon %>% select(region,geometry) )%>% 
  st_as_sf() %>% 
  ggplot()+
  geom_sf(aes(fill = istruiti))+
  scale_fill_viridis_c()+
  theme_void()


plot_grid(
  sme1,
  sme2,
  sme3,
  sme4,
  sme5,
  nrow = 2
)

par(mfrow = c(2,3))
for(i in 1:6){
plot(sort(SME[,i]),type="l",xlab="Spatial units",ylab=colnames(SME)[i])
abline(h = mean(SME[,i]),col="blue")
}


# SPATIAL CLUSTERING ####
## hclust ####

matrix_clust <- dati_pca %>% 
  # filter(region %in% region_for_hclust) %>% 
  arrange(region) %>% 
  select(
    ricchezza,
    elez.o.giov,
    vita.longeva,
    istruiti,
    delitti,
    mortalità.avanzata
    ) %>%
  drop_na %>% 
  as.matrix


### Optimal NC ####


par(mfrow = c(1,2))
plot(NbClust(matrix_clust, method = "ward.D2", min.nc = 3,max.nc = 10, index = "dunn")$All.index, type = "b", ylab = "Dunn value")
plot(NbClust(matrix_clust, method = "ward.D2", min.nc = 3,max.nc = 10, index = "silhouette")$All.index, type = "b",ylab = "Silhouette value")
par(mfrow = c(1,1))

nomi <- dati_pca %>% 
  arrange(region) %>% 
  select(
    region,
    ricchezza,
    elez.o.giov,
    vita.longeva,
    istruiti,
    delitti,
    mortalità.avanzata
  )  %>%
  # filter(region %in% region_for_hclust) %>% 
  drop_na %>% 
  select(region) 


rownames(matrix_clust) <- as.matrix(nomi)
dist <- dist(matrix_clust) 
tree <- hclustgeo(dist)

par(mfrow = c(1,1))
plot(tree,hang = -1, cex = 0.55,
     xlab = "", sub = "",
     main = "Ward dendrogram")
rect.hclust(tree ,k = 7, border = c(1,2,3,4,5,6,7))
legend("topright", legend = paste("cluster",1:7), 
       fill=1:7,bty= "n", border = "white")

tree$labels
P5 <- cutree(tree,7) %>% as_tibble %>% mutate(region = tree$labels)
P5 <- P5 %>% mutate(region = region[,"region"])

### Results ####

res1 <- dati_geo %>% 
  full_join(P5) %>% 
  mutate(cluster = factor(value)) %>% 
  st_as_sf() %>% 
  ggplot()+
  geom_sf(aes(fill = cluster), show.legend = F)+
  scale_fill_npg()+
  theme_void()


#### Esempio 6 - Centroidi per hclust ####

dati_pca %>% 
  full_join(P5) %>% 
  mutate(cluster = factor(value)) %>% 
  group_by(cluster) %>% 
  select(-region) %>% 
  filter(!is.na(cluster)) %>% 
  select(
    cluster,
    elez.o.giov,
    istruiti,
    vita.longeva,
    delitti
    ) %>% 
  gather(var, value, -cluster) %>% 
  ggplot(aes(cluster,value, fill = cluster, alpha = 0.85))+
  geom_boxplot(show.legend = F, outlier.shape = NA)+
  geom_jitter(aes(col = cluster),show.legend = F)+
  scale_fill_npg()+
  scale_color_npg()+
  theme_bw()+
  geom_hline(yintercept = 0100, lty = "dashed", col = "red")+
  facet_wrap(~var, ncol = 2)



## hclust with geo-costraints ####

centroid_hc <- dati_polygon %>% 
  as_tibble %>% 
  arrange(region) %>% 
  filter(region %in% as.matrix(nomi)) %>% 
  st_as_sf() %>% 
  as_Spatial() %>% 
  rgeos::gCentroid(byid=TRUE)

coords_hc <- centroid_hc@coords

geo_d <- coords_hc %>% 
  as_tibble %>% 
  mutate(nomi)


### Distances between Province ####

D1 <- dist(geo_d)
D1 <- as.matrix(D1)
rownames(D1) <- colnames(D1) <- geo_d$region %>% as.matrix

range.alpha <- seq(0,1,0.1)
K <- 7
cr <- choicealpha(dist, as.dist(D1), range.alpha, 
                  K, graph = FALSE)
cr$Q
plot(cr,norm = T)


tree_dist <- hclustgeo(dist,as.dist(D1),alpha=0.6)
P5bis <- cutree(tree_dist,7) %>% as_tibble %>% mutate(nomi)

### Results with constraints ####

res2 <- dati_geo %>%
  full_join(P5bis) %>% 
  mutate(cluster = factor(value)) %>% 
  st_as_sf %>% 
  ggplot()+
  geom_sf(aes(fill = cluster),show.legend = F)+
  scale_fill_npg()+
  theme_void()


#### Esempio 6b - Centroidi per hclust ####

dati_pca %>% 
  full_join(P5bis) %>% 
  mutate(cluster = factor(value)) %>% 
  group_by(cluster) %>% 
  select(-region) %>% 
  filter(!is.na(cluster)) %>% 
  select(
    cluster,
    elez.o.giov,
    istruiti,
    vita.longeva,
    delitti
  ) %>% 
  gather(var, value, -cluster) %>% 
  ggplot(aes(cluster,value, fill = cluster, alpha = 0.85))+
  geom_boxplot(show.legend = F, outlier.shape = NA)+
  geom_jitter(aes(col = cluster),show.legend = F)+
  scale_fill_npg()+
  scale_color_npg()+
  theme_bw()+
  geom_hline(yintercept = 0100, lty = "dashed", col = "red")+
  facet_wrap(~var, ncol = 2)

## Kmeans Matrix ####
kmeans_matrix <- dati_pca %>% 
  # filter(region %in% region_for_hclust) %>% 
  select(
    ricchezza,
    elez.o.giov,
    vita.longeva,
    istruiti,
    delitti,
    disponibilità.sanitaria
  ) %>%
  drop_na %>% 
  as.matrix

rownames(kmeans_matrix) <- dati_pca %>%
  select(
    region,
    ricchezza,
    elez.o.giov,
    vita.longeva,
    istruiti,
    delitti,
    disponibilità.sanitaria
  ) %>%
  drop_na %>% 
  select(region) %>%
  as.matrix

nomi_kmeans <- dati_pca %>% 
  select(
    region,
    ricchezza,
    elez.o.giov,
    vita.longeva,
    istruiti,
    delitti,
    disponibilità.sanitaria
  ) %>%
  drop_na %>% 
  select(region) 
  
### Inizializzo con Hclust ####
centers <- dati_pca %>% 
  full_join(P5bis) %>% 
  mutate(cluster = factor(value)) %>% 
  select(
    cluster,
    ricchezza,
    elez.o.giov,
    vita.longeva,
    istruiti,
    delitti,
    disponibilità.sanitaria
  ) %>%
  group_by(cluster) %>% 
  filter(!is.na(cluster)) %>% 
  summarize_all(.funs = mean, na.rm = T) %>% 
  select(-cluster)


kmeans <- kmeans(kmeans_matrix,centers = centers,iter.max = 2)
kmeans_p5 <- kmeans$cluster %>% as_tibble %>% mutate(nomi_kmeans)



### Results for kmeans ####

res3 <- dati_geo %>% 
  full_join(kmeans_p5) %>% 
  mutate(cluster = factor(value)) %>% 
  st_as_sf() %>% 
  ggplot( )+
  geom_sf(aes(fill = cluster), show.legend = F)+
  scale_fill_npg()+
  theme_void()


#### Esempio 7 - Centroidi per kmeans ####

dati_pca %>% 
  full_join(kmeans_p5) %>% 
  mutate(cluster = factor(value)) %>% 
  group_by(cluster) %>% 
  select(-region) %>% 
  filter(!is.na(cluster)) %>% 
  select(
    cluster,
    elez.o.giov,
    istruiti,
    vita.longeva,
    delitti
  ) %>% 
  gather(var, value, elez.o.giov:delitti) %>% 
  ggplot(aes(cluster,value, fill = cluster, alpha = 0.85))+
  geom_boxplot(show.legend = F, outlier.shape = NA)+
  geom_jitter(aes(col = cluster),show.legend = F)+
  scale_fill_npg()+
  scale_color_npg()+
  theme_bw()+
  geom_hline(yintercept =100, lty = "dashed", col = "red")+
  facet_wrap(~var, ncol = 2)





## Full Results ####
plot_grid(res1,res2,res3,nrow = 1)



# Clustering Visualization ####
par(mfrow = c(1,1))
## H-Clust Visualization ####
geo_d_hc <- geo_d %>% 
  full_join(P5)

plot(geo_d_hc[1:2], pch = geo_d_hc$value,col = geo_d_hc$value,lwd = 2)

dist_clust<- cbind(geo_d_hc[1:2],geo_d_hc$value)
dist_clust1<- subset(dist_clust,dist_clust[,3]==1)
dist_clust2<- subset(dist_clust,dist_clust[,3]==2)
dist_clust3<- subset(dist_clust,dist_clust[,3]==3)
dist_clust4<- subset(dist_clust,dist_clust[,3]==4)
dist_clust5<- subset(dist_clust,dist_clust[,3]==5)
dist_clust6<- subset(dist_clust,dist_clust[,3]==6)
dist_clust7<- subset(dist_clust,dist_clust[,3]==7)

coords1<- dist_clust1[,1:2]
coords2<- dist_clust2[,1:2]
coords3<- dist_clust3[,1:2]
coords4<- dist_clust4[,1:2]
coords5<- dist_clust5[,1:2]
coords6<- dist_clust6[,1:2]
coords7<- dist_clust7[,1:2]


g1<- c(chull(coords1),chull(coords1)[1])
g2<- c(chull(coords2),chull(coords2)[1])
g3<- c(chull(coords3),chull(coords3)[1])
g4<- c(chull(coords4),chull(coords4)[1])
g5<- c(chull(coords5),chull(coords5)[1])
g6<- c(chull(coords6),chull(coords6)[1])
g7<- c(chull(coords7),chull(coords7)[1])

lines(coords1[g1,],lty=5)
lines(coords2[g2,],lty=5)
lines(coords3[g3,],lty=5)
lines(coords4[g4,],lty=5)
lines(coords5[g5,],lty=5)
lines(coords6[g6,],lty=5)
lines(coords7[g7,],lty=5)

## Geo-H-Clust Visualization ####

geo_d_hcg <- geo_d %>% 
  full_join(P5bis)

plot(geo_d_hcg[1:2], pch = geo_d_hcg$value,col = geo_d_hcg$value,lwd = 2)

dist_clust<- cbind(geo_d_hcg[1:2],geo_d_hcg$value)
dist_clust1<- subset(dist_clust,dist_clust[,3]==1)
dist_clust2<- subset(dist_clust,dist_clust[,3]==2)
dist_clust3<- subset(dist_clust,dist_clust[,3]==3)
dist_clust4<- subset(dist_clust,dist_clust[,3]==4)
dist_clust5<- subset(dist_clust,dist_clust[,3]==5)
dist_clust6<- subset(dist_clust,dist_clust[,3]==6)
dist_clust7<- subset(dist_clust,dist_clust[,3]==7)

coords1<- dist_clust1[,1:2]
coords2<- dist_clust2[,1:2]
coords3<- dist_clust3[,1:2]
coords4<- dist_clust4[,1:2]
coords5<- dist_clust5[,1:2]
coords6<- dist_clust6[,1:2]
coords7<- dist_clust7[,1:2]


g1<- c(chull(coords1),chull(coords1)[1])
g2<- c(chull(coords2),chull(coords2)[1])
g3<- c(chull(coords3),chull(coords3)[1])
g4<- c(chull(coords4),chull(coords4)[1])
g5<- c(chull(coords5),chull(coords5)[1])
g6<- c(chull(coords6),chull(coords6)[1])
g7<- c(chull(coords7),chull(coords7)[1])

lines(coords1[g1,],lty=5)
lines(coords2[g2,],lty=5)
lines(coords3[g3,],lty=5)
lines(coords4[g4,],lty=5)
lines(coords5[g5,],lty=5)
lines(coords6[g6,],lty=5)
lines(coords7[g7,],lty=5)



## K-Means Visualization ####

geo_d_km <- geo_d %>% 
  full_join(kmeans_p5)

plot(geo_d_km[1:2], pch = geo_d_km$value,col = geo_d_km$value,
     main = "K-means",lwd = 2)
dist_clust<- cbind(geo_d_km[1:2],geo_d_km$value)
dist_clust1<- subset(dist_clust,dist_clust[,3]==1)
dist_clust2<- subset(dist_clust,dist_clust[,3]==2)
dist_clust3<- subset(dist_clust,dist_clust[,3]==3)
dist_clust4<- subset(dist_clust,dist_clust[,3]==4)
dist_clust5<- subset(dist_clust,dist_clust[,3]==5)
dist_clust6<- subset(dist_clust,dist_clust[,3]==6)
dist_clust7<- subset(dist_clust,dist_clust[,3]==7)

coords1<- dist_clust1[,1:2]
coords2<- dist_clust2[,1:2]
coords3<- dist_clust3[,1:2]
coords4<- dist_clust4[,1:2]
coords5<- dist_clust5[,1:2]
coords6<- dist_clust6[,1:2]
coords7<- dist_clust7[,1:2]


g1<- c(chull(coords1),chull(coords1)[1])
g2<- c(chull(coords2),chull(coords2)[1])
g3<- c(chull(coords3),chull(coords3)[1])
g4<- c(chull(coords4),chull(coords4)[1])
g5<- c(chull(coords5),chull(coords5)[1])
g6<- c(chull(coords6),chull(coords6)[1])
g7<- c(chull(coords7),chull(coords7)[1])

lines(coords1[g1,],lty=5)
lines(coords2[g2,],lty=5)
lines(coords3[g3,],lty=5)
lines(coords4[g4,],lty=5)
lines(coords5[g5,],lty=5)
lines(coords6[g6,],lty=5)
lines(coords7[g7,],lty=5)





### flexclust charts ####
c4 <- as.kcca(tree_dist, matrix_clust, k = 7)
par(mfrow = c(1,1))
barchart(c4)
plot(c4, xlim=c(-3.5,3), ylim = c(-3.5,2))+
 points(matrix_clust, pch = P5bis$value, col = P5bis$value, lwd=3)


# Conditioned SAR Model ####




## Optimal Clusters for Rho Significance ####
pval_cond <- matrix(0,nrow = 8, ncol = 1)
for(i in 2:9){
  hcg_clust_i_ <- cutree(tree_dist,i) %>% as_tibble %>% mutate(nomi) %>% filter(region %in% as.matrix(dati_sub$region)) %>% 
                  select(value) %>% as.matrix %>% as.factor
  Clag_i <- lagsarlm(ratio ~(hcg_clust_i_:(ricchezza+elez.o.giov+vita.longeva+
                                             delitti+ istruiti+disponibilità.sanitaria+hcg_clust_i_))+0,
                   data=arrange(dati_sub,region),ww1,method="eigen")

  pval_cond[i] <- Clag_i$rho/Clag_i$rho.se
}
plot(pval_cond, type = "l", ylab = "stastica t")+abline(v = which.min(pval_cond), col= "red", lty = "dashed")


## Using Geo-H-Clust ####

hcg_clust_ <- cutree(tree_dist,6) %>% as_tibble %>% mutate(nomi) %>%  
              filter(region %in% as.matrix(dati_sub$region)) %>% 
              select(value) %>% as.matrix %>% as.factor


Clag <- lagsarlm(ratio ~(hcg_clust_:(ricchezza+elez.o.giov+vita.longeva+
                                       delitti+ istruiti+mortalità.avanzata+hcg_clust_))+0,
                 data=arrange(dati_sub,region),ww1,method="eigen")
summary(Clag)

Clag %>% tidy %>% mutate(
  sign = case_when(
    p.value >0.1 ~ "",
    p.value <0.1 & p.value >0.05 ~ ".",
    p.value <0.05 & p.value >0.01 ~ "*",
    p.value <0.01 & p.value >0.001 ~ "**",
    TRUE~"***"
  )
) %>% 
  print(n= 36)

## Impacts for Clag ####
impacts(Clag, listw = ww1) 


c_eco_eff_dir <- eco_pca$loadings[,1] %*% t(impacts(Clag,listw = ww1)$direct[7:12])
c_eco_eff_ind <- eco_pca$loadings[,1] %*% t(impacts(Clag,listw = ww1)$indirect[7:12])
rownames(c_eco_eff_dir) < -rownames(c_eco_eff_ind) <- rownames(eco_pca$loadings)
colnames(c_eco_eff_dir) <- colnames(c_eco_eff_ind) <- as.vector(paste("cluster_", 1:7, sep = ""))

c_eco_eff_dir
c_eco_eff_ind




### Esempio 8 - Direct and Indirect Impact for each Cluster (ricchezza) ####

P4bis <- cutree(tree_dist,6) %>% as_tibble %>% mutate(nomi)

ricch_cimpact <- impacts(Clag,listw = ww1)$direct[7:12] %>% 
  as_tibble %>% 
  rename(ricch_cimpact = value) %>% 
  mutate(cluster = 1:6) %>% 
  mutate(cluster = as_factor(cluster))

ricch_cimpact_ind <- impacts(Clag,listw = ww1)$indirect[7:12] %>% 
  as_tibble %>% 
  rename(ricch_cimpact = value) %>% 
  mutate(cluster = 1:6) %>% 
  mutate(cluster = as_factor(cluster))

ricch_cimpact_tot <- impacts(Clag,listw = ww1)$total[7:12] %>% 
  as_tibble %>% 
  rename(ricch_cimpact = value) %>% 
  mutate(cluster = 1:6) %>% 
  mutate(cluster = as_factor(cluster))

eg8.1 <- dati_geo %>% 
  full_join(P4bis) %>% 
  mutate(cluster = factor(value)) %>% 
  full_join(ricch_cimpact, by = "cluster") %>% 
  select(region:geometry, ricch_cimpact, cluster) %>% 
  st_as_sf() %>% 
  ggplot( )+
  geom_sf(aes(fill = ricch_cimpact))+
  scale_fill_viridis_c(name = "impatto diretto")+
  theme_void()

eg8.2 <- dati_geo %>% 
  full_join(P4bis) %>% 
  mutate(cluster = factor(value)) %>% 
  full_join(ricch_cimpact_ind, by = "cluster") %>% 
  select(region:geometry, ricch_cimpact, cluster) %>% 
  st_as_sf() %>% 
  ggplot( )+
  geom_sf(aes(fill = ricch_cimpact))+
  scale_fill_viridis_c(name = "impatto indiretto")+
  theme_void()

eg8.3 <- dati_geo %>% 
  full_join(P4bis) %>% 
  mutate(cluster = factor(value)) %>% 
  full_join(ricch_cimpact_tot, by = "cluster") %>% 
  select(region:geometry, ricch_cimpact, cluster) %>% 
  st_as_sf() %>% 
  ggplot( )+
  geom_sf(aes(fill = ricch_cimpact))+
  scale_fill_viridis_c(name = "impatto totale")+
  theme_void()


plot_grid(eg8.1,eg8.2,eg8.3,nrow = 1)
