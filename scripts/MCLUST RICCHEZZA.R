library(mclust)
library(dplyr)
tibb <- dati_pca %>% 
  select(ricchezza,
         qualità.del.lavoro,
         istruiti) %>% 
  drop_na

x <- as.matrix(tibb)

x
mod <- Mclust(x)

summary(mod$BIC)
summary(mod)

mod <- Mclust(x, G = 2, modelNames = "EVE")
summary(mod)
mod$classification

drmod <- MclustDR(mod,lambda = .9)
summary(drmod)

# plot(drmod, what = "contour")
# plot(drmod, what = "classification",main = F)
# plot(drmod, what = "boundaries")

class <- mod$classification

tibb <- tibb %>% 
  mutate(class = as.factor(class))


clp <- clPairs(x, class, lower.panel = NULL)


LRT <- mclustBootstrapLRT(x, model = "EVE")
LRT

par(mfrow = c(1,2))
plot(LRT, G = 1)
plot(LRT, G = 2)


Ricchezza <- as.matrix(tibb$ricchezza)
dens <- densityMclust(Ricchezza)

summary(dens, parameters = T)

par(mfrow = c(1,2))

br <- seq(min(Ricchezza,na.rm = T), max(Ricchezza, na.rm = T), length=21)
plot(dens, what = "density", data = Ricchezza, breaks = br, main = "Ricchezza")


df <- tibb %>% 
  mutate(class = as.factor(mod$classification)) %>% 
  select(ricchezza, class)

h1 <- hist(tibb$ricchezza[tibb$class==1], breaks = br, plot = FALSE)
h1$density <- h1$density*prop.table(table(tibb$class))[1]
h2 <- hist(tibb$ricchezza[tibb$class==2], breaks = br, plot = FALSE)
h2$density <- h2$density*prop.table(table(tibb$class))[2]
x <- seq(min(tibb$ricchezza)-diff(range(tibb$ricchezza))/10,
       max(tibb$ricchezza)+diff(range(tibb$ricchezza))/10, length = 200)

cdens <- predict(dens, x, what = "cdens")
cdens <- t(apply(cdens, 1, function(d) d*dens$parameters$pro))
col <- adjustcolor(mclust.options("classPlotColors")[1:2], alpha = 0.3)
plot(h1, xlab = "Ricchezza", freq = FALSE, main = "", border = FALSE, col = col[1],
   xlim = range(x), ylim = range(h1$density, h2$density, cdens))
plot(h2, add = TRUE, freq = FALSE, border = FALSE, col = col[2])
matplot(x, cdens, type = "l", lwd = 1, add = TRUE, lty = 1:3, col = 1)
box()


tab <- dati_pca %>% 
  select(region,
         ricchezza,
         qualità.del.lavoro,
         istruiti) %>% 
  drop_na %>% 
  mutate(class = as.factor(class))

dati_pca %>% 
  full_join(tab) %>% 
  st_as_sf() %>% 
  ggplot()+
  geom_sf(aes(fill = class))+
  scale_fill_manual(values = c("#1C86EE4D","#CD00004D"))+
  theme_void()
