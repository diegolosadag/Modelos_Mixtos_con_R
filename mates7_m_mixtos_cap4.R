
######################################################
#####   
#####        SCRIPT: CODIGO TFG ESTATISTICA CLASICA
##### INTRODUCION AOS MODELOS MIXTOS
##         CAPITULO 4: MODELOS MIXTOS CON COVARIABLES RELATIVAS AO PRIMEIRO NIVEL
###                   Diego Losada Gonzalez
######################################################

rm(list = ls())
library(nlme); library(ggplot2); library(lme4); library(car)
library(dplyr); library(RColorBrewer); library(viridis)
library(gtools); library(patchwork); library(knitr)
library(lmtest); library(ggthemes); library(lattice)
library(latex2exp)

######################################
##
##   Creacion da base de datos mates7
##
######################################

mates <- merge(MathAchieve, MathAchSchool, by = "School")
mates <- mates[, -12]
names(mates) <- c("Escola", "SocialMin", "Sexo", "StSE", "NotaMates", "MStSE", "Tamaño", "Sector", 
                  "Particip", "AmbDiscrim", "MaioriaMin")
levels(mates$Sector) <- c("Publica", "Catolica")
levels(mates$SocialMin) <- c("Non", "Si")
levels(mates$Sexo) <- c("Home", "Muller")
mates$Escola <- factor(mates$Escola, levels = sort(levels(mates$Escola))) #escolas ben ordeadas
levels(mates$Escola) <- c(1:160) #renomeanse
attach(mates)

escolas_particip100 <- which(MathAchSchool$PRACAD == 1) #escolas con participacion unanime
alum_escolas_particip100 <- which(Escola %in% escolas_particip100) #alumnos de escolas con participacion unanime
length(alum_escolas_particip100) #307 alumnos de 7 escolas

mates7 <- mates[alum_escolas_particip100, ] #evitamos sesgos de eleccion
dim(mates7) # 307 alumnas/os e 11 variables
mates7$Escola <- factor(mates7$Escola, levels = escolas_particip100) #corrixir levels
levels(mates7$Escola) <- paste("E", 1:7, sep = "") #Cambiase o nome destas 7 escolas
attach(mates7)

display.brewer.pal(n = 7, name = "Dark2") #Paleta de cores que se vai empregar
dark_2 <- brewer.pal(n = 7, name = "Dark2")

######################################
##
##   Modelos mixtos con covariables relativas ao nivel 1
##
######################################

###-------------------------------matesmm1: MODELO CON INTERCEPTO ALEATORIO E PENDENTE FIXA------------

matesmm1 <- lmer(NotaMates ~ StSE + (1 | Escola), data = mates7, REML = FALSE) # modelo con intercepto fixo 
  # (por defecto), pendente fixa asociada ao indice socio-economico, intercepto aleatorio e co identificador 
  # da variable de segundo nivel Escola
summary(matesmm1)
coefsmm1 <- summary(matesmm1)$coef
sigma2_eps_mm1 <- summary(matesmm1)$sigma^2
sigma2_u_mm1 <- as.data.frame(summary(matesmm1)$varcor)[1,4]
VT_mm1 <- sigma2_u_mm1 + sigma2_eps_mm1
VPC_mm1 <- sigma2_u_mm1 / VT_mm1
#Calculo dos efectos aleatorios e dos erros (nivel 1)
u0_mm1 <- ranef(matesmm1, postVar = TRUE)
uj_mm1 <- u0_mm1[[1]][,1]
u0se_mm1 <- sqrt(attr(u0_mm1[[1]], "postVar")[1, , ])
eps_mm1 <- residuals(matesmm1)

###-------------------------------Figura matesmm1 ------------

rectamm1 <- function(x = 0, escola = NULL){
  if (!is.null(escola)){
    if (!escola %in% 1:7){
      stop("Tal escola non existe, só do 1 ao 7.")
    }
  }
  if (is.null(escola)){
    coefsmm1[1] + coefsmm1[2] * x
  }
  else{
    coefsmm1[1] + uj_mm1[escola] + coefsmm1[2] * x
  }
}


ggplot(mates7, aes(x = StSE, y = NotaMates, color = Escola)) +
  geom_point(size = 1.25) +
  coord_cartesian(xlim = c(range(StSE)[1], range(StSE)[2]), ylim = c(-.5, 25) ) +
  xlab("Status socio-económico") + 
  ylab("Nota en Matemáticas") +
  scale_color_brewer(palette = "Dark2") +
  #Media global
  geom_segment(x = -1.7, xend = 1.5, y = rectamm1(-1.7), yend = rectamm1(1.45), size = 1.35, col = 1) +
  #Medias locais
  geom_segment(x = -1.7, xend = 1.5, y = rectamm1(-1.7, 3), yend = rectamm1(1.5, 3), lwd = 1, 
               linetype = "twodash", color = dark_2[3]) +
  geom_segment(x = -1.7, xend = 1.5, y = rectamm1(-1.7, 4), yend = rectamm1(1.5, 4), lwd = 1, 
               linetype = "twodash", color = dark_2[4]) +
  geom_segment(x = -1.4, xend = 1.5, y = rectamm1(-1.4, 5), yend = rectamm1(1.5, 5), lwd = 1, 
               linetype = "twodash", color = dark_2[5]) +
  geom_segment(x = -1.4, xend = 1.5, y = rectamm1(-1.4, 7), yend = rectamm1(1.5, 7), lwd = 1, 
               linetype = "twodash", color = dark_2[7]) +
  #Frechas erros nivel 1
  geom_segment(x = StSE[69], xend = StSE[69], y = NotaMates[69], yend = NotaMates[69] - eps_mm1[69], color = 1, 
               arrow = arrow(), size = 1.15) +
  geom_segment(x = StSE[146], xend = StSE[146], y = NotaMates[146], yend = NotaMates[146] - eps_mm1[146], color = 1, 
               arrow = arrow(), size = 1.15) +
  geom_segment(x = StSE[152], xend = StSE[152], y = NotaMates[152], yend = NotaMates[152] - eps_mm1[152], color = 1, 
               arrow = arrow(), size = 1.15) +
  geom_segment(x = StSE[299], xend = StSE[299], y = NotaMates[299], yend = NotaMates[299] - eps_mm1[299], color = 1, 
               arrow = arrow(), size = 1.15) +
  #Simbolos erros nivel 1
  annotate("text", x = StSE[69] - .17, y = (NotaMates[69] + rectamm1(StSE[69], 3)) / 2 + .6, 
           parse = TRUE, label = expression(widehat(epsilon)[4][","][3]), col = 1, size = 7) +
  annotate("text", x = StSE[146] + .245, y = (NotaMates[146] + rectamm1(StSE[146], 4)) / 2, 
           parse = TRUE, label = expression(widehat(epsilon)[32][","][4]), col = 1, size = 7) +
  annotate("text", x = StSE[152] - .17, y = (NotaMates[152] + rectamm1(StSE[152], 5)) / 2 + .5, 
           parse = TRUE, label = expression(widehat(epsilon)[4][","][5]), col = 1, size = 7) +
  annotate("text", x = StSE[299] + .245, y = (NotaMates[299] + rectamm1(StSE[299], 7)) / 2, 
           parse = TRUE, label = expression(widehat(epsilon)[51][","][7]), col = 1, size = 7) +
  #Observacions
  geom_point( aes(x = StSE[69], y = NotaMates[69] ), col = dark_2[3], size = 5) +
  geom_point( aes(x = StSE[146], y = NotaMates[146] ), col = dark_2[4], size = 5) +
  geom_point( aes(x = StSE[152], y = NotaMates[152] ), col = dark_2[5], size = 5) +
  geom_point( aes(x = StSE[299], y = NotaMates[299] ), col = dark_2[7], size = 5) +
  annotate("text", x = StSE[69] + .185, y = NotaMates[69] + .35, parse = TRUE, 
           label = expression(Y[4][","][3]), col = dark_2[3], size = 6) +
  annotate("text", x = StSE[146] - .12, y = NotaMates[146] - .75, parse = TRUE, 
           label = expression(Y[32][","][4]), col = dark_2[4], size = 6) +
  annotate("text", x = StSE[152] + .185, y = NotaMates[152] + .35, parse = TRUE, 
           label = expression(Y[4][","][5]), col = dark_2[5], size = 6) +
  annotate("text", x = StSE[299] - .12, y = NotaMates[299] - .75, parse = TRUE, 
           label = expression(Y[51][","][7]), col = dark_2[7], size = 6) +
  #Pendente escola E4 widehat(beta)[1]
  geom_segment(x = -1.5, xend = -.5, y = rectamm1(-1.5, 4), yend = rectamm1(-1.5, 4), 
               linetype = "solid", size = 1, col = "violetred4") +
  geom_segment(x = -.5, xend = -.5, y = rectamm1(-1.5, 4), yend = rectamm1(-.5, 4), 
               linetype = "solid", size = 1, col = "violetred4") +
  annotate("text", x = -.375, y = (rectamm1(-1.5, 4) + rectamm1(-.5, 4)) / 2 - .15, 
           parse = TRUE, label = expression(widehat(beta)[1]), col = "violetred4", size = 6) +
  #Residuos a nivel de escola u^gorro_{0j}
  geom_segment(x = -1.65, xend = -1.65, y = rectamm1(-1.65), yend = rectamm1(-1.65, 3), 
               color = dark_2[3], arrow = arrow(length = unit(0.2, "inches")), linetype = "solid", size = .9) +
  geom_segment(x = -1.65, xend = -1.65, y = rectamm1(-1.65), yend = rectamm1(-1.65, 4), 
               color = dark_2[4], arrow = arrow(length = unit(0.2, "inches")), linetype = "solid", size = .9) +
  geom_segment(x = -1.35, xend = -1.35, y = rectamm1(-1.35), yend = rectamm1(-1.35, 5), 
               color = dark_2[5], arrow = arrow(length = unit(0.15, "inches")), linetype = "solid", size = .75) +
  geom_segment(x = -1.35, xend = -1.35, y = rectamm1(-1.35), yend = rectamm1(-1.35, 7), 
               color = dark_2[7], arrow = arrow(length = unit(0.15, "inches")), linetype = "solid", size = .75) +
  #Simbolos residuos a nivel de escola u^gorro_{0j}
  annotate("text", x = -1.525, y = (rectamm1(-1.5) + rectamm1(-1.5, 3)) / 2 - .45, parse = TRUE, 
           label = expression(widehat(u)[3]), col = dark_2[3], size = 6.5) +
  annotate("text", x = -1.525, y = (rectamm1(-1.5) + rectamm1(-1.5, 4)) / 2, parse = TRUE, 
           label = expression(widehat(u)[4]), col = dark_2[4], size = 6.5) +
  annotate("text", x = -1.22, y = (rectamm1(-1.175) + rectamm1(-1.175, 5)) / 2 + .15, parse = TRUE, 
           label = expression(widehat(u)[5]), col = dark_2[5], size = 6.5) +
  annotate("text", x = -1.22, y = (rectamm1(-1.2) + rectamm1(-1.2, 7)) / 2, parse = TRUE, 
           label = expression(widehat(u)[7]), col = dark_2[7], size = 6.5) +
  theme(
    legend.title = element_text(size = 18),
    legend.text = element_text(size = 16),
    axis.title.x = element_text(size = 18),
    axis.text.x = element_text(size = 16),
    axis.title.y = element_text(size = 18),
    axis.text.y = element_text(size = 16))



###-------------------------------matesmm2: MODELO CON INTERCEPTO ALEATORIO E PENDENTE FIXA E MAIS VARIABLE CATEGORICA------------

### Que variable categorica empregamos? Sexo ou SocialMin ???
matesmm2 <- lmer(NotaMates ~ StSE + SocialMin + (1 | Escola), data = mates7, REML = FALSE) # modelo con intercepto fixo 
  # (por defecto), pendente fixa asociada ao indice socio-economico e mais a variable discreta SocialMin, intercepto 
  # aleatorio e co identificador da variable de segundo nivel Escola
matesmmproba <- lmer(NotaMates ~ StSE + Sexo + (1 | Escola), data = mates7, REML = FALSE) # con Sexo
#Tests de razón de verosimilitudes
anova(matesmm1, matesmm2)
anova(matesmm1, matesmmproba)
lrtest(matesmm1, matesmm2) #analogo
lrtest(matesmm1, matesmmproba) #analogo
# O sexo reduce mais a varianza, mentres que o SocialMin e mais significativo.
# Vexamos que ocorre coas porcentaxes (para evitar extrapolacions):

portentaxe_SocialMin <- numeric(7)
for (i in 1:length(levels(Escola))){
  portentaxe_SocialMin[i] <- ( sum(SocialMin[Escola == levels(Escola)[i]] == "Si") / sum(Escola == levels(Escola)[i]) ) * 100
}
print(portentaxe_SocialMin) #So a escola E1 ten 0% de alumnos con SocialMin = "Si"
plot(SocialMin, StSE) #se e minoria ==>> menos status socio-economico

portentaxe_Sexo_mull <- numeric(7)
for (i in 1:length(levels(Escola))){
  portentaxe_Sexo_mull[i] <- ( sum(Sexo[Escola == levels(Escola)[i]] == "Muller") / sum(Escola == levels(Escola)[i]) ) * 100
}
print(portentaxe_Sexo_mull) #Extrapolariase demasiado ao considerar rectas para mulleres e rectas 
  # para homes en colexios unisexuais

#Polo tanto, no seguido traballarase con SocialMin

matesmm2 <- lmer(NotaMates ~ StSE + SocialMin + (1 | Escola), data = mates7, REML = FALSE) # modelo con intercepto fixo 
# (por defecto), pendente fixa asociada ao indice socio-economico e mais a variable discreta SocialMin, intercepto 
# aleatorio e co identificador da variable de segundo nivel Escola
summary(matesmm2)
coefsmm2 <- summary(matesmm2)$coef
sigma2_eps_mm2 <- summary(matesmm2)$sigma^2
sigma2_u0_mm2 <- as.data.frame(summary(matesmm2)$varcor)[1,4]
VT_mm2 <- sigma2_u0_mm2 + sigma2_eps_mm2
VPC_mm2 <- (sigma2_u0_mm2) / VT_mm2
#Calculo dos efectos aleatorios e dos erros (nivel 1)
u0_mm2 <- ranef(matesmm2, postVar = TRUE)
uj_mm2 <- u0_mm2[[1]][,1]
u0se_mm2 <- sqrt(attr(u0_mm2[[1]], "postVar")[1, , ])
eps_mm2 <- residuals(matesmm2)


###-------------------------------Figura matesmm2 ------------

rectamm2 <- function(x = 0, escola = NULL, sm = 0){
  if (!sm %in% 0:1){
    stop("Só hai dúas posibilidades para sm, non (0) ou si (1).")
  }
  if (!is.null(escola)){
    if (!escola %in% 1:7){
      stop("Tal escola non existe, só do 1 ao 7.")
    }
  }
  
  if (is.null(escola)){
    if (sm == 0){
      coefsmm2[1] + coefsmm2[2] * x
    }
    else{
      coefsmm2[1] + coefsmm2[3] + coefsmm2[2] * x
    }
  }
  else{
    if (sm == 0){
      coefsmm2[1] + uj_mm2[escola] + coefsmm2[2] * x
    }
    else{
      coefsmm2[1] + coefsmm2[3] + uj_mm2[escola] + coefsmm2[2] * x
    }
  }
}

ggplot(mates7, aes(x = StSE, y = NotaMates, color = Escola, shape = SocialMin)) +
  geom_point(size = 1.25) +
  coord_cartesian(xlim = c(range(StSE)[1], range(StSE)[2]), ylim = c(-.5, 25) ) +
  xlab("Status socio-económico") + 
  ylab("Nota en Matemáticas") +
  labs(shape = "Racial\nminori-\ntario") +
  scale_color_brewer(palette = "Dark2") +
  scale_shape_manual(values = c(16, 17)) +
  #Media global
  geom_segment(x = -1.7, xend = 1.5, y = rectamm2(-1.7), yend = rectamm2(1.5), size = 1.35, col = 1) +
  geom_segment(x = -1.7, xend = 1.5, y = rectamm2(-1.7, sm = 1), yend = rectamm2(1.5, sm = 1), 
               size = 1.35, col = 1) +
  #Media local 4
  geom_segment(x = -1.7, xend = 1.5, y = rectamm2(-1.7, escola = 4), yend = rectamm2(1.5, escola = 4), 
               lwd = 1, linetype = "twodash", color = dark_2[4]) +
  geom_segment(x = -1.7, xend = 1.5, y = rectamm2(-1.7, escola = 4, sm = 1), yend = 
                 rectamm2(1.5, escola = 4, sm = 1), lwd = 1, linetype = "twodash", color = dark_2[4]) +
  #Media local 5
  geom_segment(x = -1.7, xend = 1.5, y = rectamm2(-1.7, escola = 5), yend = rectamm2(1.5, escola = 5), 
               lwd = 1, linetype = "twodash", color = dark_2[5]) +
  geom_segment(x = -1.7, xend = 1.5, y = rectamm2(-1.7, escola = 5, sm = 1), 
               yend = rectamm2(1.5, escola = 5, sm = 1), lwd = 1, linetype = "twodash", color = dark_2[5]) +
  #Frechas erros nivel 1
  geom_segment(x = StSE[147], xend = StSE[147], y = NotaMates[147], 
               yend = NotaMates[147] - eps_mm2[147], color = 1, arrow = arrow(), size = 1.15) +
  geom_segment(x = StSE[174], xend = StSE[174], y = NotaMates[174], 
               yend = NotaMates[174] - eps_mm2[174], color = 1, arrow = arrow(), size = 1.15) +
  #Alumnos
  geom_point( aes(x = StSE[147], y = NotaMates[147] ), col = dark_2[4], size = 5, pch = 17) +
  geom_point( aes(x = StSE[174], y = NotaMates[174] ), col = dark_2[5], size = 5, pch = 16) +
  #Simbolos erros nivel 1
  annotate("text", x = StSE[147] - .22, y = (NotaMates[147] + (rectamm2(StSE[147], escola = 4, sm = 1)) ) 
           / 2, parse = TRUE, label = expression(widehat(epsilon)[33][","][4]), col = 1, size = 7) +
  annotate("text", x = StSE[174] - .22, y = (NotaMates[174] + (rectamm2(StSE[174], escola = 5)) ) 
           / 2 + .35, parse = TRUE, label = expression(widehat(epsilon)[26][","][5]), col = 1, size = 7) +
  #Observacions
  annotate("text", x = StSE[147] - .15, y = NotaMates[147] - .7, parse = TRUE, 
           label = expression(Y[33][","][4]), col = dark_2[4], size = 6) +
  annotate("text", x = StSE[174] + .22, y = NotaMates[174] + .45, parse = TRUE, 
           label = expression(Y[26][","][5]), col = dark_2[5], size = 6) +
  #Pendente escola E4 widehat(beta)[1]
  geom_segment(x = -.25, xend = .75, y = rectamm2(-.25, 4, 1), yend = rectamm2(-.25, 4, 1), 
               linetype = "solid", size = 1, col = "violetred4") +
  geom_segment(x = .75, xend = .75, y = rectamm2(-.25, 4, 1), yend = rectamm2(.75, 4, 1), 
               linetype = "solid", size = 1, col = "violetred4") +
  annotate("text", x = .87, y = (rectamm2(-.25, 4, 1) + rectamm2(.75, 4, 1)) / 2 - .23, 
           parse = TRUE, label = expression(widehat(beta)[1]), col = "violetred4", size = 6) +
  #Pendente escola E4 widehat(beta)[1] v2
  geom_segment(x = -.25, xend = .75, y = rectamm2(-.25, 4), yend = rectamm2(-.25, 4), 
               linetype = "solid", size = 1, col = "violetred4") +
  geom_segment(x = .75, xend = .75, y = rectamm2(-.25, 4), yend = rectamm2(.75, 4), 
               linetype = "solid", size = 1, col = "violetred4") +
  annotate("text", x = .87, y = (rectamm2(-.25, 4) + rectamm2(.75, 4)) / 2 - .23, 
           parse = TRUE, label = expression(widehat(beta)[1]), col = "violetred4", size = 6) +
  #Efectos intercepto SocialMin
  geom_segment(x = -.75, xend = -.75, y = rectamm2(-.75, 5), yend = rectamm2(-.75, 5, 1), color = "violetred4", 
               arrow = arrow(length = unit(0.175, "inches")), linetype = "solid", size = 1) +
  annotate("text", x = -.63, y = (rectamm2(-.75, 5) + rectamm2(-.75, 5, 1)) / 2 + .525, 
           parse = TRUE, label = expression(widehat(beta)[2]), col = "violetred4", size = 6) +
  geom_segment(x = 1.2, xend = 1.2, y = rectamm2(1.2, 4), yend = rectamm2(1.2, 4, 1), color = "violetred4", 
               arrow = arrow(length = unit(0.175, "inches")), linetype = "solid", size = 1) +
  annotate("text", x = 1.32, y = (rectamm2(1.2, 4) + rectamm2(1.2, 4, 1)) / 2 + .27, parse = TRUE, 
           label = expression(widehat(beta)[2]), col = "violetred4", size = 6) +
  geom_segment(x = 0, xend = 0, y = rectamm2(), yend = rectamm2(sm = 1), color = "violetred4", 
               arrow = arrow(length = unit(0.175, "inches")), linetype = "solid", size = 1) +
  annotate("text", x = .12, y = (rectamm2() + rectamm2(sm = 1)) / 2 + .27, parse = TRUE, 
           label = expression(widehat(beta)[2]), col = "violetred4", size = 6) +
  #Residuos a nivel de escola u^gorro_{j}, para SocialMin = 0
  geom_segment(x = -1.65, xend = -1.65, y = rectamm2(-1.65), yend = rectamm2(-1.65, escola = 4), 
               color = dark_2[4], arrow = arrow(length = unit(0.15, "inches")), linetype = "solid", size = .9) +
  geom_segment(x = -1.60, xend = -1.60, y = rectamm2(-1.60), yend = rectamm2(-1.60, escola = 5), 
               color = dark_2[5], arrow = arrow(length = unit(0.14, "inches")), linetype = "solid", size = .9) +
  annotate("text", x = -1.535, y = (rectamm2(-1.525) + rectamm2(-1.525, escola = 4) ) / 2 - .25, 
           parse = TRUE, label = expression(widehat(u)[4]), col = dark_2[4], size = 6.5) +
  annotate("text", x = -1.445, y = (rectamm2(-1.445) + rectamm2(-1.445, escola = 5) ) / 2, 
           parse = TRUE, label = expression(widehat(u)[5]), col = dark_2[5], size = 6.5) +
  #Residuos a nivel de escola u^gorro_{j}, para SocialMin = 1
  geom_segment(x = -1.40, xend = -1.40, y = rectamm2(-1.40, sm = 1), yend = rectamm2(-1.40, escola = 4, sm = 1), 
               color = dark_2[4], arrow = arrow(length = unit(0.15, "inches")), linetype = "solid", size = .9) +
  geom_segment(x = -1.35, xend = -1.35, y = rectamm2(-1.35, sm = 1), yend = rectamm2(-1.35, escola = 5, sm = 1), 
               color = dark_2[5], arrow = arrow(length = unit(0.14, "inches")), linetype = "solid", size = .9) +
  annotate("text", x = -1.285, y = (rectamm2(-1.275, sm = 1) + rectamm2(-1.275, escola = 4, sm = 1) ) 
           / 2 - .25, parse = TRUE, label = expression(widehat(u)[4]), col = dark_2[4], size = 6.5) +
  annotate("text", x = -1.195, y = (rectamm2(-1.195, sm = 1) + rectamm2(-1.195, escola = 5, sm = 1) ) 
           / 2, parse = TRUE, label = expression(widehat(u)[5]), col = dark_2[5], size = 6.5) +
  theme(
    legend.title = element_text(size = 18),
    legend.text = element_text(size = 16),
    axis.title.x = element_text(size = 18),
    axis.text.x = element_text(size = 16),
    axis.title.y = element_text(size = 18),
    axis.text.y = element_text(size = 16))



###-------------------------------matesmm3: MODELO CON INTERCEPTO E PENDENTE ALEATORIOS E MAIS VARIABLE CATEGORICA------------
# Axuste do modelo, con variable explicativa asociada ao nivel inferior, con intercepto e
  # pendente aleatorios; e mais outra variable explicativa fixa:
  # Y_{ij} = beta_{0j} + beta_{1j} * StSE_{ij}+ beta_{2}*SocialMin_{ij} + eps_{ij}, onde
  # beta_{0j} = gamma_{00} + u_{0j}, u_{0j}~N(0,sigma^2_{u0})
  # beta_{1j} = gamma_{10} + u_{1j}, u_{1j}~N(0,sigma^2_{u1})
  # i.e., Y_{ij} = gamma_{00} + gamma_{10}*StSE_{ij} + beta_{2}*SocialMin{ij} + u_{0j} + u_{1j}*StSE_{ij} + eps{ij}
matesmm3 <- lmer(NotaMates ~ StSE + SocialMin + (1 + StSE | Escola), REML = TRUE) # modelo con intercepto fixo 
  # (por defecto), pendente fixa asociada ao status socio-economico e mais a variable discreta SocialMin, 
  # intercepto aleatorio, pendente aleatoria asociada a StSE e co identificador da variable de segundo nivel Escola
summary(matesmm3)
coefsmm3 <- summary(matesmm3)$coef
sigma2_eps_mm3 <- summary(matesmm3)$sigma^2
sigma2_u0_mm3 <- as.data.frame(summary(matesmm3)$varcor)[1,4]
sigma2_u1_mm3 <- as.data.frame(summary(matesmm3)$varcor)[2,4]
sigma_u01_mm3 <- as.data.frame(summary(matesmm3)$varcor)[3,4]
VT_mm3 <- sigma2_u0_mm3 + sigma2_u1_mm3 + sigma2_eps_mm3
VPC_mm3 <- (sigma2_u0_mm3 + sigma2_u1_mm3) / VT_mm3
#Calculo dos efectos aleatorios e dos erros (nivel 1)
u0_mm3 <- ranef(matesmm3, postVar = TRUE)
u0j_mm3 <- u0_mm3[[1]][,1]
u1j_mm3 <- u0_mm3[[1]][,2]
eps_mm3 <- residuals(matesmm3)

###-------------------------------Figura matesmm3 ------------

rectamm3 <- function(x = 0, escola = NULL, sm = 0){
  if (!sm %in% 0:1){
    stop("Só hai dúas posibilidades para sm, non (0) ou si (1).")
  }
  if (!is.null(escola)){
    if (!escola %in% 1:7){
      stop("Tal escola non existe, só do 1 ao 7.")
    }
  }
  
  if (is.null(escola)){
    if (sm == 0){
      coefsmm3[1] + coefsmm3[2] * x
    }
    else{
      coefsmm3[1] + coefsmm3[3] + coefsmm3[2] * x
    }
  }
  else{
    if (sm == 0){
      coefsmm3[1] + u0j_mm3[escola] + (coefsmm3[2] + u1j_mm3[escola]) * x
    }
    else{
      coefsmm3[1] + coefsmm3[3] + u0j_mm3[escola] + (coefsmm3[2] + u1j_mm3[escola]) * x
    }
  }
}

ggplot(mates7, aes(x = StSE, y = NotaMates, color = Escola, shape = SocialMin)) +
  geom_point(size = 1.25) +
  coord_cartesian(xlim = c(range(StSE)[1], range(StSE)[2]), ylim = c(-.5, 25) ) +
  xlab("Status socio-económico") + 
  ylab("Nota en Matemáticas") +
  labs(shape = "Racial\nminori-\ntario") +
  scale_color_brewer(palette = "Dark2") +
  scale_shape_manual(values = c(16, 17)) +
  #Media global
  geom_segment(x = -1.7, xend = 1.5, y = rectamm3(-1.7), yend = rectamm3(1.5), size = 1.35, col = 1) +
  #Medias locais
  geom_segment(x = -1.7, xend = 1.50, y = rectamm3(-1.7, escola = 3), yend = rectamm3(1.50, 
                                                                                      escola = 3), lwd = 1, linetype = "twodash", color = dark_2[3]) +
  geom_segment(x = -1.7, xend = 1.50, y = rectamm3(-1.7, escola = 4), yend = rectamm3(1.50, 
                                                                                      escola = 4), lwd = 1, linetype = "twodash", color = dark_2[4]) +
  geom_segment(x = -1.7, xend = 1.50, y = rectamm3(-1.7, escola = 4, 1), 
               yend = rectamm3(1.50, escola = 4, 1), lwd = 1, linetype = "dotted", color = dark_2[4]) +
  #Frechas erros nivel 1
  geom_segment(x = StSE[72], xend = StSE[72], y = NotaMates[72], yend = NotaMates[72] - eps_mm3[72], 
               color = 1, arrow = arrow(), size = 1.15) +
  geom_segment(x = StSE[140], xend = StSE[140], y = NotaMates[140], 
               yend = NotaMates[140] - eps_mm3[140], color = 1, arrow = arrow(), size = 1.15) +
  #Alumnos
  geom_point( aes(x = StSE[72], y = NotaMates[72] ), col = dark_2[3], size = 5, pch = 16) +
  geom_point( aes(x = StSE[140], y = NotaMates[140] ), col = dark_2[4], size = 5, pch = 16) +
  #Simbolos erros nivel 1
  annotate("text", x = StSE[72] - .2, y = (NotaMates[72] + (rectamm3(StSE[72], escola = 3)) ) / 2 + .35, parse = TRUE, label = expression(widehat(epsilon)[7][","][3]), col = 1, size = 8) +
  annotate("text", x = StSE[140] - .24, y = (NotaMates[140] + (rectamm3(StSE[140], escola = 4)) ) / 2 - .35, parse = TRUE, label = expression(widehat(epsilon)[26][","][4]), col = 1, size = 8) +
  #Observacions
  annotate("text", x = StSE[72] + .18, y = NotaMates[72] + .425, parse = TRUE, 
           label = expression(Y[7][","][3]), col = dark_2[3], size = 7) +
  annotate("text", x = StSE[140] - .125, y = NotaMates[140] - .75, parse = TRUE,
           label = expression(Y[26][","][4]), col = dark_2[4], size = 7) +
  #Residuos a nivel de escola de intercepto u^gorro_{0j}, para SocialMin = 0
  geom_segment(x = 0, xend = 0, y = rectamm3(), yend = rectamm3(0, 3), color = dark_2[3], 
               arrow = arrow(length = unit(0.15, "inches")), linetype = "solid", size = .9) +
  geom_segment(x = 0, xend = 0, y = rectamm3(), yend = rectamm3(0, 4), color = dark_2[4], 
               arrow = arrow(length = unit(0.15, "inches")), linetype = "solid", size = .9) +
  annotate("text", x = +.155, y = (rectamm3() + rectamm3(0, 3) ) / 2 - .1, parse = TRUE, 
           label = expression(widehat(u)[0][3]), col = dark_2[3], size = 6.5) +
  annotate("text", x = +.155, y = (rectamm3() + rectamm3(0, 4) ) / 2, parse = TRUE, 
           label = expression(widehat(u)[0][4]), col = dark_2[4], size = 6.5) +
  #Pendente xeral gamma_{10}
  geom_segment(x = -1.5, xend = -.5, y = rectamm3(-1.5), yend = rectamm3(-1.5), linetype = "solid", 
               size = 1, col = "violetred4") +
  geom_segment(x = -.5, xend = -.5, y = rectamm3(-1.5), yend = rectamm3(-.5), linetype = "solid", 
               size = 1, col = "violetred4") +
  annotate("text", x = -.345, y = (rectamm3(-1.5) + rectamm3(-.5) ) / 2 - .35, parse = TRUE, 
           label = expression(widehat(gamma)[1][0]), col = "violetred4", size = 6) +
  #Pendente escola E4 gamma_{10} + ugorro_{14} con SocialMin = 0
  geom_segment(x = -1.5, xend = -.5, y = rectamm3(-1.5, 4), yend = rectamm3(-1.5, 4), 
               linetype = "solid", size = 1, col = "violetred4") +
  geom_segment(x = -.5, xend = -.5, y = rectamm3(-1.5, 4), yend = rectamm3(-.5, 4), 
               linetype = "solid", size = 1, col = "violetred4") +
  annotate("text", x = -.175, y = (rectamm3(-1.5, 4) + rectamm3(-.5, 4) ) / 2 - .1, parse = TRUE, 
           label = expression(widehat(gamma)[1][0] + widehat(u)[1][4]), col = "violetred4", size = 6) +
  #Pendente escola E4 gamma_{10} + ugorro_{14} con SocialMin = 1
  geom_segment(x = -1.5, xend = -.5, y = rectamm3(-1.5, 4, 1), yend = rectamm3(-1.5, 4, 1), 
               linetype = "solid", size = 1, col = "violetred4") +
  geom_segment(x = -.5, xend = -.5, y = rectamm3(-1.5, 4, 1), yend = rectamm3(-.5, 4, 1), 
               linetype = "solid", size = 1, col = "violetred4") +
  annotate("text", x = -.175, y = (rectamm3(-1.5, 4, 1) + rectamm3(-.5, 4, 1) ) / 2 - .1, parse = TRUE, 
           label = expression(widehat(gamma)[1][0] + widehat(u)[1][4]), col = "violetred4", size = 6) +
  #Pendente escola E3 gamma_{10} + ugorro_{13}
  geom_segment(x = -1.5, xend = -.5, y = rectamm3(-1.5, 3), yend = rectamm3(-1.5, 3), 
               linetype = "solid", size = 1, col = "violetred4") +
  geom_segment(x = -.5, xend = -.5, y = rectamm3(-1.5, 3), yend = rectamm3(-.5, 3), 
               linetype = "solid", size = 1, col = "violetred4") +
  annotate("text", x = -.375, y = (rectamm3(-1.5, 3) + rectamm3(-.5, 3) ) / 2 - .85, parse = TRUE, 
           label = expression(widehat(gamma)[1][0] + widehat(u)[1][3]), col = "violetred4", size = 6, angle = 10) +
  #Pendente asociada a SocialMin beta_2
  geom_segment(x = 1.25, xend = 1.25, y = rectamm3(1.25, 4), yend = rectamm3(1.25, 4, 1), 
               color = dark_2[4], 
               arrow = arrow(length = unit(0.15, "inches")), linetype = "solid", size = .9) +
  annotate("text", x = 1.375, y = (rectamm3(1.35, 4) + rectamm3(1.35, 4, 1) ) / 2 + .15, 
           parse = TRUE, label = expression(widehat(beta)[2]), col = dark_2[4], size = 6) +
  theme(
    legend.title = element_text(size = 18),
    legend.text = element_text(size = 16),
    axis.title.x = element_text(size = 18),
    axis.text.x = element_text(size = 16),
    axis.title.y = element_text(size = 18),
    axis.text.y = element_text(size = 16))

#########Contraste sobre as pendentes aleatorias
LR <- 2 * logLik(matesmm3)[1] - 2 * logLik(matesmm2)[1] #segue unha chi cadrado con 2 grados de liberdade 
  # (diferenza de parametros entre os modelos)
1 - pchisq(LR, df = 2) #p-valor = 0.1898, maior que 0.001; logo pendentes aleatorias non significativas, i.e., 
  # o efecto do StSE sobre as notas non varia entre as diferentes escolas
lrtest(matesmm2, matesmm3) #test de verosimilitudes directo

#########Contraste sobre as pendentes aleatorias relativas a SocialMin
matesmm_SMa <- lmer(NotaMates ~ StSE + SocialMin + (1 + StSE + SocialMin| Escola), REML = TRUE)
summary(matesmm_SMa)
LR_mm3_SMa <- 2 * logLik(matesmm_SMa)[1] - 2 * logLik(matesmm3)[1] #segue unha chi cadrado con 3 grados 
  # de liberdade (diferenza de parametros entre os modelos)
pval_mm3_SMa <- 1 - pchisq(LR_mm3_SMa, df = 3) # nivel critico de 0.8046, maior que os habituais (e que 0.001),
  # polo que non existen probas estatisticamente significativas  a favor da hipotese alternativa, i.e., o efecto 
  # de SocialMin sobre as notas non varia entre as diferentes escolas.
lrtest(matesmm3, matesmm_SMa) #test de verosimilitudes directo



#-------------------------------------------------#
# Rectas axustadas
#Rectas axustada para escola j:
# se SM = 0: NotaMates^gorro_{ij} = (16.9257 + u^gorro_{0j}) + (1.0142 + u^gorro_{1j}) * StSE_{ij}
# se SM = 1: NotaMates^gorro_{ij} = (16.9257 + betagorro_2  + u^gorro_{0j}) + (1.0142 + u^gorro_{1j}) * StSE_{ij}

prednotasmm3 <- fitted(matesmm3) #predicion das notas para cada estudante

#Debuxamos as rectas de regresion axustadas
datapred <- cbind(prednotasmm3 = prednotasmm3, StSE = StSE, Escola = Escola, SocialMin = SocialMin)
datapred <- data.frame(unique(datapred)) #eliminamos repetidos
datapred <- datapred[order(datapred$Escola, datapred$StSE), ] #ordeamos as notas de menor a maior

ggplot(data = datapred[datapred$SocialMin == 1,]) + 
  ggtitle("SocialMin = \"No\"") +
  coord_cartesian(xlim = c(range(StSE)[1], range(StSE)[2]), ylim = c(range(prednotasmm3)[1], range(prednotasmm3)[2]) ) +
  geom_point(mapping = aes(x = StSE, y = prednotasmm3, group = Escola), colour= "tomato3") +
  xlab("StSE") + ylab("Notas axustadas") +
  geom_line(aes(x = StSE, y = prednotasmm3, group = Escola), col = "gray25", size = 0.75) +
  theme_bw() +
  
  ggplot(data = datapred[datapred$SocialMin == 2,], col = Escola) + 
  ggtitle("SocialMin = \"Si\"") +
  coord_cartesian(xlim = c(range(StSE)[1], range(StSE)[2]), ylim = c(range(prednotasmm3)[1], range(prednotasmm3)[2]) ) +
  geom_point(mapping = aes(x = StSE, y = prednotasmm3, group = Escola), colour= "tomato3") +
  xlab("StSE") +
  geom_line(aes(x = StSE, y = prednotasmm3, group = Escola), col = "gray25", size = 0.75) +
  theme_bw() +
  theme(
    axis.title.y = element_blank(),
  )
