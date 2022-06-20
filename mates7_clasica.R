
######################################################
#####   
#####        SCRIPT: CODIGO TFG ESTATISTICA CLASICA
##### INTRODUCION AOS MODELOS MIXTOS
##         INTRODUCION E CAPITULO 1
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
##   Creacion da base de datos mates
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


######################################
##
##   Introducion
##
######################################

set.seed(123)
ind <- sample(1:dim(mates)[1], 40) #40 alumnas/os aleatorios
mls0 <- lm(NotaMates[ind] ~ StSE[ind]) #modelo linear simple
eps <- residuals(mls0)

ggplot(data.frame(cbind(StSE[ind], NotaMates[ind])), aes(x = StSE[ind], y = NotaMates[ind])) +
  geom_point() +
  xlab("Status socio-económico de 40 nenas/os") + ylab("Nota en Matemáticas de 40 nenas/os") +
  geom_smooth(method='lm', se = FALSE, col = "darkcyan") +
  geom_segment(x = StSE[ind][12], y = NotaMates[ind][12], xend = StSE[ind][12], 
               yend = NotaMates[ind][12] - eps[12], color = "tomato3", arrow = arrow(), size = 1) +
  geom_point( aes(x = StSE[ind][12], y = NotaMates[ind][12] ), col = "purple4", size = 4 ) +
  annotate("text", x = StSE[ind][12] + 1/6, y = NotaMates[ind][12] - eps[12]/2 + 1/3, 	parse = TRUE,
           label = expression(widehat(epsilon)[i]), size = 10, col = "tomato3", cex = 2) +
  annotate("text", x = StSE[ind][12] - 1/6, y = NotaMates[ind][12], 	parse = TRUE,
           label = expression(Y[i]), size = 9, col = "purple4", cex = 2) +
  annotate("text", x = 0.1, y = mls0$coefficients[[1]] - 2, parse = TRUE, label =
             expression(paste(widehat(beta)[0], + widehat(beta)[1], "x")), size = 8, col = "darkcyan") +
  #	theme_bw() +
  theme(
    axis.title.x = element_text(size = 20),
    axis.text.x = element_text(size = 18),
    axis.title.y = element_text(size = 20),
    axis.text.y = element_text(size = 18))


######################################
##
##   Creacion da base de datos mates7
##
######################################

escolas_particip100 <- which(MathAchSchool$PRACAD == 1) #escolas con participacion unanime
alum_escolas_particip100 <- which(Escola %in% escolas_particip100) #alumnos de escolas con participacion unanime
length(alum_escolas_particip100) #307 alumnos de 7 escolas

mates7 <- mates[alum_escolas_particip100, ] #evitamos sesgos de eleccion
dim(mates7) # 307 alumnas/os e 11 variables
mates7$Escola <- factor(mates7$Escola, levels = escolas_particip100) #corrixir levels
levels(mates7$Escola) <- paste("E", 1:7, sep = "") #Cambiase o nome destas 7 escolas
attach(mates7)


######################################
##
##   ANOVA
##
######################################

anova_mates7 <- lm(NotaMates ~ Escola - 1)
summary(anova_mates7)
mu_local <- numeric(7)
for (i in 1:length(levels(mates7$Escola))){
  mu_local[i] <- mean(NotaMates[Escola == levels(mates7$Escola)[i]])
} #vector de medias locais


ggplot(mates7, aes(x = Escola, y = NotaMates, color = Escola)) + 
  geom_boxplot() +
  labs(x = "Escolas", y = "Notas en Matemáticas", color = "Escolas") +
  scale_color_brewer(palette = "Dark2") +
  geom_segment(x = 0.625, y = mu_local[1], xend = 1.375, yend = mu_local[1], linetype = "dotted", col = "firebrick4") +
  geom_segment(x = 1.625, y = mu_local[2], xend = 2.375, yend = mu_local[2], linetype = "dotted", col = "firebrick4") +
  geom_segment(x = 2.625, y = mu_local[3], xend = 3.375, yend = mu_local[3], linetype = "dotted", col = "firebrick4") +
  geom_segment(x = 3.625, y = mu_local[4], xend = 4.375, yend = mu_local[4], linetype = "dotted", col = "firebrick4") +
  geom_segment(x = 4.625, y = mu_local[5], xend = 5.375, yend = mu_local[5], linetype = "dotted", col = "firebrick4") +
  geom_segment(x = 5.625, y = mu_local[6], xend = 6.375, yend = mu_local[6], linetype = "dotted", col = "firebrick4") +
  geom_segment(x = 6.625, y = mu_local[7], xend = 7.375, yend = mu_local[7], linetype = "dotted", col = "firebrick4") +
  geom_hline(yintercept = mean(NotaMates), linetype = "dashed", col = "navy") +
  #theme_bw() +
  theme(
    legend.position = "none",
    axis.title.x = element_text(size = 18),
    axis.text.x = element_text(size = 16),
    axis.title.y = element_text(size = 18),
    axis.text.y = element_text(size = 16))


#----------------------------------------------------#
#Test F e contrastes pareados
anova(lm(NotaMates ~ Escola)) #Taboa ANOVA
pf(10.475, df1 = 6, df2 = 300, lower.tail = F) #p-valor
source("Bonferroni_taboa.R") #cargado da funcion, vexase Bonferroni_taboa.R (GitHub)
Bonferroni_taboa(NotaMates, Escola, alpha_sen_CB = 0.001, ndixitos = 3)
mates7_bonf <- Bonferroni_taboa(NotaMates, Escola, alpha_sen_CB = 0.001, ndixitos = 3) 
mates7_bonf$intervsim # intervalos de confianza simultaneos
mates7_bonf$difiren # difiren as escolas: E1-E4, E3-E4, E3-E7 e E4-E5
mates7_bonf$nondifiren # escolas que non difiren
mates7_bonf$alpha_CB #alpha empregado coa corrección de Bonferroni


######################################
##
##   ANCOVA
##
######################################

ancova1 <- lm(NotaMates ~ StSE + Escola) #ANCOVA sen interaccion
ancova2 <- lm(NotaMates ~ StSE * Escola) #ANCOVA con interaccion

anova(ancova1, ancova2) #test F, interacción non significativa

#Contraste sobre o efecto dos grupos e de StSE
mod_StSE <- lm(NotaMates ~ StSE) #sen grupos
mod_g <- lm(NotaMates ~ Escola) #sen StSE
anova(ancova1, mod_StSE) #efecto das escolas significativo
anova(ancova1, mod_g) #efecto do StSE significativo

###-------------------------------GRAFICO ANCOVA------------

coefs_anc1 <- ancova1$coefficients #estimacions ANCOVA sen interacción
display.brewer.pal(n = 7, name = "Dark2") #Paleta de cores que se vai empregar
dark_2 <- brewer.pal(n = 7, name = "Dark2")
rang <- data.frame(i = numeric(7), f = numeric(7))
for (k in 1:length(levels(Escola))){
  rang[k ,] <- range(StSE[Escola == levels(Escola)[k]])
} #Funcion para os rangos das rectas
recta <- function(x = 0, grupo = 1){
  if (!grupo %in% 1:7){
    stop("Tal grupo non existe.")
  }
  if (grupo == 1){
    as.numeric(coefs_anc1[1]) + as.numeric(coefs_anc1[2]) * x
  }
  else{
    as.numeric(coefs_anc1[1]) + as.numeric(coefs_anc1[grupo + 1]) + as.numeric(coefs_anc1[2]) * x
  }
} #Funcion para as ecuacions das rectas

ancova_si <- ggplot(mates7, aes(x = StSE, y = NotaMates, color = Escola)) + 
  geom_point() +
  labs(x = "Status socio-económico", y = "Notas en Matemáticas", color = "Escola") +
  coord_cartesian(ylim = c(-.5, 25) ) +
  scale_color_brewer(palette = "Dark2") +
  geom_segment(x = rang[1,1], xend = rang[1,2], y = recta(rang[1,1]), yend = recta(rang[1,2]), 
               col = dark_2[1], lwd = 1.25) +
  geom_segment(x = rang[2,1], xend = rang[2,2], y = recta(rang[2,1], 2), yend = recta(rang[2,2], 2), 
               col = dark_2[2], lwd = 1.25) +
  geom_segment(x = rang[3,1], xend = rang[3,2], y = recta(rang[3,1], 3), yend = recta(rang[3,2], 3), 
               col = dark_2[3], lwd = 1.25) +
  geom_segment(x = rang[4,1], xend = rang[4,2], y = recta(rang[4,1], 4), yend = recta(rang[4,2], 4), 
               col = dark_2[4], lwd = 1.25) +
  geom_segment(x = rang[5,1], xend = rang[5,2], y = recta(rang[5,1], 5), yend = recta(rang[5,2], 5), 
               col = dark_2[5], lwd = 1.25) +
  geom_segment(x = rang[6,1], xend = rang[6,2], y = recta(rang[6,1], 6), yend = recta(rang[6,2], 6), 
               col = dark_2[6], lwd = 1.25) +
  geom_segment(x = rang[7,1], xend = rang[7,2], y = recta(rang[7,1], 7), yend = recta(rang[7,2], 7), 
               col = dark_2[7], lwd = 1.25) +
  theme(
    legend.position = "none",
    axis.title.x = element_text(size = 16),
    axis.text.x = element_text(size = 14),
    axis.title.y = element_text(size = 16),
    axis.text.y = element_text(size = 14))

ancova_ci <- ggplot(mates7, aes(x = StSE, y = NotaMates, color = Escola)) + 
  geom_point() +
  labs(x = "Status socio-económico", color = "Escola") +
  scale_color_brewer(palette = "Dark2") +
  geom_smooth(method = "lm", se = FALSE, lwd = 1.25) +
  #  theme_bw() +
  theme(
    legend.title = element_text(size = 16),
    legend.text = element_text(size = 14),
    axis.title.x = element_text(size = 16),
    axis.text.x = element_text(size = 14),
    axis.title.y = element_blank(),
    axis.text.y = element_text(size = 14))

ancova_si + ancova_ci

