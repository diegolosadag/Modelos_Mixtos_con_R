
######################################################
#####   
#####        SCRIPT: CODIGO TFG ESTATISTICA CLASICA
##### INTRODUCION AOS MODELOS MIXTOS
##         CAPITULO 5: MODELOS MIXTOS CON COVARIABLES RELATIVAS AO SEGUNDO NIVEL
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
##   Modelos mixtos con covariables relativas ao nivel 2
##
######################################

### Modelos Capitulo 4:
matesmm1 <- lmer(NotaMates ~ StSE + (1 | Escola), data = mates7, REML = FALSE)
matesmm2 <- lmer(NotaMates ~ StSE + SocialMin + (1 | Escola), data = mates7, REML = FALSE)
matesmm3 <- lmer(NotaMates ~ StSE + SocialMin + (1 + StSE | Escola), REML = TRUE)

###-------------------------------matesmm4: Variable contextual composicional------------
matesmm4 <- lmer(NotaMates ~ StSE + SocialMin + MStSE + (1 + StSE | Escola), REML = TRUE)
summary(matesmm4)
coefsmm4 <- summary(matesmm4)$coef
sigma2_eps_mm4 <- summary(matesmm4)$sigma^2
sigma2_u0_mm4 <- as.data.frame(summary(matesmm4)$varcor)[1,4]
sigma2_u1_mm4 <- as.data.frame(summary(matesmm4)$varcor)[2,4]
sigma_u01_mm4 <- as.data.frame(summary(matesmm4)$varcor)[3,4]
#Calculo dos efectos aleatorios e dos erros (nivel 1)
u0_mm4 <- ranef(matesmm4, postVar = TRUE)
u0j_mm4 <- u0_mm4[[1]][,1]
u1j_mm4 <- u0_mm4[[1]][,2]
eps_mm4 <- residuals(matesmm4)

# Contraste sobre efecto fixo de MStSE
LR_mm4 <- 2 * logLik(matesmm4)[1] - 2 * logLik(matesmm3)[1]
pval_mm4_MStSE <- 1 - pchisq(LR_mm4, df = 1) # p-valor

###-------------------------------Figura matesmm4 ------------

rectamm4 <- function(x = 0, escola = NULL, sm = 0){
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
      coefsmm4[1] + coefsmm4[2] * x
    }
    else{
      coefsmm4[1] + coefsmm4[3] + coefsmm4[2] * x
    }
  }
  
  else{
    if (sm == 0){
      coefsmm4[1] + u0j_mm4[escola] + coefsmm4[4] * MStSE[Escola == levels(Escola)[escola]][1] + (coefsmm4[2] + u1j_mm4[escola]) * x
    }
    else{
      coefsmm4[1]+ coefsmm4[3] + coefsmm4[4] * MStSE[Escola == levels(Escola)[escola]][1] + u0j_mm4[escola] + (coefsmm4[2] + u1j_mm4[escola]) * x
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
  geom_segment(x = -1.7, xend = 1.5, y = rectamm4(-1.7), yend = rectamm4(1.5), size = 1.35, col = 1) +
  #Media local
  geom_segment(x = -1.7, xend = 1.5, y = rectamm4(-1.7, escola = 3), yend = rectamm4(1.5, escola = 3), 
               lwd = 1, linetype = "twodash", color = dark_2[3]) +
  geom_segment(x = -1.7, xend = 1.5, y = rectamm4(-1.7, escola = 4), yend = rectamm4(1.5, escola = 4), 
               lwd = 1, linetype = "twodash", color = dark_2[4]) +
  geom_segment(x = -1.7, xend = 1.5, y = rectamm4(-1.7, escola = 4, 1), 
               yend = rectamm4(1.5, escola = 4, 1), lwd = 1, linetype = "dotted", color = dark_2[4]) +
  #Frechas erros nivel 1
  geom_segment(x = StSE[72], xend = StSE[72], y = NotaMates[72], yend = NotaMates[72] - eps_mm4[72], 
               color = 1, arrow = arrow(), size = 1.15) +
  geom_segment(x = StSE[140], xend = StSE[140], y = NotaMates[140], yend = NotaMates[140] - eps_mm4[140], 
               color = 1, arrow = arrow(), size = 1.15) +
  #Alumnos
  geom_point( aes(x = StSE[72], y = NotaMates[72] ), col = dark_2[3], size = 5, pch = 16) +
  geom_point( aes(x = StSE[140], y = NotaMates[140] ), col = dark_2[4], size = 5, pch = 16) +
  #Simbolos erros nivel 1
  annotate("text", x = StSE[72] - .18, y = (NotaMates[72] + (rectamm4(StSE[72], escola = 3)) ) / 2 + .35, 
           parse = TRUE, label = expression(widehat(epsilon)[7][","][3]), col = 1, size = 7) +
  annotate("text", x = StSE[140] - .22, y = (NotaMates[140] + (rectamm4(StSE[140], escola = 4)) ) / 2 
           - .35, parse = TRUE, label = expression(widehat(epsilon)[26][","][4]), col = 1, size = 7) +
  #Observacions
  annotate("text", x = StSE[72] + .175, y = NotaMates[72] + .475, parse = TRUE, 
           label = expression(Y[7][","][3]), col = dark_2[3], size = 6) +
  annotate("text", x = StSE[140] - .12, y = NotaMates[140] - .775, parse = TRUE,
           label = expression(Y[26][","][4]), col = dark_2[4], size = 6) +
  #Residuos a nivel de escola de intercepto u^gorro_{0j}, para sm = 0
  geom_segment(x = 0, xend = 0, y = rectamm4(), yend = rectamm4(escola = 3), color = dark_2[3], 
               arrow = arrow(length = unit(0.15, "inches")), linetype = "solid", size = .9) +
  geom_segment(x = 0, xend = 0, y = rectamm4(), yend = rectamm4(escola = 4), color = dark_2[4], 
               arrow = arrow(length = unit(0.15, "inches")), linetype = "solid", size = .9) +
  annotate("text", x = .65, y = (rectamm4() + rectamm4(0, 3)) / 2 + .35, parse = TRUE, 
           label = TeX('$\\widehat{u}_{0 3} + \\widehat{\\beta}_3 \\times \\bar{StSE}_{\\bullet 3}$'), col = dark_2[3], size = 5.9, angle = 4) +
  annotate("text", x = .65, y = (rectamm4() + rectamm4(0, 4)) / 2 + .35, parse = TRUE, 
           label = TeX('$\\widehat{u}_{0 4} + \\widehat{\\beta}_3 \\times \\bar{StSE}_{\\bullet 4}$'), col = dark_2[4], size = 5.9, angle = 4) +
  #Pendente xeral gamma_{10}
  geom_segment(x = -1.5, xend = -.5, y = rectamm4(-1.5), yend = rectamm4(-1.5), linetype = "solid", 
               size = 1, col = "violetred4") +
  geom_segment(x = -.5, xend = -.5, y = rectamm4(-1.5), yend = rectamm4(-.5), linetype = "solid", 
               size = 1, col = "violetred4") +
  annotate("text", x = -.345, y = (rectamm4(-1.5) + rectamm4(-.5) ) / 2 - .35, parse = TRUE, 
           label = expression(widehat(gamma)[1][0]), col = "violetred4", size = 6) +
  #Pendente escola 4 gamma_{10} + ugorro_{14} con sm = 0
  geom_segment(x = -1.5, xend = -.5, y = rectamm4(-1.5, 4), yend = rectamm4(-1.5, 4), 
               linetype = "solid", size = 1, col = "violetred4") +
  geom_segment(x = -.5, xend = -.5, y = rectamm4(-1.5, 4), yend = rectamm4(-.5, 4), 
               linetype = "solid", size = 1, col = "violetred4") +
  annotate("text", x = -.175, y = (rectamm4(-1.5, 4) + rectamm4(-.5, 4) ) / 2 - .1, parse = TRUE, 
           label = expression(widehat(gamma)[1][0] + widehat(u)[1][4]), col = "violetred4", size = 6) +
  #Pendente escola 4 gamma_{10} + ugorro_{14} con sm = 1
  geom_segment(x = -1.5, xend = -.5, y = rectamm4(-1.5, 4, 1), yend = rectamm4(-1.5, 4, 1), 
               linetype = "solid", size = 1, col = "violetred4") +
  geom_segment(x = -.5, xend = -.5, y = rectamm4(-1.5, 4, 1), yend = rectamm4(-.5, 4, 1), 
               linetype = "solid", size = 1, col = "violetred4") +
  annotate("text", x = -.175, y = (rectamm4(-1.5, 4, 1) + rectamm4(-.5, 4, 1) ) / 2 - .1, parse = TRUE, 
           label = expression(widehat(gamma)[1][0] + widehat(u)[1][4]), col = "violetred4", size = 6) +
  #Pendente escola 3 gamma_{10} + ugorro_{13}
  geom_segment(x = -1.5, xend = -.5, y = rectamm4(-1.5, 3), yend = rectamm4(-1.5, 3), 
               linetype = "solid", size = 1, col = "violetred4") +
  geom_segment(x = -.5, xend = -.5, y = rectamm4(-1.5, 3), yend = rectamm4(-.5, 3), 
               linetype = "solid", size = 1, col = "violetred4") +
  annotate("text", x = -.375, y = (rectamm4(-1.5, 3) + rectamm4(-.5, 3) ) / 2 - .85, parse = TRUE, 
           label = expression(widehat(gamma)[1][0] + widehat(u)[1][3]), col = "violetred4", size = 6, 
           angle = 10) +
  #Pendente asociada a SocialMin beta_2
  geom_segment(x = 1.25, xend = 1.25, y = rectamm4(1.25, 4), yend = rectamm4(1.25, 4, 1), 
               color = dark_2[4], arrow = arrow(length = unit(0.15, "inches")), linetype = "solid", size = .9) +
  annotate("text", x = 1.375, y = (rectamm4(1.35, 4) + rectamm4(1.35, 4, 1) ) / 2 + .15, 
           parse = TRUE, label = expression(widehat(beta)[2]), col = dark_2[4], size = 6) +
  theme(
    legend.title = element_text(size = 18),
    legend.text = element_text(size = 16),
    axis.title.x = element_text(size = 18),
    axis.text.x = element_text(size = 16),
    axis.title.y = element_text(size = 18),
    axis.text.y = element_text(size = 16))


###-------------------------------matesmm5,6,7: Variable contextual global------------

#-------------------------------------------------#
# Engadiremos Tamaño ao modelo matesmm3
# matesmm5 <- update(matesmm3, . ~ . + Tamaño)
matesmm5 <- lmer(NotaMates ~ StSE + SocialMin + Tamaño + (1 + StSE | Escola), REML = FALSE)
summary(matesmm5)
lrtest(matesmm3, matesmm5)

# Porcentaxes de estudantes con nota menor que 5
notas_menor5 <- numeric(7)
for (i in 1:length(levels(Escola))){
  notas_menor5[i] <- sum(NotaMates[Escola == levels(Escola)[i]] < 5)
}
round(notas_menor5 / table(Escola) * 100, 2) #porcentaxes de estudantes con nota menor que 5

#-------------------------------------------------#
# Engadiremos AmbDiscrim  ao modelo matesmm3
# matesmm6 <- update(matesmm3, . ~ . + AmbDiscrim )
matesmm6 <- lmer(NotaMates ~ StSE + SocialMin + AmbDiscrim + (1 + StSE | Escola), REML = FALSE)
summary(matesmm6)
lrtest(matesmm3, matesmm6)

#-------------------------------------------------#
# Engadiremos MaioriaMin  ao modelo matesmm3
# matesmm7 <- update(matesmm3, . ~ . + MaioriaMin )
matesmm7 <- lmer(NotaMates ~ StSE + SocialMin + MaioriaMin + (1 + StSE | Escola), REML = FALSE)
summary(matesmm7)
lrtest(matesmm3, matesmm7)

