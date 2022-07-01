
######################################################
#####   
#####        SCRIPT: CODIGO TFG RANOVA
##### INTRODUCION AOS MODELOS MIXTOS
##         CAPITULO 3: RANOVA
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


######################################
##
##   RANOVA
##
######################################

ranovamates <- lmer(NotaMates ~ (1 | Escola), data = mates7, REML = FALSE)
summary(ranovamates)
sigma2_eps_ran = summary(ranovamates)$sigma^2
sigma2_u_ran = as.data.frame(summary(ranovamates)$varcor)[1,4]
VT_ran = sigma2_u_ran + sigma2_eps_ran
VPC_ran = sigma2_u_ran / VT_ran

###-------------------------------GRAFICO VARCOMP------------
vcmates <- lm(NotaMates ~ 1, data = mates7) #eliminamos o efecto aleatorio das escolas
summary(vcmates)
mu_t <- vcmates$coef[[1]] #estimacion media global (a mostral)


mates7$Estudante <- 1:nrow(mates7)
attach(mates7)
eps_vc <- residuals(vcmates)

display.brewer.pal(n = 7, name = "Dark2") #Paleta de cores que se vai empregar
dark_2 <- brewer.pal(n = 7, name = "Dark2")


ggplot(mates7, aes(x = Estudante, y = NotaMates, color = Escola)) +
  geom_point(size = 1.5) +
  ylab("Nota en Matemáticas") +
  coord_cartesian(ylim = c(.5, 25)) +
  scale_color_brewer(palette = "Dark2") +
  #Media global
  geom_segment(x = 0, y = mean(NotaMates), xend = 307, yend = mean(NotaMates), size = 1.5, col = 1) +
  #Medias locais
  geom_segment(x = Estudante[17], y = NotaMates[17], xend = Estudante[17], 
               yend = NotaMates[17] - eps_vc[17], color = 1, arrow = arrow(), size = 1.25) +
  geom_segment(x = Estudante[64], y = NotaMates[64], xend = Estudante[64], 
               yend = NotaMates[64] - eps_vc[64], color = 1, arrow = arrow(), size = 1.25) +
  geom_segment(x = Estudante[190], y = NotaMates[190], xend = Estudante[190], 
               yend = NotaMates[190] - eps_vc[190], color = 1, arrow = arrow(), size = 1.25) +
  geom_segment(x = Estudante[229], y = NotaMates[229], xend = Estudante[229], 
               yend = NotaMates[229] - eps_vc[229], color = 1, arrow = arrow(), size = 1.25) +
  #Erros
  annotate("text", x = Estudante[17] + 21, y = (NotaMates[17] + mu_t)/2, 
           parse = TRUE, label = expression(widehat(epsilon)[17][","][1]), col = 1, size = 7) +
  annotate("text", x = Estudante[64] + 21, y = (NotaMates[64] + mu_t)/2, 
           parse = TRUE, label = expression(widehat(epsilon)[20][","][2]), col = 1, size = 7) +
  annotate("text", x = Estudante[190] + 21, y = (NotaMates[190] + mu_t)/2, 
           parse = TRUE, label = expression(widehat(epsilon)[42][","][5]), col = 1, size = 7) +
  annotate("text", x = Estudante[229] + 21, y = (NotaMates[229] + mu_t)/2, 
           parse = TRUE, label = expression(widehat(epsilon)[24][","][6]), col = 1, size = 7) +
  #Observacions
  geom_point( aes(x = Estudante[17], y = NotaMates[17] ), col = dark_2[1], size = 5 ) +
  geom_point( aes(x = Estudante[64], y = NotaMates[64] ), col = dark_2[2], size = 5 ) +
  geom_point( aes(x = Estudante[190], y = NotaMates[190] ), col = dark_2[5], size = 5 ) +
  geom_point( aes(x = Estudante[229], y = NotaMates[229] ), col = dark_2[6], size = 5 ) +
  annotate("text", x = Estudante[17] - 11, y = NotaMates[17] - .75, parse = TRUE, label = 
             expression(Y[17][","][1]), col = dark_2[1], size = 6) +
  annotate("text", x = Estudante[64] + 22, y = NotaMates[64], parse = TRUE, label = 
             expression(Y[20][","][2]), col = dark_2[2], size = 6) +
  annotate("text", x = Estudante[190] - 11, y = NotaMates[190] - .75, parse = TRUE, label = 
             expression(Y[42][","][5]), col = dark_2[5], size = 6) +
  annotate("text", x = Estudante[229] + 22, y = NotaMates[229], parse = TRUE, label = 
             expression(Y[24][","][6]), col = dark_2[6], size = 6) +
  #Media global
  annotate("text", x = 320, y = mean(NotaMates), parse = TRUE, label = expression(widehat(mu)), size = 7.25) +
  theme(
    legend.title = element_text(size = 18),
    legend.text = element_text(size = 16),
    axis.title.x = element_blank(),
    axis.text.x = element_blank(),
    axis.title.y = element_text(size = 18),
    axis.text.y = element_text(size = 16))


### Contraste sobre o efecto da escola
LR <- -2 * logLik(vcmates)[1] - (-2 * logLik(ranovamates))[1]
1 - pchisq(LR, df = 1) # o efecto aleatorio da escola e significativo

###-------------------------------GRAFICO CONTRASTE EFECTO GRUPOS------------
x <- seq(0, 45, length = 1000)
y <- dchisq(x, df = 1)
df <- data.frame(x,y)
ggplot(df, aes(x = x, y = y)) +
  geom_line(color = "orange", size = 1.25) +
  xlab(" ") +
  coord_cartesian(ylim = c(0, .15), clip = "off") +
  geom_hline(yintercept = 0, col= "green2") +
  geom_vline(xintercept = 0, col= "green2") +
  geom_segment(x = LR, y = -0.004, xend = LR, yend = -.0115, col= "tomato3", linetype = "solid", 
               size = 1.5) +
  annotate("text", x = 6, y = .1, parse = TRUE, label = expression(chi[1]^2), col = "orange", size = 17) +
  annotate("text", x = LR, y = -0.017, parse = TRUE, label = expression(LR[obs]), col = "tomato3", 
           size = 10) +
  annotate("text", x = LR + 6, y = .007, parse = TRUE, label = expression(1.96e-09), col = "lightsalmon",
           size = 8) +
  theme_classic() +
  theme(
    axis.title.x = element_text(size = 10),
    axis.text.x = element_text(size = 17),
    axis.title.y = element_blank(),
    axis.text.y = element_text(size = 17))

###-------------------------------CONXUNTO DE DATOS u0df ------------
u0 <- ranef(ranovamates, postVar = TRUE) #estimacion dos residuos a nivel de escola u^gorro_{0j} e das suas
  # desviacions tipicas asociadas. postVar=TRUE crea un obxecto de efectos aleatorios contendo a matriz
  # de varianzas covarianzas no atributo postVar.
u0se <- sqrt(attr(u0[[1]], "postVar")[1, , ]) #desviacions tipicas dos u0
escolaidentif <- rownames(u0[[1]])
u0df <- cbind(escolaidentif, u0[[1]], u0se)
colnames(u0df) <- c("escolaidentif", "u0", "u0se")
u0df <- u0df[order(u0df$u0), ] #Ordeamos por orde ascendente dos valores de u0
u0df <- cbind(u0df, c(1:dim(u0df)[1])) #Nova columna contendo ranking
colnames(u0df)[4] <- "u0pos" #u0pos nome da nova columna
#Transformamos escolaidentif a clase numeric para poder ordear de novo
for (k in 1:nrow(u0df)){
  numero <- as.numeric(strsplit(u0df$escolaidentif[k], split = "")[[1]][2])
  u0df$escolaidentif[k] <- numero
}
#Reordeamos a taboa en funcion do identificador escola, tal  como estaba ao inicio pero coa nova columna
u0df <- u0df[order(u0df$escolaidentif), ] #reordenacion
estloc <- summary(ranovamates)$coef[1] + u0df$u0 #estimacion media para cada escola
u0df$estloc <- estloc
u0df$escolaidentif <- paste("E", 1:7, sep = "") #nomes escolas
mu_local <- numeric(7)
for (i in 1:length(levels(mates7$Escola))){
  mu_local[i] <- mean(NotaMates[Escola == levels(mates7$Escola)[i]])
} #vector de medias locais
naive_res <- mu_local - summary(ranovamates)$coef[1] #residuos naive
u0df$naive_res <- naive_res
shrink <- numeric(7) #coeficientes de fiabilidade
for (i in 1:length(levels(mates7$Escola))){
  shrink[i] <- u0[[1]][i, 1] / naive_res[i]
}
#Outra forma de calcular coeficientes de fiabilidade:
shrink2 <- numeric(7)
for (i in 1:length(levels(mates7$Escola))){
  shrink2[i] <- sigma2_u_ran / (sigma2_u_ran + sigma2_eps_ran / as.data.frame(table(Escola))[i, 2])
}
print(shrink2) #efectivamente moi similar

u0df$shrinkage <- shrink
kable(u0df, format = "pipe", digits = 3, row.names = FALSE) # format = "latex" para codigo latex


###-------------------------------Plot de Eiruga ------------
# Mostra os efectos da escola ordeados segun o ranking xunto con intervalos de confianza ao 95%
ggplot(u0df, aes(x = u0pos, y = u0)) +
  geom_segment(x = u0df$u0pos, y = u0df$u0 - 1.96*u0df$u0se, xend = u0df$u0pos, 
               yend = u0df$u0 + 1.96*u0df$u0se, size = 1, col = "darkcyan") +
  geom_point(size = 3, col = "tomato3") +
  xlab("") +
  ylab("Residuos a nivel de Escola") +
  scale_x_continuous(breaks = 1:7) +
  geom_hline(yintercept = 0, size = 1) +
  annotate("text", x = u0df$u0pos + 0.3, y = u0df$u0, parse = TRUE, label = paste("E", 1:7, sep = ""), 
           col = "tomato3", size = 7) +
  theme(
    axis.title.x = element_text(size = 20),
    axis.text.x = element_text(size = 18),
    axis.title.y = element_text(size = 20),
    axis.text.y = element_text(size = 18))


###-------------------------------Figura RANOVA ------------
eps_ran <- residuals(ranovamates)

ggplot(mates7, aes(x = Estudante, y = NotaMates, color = Escola)) +
  geom_point(size = 1.25) +
  coord_cartesian(xlim = c(0, 350), ylim = c(1, 25)) +
  xlab("Indentificador de estudante") + 
  ylab("Nota en Matemáticas") + 
  scale_color_brewer(palette = "Dark2") +
  #Media global
  geom_segment(x = -7, y = summary(ranovamates)$coef[1], xend = 317, 
               yend = summary(ranovamates)$coef[1], size = 1.35, col = 1) +
  #Medias locais
  geom_segment(x = -7, y = estloc[3], xend = 294, yend = estloc[3], lwd = 1, linetype = "twodash", 
               color = dark_2[3]) +
  geom_segment(x = -7, y = estloc[4], xend = 294, yend = estloc[4], lwd = 1, linetype = "twodash",
               color = dark_2[4]) +
  geom_segment(x = 0, y = estloc[5], xend = 294, yend = estloc[5], lwd = 1, linetype = "twodash", 
               color = dark_2[5]) +
  geom_segment(x = 0, y = estloc[7], xend = 294, yend = estloc[7], lwd = 1, linetype = "twodash", 
               color = dark_2[7]) +
  #Frechas
  geom_segment(x = Estudante[72], y = NotaMates[72], xend = Estudante[72], 
               yend = NotaMates[72] - eps_ran[72], color = 1, arrow = arrow(), size = 1.15) +
  geom_segment(x = Estudante[140], y = NotaMates[140], xend = Estudante[140], 
               yend = NotaMates[140] - eps_ran[140], color = 1, arrow = arrow(), size = 1.15) +
  geom_segment(x = Estudante[185], y = NotaMates[185], xend = Estudante[185], 
               yend = NotaMates[185] - eps_ran[185], color = 1, arrow = arrow(), size = 1.15) +
  geom_segment(x = Estudante[264], y = NotaMates[264], xend = Estudante[264], 
               yend = NotaMates[264] - eps_ran[264], color = 1, arrow = arrow(), size = 1.15) +
  #Erros
  annotate("text", x = Estudante[72] + 20, y = (NotaMates[72] + estloc[3])/2, parse = TRUE, 
           label = expression(widehat(epsilon)[7][","][3]), col = 1, size = 7) +
  annotate("text", x = Estudante[140] - 23, y = (NotaMates[140] + estloc[4])/2, parse = TRUE, 
           label = expression(widehat(epsilon)[26][","][4]), col = 1, size = 7) +
  annotate("text", x = Estudante[185] + 24.5, y = (NotaMates[185] + estloc[5])/2 - .1, parse = TRUE,
           label = expression(widehat(epsilon)[37][","][5]), col = 1, size = 7) +
  annotate("text", x = Estudante[264] - 23, y = (NotaMates[264] + estloc[7])/2 - .5, parse = TRUE, 
           label = expression(widehat(epsilon)[16][","][7]), col = 1, size = 7) +
  #Observacions
  geom_point(aes(x = Estudante[72], y = NotaMates[72]), col = dark_2[3], size = 5) +
  geom_point(aes(x = Estudante[140], y = NotaMates[140]), col = dark_2[4], size = 5) +
  geom_point(aes(x = Estudante[185], y = NotaMates[185]), col = dark_2[5], size = 5) +
  geom_point(aes(x = Estudante[264], y = NotaMates[264]), col = dark_2[7], size = 5) +
  annotate("text", x = Estudante[72] - 18, y = NotaMates[72] - .55, parse = TRUE, label = 
             expression(Y[7][","][3]), col = dark_2[3], size = 6) +
  annotate("text", x = Estudante[140] - 12, y = NotaMates[140] - .65, parse = TRUE, label = 
             expression(Y[26][","][4]), col = dark_2[4], size = 6) +
  annotate("text", x = Estudante[185] - 12, y = NotaMates[185] - .65, parse = TRUE, label = 
             expression(Y[37][","][5]), col = dark_2[5], size = 6) +
  annotate("text", x = Estudante[264] - 12, y = NotaMates[264] - .65, parse = TRUE, label = 
             expression(Y[16][","][7]), col = dark_2[7], size = 6) +
  #Media global simbolo
  annotate("text", x = 329, y = summary(ranovamates)$coef[1], parse = TRUE, 
           label = expression(widehat(mu)), size = 7) +
  #Medias locais simbolos
  annotate("text", x = 330, y = estloc[3] + .25, parse = TRUE, 
           label = expression(paste(widehat(mu) + widehat(u)[3], "=", widehat(mu)[3])), size = 6, 
           col = dark_2[3]) +
  annotate("text", x = 330, y = estloc[4], parse = TRUE, 
           label = expression(paste(widehat(mu) + widehat(u)[4], "=", widehat(mu)[4])), size = 6, 
           col = dark_2[4]) +
  annotate("text", x = 330, y = estloc[5] - .1, parse = TRUE, 
           label = expression(paste(widehat(mu) + widehat(u)[5], "=", widehat(mu)[5])), size = 6, 
           col = dark_2[5]) +
  annotate("text", x = 330, y = estloc[7], parse = TRUE, 
           label = expression(paste(widehat(mu) + widehat(u)[7], "=", widehat(mu)[7])), size = 6, 
           col = dark_2[7]) +
  #Residuos a nivel de escola u^gorro_{0j}
  geom_segment(x = -7, y = summary(ranovamates)$coef[1], xend = -7, yend = estloc[3], 
               color = dark_2[3], arrow = arrow(length = unit(0.2, "inches")), linetype = "solid", size = .9) +
  geom_segment(x = -7, y = summary(ranovamates)$coef[1], xend = -7, yend = estloc[4], 
               color = dark_2[4], arrow = arrow(length = unit(0.2, "inches")), linetype = "solid", size = .9) +
  geom_segment(x = 20, y = summary(ranovamates)$coef[1], xend = 20, yend = estloc[5], 
               color = dark_2[5], arrow = arrow(length = unit(0.15, "inches")), linetype = "solid", size = .75) +
  geom_segment(x = 20, y = summary(ranovamates)$coef[1], xend = 20, yend = estloc[7], 
               color = dark_2[7], arrow = arrow(length = unit(0.15, "inches")), linetype = "solid", size = .75) +
  #Simbolos residuos a nivel de escola u^gorro_{0j}
  annotate("text", x = 5, y = (summary(ranovamates)$coef[1] + estloc[3]) / 2 - .3, parse = TRUE, 
           label = expression(widehat(u)[3]), col = dark_2[3], size = 6.5) +
  annotate("text", x = 5, y = (summary(ranovamates)$coef[1] + estloc[4]) / 2, parse = TRUE, 
           label = expression(widehat(u)[4]), col = dark_2[4], size = 6.5) +
  annotate("text", x = 36, y = (summary(ranovamates)$coef[1] + estloc[5]) / 2, parse = TRUE, 
           label = expression(widehat(u)[5]), col = dark_2[5], size = 6.5) +
  annotate("text", x = 36, y = (summary(ranovamates)$coef[1] + estloc[7]) / 2, parse = TRUE, 
           label = expression(widehat(u)[7]), col = dark_2[7], size = 6.5) +
  theme(
    legend.title = element_text(size = 18),
    legend.text = element_text(size = 16),
    axis.title.x = element_blank(),
    axis.text.x = element_blank(),
    axis.title.y = element_text(size = 18),
    axis.text.y = element_text(size = 16))

