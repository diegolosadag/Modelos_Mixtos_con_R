
######################################################
#####   
#####        SCRIPT: CODIGO TFG INTEGRO
##### INTRODUCION AOS MODELOS MIXTOS
##
###                   Diego Losada Gonzalez
######################################################


rm(list = ls())

######################################
##
##   Creacion da base de datos mates
##
######################################

library(nlme); library(ggplot2); library(lme4); library(car)
library(dplyr); library(RColorBrewer); library(viridis)
library(gtools); library(patchwork); library(knitr)
library(lmtest); library(ggthemes); library(lattice)
library(latex2exp)

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
### Taboa presentacion data.frame
kable(head(mates, 5), format = "pipe", digits = 2, row.names = FALSE) # format = "latex" para codigo latex
#(i) Variables relativas ao alumnado:
  # Escola: identificador da escola á que acode cada alumna/o, numeradas do 1 ao 160
  # SocialMin: variable dicotomica que toma os valores ``Si'' ou ``Non'' indicando se a/o estudante é membro dun 
    # grupo racial minoritario ou non, respectivamente.
  # Sexo: variable discreta indicadora do sexo que toma os valores  ``Home'' ou  ``Muller''.
  # StSE: status socio-economico da familia a que pertence a/o alumna/o.
  # NotaMates: nota acadada na materia de Matematicas.
#(ii) Variables relativas as escolas:
  # MStSE: media dos StSE das/os alumnas/os para cada centro educativo. Mide o status socio-económico promedio dos colexios.
  # Tamaño: numero de estudantes en cada escola.
  # Sector: variable categorica indicando o carácter publico ou privado da escola.
  # Particip: proporcion de estudantes da escola que participan no estudo academico, de 0 a 1.
  # AmbDiscrim: medida do ambiente discriminatorio presente na escola.
  # MaioriaMin: variable dicotómica que toma o valor 1 para as escolas con máis do 40% das/os matriculadas/os 
    # membros dun grupo racial minoritario, e 0 para o caso contrario.


### Validación da normalidade por grupos de escola
table(Escola)
length(levels(Escola)) #160 escolas
pval <- rep(0, length(levels(Escola)))
for (i in 1:length(levels(Escola))){
  aux <- NotaMates[Escola == levels(Escola)[i]]
  pval[i] <- shapiro.test(aux)$p.value
}
sum(pval < 0.001) #só 5 escolas

#Validacion da homoxeneidade de varianzas
leveneTest(lm(NotaMates ~ as.vector(Escola)), center = mean)


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

### Validación da normalidade por grupos de escola
pval <- rep(0, length(escolas_particip100))
alpha <- 0.001
for (i in 1:length(escolas_particip100)){
  aux <- NotaMates[Escola == escolas_particip100[i]]
  pval[i] <- shapiro.test(aux)$p.value
}
sum(pval < alpha)  #resultan significativamente normais

mates7 <- mates[alum_escolas_particip100, ] #evitamos sesgos de eleccion
dim(mates7) # 307 alumnas/os e 11 variables
mates7$Escola <- factor(mates7$Escola, levels = escolas_particip100) #corrixir levels
levels(mates7$Escola) <- paste("E", 1:7, sep = "") #Cambiase o nome destas 7 escolas
attach(mates7)
### Taboa presentacion data.frame
kable(head(mates7, 5), format = "pipe", digits = 2, row.names = FALSE) # format = "latex" para codigo latex


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
  geom_segment(x = 0.625, y = mu_local[1], xend = 	1.375, yend = mu_local[1], linetype = "dotted", col = "firebrick4") +
  geom_segment(x = 1.625, y = mu_local[2], xend = 	2.375, yend = mu_local[2], linetype = "dotted", col = "firebrick4") +
  geom_segment(x = 2.625, y = mu_local[3], xend = 	3.375, yend = mu_local[3], linetype = "dotted", col = "firebrick4") +
  geom_segment(x = 3.625, y = mu_local[4], xend = 	4.375, yend = mu_local[4], linetype = "dotted", col = "firebrick4") +
  geom_segment(x = 4.625, y = mu_local[5], xend = 	5.375, yend = mu_local[5], linetype = "dotted", col = "firebrick4") +
  geom_segment(x = 5.625, y = mu_local[6], xend = 	6.375, yend = mu_local[6], linetype = "dotted", col = "firebrick4") +
  geom_segment(x = 6.625, y = mu_local[7], xend = 	7.375, yend = mu_local[7], linetype = "dotted", col = "firebrick4") +
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
colnames(u0df) <- c("escolaidentif","u0","u0se")
u0df <- u0df[order(u0df$u0), ] #Ordeamos por orde ascendente dos valores de u0
u0df <- cbind(u0df, c(1:dim(u0df)[1])) #Nova columna contendo ranking
colnames(u0df)[4] <- "u0pos" #u0pos nome da nova columna
#Transformamos escolaidentif a clase numeric para poder ordear de novo
for (k in 1:nrow(u0df)){
  numero <- as.numeric(strsplit(u0df$escolaidentif[k], split = "")[[1]][2])
  u0df$escolaidentif[k] <- numero
}
#Reordeamos a taboa en funcion do identificador escola, tal  como estaba ao inicio pero coa nova columna
u0df <- u0df[order(u0df$escolaidentif), ]
estloc <- summary(ranovamates)$coef[1] + u0df$u0 #estimacion media para cada escola
u0df$estloc <- estloc
u0df$escolaidentif <- paste("E", 1:7, sep = "") #nomes escolas
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
  xlab(expression(paste("Indices dos ", widehat(u)[j] ," ordeados de menor a maior"))) +
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
  #Residuos a nivel de escola u^gorro_{j}, para sm = 0
  geom_segment(x = -1.65, xend = -1.65, y = rectamm2(-1.65), yend = rectamm2(-1.65, escola = 4), 
               color = dark_2[4], arrow = arrow(length = unit(0.15, "inches")), linetype = "solid", size = .9) +
  geom_segment(x = -1.60, xend = -1.60, y = rectamm2(-1.60), yend = rectamm2(-1.60, escola = 5), 
               color = dark_2[5], arrow = arrow(length = unit(0.14, "inches")), linetype = "solid", size = .9) +
  annotate("text", x = -1.535, y = (rectamm2(-1.525) + rectamm2(-1.525, escola = 4) ) / 2 - .25, 
           parse = TRUE, label = expression(widehat(u)[4]), col = dark_2[4], size = 6.5) +
  annotate("text", x = -1.445, y = (rectamm2(-1.445) + rectamm2(-1.445, escola = 5) ) / 2, 
           parse = TRUE, label = expression(widehat(u)[5]), col = dark_2[5], size = 6.5) +
  #Residuos a nivel de escola u^gorro_{j}, para sm = 1
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
  #Media local 4
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
  #Residuos a nivel de escola de intercepto u^gorro_{0j}, para sm = 0
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
  #Pendente escola 4 gamma_{10} + ugorro_{14} con sm = 0
  geom_segment(x = -1.5, xend = -.5, y = rectamm3(-1.5, 4), yend = rectamm3(-1.5, 4), 
               linetype = "solid", size = 1, col = "violetred4") +
  geom_segment(x = -.5, xend = -.5, y = rectamm3(-1.5, 4), yend = rectamm3(-.5, 4), 
               linetype = "solid", size = 1, col = "violetred4") +
  annotate("text", x = -.175, y = (rectamm3(-1.5, 4) + rectamm3(-.5, 4) ) / 2 - .1, parse = TRUE, 
           label = expression(widehat(gamma)[1][0] + widehat(u)[1][4]), col = "violetred4", size = 6) +
  #Pendente escola 4 gamma_{10} + ugorro_{14} con sm = 1
  geom_segment(x = -1.5, xend = -.5, y = rectamm3(-1.5, 4, 1), yend = rectamm3(-1.5, 4, 1), 
               linetype = "solid", size = 1, col = "violetred4") +
  geom_segment(x = -.5, xend = -.5, y = rectamm3(-1.5, 4, 1), yend = rectamm3(-.5, 4, 1), 
               linetype = "solid", size = 1, col = "violetred4") +
  annotate("text", x = -.175, y = (rectamm3(-1.5, 4, 1) + rectamm3(-.5, 4, 1) ) / 2 - .1, parse = TRUE, 
           label = expression(widehat(gamma)[1][0] + widehat(u)[1][4]), col = "violetred4", size = 6) +
  #Pendente escola 3 gamma_{10} + ugorro_{13}
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


######################################
##
##   Modelos mixtos con covariables relativas ao nivel 2
##
######################################

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
