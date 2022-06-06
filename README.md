# Modelos_Mixtos_con_R
## Abstract / Resumo
<style>
red { color: red }
yellow { color: yellow }
</style>

<red> ----- English:</red>
----- English:

  In the Statistical field, models of regression are the main tool employed when estimating the relation among random variables is needed. In particular, we will see how one or several variables (that we will call explanatory variables) influence another variable (that we will call response variable).
	
A lot of databases concerning the fields of Education, Medicine or Environmental Sciences are hierarchically organized due to their own nature, so that individuals are nested in groups; for example, a set of students grouped by schools. It is obvious to think that individuals classified in a same group will tend to have a more similar behaviour than any individual from different groups, with less information in common. In these cases, models of classical regression stop being useful and the necessity to take into account the effect produced by these groupings in the response variable arises. The first proposals to study this type of data, without ignoring the existing groupings, are the models of analysis of variance (\textit{ANOVA}) or models of analysis of covariance (\textit{ANCOVA}); but these models are only interesting when the goal is to apply Statistical Inference techniques on certain features of the groups present in the database. 
	
Going even further in the analysis of hierarchical data, the present groups in the dataset can be considered a random sample of a bigger population of groups to make inference about all groups in general. In this case, the regression models \textit{ANOVA} and \textit{ANCOVA} stop being valid, and the named mixed models or multilevel models arises. Throughout this paper, mixed models will be introduced and it will be exposed their utility in the study of databases with a structure of two levels, where individuals are on the first level and are nested in groups on the second level, by incorporating random effects. To carry out this illustration, it will be used a real database that will be analysed employing the statistical tool R.

----- Galego:

No eido da Estatística, os modelos de regresión son a principal ferramenta empregada cando o que se precisa é estimar a relación entre variables aleatorias. En concreto, veremos como unha ou varias variables (que chamaremos variables explicativas) inflúen sobre outra variable (que chamaremos variable resposta).
	
Moitas bases de datos concernentes ao eido da Educación, a Medicina ou as Ciencias Medioambientais están xerarquicamente organizadas debido á propia natureza destas, de xeito que os individuos se atopan aniñados en grupos; como por exemplo, un conxunto de alumnas/os agrupados por escolas. É obvio pensar que individuos clasificados nun mesmo grupo tenderán a ter un comportamento máis semellante que uns individuos calesquera de grupos diferentes, con menos información en común. Nestes casos, os modelos de regresión clásicos deixan de ser útiles e xorde a necesidade de ter en conta o efecto que producen estas agrupacións na variable resposta. As primeiras propostas para estudar este tipo de datos, sen ignorar as agrupacións existentes, son os modelos de análise da varianza (coñecidos como modelos \textit{ANOVA}) ou modelos de análise da covarianza (coñecidos como modelos \textit{ANCOVA}); mais estes modelos só son interesantes cando o que se quere é aplicar técnicas da Inferencia Estatística sobre certas características dos grupos presentes na base de datos. 
	
Afondando aínda máis na análise de datos xerárquicos, os grupos presentes no conxunto de datos poden considerarse unha mostra aleatoria dunha poboación máis grande de grupos para facer Inferencia sobre os grupos en xeral. Neste caso, os modelos de regresión \textit{ANOVA} e \textit{ANCOVA} deixan de ser válidos, e xorden os denominados modelos mixtos ou modelos multinivel. Ao longo deste traballo introduciranse os modelos mixtos e poñerase de manifesto a súa utilidade para estudar bases de datos cunha estrutura de dous niveis, onde os individuos se atopan no primeiro nivel e están aniñados en grupos no segundo nivel, mediante a incorporación de efectos aleatorios. Para levar a cabo esta ilustración empregarase unha base de datos reais que será analizada empregando a ferramenta estatística R.

## Guía
No script *mates7_integro.R* atópase o código íntegro relativo a este TFG.

No script *mates7_clasica.R* atópase o código concernente á Estatística Clásica (Introdución e Capítulo 1: ANOVA e ANCOVA) relativo a este TFG.

No script *mates7_ranova.R* atópase o código concernente ao modelo RANOVA (Capítulo 3 do TFG).

No script *mates7_m_mixtos_cap4.R* atópase o código concernente ao Capítulo 4 deste TFG: Modelos mixtos con covariables relativas ao primeiro nivel.

No script *mates7_m_mixtos_cap5.R* atópase o código concernente ao Capítulo 5 deste TFG: Modelos mixtos con covariables relativas ao segundo nivel.
