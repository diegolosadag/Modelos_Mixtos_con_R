# Modelos_Mixtos_con_R
## Abstract / Resumo

----- English:

  In the Statistical field, regression models are the main tool employed when estimating the relation among random variables is needed. In particular, we will see the effect of one or several variables (that will be denoted by explanatory variables) in another variable (that will be denoted by response variable).
	
A lot of databases concerning the fields of Education, Medicine or Environmental Sciences are hierarchically organized due to their own nature, so that individuals are organized in groups; for example, a set of students grouped by schools. It is obvious to think that individuals classified in a same group will tend to have a more similar behaviour than any individual from different groups, with less information in common. In these cases, classical regression models stop being useful and the necessity to take into account the effect produced by these groupings in the response variable arises. The first proposals to study this type of data sets, without ignoring the existing groupings, are the models of analysis of variance (\textit{ANOVA}) or models of analysis of covariance (\textit{ANCOVA}); but these models are only interesting when the goal is to apply Statistical Inference techniques on certain features of the groups present in the database. 
	
Going even further in the analysis of hierarchical data, the present groups in the dataset can be considered a random sample of a bigger population of groups to make Inference about all groups in general. In this case, the regression models \textit{ANOVA} and \textit{ANCOVA} stop being valid, and the named mixed models or multilevel models arises. Throughout this work, mixed models will be introduced and it will be presented their utility in the study of databases with a structure of two levels, where individuals are on the first level and are nested in groups on the second level, by incorporating random effects. To carry out this illustration, it will be used a real database that will be analysed employing the statistical tool R.

----- Galego:

No eido da Estat??stica, os modelos de regresi??n son a principal ferramenta empregada cando o que se precisa ?? estimar a relaci??n entre variables aleatorias. En concreto, veremos como unha ou varias variables (que chamaremos variables explicativas) infl??en sobre outra variable (que chamaremos variable resposta).
	
Moitas bases de datos concernentes ao eido da Educaci??n, a Medicina ou as Ciencias Medioambientais est??n xerarquicamente organizadas debido ?? propia natureza destas, de xeito que os individuos se atopan ani??ados en grupos; como por exemplo, un conxunto de alumnas/os agrupadas/os por escolas. ?? obvio pensar que individuos clasificados nun mesmo grupo tender??n a ter un comportamento m??is semellante que uns individuos calesquera de grupos diferentes, con menos informaci??n en com??n. Nestes casos, os modelos de regresi??n cl??sicos deixan de ser ??tiles e xorde a necesidade de ter en conta o efecto que producen estas agrupaci??ns na variable resposta. As primeiras propostas para estudar este tipo de datos, sen ignorar as agrupaci??ns existentes, son os modelos de an??lise da varianza (co??ecidos como modelos \textit{ANOVA}) ou modelos de an??lise da covarianza (co??ecidos como modelos \textit{ANCOVA}); mais estes modelos s?? son interesantes cando o que se quere ?? aplicar t??cnicas da Inferencia Estat??stica sobre certas caracter??sticas dos grupos presentes na base de datos. 
	
Afondando a??nda m??is na an??lise de datos xer??rquicos, os grupos presentes no conxunto de datos poden considerarse unha mostra aleatoria dunha poboaci??n m??is grande de grupos para facer Inferencia sobre os grupos en xeral. Neste caso, os modelos de regresi??n \textit{ANOVA} e \textit{ANCOVA} deixan de ser v??lidos, e xorden os denominados modelos mixtos ou modelos multinivel. Ao longo deste traballo introduciranse os modelos mixtos e po??erase de manifesto a s??a utilidade para estudar bases de datos cunha estrutura de dous niveis, onde os individuos se atopan no primeiro nivel e est??n ani??ados en grupos no segundo nivel, mediante a incorporaci??n de efectos aleatorios. Para levar a cabo esta ilustraci??n empregarase unha base de datos reais que ser?? analizada empregando a ferramenta estat??stica R.

## Gu??a
Primeiramente, executar *requerimentos.R* para automatizar a instalaci??n dos paquetes necesarios.

No script *mates7_integro.R* at??pase o c??digo ??ntegro relativo a este TFG.

No script *mates7_clasica.R* at??pase o c??digo concernente ?? Estat??stica Cl??sica (Introduci??n e Cap??tulo 1: ANOVA e ANCOVA) relativo a este TFG.

No script *mates7_ranova.R* at??pase o c??digo concernente ao modelo RANOVA (Cap??tulo 3 do TFG).

No script *mates7_m_mixtos_cap4.R* at??pase o c??digo concernente ao Cap??tulo 4 deste TFG: Modelos mixtos con covariables relativas ao primeiro nivel.

No script *mates7_m_mixtos_cap5.R* at??pase o c??digo concernente ao Cap??tulo 5 deste TFG: Modelos mixtos con covariables relativas ao segundo nivel.
